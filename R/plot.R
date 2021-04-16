#' Set Alpha
#'
#' @param x Colour vector
#' @param alpha Transparency
#' @export
#' @examples set_alpha(x = c("red", "blue"), alpha = c(0.1, .3))
#'
set_alpha <- function(x, alpha = 1){
  rgb <- grDevices::col2rgb(x, alpha = T)
  rgb[4,] <- round(alpha*255)
  apply(rgb, 2, function(i) grDevices::rgb(i[1], i[2], i[3], maxColorValue = 255, alpha = i[4]))
}

#' Plot METARgram
#'
#' @author https://github.com/CollinErickson/ContourFunctions/blob/master/R/cf_multicolor_title.R
#' @param main text vector
#' @param col.main corresponding colour vector
#' @param collapse collapse
#' @param ... further arguments for title()
#' @export
#'
multicolor.title <- function(main, col.main, collapse='', ...) {
  if (length(main) != length(col.main)) {stop('main and col must have same length')}
  n <- length(main)
  if(n==1) {
    graphics::title(bquote(.(main[1])),col.main=col.main[1], ...)
  } else {
    # print first
    graphics::title(bquote(.(main[1]) * phantom(.(paste0(main[2:n],collapse=collapse)))),col.main=col.main[1], ...)

    # print middle
    if(n > 2) {
      for(i in 2:(n-1)) {
        graphics::title(bquote(phantom(.(paste0(main[1:(i-1)],collapse=collapse))) * .(main[i]) * phantom(.(paste0(main[(i+1):n],collapse=collapse)))),col.main=col.main[i], ...)
      }
    }

    # print last
    graphics::title(bquote(phantom(.(paste0(main[1:(n-1)],collapse=collapse))) * .(main[n])),col.main=col.main[n], ...)
  }
}

#' Plot METARgram
#'
#' @author M. Saenger
#' @param dat data set
#' @param cex global text size
#' @export
#'
plot_metargram <- function(dat, cex = .9){

  if(nrow(dat) < 1){
    warning("No data found - returning empty data table")
    return(data.table())
  }

  fill.alpha <- 0.2
  dat.stn <- read_station(fi.icao = dat$icao[1])
  title <- sprintf("%s | %s | %s | %1.0fm AMSL", dat.stn$icao, dat.stn$name, dat.stn$ctry, dat.stn$z)

  # Average time interval
  dt <- as.numeric(stats::median(diff(dat$time), na.rm = TRUE), "secs")

  # Layout
  mat <- matrix(1:5, 5, 1, byrow = TRUE)

  # Plot
  def.par <- graphics::par(no.readonly = TRUE) # save default, for resetting...
  graphics::layout(mat, heights = c(0.05, rep((1 - 0.05 - 0.02)/4, 3),(1 - 0.05 - 0.02)/4 + 0.02))

  # Title
  graphics::par(mai = c(0, 0.5, 0.3, 0.1)*cex)
  graphics::plot.new()
  graphics::mtext(title, side = 3, line = 0, cex=cex*1.75, font=2, adj = 0, xpd = TRUE)
  graphics::mtext("METARgram", side = 3, line = 0, cex=cex, font=2, adj = 1, xpd = TRUE)

  graphics::par(mai = c(0.1, 0.5, 0.3, 0.1)*cex, cex = cex, cex.lab = cex, cex.axis = cex*.9, cex.main = cex*1.2,
                mgp = c(2, .5, 0)/cex, tcl = -0.3*cex)


  # Temperature/Dew point
  dt.rh <- dat[, .(time, rh = wx_rh(tt, td))]
  brks <- c(0, 20, seq(25, 65, 5), 70, 100)
  cols <- grDevices::colorRampPalette(c("firebrick", "goldenrod", "white"))(length(brks) - 1)
  dt.rh[, col := cut(rh, brks, cols)]

  title.txt <- c("Temperature | ", "Dew Point", " [°C]")
  title.col <-  c( "black", "dodgerblue2", "black")
  title.2.txt <- c("Relative humidity [%]", brks)
  title.2.col <-  c("black",  grDevices::colorRampPalette(c("firebrick", "goldenrod", "white"))(length(brks)))

  plot(dat$time, dat$tt, ylim = range(c(dat$tt, dat$td), na.rm = TRUE), type = "n", ann = FALSE, xaxs="i", yaxt = "n", xaxt = "n")
  ylim <- c(graphics::par("usr")[3], graphics::par("usr")[4])
  multicolor.title(title.txt, title.col, adj = 0, line = .70, font = 2)
  multicolor.title(title.2.txt, title.2.col, collapse = " ", adj = 1, line = .70, font = 2)
  graphics::rect(xleft = dt.rh$time, xright = shift(dt.rh$time, type = "lag"), ybottom = ylim[1], ytop = ylim[2],
      col = set_alpha(dt.rh$col, .3), lty = 0)
  graphics::grid(nx = NA, ny = NULL)
  graphics::axis(2, las = 2)
  graphics::axis.POSIXct(1, x = dat$time, tck=1, labels = FALSE, lty = "dotted", col = "lightgray")
  graphics::lines(dat$time, dat$tt, type = "l", lty = 1)
  graphics::lines(dat$time, dat$td, type = "l", lty = 1, col = "dodgerblue2")
  graphics::box()

  # Wind
  r <- rev(grDevices::rainbow(8))
  wind.pal <- grDevices::colorRampPalette(c(r[3:8], r[1:2], r[3]))(361)
  dat.wind <- dat[, .(time, ff, col = wind.pal[as.integer(round(dir))+1])]
  dat.wind[, col := fifelse(ff == 0, NA_character_, col)]

  title.txt <- c("Wind [kn]: ", "Speed | ", "Gusts (+)")
  title.col <-  c("black", "black", "dodgerblue2")
  title.2.txt <- c("Wind Dir: ", "North", "Northeast", "East", "Southeast", "South", "Southwest", "West", " Northwest")
  title.2.col <-  c("black", wind.pal[seq(0, 315, 45)+1])


  plot(dat$time, dat$ff, ylim = range(c(dat$ff, dat$fx), na.rm = TRUE), type = "n", ann = FALSE,  xaxs="i", yaxt = "n", xaxt = "n")
  ylim <- c(graphics::par("usr")[3], graphics::par("usr")[4])
  multicolor.title(title.txt, title.col, adj = 0, line = .70, font = 2)
  multicolor.title(title.2.txt, title.2.col, collapse = " ", adj = 1, line = .70, font = 2)
  graphics::rect(xleft = dat.wind$time, xright = shift(dat.wind$time, type = "lag"), ybottom = ylim[1], ytop = ylim[2],
      col = set_alpha(dat.wind$col, fill.alpha), lty = 0)
  graphics::grid(nx = NA, ny = NULL)
  graphics::axis(2, las = 2)
  graphics::axis.POSIXct(1, x = dat$time, tck=1, labels = FALSE, lty = "dotted", col = "lightgray")
  graphics::lines(dat$time, dat$ff, type = "l", lty = 1)
  graphics::points(dat$time, dat$fx, pch = "+", col = "dodgerblue2")
  graphics::box()

  # Pressure
  lag <- as.integer(3600*3/dt)
  brks <- c(-15, seq(-9, 9, 2), 15)
  cols <- grDevices::colorRampPalette(c("dodgerblue2", "white", "firebrick"))(length(brks) - 1)
  dt.p <- dat[, .(time, dt = as.numeric(time - shift(time, lag), "secs"), dp = qnh - shift(qnh, lag))]
  dt.p[, col := cut(dp, brks, cols)]

  title.1.txt <- c("Pressure QNH [hPa]", "")
  title.1.col <-  c("black", "black")
  title.2.txt <- c("Pressure Tendency [hPA/3h]", brks)
  title.2.col <-  c("black",  grDevices::colorRampPalette(c("dodgerblue2", "white", "firebrick"))(length(brks)))


  plot(dat$time, dat$qnh, ylim = range(dat$qnh, na.rm = TRUE), type = "n", ann = FALSE,  xaxs="i", yaxt = "n", xaxt = "n")
  ylim <- c(graphics::par("usr")[3], graphics::par("usr")[4])
  multicolor.title(title.1.txt, title.1.col, adj = 0, line = .70, font = 2)
  multicolor.title(title.2.txt, title.2.col, collapse = " ", adj = 1, line = .70, font = 2)
  graphics::rect(xleft = dt.p$time, xright = shift(dt.p$time, type = "lag"), ybottom = ylim[1], ytop = ylim[2],
      col = set_alpha(dt.p$col, 1), lty = 0)
  graphics::grid(nx = NA, ny = NULL)
  graphics::axis(2, las = 2)
  graphics::axis.POSIXct(1, x = dat$time, tck=1, labels = FALSE, lty = "dotted", col = "lightgray")
  graphics::lines(dat$time, dat$qnh, type = "l")
  graphics::box()

  # Vis
  dat.pw <- dat[,
    .(time, pw = dplyr::case_when(
      TS == 1 ~ "firebrick",
      is_solid == 1 ~ "magenta",
      is_liquid == 1 ~ "dodgerblue2",
      FG == 1 ~ "#888888",
      TRUE ~ NA_character_
    ))
  ]

  title.1.txt <- c("Visibility [km]: ", "Meteorological | ", "Vertical (+)")
  title.1.col <-  c("black", "black", "dodgerblue2")
  title.2.txt <- c("Weather: ", " Fog", " Liquid", " Solid", " Thunderstorm")
  title.2.col <-  c("black", "#888888", "dodgerblue2", "magenta", "firebrick")

  graphics::par(mai = c(0.3, 0.5, 0.4, 0.1)*cex)
  plot(dat$time, dat$vis, ylim = c(0.1, 25), type = "n", ann = FALSE, log = "y",  xaxs="i", yaxt = "n")
  multicolor.title(title.1.txt, title.1.col, adj = 0, line = .70, font = 2)
  multicolor.title(title.2.txt, title.2.col, adj = 1, line = .70, font = 2)
  # Weather
  ylim <- 10^c(graphics::par("usr")[3], graphics::par("usr")[4])
  graphics::rect(xleft = dat.pw$time, xright = shift(dat.pw$time, type = "lag"), ybottom = ylim[1], ytop = ylim[2],
                 col = set_alpha(dat.pw$pw, fill.alpha), lty = 0)
  graphics::grid(nx = NA, ny = NULL)
  graphics::axis.POSIXct(1, dat$time, tck=1, lty = "dotted", col = "lightgray")
  graphics::axis(2, c(100, 300, 1000, 3000, 10000)/1e3, tck=1, las = 2, lty = "dotted", col = "lightgray")
  graphics::lines(dat$time, dat$vis/1e3, type = "s")
  graphics::points(dat$time, dat$vvis, pch = "+", col = "dodgerblue2")
  graphics::box()

  graphics::par(def.par)  #- reset to default

}

# library(metar); library(stringr) library(data.table)
# id.icao.list <- c("PTRO", "PTYA", "BIKF", "YGEL", "LSZH", "FTTJ", "SAWH", "GCXO")
#
# void <- lapply(id.icao.list, function(id.icao){
#   # id.icao <- "GCXO"
#   date.start <- "2021-03-17"
#   date.end <- "2021-04-17"
#   dat.metar <- read_mesonet(id_icao = id.icao, date_start = date.start, date_end = date.end)
#   dat.parsed <- parse_metar(dat.metar$metar)
#   dat.parsed$time <- dat.metar$valid
#
#   dat.1 <- cbind(dat.parsed, dat.parsed[, metar_pw(pw)])
#
#   file.name <- file.path("C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Data/metar", sprintf("%s_%s.png", id.icao, date.end))
#   png(file.name, width = 1920, height = 1080, units = "px", res = 96)
#   plot_metargram(dat = dat.1[(fx < 100) | is.na(fx)], cex = 1.1)
#   dev.off()
# })


