
## --------------------------------------------------- Auxilliary  ---------------------------------------------------

#' Set Alpha
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
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
    graphics::title(bquote(.(main[1])), col.main=col.main[1], ...)
  } else {
    # print first
    graphics::title(bquote(.(main[1]) * phantom(.(paste0(main[2:n],collapse=collapse)))), col.main=col.main[1], ...)

    # print middle
    if(n > 2) {
      for(i in 2:(n-1)) {
        graphics::title(bquote(phantom(.(paste0(main[1:(i-1)],collapse=collapse))) * .(main[i]) * phantom(.(paste0(main[(i+1):n],collapse=collapse)))),
                        col.main=col.main[i], ...)
      }
    }

    # print last
    graphics::title(bquote(phantom(.(paste0(main[1:(n-1)],collapse=collapse))) * .(main[n])), col.main=col.main[n], ...)
  }
}

#' Fill time gaps
#'
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
#' @param x time vector
#' @export
#'
fill_time_gaps <- function(x){
  dtime <- as.numeric(diff(x), "secs")
  dtime.pick <- as.numeric(names(sort(table(dtime), decreasing = TRUE)[1]))
  gaps <- which(dtime > dtime.pick)
  fill.seq <- sapply(gaps, function(i) seq(x[i], x[i + 1], sprintf("%1.0f sec", dtime.pick)), simplify = FALSE)
  fill.seq <- do.call(c, fill.seq)
  sort(c(x, fill.seq))
}

#' Determine value range
#'
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
#' @param x vector
#' @param para parameter
#' @export
#'
get_range <- function(x, para){
  def.para <- metar.para[id_para == para]
  if(all(is.na(x))){
    return(c(def.para$min, def.para$max))
  }
  range(x, na.rm = TRUE)
}

## --------------------------------------------------- Plot  ---------------------------------------------------


#' Plot METARgram
#'
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
#' @param dat data set
#' @param cex global text size
#' @param attribution data source
#' @export
#'
plot_metargram <- function(dat, cex = .9, attribution = "Data: Iowa State Univ. mesonet.agron.iastate.edu/"){

  if(nrow(dat) < 2 | all(is.na(dat$tt)) ){
    warning("No data found - returning empty data table")
    return(data.table())
  }
  # Fill time gaps
  setorder(dat, time)
  # dat[, time := lubridate::floor_date(time, "15 min")]
  # dat <- unique(dat, by = "time")
  dt.time <- data.table(time = fill_time_gaps(x = dat$time))
  dt.time$icao <- dat$icao[1]
  dat <- dat[dt.time, on = c("time", "icao")]

  # Map airport info
  dat <- metar.stn[dat, on = "icao"]

  # Average time interval
  dt <- as.numeric(stats::median(diff(dat$time), na.rm = TRUE), "secs")
  time.ticks <- pretty(dat$time, 8)
  time.int.tick <- as.numeric(median(diff(time.ticks)), "days")

  # Determine axis formats as axis.POSIXct does not work as expected
  time.int <- c(0, 0.5, 1, 2, 5, 16, 50, 100, 1000, 10000) # in hours
  time.formats <- c("%H:%M", "%H", "%d. %H", "%d", "%d.%m", "%m", "%m.%Y", "%Y", "%Y")
  time.format <- time.formats[findInterval(time.int.tick, time.int)]
  time.str <- paste(format(range(dt.time$time), "%Y-%m-%d %H:%M"), collapse = " - ")

  # Layout
  fill.alpha <- 0.2
  sub.cex <- .9*cex
  title <- sprintf("%s | %s | %s | %1.0fm | \U03BB %1.2f\U00B0 \U03A6 %1.2f\U00B0",
    dat$icao[1], dat$ap_name[1], dat$ctry[1], dat$elev[1], dat$lon[1], dat$lat[1])
  mat <- matrix(1:5, 5, 1, byrow = TRUE)

  # Plot
  def.par <- graphics::par(no.readonly = TRUE) # save default, for resetting...
  graphics::layout(mat, heights = c(0.06, rep((1 - 0.06 - 0.02)/4, 3),(1 - 0.06 - 0.02)/4 + 0.02))

  # Title
  graphics::par(mai = c(0, 0.5, 0.3, 0.1)*cex)
  graphics::plot.new()
  graphics::mtext(title, side = 3, line = 0, cex = cex*1.3, font=2, adj = 0, xpd = TRUE)
  graphics::mtext(time.str, side = 3, line = -1.5, cex = cex, font=2, adj = 0, xpd = TRUE)
  graphics::mtext(sprintf("METARgram: https://github.com/m-saenger/metar\n%s", attribution), side = 3, line = 0, cex=cex*.8, font=1, adj = 1, xpd = TRUE)

  graphics::par(mai = c(0.1, 0.5, 0.3, 0.1)*cex, cex = cex, cex.lab = cex, cex.axis = cex*.9, cex.main = cex*1.0,
                mgp = c(2, .5, 0)/cex, tcl = -0.25*cex)


  # Temperature/Dew point
  dt.rh <- dat[, .(time, rh = wx_rh(tt, td))]
  brks <- c(0, 20, seq(25, 65, 5), 70, 100)
  cols <- grDevices::colorRampPalette(c("firebrick", "goldenrod", "white"))(length(brks) - 1)
  dt.rh[, col := cut(rh, brks, cols)]

  title.txt <- c("Temperature | ", "Dew Point", " [\U0B0 C]")
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
  graphics::axis.POSIXct(1, dat$time, time.ticks, tck=1, lty = "dotted", col = "lightgray", labels = FALSE)
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
  ylim <- range(get_range(dat$ff, "ff"), get_range(dat$fx, "fx"))

  plot(dat$time, dat$ff, ylim = ylim, type = "n", ann = FALSE,  xaxs="i", yaxt = "n", xaxt = "n")
  ylim <- c(graphics::par("usr")[3], graphics::par("usr")[4])
  multicolor.title(title.txt, title.col, adj = 0, line = .70, font = 2)
  multicolor.title(title.2.txt, title.2.col, collapse = " ", adj = 1, line = .70, font = 2)
  graphics::rect(xleft = dat.wind$time, xright = shift(dat.wind$time, type = "lag"), ybottom = ylim[1], ytop = ylim[2],
      col = set_alpha(dat.wind$col, fill.alpha), lty = 0)
  graphics::grid(nx = NA, ny = NULL)
  graphics::axis(2, las = 2)
  graphics::axis.POSIXct(1, dat$time, time.ticks, tck=1, lty = "dotted", col = "lightgray", labels = FALSE)
  graphics::lines(dat$time, dat$ff, type = "l", lty = 1)
  graphics::points(dat$time, dat$fx, pch = "+", col = "dodgerblue2")
  graphics::box()

  # Pressure
  lag <- as.integer(3600*3/dt)
  brks <- c(-25, -10, -7, -5, -3, -2, -1, -0.5, 0.5, 1, 2, 3, 5, 7, 10, 25)
  cols <- grDevices::colorRampPalette(c("dodgerblue2", "white", "firebrick"))(length(brks) - 1)
  dt.p <- dat[, .(time, dt = as.numeric(time - shift(time, lag), "secs"), dp = qnh - shift(qnh, lag))]
  dt.p[, col := cut(dp, brks, cols)]

  title.1.txt <- c("Pressure QNH [hPa]", "")
  title.1.col <-  c("black", "black")
  title.2.txt <- c("Pressure Tendency [hPa/3h]", brks)
  title.2.col <-  c("black",  grDevices::colorRampPalette(c("dodgerblue2", "white", "firebrick"))(length(brks)))

  plot(dat$time, dat$qnh, ylim = get_range(x = dat$qnh, para = "qnh"), type = "n", ann = FALSE,  xaxs="i", yaxt = "n", xaxt = "n")
  ylim <- c(graphics::par("usr")[3], graphics::par("usr")[4])
  multicolor.title(title.1.txt, title.1.col, adj = 0, line = .70, font = 2)
  multicolor.title(title.2.txt, title.2.col, collapse = " ", adj = 1, line = .70, font = 2)
  graphics::rect(xleft = dt.p$time, xright = shift(dt.p$time, type = "lag"), ybottom = ylim[1], ytop = ylim[2],
      col = set_alpha(dt.p$col, 1), lty = 0)
  graphics::grid(nx = NA, ny = NULL)
  graphics::axis(2, las = 2)
  graphics::axis.POSIXct(1, dat$time, time.ticks, tck=1, lty = "dotted", col = "lightgray", labels = FALSE)
  graphics::lines(dat$time, dat$qnh, type = "l")
  graphics::box()


  # Vis /Signif Weather
  dt.sigwx <- metar.class[id_para == "sigwx"][c(1:10)]
  sigwx.id <- dt.sigwx$id_class
  sigwx.name <- dt.sigwx$short_class
  sigwx.col <- dt.sigwx$col
  sigwx.cond <- c(setNames(sigwx.col, sigwx.id))

  dat$ctrl <- 1
  dat.pw <- dat[,  .(time, pw, pw_col = sigwx.col[match(sigwx, sigwx.id)])]

  title.1.txt <- c("Visibility [km]: ", "Meteorological | ", "Vertical [100 ft] (+)")
  title.1.col <-  c("black", "black", "dodgerblue2")

  title.2.txt <- c("Sig. Weather: ", paste0(" ", sigwx.name))
  title.2.col <-  c("black", sigwx.col)


  graphics::par(mai = c(0.3, 0.5, 0.4, 0.1)*cex)
  plot(dat$time, dat$vis, ylim = c(0.1, 25), type = "n", ann = FALSE, log = "y",  xaxs="i", xaxt = "n", yaxt = "n")
  multicolor.title(title.1.txt, title.1.col, adj = 0, line = .70, font = 2)
  multicolor.title(title.2.txt, title.2.col, adj = 1, line = .70, font = 2)
  # Weather
  ylim <- 10^c(graphics::par("usr")[3], graphics::par("usr")[4])
  graphics::rect(xleft = dat.pw$time, xright = shift(dat.pw$time, type = "lag"), ybottom = ylim[1], ytop = ylim[2],
                 col = set_alpha(dat.pw$pw_col, 0.75), lty = 0)
  graphics::grid(nx = NA, ny = NULL)
  axis.POSIXct(1, x = dat$time, at = time.ticks, format = time.format, tck=1, lty = "dotted", col = "lightgray")

  graphics::axis(2, c(100, 300, 1000, 3000, 10000)/1e3, tick = TRUE, las = 2, col.ticks	= "black")
  abline(h = c(100, 300, 1000, 3000, 10000)/1e3, lty = 3, col = "lightgray")
  graphics::lines(dat$time, dat$vis/1e3, type = "s")
  graphics::points(dat$time, dat$vvis/1e2, pch = "+", col = "dodgerblue2")
  graphics::box()

  graphics::par(def.par)  #- reset to default

}
