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
    title(bquote(.(main[1])),col.main=col.main[1], ...)
  } else {
    # print first
    title(bquote(.(main[1]) * phantom(.(paste0(main[2:n],collapse=collapse)))),col.main=col.main[1], ...)

    # print middle
    if(n > 2) {
      for(i in 2:(n-1)) {
        title(bquote(phantom(.(paste0(main[1:(i-1)],collapse=collapse))) * .(main[i]) * phantom(.(paste0(main[(i+1):n],collapse=collapse)))),col.main=col.main[i], ...)
      }
    }

    # print last
    title(bquote(phantom(.(paste0(main[1:(n-1)],collapse=collapse))) * .(main[n])),col.main=col.main[n], ...)
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

  dat.stn <- read_station(fi.icao = dat$icao[1])
  title <- sprintf("%s | %s | %s | %1.0fm AMSL", dat.stn$icao, dat.stn$name, dat.stn$ctry, dat.stn$z)

  # Layout
  mat <- matrix(1:5, 5, 1, byrow = TRUE)

  # Plot
  def.par <- graphics::par(no.readonly = TRUE) # save default, for resetting...
  graphics::layout(mat, heights = c(0.05, rep((1 - 0.05)/4, 4)))

  # Title
  graphics::par(mai = c(0, 0.4, 0.3, 0.1)*cex)
  graphics::plot.new()
  graphics::mtext(title, side = 3, line = 0, cex=cex*1.75, font=2, adj = 0, xpd = TRUE)
  graphics::mtext("METARgram", side = 3, line = 0, cex=cex, font=2, adj = 1, xpd = TRUE)

  graphics::par(mai = c(0.3, 0.4, 0.4, 0.1)*cex, cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex*1.2,
                mgp = c(2, .5, 0)/cex, tcl = -0.35*cex)


  # Temperature/Dew point
  title.txt <- c("Temperature [°C]: ", "Temperature | ", "Dew Point")
  title.col <-  c("black", "black", "dodgerblue2")

  plot(dat$time, dat$tt, ylim = range(c(dat$tt, dat$td), na.rm = TRUE), type = "n", ann = FALSE)
  multicolor.title(title.txt, title.col, adj = 0, line = .70, font = 2)
  graphics::grid(nx = NA, ny = NULL)
  graphics::axis.POSIXct(1, dat$time, tck=1, lty = "dotted", col = "lightgray")
  graphics::lines(dat$time, dat$tt, type = "l", lty = 1)
  graphics::lines(dat$time, dat$td, type = "l", lty = 1, col = "dodgerblue2")
  graphics::box()

  # Wind
  title.txt <- c("Wind [kn]: ", "Speed | ", "Gusts")
  title.col <-  c("black", "black", "dodgerblue2")

  plot(dat$time, dat$ff, ylim = range(c(dat$ff, dat$fx), na.rm = TRUE), type = "n", ann = FALSE)
  multicolor.title(title.txt, title.col, adj = 0, line = .70, font = 2)
  graphics::grid(nx = NA, ny = NULL)
  graphics::axis.POSIXct(1, dat$time, tck=1, lty = "dotted", col = "lightgray")
  graphics::lines(dat$time, dat$ff, type = "l", lty = 1)
  graphics::points(dat$time, dat$fx, pch = "+", col = "dodgerblue2")
  graphics::box()

  # Pressure
  title.txt <- c("Pressure QNH [°C]", "")
  title.col <-  c("black", "black")

  plot(dat$time, dat$qnh, ylim = range(dat$qnh, na.rm = TRUE), type = "n", ann = FALSE)
  multicolor.title(title.txt, title.col, adj = 0, line = .70, font = 2)
  graphics::grid(nx = NA, ny = NULL)
  graphics::axis.POSIXct(1, dat$time, tck=1, lty = "dotted", col = "lightgray")
  graphics::lines(dat$time, dat$qnh, type = "l")
  graphics::box()

  # dat$dqnh6 <- dat$qnh - lag(dat$qnh, 6)
  # m <- mean(range(dat$qnh, na.rm = TRUE))
  # abline(h = m)
  # graphics::lines(dat$time, scales::rescale(dat$dqnh6, to = range(dat$qnh, na.rm = TRUE)), type = "l", lty = 3)
  # t <- pretty(range(dat$qnh, na.rm = TRUE), 5)
  # axis(4, at = t, labels = scales::rescale(t, to = range(dat$qnh, na.rm = TRUE)))

  # Vis

  dat.FG <- dat[FG==1]
  dat.LIQUID <- dat[is_liquid ==1]
  dat.SOLID <- dat[is_solid==1]
  dat.TS <- dat[TS==1]
  title.txt <- c("Visibility [km]: ", "Meteorological | ", "Vertical", "Weather: ", " Fog", " Liquid", " Solid", " Thunderstorm")
  title.col <-  c("black", "black", "dodgerblue2", "black", "#cccccc", "darkblue", "magenta", "firebrick")

  plot(dat$time, dat$vis, ylim = c(0.1, 3e1), type = "n", ann = FALSE, log = "y", yaxt = "n")
  multicolor.title(title.txt, title.col, adj = 0, line = .70, font = 2)
   # Weather
  ymax <- 10^par("usr")[4]
  ymin <- 10^par("usr")[3]
  graphics::rect(xleft = dat.FG$time, xright = dat.FG$time+3600/2, ybottom = rep(ymin, nrow(dat.FG)), ytop = rep(ymax, nrow(dat.FG)), col = "#cccccc", lty = 0)
  graphics::rect(xleft = dat.LIQUID$time, xright = dat.LIQUID$time+3600/2, ybottom = rep(ymin, nrow(dat.LIQUID)), ytop = rep(ymax, nrow(dat.LIQUID)), col = "lightblue", lty = 0)
  graphics::rect(xleft = dat.SOLID$time, xright = dat.SOLID$time+3600/2, ybottom = rep(ymin, nrow(dat.SOLID)), ytop = rep(ymax, nrow(dat.SOLID)), col = "#ffccff", lty = 0)
  graphics::rect(xleft = dat.TS$time, xright = dat.TS$time+3600/2, ybottom = rep(ymin, nrow(dat.TS)), ytop = rep(ymax, nrow(dat.TS)), col = "firebrick", lty = 0)

  graphics::grid(nx = NA, ny = NULL)
  graphics::axis.POSIXct(1, dat$time, tck=1, lty = "dotted", col = "lightgray")
  graphics::axis(2, c(100, 500, 1000, 2000, 5000, 10000)/1e3, tck=1, lty = "dotted", col = "lightgray")
  graphics::lines(dat$time, dat$vis/1e3, type = "s")
  graphics::points(dat$time, dat$vvis, pch = "+", col = "dodgerblue2")
  graphics::box()

  graphics::par(def.par)  #- reset to default

}

library(metar)
#"PTRO" "PTYA"
id.icao <- "LSZH"
date.start <- as.POSIXct("2021-04-10")
date.end <- as.POSIXct("2021-04-15")
dat.metar <- read_mesonet(id_icao = id.icao, date_start = date.start, date_end = date.end)
dat.parsed <- parse_metar(dat.metar$metar)
dat.parsed$time <- dat.metar$valid

dat.1 <- cbind(dat.parsed, dat.parsed[, metar_pw(pw)])

file.name <- file.path("C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Data/metar", sprintf("%s_%s.png", id.icao, format(date.end, "%Y%m%d%H%M")))
png(file.name, width = 1920, height = 1080, units = "px", res = 96)
plot_metargram(dat = dat.1[(fx < 100) | is.na(fx)], cex = 1.2)
dev.off()

