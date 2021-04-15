library(metar)
library(data.table)

t.from <- as.POSIXct("2021-03-11 00:00:00")
t.to <-  as.POSIXct("2021-03-13 23:50:00")
dir.out <- "~/Data/synop"

dt.stat <- metar::read_station(fi.lat = c(30,70), fi.lon = c(-20, 25))

dat.list <- parallel::mclapply(dt.stat$icao[], function(i){
  # i = "LSZH"
  print(i)
  dat <- metar::read_mesonet(i, date_start = t.from, date_end = t.to)
  if(nrow(dat) == 0) return(NULL)
  dat.parsed <- metar::parse_metar(dat$metar)
  dt.max <- dat.parsed[, .SD[which.max(fx)]][, .(time, fx = fx*1.852001, ff = ff*1.852001)]
  cbind(dt.stat[icao == i], dt.max)
})
dat <- rbindlist(dat.list)
dat[order(-fx, -ff)]

dat <- dat[!is.na(fx)][order(-fx, -ff)]
dat[, `:=`(fx = round(fx, 1), ff = round(ff, 1))]

readr::write_excel_csv(dat, file.path(dir.out, "peak_gusts_20210311_20210313.csv"), na = "", delim = ";")
