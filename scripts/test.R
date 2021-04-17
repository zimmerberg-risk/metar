library(metar)
library(stringr)
library(data.table)

read_station(fi.name = "^dubai")

stn <- read_station(fi.lat = c(40, 50), fi.lon = c(5, 15))$icao
stn <- read_station(fi.lat = c(-40, 20), fi.lon = c(-20, 40))$icao

stn <- c("PTRO", "BIKF", "SAWH", "FTTJ", "YGEL")
stn <- c("PTRO", "PTYA", "BIKF", "YGEL", "LSZH", "LSZA", "FTTJ", "GABS", "GCXO", "NZSP", "LOWI", "UUDD", "GCXO", "WSSS", "BGTL", "SBMN", "OMDB")

id.folder <- "C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Data/metar"

void <- lapply(stn[], function(id.icao){

  # id.icao <- "OMDB"

  cat(id.icao, " ", match(id.icao, stn), "\n")
  date.start <- "2021-03-19"
  date.end <- "2021-04-18"
  dat.metar <- read_mesonet(id_icao = id.icao, date_start = date.start, date_end = date.end)
  if(nrow(dat.metar) == 0) return(NULL)
  dat.parsed <- parse_metar(x = dat.metar$metar, date = dat.metar$valid)
  dat.1 <- cbind(dat.parsed, dat.parsed[, metar_pw(pw)])

  file.name <- file.path(id.folder, sprintf("%s_%s.png", id.icao, date.end))

  png(file.name, width = 1600, height = 900, units = "px", res = 96)
  plot_metargram(dat = dat.1, cex = 1.2)
  dev.off()

})


stn <- metar::read_station(fi.icao = "LSZH")
dt.in <- read_mesonet(id_icao = "LSZH", date_start = Sys.time() - 3600*24*3, date_end = Sys.time() + 3600*24) #"RKPK"
dt <- parse_metar(x = dt.in$metar, date = dt.in$valid)

dt.pw <- metar_pw(pw = dt$pw)
dt.cld <- metar_clouds(cld = dt$cld)
dt.rvr <- metar_rvr(rvr = dt$rvr)
dt.comb <- cbind(dt, dt.pw, dt.cld, dt.rvr)

dt.1 <- merge(stn, dt.comb, by = "icao")

head(dt.1[cld == "IMC", .(icao, name, metar, wx, pw, cld, qnh, fx, ff, rvr)][order(-fx)], 30)

