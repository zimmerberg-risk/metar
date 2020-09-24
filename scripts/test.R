library(metar)
library(data.table)
library(tidyverse)
library(rlang)

stn <- metar::read_station(fi.lat = c(40, 50), fi.lon = c(5, 15))
stn <- metar::read_station(fi.lat = c(-180, 180), fi.lon = c(-90, 90))
metar::read_station(fi.name = "^Kefa")

dt.in <- read_mesonet(id_icao = "LSZH", date_start = Sys.time() - 3600*24*3, date_end = Sys.time() + 3600*24) #"RKPK"
dt.in <- read_mesonet(id_icao = "LGKF", date_start = as.POSIXct("2000-01-01", tz = "UTC"), date_end = Sys.time() + 3600*24) #"RKPK"
dt.in <- metar_latest(id_icao = stn$icao, latest = TRUE)

# dt.in <- read_csv("C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Kunden/05 MeteoSchweiz/AutoMETAR/metar_LSZH_2000_2020.txt", guess_max = 1e5)
# dt.in <- dt.in %>% filter(valid > "2017-01-01")
dt <- parse_metar(x = metar.test)
dt <- parse_metar(x = dt.in$metar, date = dt.in$valid)

dt <- parse_metar(x = dt.in)
dt.pw <- metar_pw(pw = dt$pw)
dt.cld <- metar_clouds(cld = dt$cld)
#dt.rvr <- metar_rvr(rvr = dt$rvr)
dt.comb <- cbind(dt, dt.pw, dt.cld)
dt.1 <- merge(stn, dt.comb, by = "icao")

head(dt.1[cld == "IMC", .(icao, name, metar, wx, pw, cld, qnh, fx, ff, rvr)][order(-fx)], 30)

unique(dt.1$cld)

# Checks
dt[grepl("^KSF", icao)]

dt[is.na(vis), .(vis, wx, metar)]
dt[!is.na(vvis)]
hist(dt$fx, 50)
hist(dt$fx, 50)
dt.comb[, .N, wx]


dt.comb[is_obsc == 1, .(time, pw, metar, recent, pw_grp_1, pw_grp_2)]
dt.comb[grepl("TS", pw), .(time, pw, TS, VC_TS, metar, recent, pw_grp_1, pw_grp_2)]
dt.comb[!is.na(ws)]
dt.comb[grepl("CAVOK", wx),]
dt.comb[, .N, is_ceiling]
