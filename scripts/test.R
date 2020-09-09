library(metar)
library(data.table)
library(tidyverse)

metar::read_station(fi.lat = c(38, 41), fi.lon = c(-105, -103))
metar::read_station(fi.name = "^Geno")

dt.in <- read_mesonet(id_icao = "LIMJ", date_start = Sys.time() - 3600*24*3, date_end = Sys.time() + 3600*24) #"RKPK"


dt.in <- read_csv("C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Kunden/05 MeteoSchweiz/AutoMETAR/metar_LSZH_2000_2020.txt", guess_max = 1e5)
dt.in <- dt.in %>% filter(valid > "2017-01-01")

dt <- parse_metar(x = metar.test)
dt <- parse_metar(x = dt.in$metar, date = dt.in$valid)

dt$pw
dt$vis
dt[, unique(tt)]
dt[, unique(pw)]
dt[is.na(qnh)]

metar_clouds(cld = dt$cld)
metar_rvr(rvr = dt$rvr)
metar_pw(pw = dt$pw)

