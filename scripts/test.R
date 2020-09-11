library(metar)
library(data.table)
library(tidyverse)
library(rlang)

metar::read_station(fi.lat = c(38, 41), fi.lon = c(-105, -103))
metar::read_station(fi.name = "^Geno")

dt.in <- read_mesonet(id_icao = "LSZH", date_start = Sys.time() - 3600*24*3, date_end = Sys.time() + 3600*24) #"RKPK"
dt.in <- read_mesonet(id_icao = "LSGG", date_start = as.POSIXct("2012-01-01", tz = "UTC"), date_end = Sys.time() + 3600*24) #"RKPK"

# dt.in <- read_csv("C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Kunden/05 MeteoSchweiz/AutoMETAR/metar_LSZH_2000_2020.txt", guess_max = 1e5)
# dt.in <- dt.in %>% filter(valid > "2017-01-01")

dt <- parse_metar(x = metar.test)

dt <- parse_metar(x = dt.in$metar, date = dt.in$valid)
dt <- dt[auto == 0]

dt.pw <- metar_pw(pw = dt$pw)
dt.cld <- metar_clouds(cld = dt$cld)
#dt.rvr <- metar_rvr(rvr = dt$rvr)
dt.comb <- cbind(dt, dt.pw, dt.cld)

# Checks
dt.comb[is_mixed == 1 & is_obsc == 1, .(time, pw, metar, recent, pw_grp_1, pw_grp_2)]
dt.comb[grepl("TS", pw), .(time, pw, TS, VC_TS, metar, recent, pw_grp_1, pw_grp_2)]
dt.comb[!is.na(ws)]
dt.comb[grepl("CAVOK", wx),]
dt.comb[, .N, is_ceiling]

`%!=na%` <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))
dt.comb[, `:=`(
  is_cavok = (!wx %!=na% "CAVOK")*1,
  `ff>15` = (ff > 15)*1,
  `fx>30` = (fx > 30)*1,
  is_ceiling = ifelse(is.na(ceiling), 0, 1),
  # pp_1 = (!pp_int %!=na% 1)*1,
  # pp_2 = (!pp_int %!=na% 2)*1,
  # pp_3 = (!pp_int %!=na% 3)*1,
  `vis<1k` = (vis < 1000)*1
)]

fi.1 <- c(`ff>15`=0, is_cavok=0, is_ceiling=1, is_mixed=1)
fi.1 <- c(`ff>15`=1, is_pp=1, TS=0, GS=1)

dt.comb[, fi_1 := apply(.SD, 1, function(i){sum(i == fi.1, na.rm = TRUE)}), .SDcols = names(fi.1)]
dt.comb[, fi_1_ok := ifelse(fi_1 == length(names(fi.1)), 1, 0)]
dt.comb[, .N, fi_1]
dt.comb[fi_1 >= 4, .SD, .SDcols = c("metar", "time", "fi_1_ok", names(fi.1))]

dt.melt <- melt(dt.comb[, .SD, .SDcols = c("id", "time", "pw", "is_cavok", "ff>15", "fx>30", "VC_TS", "is_pp", "is_mixed", "is_obsc", "is_ceiling", names(metar.ph))], id.vars = c("id", "time","pw"))

dt.1 <- dt.melt[value == 1, .N, .(variable, category = year(time))]
dt.2 <- rollup(dt.1, j = sum(N), by = c("variable", "category"))
dcast(dt.2,  category ~ variable, fill = 0)

dt.1 <- dt.melt[value == 1, .N, .(variable, category = month(time))]
dt.2 <- rollup(dt.1, j = sum(N), by = c("variable", "category"))
dcast(dt.2,  category ~ variable, fill = 0)

dt.1 <- dt.melt[value == 1, .N, .(variable, category = hour(time))]
dt.2 <- rollup(dt.1, j = sum(N), by = c("variable", "category"))
dcast(dt.2,  category ~ variable, fill = 0)
