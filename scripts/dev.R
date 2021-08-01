library(data.table)
library(metar)
library(vroom)
library(stringr)
library(metar)
library(ggplot2)

#library(stringdist)

# https://tgftp.nws.noaa.gov/data/observations/metar/cycles/11Z.TXT

dir.base <- "C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Data/metar"
dir.data <- file.path(dir.base, "data")
p <- file.path(dir.data, "10Z.TXT")

dt.noaa <-  read_metar_noaa(12, remote = F, sprintf("%s/%s",dir.data, "11Z.txt"))
dt.noaa.1 <- read_metar_noaa(12, remote = F, sprintf("%s/%s",dir.data, "12Z.txt"))
dt.noaa.2 <- read_metar_noaa(12, remote = F, sprintf("%s/%s",dir.data, "13Z.txt"))

dt.meso <- read_metar_mesonet(remote = F, path = sprintf("%s/%s",dir.data, "LSZH.txt"), verbose = T)
dt.meso.LOWW <- read_metar_mesonet(remote = F, path = sprintf("%s/%s",dir.data, "LOWW.txt"), verbose = T)

grep("TSRA\\b", dt.noaa$metar, value = T) # CAVOK|NSC|NCD|WXNIL|CLR|SKC|NSW

## --------------------------------------------------- Speed  ---------------------------------------------------
dt.test <- parse_metar_grp(x = metar.test$code)

microbenchmark::microbenchmark({  dt.grp <- parse_metar_grp(x = dt.noaa$metar, t = dt.noaa$time_valid) }, times = 1)
dt.grp[id_icao %in% c("LSZH", "YSSY", "KDEN", "LOWI", "EGGW", "BIAR", "DRRN", "ETMN", "RJAA", "UUMO")]
vroom::vroom_write(dt.grp, "C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Data/metar/test.csv", delim = ",", na = "")

# microbenchmark::microbenchmark({  dt.grp.1 <- parse_metar(x = dt.noaa$metar, dt.noaa$time_valid) }, times = 1)
# microbenchmark::microbenchmark({  dt.grp.2 <- parse_metar_grp(x = rep(dt.noaa$metar, 200)) }, times = 1)

parse_metar_cld(x = dt.test$cld)

x1 <- read_metar_mesonet("SBPA", date_start = "2021-07-15")
x2 <- parse_metar(x1$metar, x1$valid) #dt.noaa$metar
x3 <- parse_metar_pw(x2$pw)
x2 <- metar.stn[x2, on = "icao"]
plot_metargram(dat = cbind(x2, x3))

## --------------------------------------------------- # Cross table  ---------------------------------------------------

dt.1 <- rbind(dt.noaa.1, dt.noaa.2)
dt.2 <- parse_metar(x = dt.1$metar, t = dt.1$time_valid)
dt.2a <- validate_metar(dt.2)
dt.2a[, n := 1:.N, icao]
dt.2a[icao == "LSZH"]

dt.3 <- dcast(dt.2a, icao ~ n, value.var = "ff")
dt.3[, diff := `2` - `1`]

ggplot(dt.3, aes(`1`, `2`)) +
  geom_jitter() +
  geom_text(aes(label = icao), dt.3[order(-abs(diff))][1:10])

## --------------------------------------------------- Time Series LSZH/LOWW  ---------------------------------------------------
dt.1 <- rbind(dt.meso, dt.meso.LOWW)
dt.2 <- parse_metar(dt.1$metar, dt.1$valid) #dt.noaa$metar
dt.3 <- validate_metar(dt.2)

setkey(dt.3, icao, time)
dt.4 <- dt.3[!duplicated(dt.3, by = key(dt.3))]

dt.time <- data.table(time = seq(as.POSIXct("2013-01-01 00:20:00"), as.POSIXct("2021-07-31 23:50:00"), "30 min"))
dt.4 <- merge(dt.3, dt.time, by = "time", all = T)
dt.4[, .N, .(icao, time)][order(N)]

dt.5 <- dcast(dt.4, time ~ icao, value.var = "tt")
dt.5[, diff := LOWW - LSZH]
dt.5[, roll := RcppRoll::roll_mean(diff, n = 24*2*7, na.rm = T, fill = NA)]

ggplot(dt.5, aes(time, diff)) +
  #geom_path(aes(time, diff), colour = "black", size = .25) +
  geom_path(aes(time, roll), colour = "red")


## --------------------------------------------------- Time Series  ---------------------------------------------------

dt.meso.parsed <- parse_metar(dt.meso$metar, dt.meso$valid) #dt.noaa$metar
dt.meso.vali <- validate_metar(dt.meso.parsed)
dt.meso.pw <- parse_metar_pw(dt.meso.vali$pw)
dt.meso.cld <- parse_metar_cld(dt.meso.vali$cld)
dt.plot <- cbind(dt.meso.vali, dt.meso.pw[, -"pw"], dt.meso.cld)[month(time) %in% 1:7]

dt.count <- dt.plot[sigwx != "NOSIG", .N, .(icao, sigwx, time = lubridate::floor_date(time, "1 year"))]
ggplot(dt.count) + facet_wrap(~sigwx, scales = "free_y") + geom_bar(aes(time, N, fill = sigwx), stat = "identity")

dt.week <- dt.plot[, .(sum = sum(PP)), .(time = lubridate::ceiling_date(time, "1 week"))]
ggplot(dt.week) + geom_area(aes(time, sum))

ggplot(dt.plot) + geom_path(aes(time, TS))

## --------------------------------------------------- Tests  ---------------------------------------------------



