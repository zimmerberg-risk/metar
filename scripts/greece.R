library(metar)
library(data.table)
library(tidyverse)
library(rlang)
library(ggplot2)

metar::read_station(fi.ctry = "GR") #LGKF LGZA LGRX
id.icao <- c("LGZA", "LGKF", "LGRX", "LGAD", "LGPZ")
metar_latest(id.icao)

dt.in <- read_mesonet(id_icao = "LGKF", date_start = as.POSIXct("2011-01-01", tz = "UTC")) #"RKPK"
#dt.in <- rbindlist(lapply(id.icao, read_mesonet, date_start = as.POSIXct("2020-09-17", tz = "UTC")))

dt <- parse_metar(x = dt.in$metar, date = dt.in$valid)

dt.pw <- metar_pw(pw = dt$pw)
dt.cld <- metar_clouds(cld = dt$cld)
dt.comb <- cbind(dt, dt.pw, dt.cld)
dt.comb[, `:=`(date = as.Date(time))]

tail(dt.comb$metar, 30)
dt.comb[pw_grp_1=="+TSRA", .(time, metar)]

ggplot(dt.comb, aes(time, qnh, group = icao)) + geom_point(aes(colour = icao)) + geom_path(aes(colour = icao))

# fx
dt.daily.max <- dt.comb[!is.na(fx), .SD[which.max(fx)], .(icao, date)][order(-fx)]
head(dt.daily.max[, .(icao, time, date, fx, ff, qnh, metar)], 30)

# p
# dt.daily.max <- dt.comb[, .SD[which.min(qnh)], date][order(qnh)]
# head(dt.daily.max[, .(time, date, fx, ff, qnh, metar)], 30)

dt.tbl <- head(dt.daily.max[icao == "LGKF", .(Rank = frank(-fx, ties.method = "dense"), Time = time, `Gusts kt`=fx, `Wind kt`=ff, QNH=round(qnh), PW=pw_grp_1, METAR = metar)], 15)
options(knitr.kable.NA = '')
knitr::kable(dt.tbl) %>%
  kableExtra::kable_styling(font_size = 16) %>%
  kableExtra::row_spec(row = which(year(dt.tbl$Time) %in% 2020), bold = TRUE)

k <- knitr::kable()
print(k)
