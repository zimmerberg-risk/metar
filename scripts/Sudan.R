library(metar)
library(data.table)
library(tidyverse)

metar::read_station("Khart")

dt <- read_mesonet("HSSS", date_start = as.POSIXct("2010-01-01", tz = "UTC"), verbose = TRUE)

dt.parsed <- parse_metar(dt$metar, dt$valid)
dt.parsed[, `:=`(date = as.Date(time), day = mday(time), month = month(time), year = year(time), yday = yday(time), is_TS = grepl("TS", pw),
           is_CB = grepl("CB", cld), is_TCU = grepl("TCU", cld), is_FG = grepl("FG\\s", pw, perl = T),  is_any_FG = grepl("FG", pw))]
dt.pw <- metar_pw(pw = dt.parsed$pw)

dt.parsed <- cbind(dt.parsed, dt.pw[,-1])
dt.parsed[, ph := str_match(dt.parsed$pw, sprintf("(?i)(%s)", paste(names(metar.ph), collapse = "|")))[,2]]

dt.ph <- data.table(ph =  names(metar.ph), ph_name = factor(unname(unlist(lapply(metar.ph, "[", "name")))), ph_type = factor(unname(unlist(lapply(metar.ph, "[", "type"))) ))
dt.comb <- merge(dt.parsed, dt.ph, by = "ph")[order(time)]

tail(dt.comb, 50)
dt.comb[grepl("RA", pw)]

dt.plot <- dt.comb[year > 2011 & yday <= yday(Sys.Date())  & !is.na(ph_name), .N,  .(date, year, ph_type, ph_name)][N > 0, .N, .(year, ph_type, ph_name)][order(ph_type, year)]
dt.cast <- dcast(dt.plot, year ~ ph_name, value.var = "N")
setcolorder(dt.cast, neworder = c("year", intersect(unname(unlist(lapply(metar.ph, "[", "name"))), names(dt.cast))))

cols <- c(rainbow(8, alpha = .2), "black")
par(cex = 1.5)
barplot(as.matrix(dt.cast, rownames = TRUE), beside = TRUE, col = cols, las = 2, ylab = "Days per year")
legend("topleft", legend = 2012:2020, col = cols, pch = 16)
title(main = sprintf("METAR Present Weather Reports\nKhartoum/Sudan [387 m] 2012 - 2020 | Period 01 Jan - %s", format(Sys.Date(), "%d %b")))
