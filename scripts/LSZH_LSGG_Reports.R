#usethis::edit_r_environ()
# devtools::install_github("m-saenger/metar", auth_token = devtools::github_pat())

library(metar)
library(data.table)
library(tidyverse)

dt.in <- read_csv("C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Kunden/05 MeteoSchweiz/AutoMETAR/metar_LSZH_2000_2020.txt", guess_max = 1e5)
#dt.in <- dt.in %>% filter(valid > "2019-01-01")

dt <- parse_metar(x = dt.in$metar, date = dt.in$valid)
dt.clouds <- metar_clouds(cld = dt$cld)
dt.rvr <- metar_rvr(rvr = dt$rvr)
dt.pw <- metar_pw(pw = dt$pw)
dat <- cbind(dt, dt.clouds[,-1], dt.rvr[,-1], dt.pw[,-1])

write_csv(dat, "C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Kunden/05 MeteoSchweiz/AutoMETAR/metar_LSZH_2000_2020_processed.txt")
dat <- read_csv("C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Kunden/05 MeteoSchweiz/AutoMETAR/metar_LSZH_2000_2020_processed.txt", guess_max = 1e5) %>%
  as.data.table
dat <- dat[is.na(auto) & !duplicated(dat$time)]

dat[, `:=`(date = as.Date(time), day = mday(time), month = month(time), year = year(time), yday = yday(time), is_TS = grepl("TS", pw),
           is_CB = grepl("CB", cld), is_TCU = grepl("TCU", cld), is_FG = grepl("FG\\s", pw, perl = T),  is_any_FG = grepl("FG", pw))]

dat[, unique(pw)]

dat[is_TS == TRUE, .(time, pw, fx)][order(-fx)]
dat[is_TCU == TRUE, .(time, pw, cld, fx)]

dt.plot <- dat[is_FG == TRUE, .N,  .(date, year, month)][N > 0, .N, .(year, month)][order(year)]
dt.plot[, col := factor(year, labels = rainbow(10))]

cols <- c(rainbow(8, alpha = .2), "red", "black")
barplot(as.matrix(dcast(dt.plot, year ~ month, value.var = "N"), rownames = TRUE), beside = TRUE, col = cols)
legend("topleft", legend = 2011:2020, col = cols, pch = 16)


plot(1, type = "n", xlim = c(1, 12), ylim = c(0, max(dt.plot$N)))
lapply(split(dt.plot, list(dt.plot$year)), function(i){
  points(i$month, i$N, type = "l", col = i$col)
})
legend("topleft", legend = 2011:2020, col = rainbow(10), pch = 16)
