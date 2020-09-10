library(metar)
library(wxPlot)
library(data.table)
library(tidyverse)

metar::read_station("Yakut")

id.icao <- "LSZH"; name <- "Zürich/Switzerland [424 m]"
id.icao <- "DNMM"; name <- "Lagos/Nigeria [27 m]"
id.icao <- "YSSY"; name <- "Sydney/Australia [3 m]"
id.icao <- "HSSS"; name <- "Khartoum/Sudan [387 m]"
id.icao <- "YBAS"; name <- "Alice Springs/Australia [549 m]"
id.icao <- "UEEE"; name <- "Yakutsk/Russia [106 m]"

force <- FALSE

file.data <- file.path("~/Data/metar", "data", sprintf("%s.RDS", id.icao))

if(file.exists(file.data) & !force){
  dt.metar <- readRDS()
} else {
  dt <- read_mesonet(id.icao, date_start = as.POSIXct("2012-01-01", tz = "UTC"), verbose = TRUE)

  dt.parsed <- parse_metar(x = dt$metar, date = dt$valid)
  dt.parsed[, `:=`(date = as.Date(time), day = mday(time), month = month(time), year = year(time), yday = yday(time), is_TS = grepl("TS", pw),
    is_CB = grepl("CB", cld), is_TCU = grepl("TCU", cld), is_FG = grepl("FG\\s", pw, perl = T),  is_any_FG = grepl("FG", pw))]

  print(dt.parsed[, .N, year])

  dt.pw <- metar_pw(pw = dt.parsed$pw)

  dt.metar <- cbind(dt.parsed, dt.pw[,-1])
  dt.metar[, ph := str_match(dt.metar$pw, sprintf("(?i)(%s)", paste(names(metar.ph), collapse = "|")))[,2]]

  saveRDS(dt.metar, file.data)
}

dt.ph <- rbindlist(metar.ph, idcol = "ph")[ph != "NSW"]
dt.comb <- merge(dt.metar, dt.ph, by = "ph")

# Group
dt.plot <- dt.comb[yday <= yday(Sys.Date())  & !is.na(group), .N,  .(date, year, group)][N > 0, .N, .(year, group)][order(group, year)]
# Fill
dt.plot <- merge(dt.plot, expand.grid(year = unique(dt.plot$year), group = unique(dt.ph$group)), by = c("group", "year"), all.y = T)
# Cast and reorder
dt.cast <- dcast(dt.plot, year ~ group, value.var = "N")
setcolorder(dt.cast, neworder = unique(dt.ph$group))
#names(dt.cast) <- str_replace(names(dt.cast), "/", "\n")
m.cast <- as.matrix(dt.cast, rownames = TRUE)

l <- list(
  matrix(c(1, 2, 3, 3, 4, 4, 5, 5), 4, 2, byrow = TRUE),
  widths = c(0.80, .20), heights = c(.07, 0.04, .85, 0.025)
)
def.theme <- modifyList(def.theme.global, lib.theme$default)
def.mod <- list(par = list(mai = rep(0.1, 4), omi = rep(0.15, 4), ann = F, xpd = F, bg = "#ffffff", family = "Arial Narrow", cex = 1.5))
def.theme <-  modifyList(def.theme, def.mod)


grDevices::png(sprintf("~/Data/metar/plots/%s_%s.png", id.icao, str_remove(name, "\\/")), lib.paper$p1080$x.mm*2, lib.paper$p1080$y.mm*2, units = "mm", res = 200)

exec(layout, !!!l)
exec(par, !!!def.theme$par)

# Text
plot_txt(txt = sprintf("METAR Present Weather Reports %s", name), cex.lab = 1.75)

# Logo
p_logo()

plot_txt(txt = sprintf("Daily occurence | Years: 2012 - 2020 | Period: 01 Jan - %s", format(Sys.Date(), "%d %b")), fit = T)

# Main
par(mai = c(1.0, 0.5, 0.2, 0.1), mgp = c(1.5, .75, 0))
cols <- c(hcl.colors(8, palette = "Oslo", alpha = .5), "red")
xxx <- barplot(pmin(m.cast, 140), beside = TRUE, ylim = c(0, 160), col = cols, las = 1, ylab = "Days per year")
xxxx <- xxx[pmin(m.cast, 140) == 140]
lab <- m.cast[pmin(m.cast, 140) == 140]
text(xxxx, 139, labels =  lab, srt = 90, adj = c(1, 0.5), family = "Arial Narrow", cex = .75)
text(xxxx, 141, labels =  "+", srt = 90, adj = c(0, 0.5), family = "Arial Narrow", cex = .75)
abline(v = xxx[1,]-1, xpd = F)
abline(h = 0, xpd = F)
legend("topleft", legend = 2012:2020, col = cols, pch = 16, horiz = TRUE, box.lwd = 0, box.col =  "white")

# Footer
par(mai = rep(0.1, 4))
plot_txt(txt = sprintf("© 2020 Zimmerberg Risk Analytics GmbH | Data: https://mesonet.agron.iastate.edu | Only dominant phenomenon per report taken into account: thunderstorm > precipitation > obscuration"), fit = T)

dev.off()
