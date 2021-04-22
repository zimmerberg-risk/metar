# x <- rbindlist(metar.vars, fill = T, idcol = "id_para")
# write.table(x, "clipboard", sep="\t", row.names=FALSE, na = "")
#
# write.table(data.frame(x = metar.test), "clipboard", sep="\t", row.names=FALSE, na = "")

library(metar)
library(stringr)
library(data.table)
library(maps)


x <- metar_latest(id_icao = "")
dat <- parse_metar(x = x)
xxx <- metar_validate(dat, set.na = TRUE)
xxx[order(-flag)]



metar_stn(fi.name = "^darwin")

stn <- metar_stn(fi.lat = c(40, 50), fi.lon = c(5, 15))$icao
stn <- metar_stn(fi.ctry = "Australia")$icao

data(world.cities)
dt.cities <- as.data.table(world.cities)
setorder(dt.cities, -pop)
dt.cities <- dt.cities[pop>1e4, .SD[1:5], country.etc]
icao.cities <- sapply(unique(dt.cities$name[]), function(i) metar.stn[active == T]$icao[grepl(i, metar.stn[active == T]$ap_name_long)][1])


stn.list <- list(
  oce = c("YAYE", "YSSY", "YMML", "YPPH", "YMLT", "YPDN", "NZAA", "NZCH", "NFNA", "NVVV"),
  select = c("PTRO", "PTYA", "BIKF", "YGEL", "LSZH", "LSZA", "FTTJ", "GABS", "GCXO", "NZSP", "LOWI",
         "UUDD", "GCXO", "WSSS", "BGTL", "SBMN", "OMDB", "KBOS"),
  #world = unique(unname(icao.cities)),
  ch = stn <- metar_stn(fi.ctry = "Switzerland")$icao
)

id.area <- "ch"
stn <- stn.list[[id.area]]
id.folder <- sprintf("C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Data/metar/%s", id.area)

void <- lapply(stn[], function(id.icao){

  # id.icao <- "LSZH"

  cat(id.icao, " ", match(id.icao, stn), "\n")

  date.start <- "2021-03-22"
  date.end <- "2021-04-22"
  dat.metar <- read_mesonet(id_icao = id.icao, date_start = date.start, date_end = date.end)
  if(nrow(dat.metar) == 0) return(NULL)
  dat.parsed <- parse_metar(x = dat.metar$metar, date = dat.metar$valid)
  dat.parsed <- metar_validate(dat.parsed, set.na = TRUE)
  dat.plot <- cbind(dat.parsed, dat.parsed[, metar_pw(pw)])

  file.name <- file.path(id.folder, sprintf("%s_%s_%s.png", id.icao, str_remove(dat.parsed$ap_name[1], "\\/|\\?"), substr(date.end, 1, 10)))

  png(file.name, width = 1600, height = 900, units = "px", res = 96)
  plot_metargram(dat = dat.plot, cex = 1.3)
  dev.off()

})

# Map

dat <- metar_latest(id_icao = "")
dat <- parse_metar(x = dat)

dat.1 <- cbind(dt.all, dt.all[, metar_pw(pw)])
dat.map <- merge(dt.stn, dat.1, by = "icao")
dat.map[order(-fx), .(icao, ctry, ap_name, fx)]

brks <- seq(0, 100, 5) #seq(-30, 48, 2)
cols <- rainbow(length(brks) - 1)
dat.map$col <- as.character(cut(dat.map$fx, brks, cols))

plot(dat.map$lon, dat.map$lat, type = "n")
points(dat.map$lon, dat.map$lat, col = dat.map$col , pch =16)



# Groups
dt.stn <- metar::read_station()
dt.in <- read_mesonet(id_icao = "KSFO", date_start = Sys.time() - 3600*24*3, date_end = Sys.time() + 3600*24) #"RKPK"

dt.in <- metar_latest(id_icao = "")
dt <- parse_metar(x = dt.in)
unique(dt$pw)

dt.pw <- metar_pw(pw = dt$pw)
dt.cld <- metar_clouds(cld = dt$cld)
dt.rvr <- metar_rvr(rvr = dt$rvr)
dt.comb <- cbind(dt, dt.pw, dt.cld, dt.rvr)

head(dt.comb[, .(icao, ap_name, metar, wx, pw, cld, qnh, fx, ff, rvr)][order(-fx)], 30)

