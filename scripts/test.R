# xxx <- rbindlist(metar.vars, fill = T, idcol = "id_para")
# write.table(xxx, "clipboard", sep="\t", row.names=FALSE, na = "")
#
# write.table(data.frame(x = metar.test), "clipboard", sep="\t", row.names=FALSE, na = "")

library(metar)
library(stringr)
library(data.table)
library(maps)

# ---------------------------------------- Latest -----------------------------------------------

x <- read_metar_noaa(hour = 7)
dat.parsed <- parse_metar(x = x$metar, t =x$time_valid)
dt <- validate_metar(dat.parsed)

dt.pw <- parse_metar_pw(dt$pw)
dt.cld <- parse_metar_cld(dt$cld)
dt.rvr <- parse_metar_rvr(dt$rvr)
dt.comb <- cbind(dt, dt.pw, dt.cld, dt.rvr)
dt.comb <- metar.stn[dt.comb, on = "icao"]
# unique(dt$rwc)
# unique(dt$rvr)

# dt.comb[is.na(td)]
# head(dt.comb[, .(icao, ap_name, metar, wx, pw, cld, qnh, fx, ff, rvr)][order(-fx)], 30)
# head(dt.comb[, .(icao, ctry, ap_name, SIGWX, metar)][SIGWX != "NOSIG"], 30)
#
# dt.comb[, .N, SIGWX]
# dat.parsed[ctry == "PL"]

# ---------------------------------------- Leaflet Numerical -----------------------------------------------
library(leaflet)
id.para <- "ff"
pal <- colorNumeric("Spectral", reverse = TRUE, domain = NULL, na.color = "#eeefff")
leaflet(data = dt.comb) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(~lon, ~lat, stroke = T, radius = 3.5, weight = 1, color = "black", fillColor = ~pal(dt.comb[[id.para]]), fillOpacity = 1,
                   popup = ~ sprintf("<p>%s %s (%sm)</p><p>%s</p><p>%s</p>", icao, ap_name, elev, time, metar)) %>%
  addLegend(pal = pal, values = dt.comb[[id.para]], bins = 20)

# ---------------------------------------- Leaflet Categorical -----------------------------------------------

id.para <- "sigwx"
levels <- metar.class[id_para == "sigwx"][1:10]$id_class
dt.map <- dt.comb[sigwx != "NIL"]
dt.map[, sigwx := factor(sigwx, levels)]
pal <- colorFactor(palette = metar.class[id_para == "sigwx"][1:10]$col, levels = levels, na.color = "#eeefff")

leaflet(data = dt.map) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(~lon, ~lat, stroke = T, radius = 3.5, weight = 1, color = "black", fillColor = ~pal(dt.map[[id.para]]), fillOpacity = 1,
                   popup = ~ sprintf("<p>%s %s (%sm)</p><p>%s</p><p>%s</p>", icao, ap_name, elev, time, metar)) %>%
  addLegend(pal = pal, values = dt.map[[id.para]], opacity = 1)

# ---------------------------------------- Map -----------------------------------------------
library(sp)
library(wxPlot)
library(rnaturalearth)

id.para <- "qnh"
id.proj <- "mercator"
dt.plot <- dt.comb[!is.na(get(id.para)) & !is.na(lon),]
coordinates(dt.plot) <- ~lon + lat
proj4string(dt.plot) <- lib.crs$longlat[[2]]

dt.plot.longlat = spTransform(dt.plot, CRS(lib.crs[[id.proj]][[2]]))
sp.map <- ne_countries("small")
sp.map <- raster::crop(sp.map, raster::extent(-175, 180, -55, 80))
sp.map <- spTransform(sp.map, CRS(lib.crs[[id.proj]][[2]]))


val <- dt.plot.longlat[[id.para]]
brks <- pretty(val, 15)
cols <- hcl.colors((length(brks) - 1), "Plasma")
sp::plot(sp.map, add = F, bg = "lightblue", col = "#eeeeee", xlim = as.vector(sp.map@bbox[1,]), ylim = as.vector(sp.map@bbox[2,]), xaxs = "i", yaxs = "i")
sp::plot(dt.plot.longlat, pch = 16, col = "grey", cex = .5, add = T)
points(dt.plot.longlat, pch = 16, cex = 0.7, col = as.character(cut(val, brks, cols, include.lowest = T)))
legend("bottomleft", legend = brks[-1], title = id.para,fill = cols)

# ---------------------------------------- World -----------------------------------------------

metar_stn(fi.name = "^berli")

stn <- metar_stn(fi.lat = c(40, 50), fi.lon = c(5, 15))$icao
stn <- metar_stn(fi.ctry = "Switz")$icao

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
  ch = stn <- get_metar_stn(fi.ctry = "Switzerland")$icao
)

id.area <- "ch"
stn <- stn.list[[id.area]]
id.folder <- sprintf("C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Data/metar/%s", id.area)

void <- lapply(stn[], function(id.icao){

  id.icao <- "LSZH" #CYYT RCFN  RCKH   URSS URKK LTFH LICZ URKA

  cat(id.icao, " ", match(id.icao, stn), "\n")

  date.end <- Sys.Date()
  date.start <- date.end - 100 #Sys.Date() - 14  "2021-04-01"

  dat.metar <- read_metar_mesonet(id_icao = id.icao, date_start = date.start, date_end = date.end)
  if(nrow(dat.metar) == 0) return(NULL)
  dat.parsed <- parse_metar(x = dat.metar$metar, t = dat.metar$valid)
  dat.parsed <- validate_metar(dat.parsed)
  dat.plot <- cbind(dat.parsed, dat.parsed[, parse_metar_pw(pw)])
  dat.plot <- metar.stn[dat.plot, on = "icao"]

  file.name <- file.path(id.folder, sprintf("%s_%s_%s.png", id.icao, str_remove(dat.plot$ap_name[1], "\\/|\\?"), substr(date.end, 1, 10)))

  png(file.name, width = 1600, height = 900, units = "px", res = 96)
  plot_metargram(dat = dat.plot, cex = 1.3)
  dev.off()

})


dat.parsed[, .SD[which.max(tt)]]
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


