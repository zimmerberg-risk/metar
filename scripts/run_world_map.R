library(metar)
library(data.table)
library(sp)
library(wxPlot)
library(rnaturalearth)

x <- read_metar_noaa()
dat.parsed <- parse_metar(x = x$metar, t =x$time_valid)
dt <- validate_metar(dat.parsed)

dt.pw <- parse_metar_pw(dt$pw)
dt.cld <- parse_metar_cld(dt$cld)
dt.rvr <- parse_metar_rvr(dt$rvr)
dt.comb <- cbind(dt, dt.pw, dt.cld, dt.rvr)
dt.comb <- metar.stn[dt.comb, on = "icao"]

# ---------------------------------------- Map -----------------------------------------------

id.para <- "qnh"
id.proj <- "mercator"
dt.plot <- dt.comb[!is.na(get(id.para)) & !is.na(lon),]
coordinates(dt.plot) <- ~lon + lat
proj4string(dt.plot) <- lib.crs$longlat[[2]]

dt.plot.longlat = spTransform(dt.plot, CRS(lib.crs[[id.proj]][[2]]))
sp.map <- ne_countries("small")
sp.map <- raster::crop(sp.map, raster::extent(-175, 180, -55, 75))
sp.map <- spTransform(sp.map, CRS(lib.crs[[id.proj]][[2]]))

val <- dt.plot.longlat[[id.para]]
brks <- pretty(val, 15)
cols <- hcl.colors((length(brks) - 1), "Plasma")
sp::plot(sp.map, add = F, bg = "lightblue", col = "#eeeeee", xlim = as.vector(sp.map@bbox[1,]), ylim = as.vector(sp.map@bbox[2,]), xaxs = "i", yaxs = "i")
sp::plot(dt.plot.longlat, pch = 16, col = "grey", cex = .5, add = T)
points(dt.plot.longlat, pch = 16, cex = 0.7, col = as.character(cut(val, brks, cols, include.lowest = T)))
legend("bottomleft", legend = brks[-1], title = id.para,fill = cols)

