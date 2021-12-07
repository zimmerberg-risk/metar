library(metar)
library(leaflet)
library(data.table)

x <- lapply(6:9, read_metar_noaa)
x <- rbindlist(x)

dat.parsed <- parse_metar(x = x$metar, t =x$time_valid)
dt <- validate_metar(dat.parsed)

dt.pw <- parse_metar_pw(dt$pw)
dt.cld <- parse_metar_cld(dt$cld)
dt.rvr <- parse_metar_rvr(dt$rvr)
dt.comb <- cbind(dt, dt.pw, dt.cld, dt.rvr)
dt.comb <- metar.stn[dt.comb, on = "icao"][!is.na(lon)]

dt.comb[order(time), `:=`(dt = time - data.table::shift(time, n = 3, fill = NA), dp = qnh - data.table::shift(qnh, n = 3, fill = NA)), icao]
dt.comb[, dp_3h := dp/(3/as.numeric(dt, "hours"))]

dt.comb <- dt.comb[, .SD[which.max(time)], icao]
dt.comb[order(dp_3h)]
# ---------------------------------------- Leaflet Numerical -----------------------------------------------

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
