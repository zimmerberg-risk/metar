library(metar)
library(leaflet)
library(data.table)

x <- lapply(4:5, read_metar_noaa)
x <- rbindlist(x)

dat.parsed <- parse_metar(x = x$metar, t =x$time_valid)
dt <- validate_metar(dat.parsed)
dt[grep("^FAL", icao)]

dt.pw <- parse_metar_pw(dt$pw)
dt.cld <- parse_metar_cld(dt$cld)
dt.rvr <- parse_metar_rvr(dt$rvr)
dt.comb <- cbind(dt, dt.pw, dt.cld, dt.rvr)
dt.comb <- metar.stn[dt.comb, on = "icao"][!is.na(lon)]

dt.comb <- dt.comb[, .SD[which.max(time)], icao]

# ---------------------------------------- Leaflet Numerical -----------------------------------------------
id.para <- "fx"
dt.comb[, rad := fifelse(is.na(get(id.para)), 2, 7)]
pal <- colorNumeric("Spectral", reverse = TRUE, domain = NULL, na.color = "#eeefff")
leaflet(data = dt.comb) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(~lon, ~lat, stroke = T, radius = ~rad,  weight = 1, color = "black", fillColor = ~pal(dt.comb[[id.para]]), fillOpacity = 1,
                   popup = ~ sprintf("<p>%s %s (%sm)</p><p>%s</p><p>%s</p>", icao, ap_name, elev, time, metar)) %>%
  addLegend(pal = pal, values = dt.comb[[id.para]], bins = 20)

# ---------------------------------------- Leaflet Categorical -----------------------------------------------

id.para <- "sigwx"
levels <- metar.class[id_para == "sigwx"][1:10]$id_class
dt.map <- dt.comb[sigwx != "NIL"]
dt.map[, sigwx := factor(sigwx, levels)]
dt.map[, rad := fifelse(is.na(get(id.para)), 2, 7)]
pal <- colorFactor(palette = metar.class[id_para == "sigwx"][1:10]$col, levels = levels, alpha = 1, na.color = "#777777")

leaflet(data = dt.map) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(~lon, ~lat, stroke = T, radius = ~rad, weight = 1, color = ~pal(dt.map[[id.para]]), fillColor = ~pal(dt.map[[id.para]]),
                   fillOpacity = .5, popup = ~ sprintf("<p>%s %s (%sm)</p><p>%s</p><p>%s</p>", icao, ap_name, elev, time, metar)) %>%
  addLegend(pal = pal, values = dt.map[[id.para]], opacity = .7)

# ---------------------------------------- PW -----------------------------------------------
id.para <- "pw"
dt.map <- dt.comb[!is.na(pw)]
dt.map[, rad := fifelse(is.na(get(id.para)), 2, 7)]

leaflet(data = dt.map[, .(lon, lat, pw)]) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addLabelOnlyMarkers(~lon, ~lat, label = ~pw,  labelOptions = labelOptions(textsize = "16pt", noHide = T, direction = 'top', textOnly = T))

