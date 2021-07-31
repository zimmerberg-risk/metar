## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# usethis::use_vignette("leaflet-map")


## ----setup--------------------------------------------------------------------
library(metar)
library(leaflet)

dat <- read_metar_noaa(hour = 15, latest.only = TRUE)
dat.parsed <- parse_metar(x = dat$metar)
dat.valid <- validate_metar(dat.parsed, set.na = TRUE)
dat.map <- merge(dat.valid, metar.stn, by = "icao")


id.para <- "tt"
pal <- colorNumeric("Spectral", reverse = TRUE, domain = NULL, na.color = "#eeefff")

leaflet(data = dat.map, width = "100%", height = "640px") %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(~lon, ~lat, stroke = T, radius = 3.5, weight = 1, color = "black", 
                   fillColor = ~pal(dat.map[[id.para]]), fillOpacity = 1,
                   popup = ~ sprintf("<p>%s %s (%sm)</p><p>%s</p>", icao, ap_name, elev, metar)) %>%
  addLegend(pal = pal, values = dat.map[[id.para]], bins = 20)



