## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# usethis::use_vignette("leaflet-map")


## ----setup--------------------------------------------------------------------
library(metar)
library(leaflet)

x <- metar_latest(id_icao = "", report.hour = 15)
dat.parsed <- parse_metar(x = x)
dt <- metar_validate(dat.parsed, set.na = TRUE)

id.para <- "tt"

pal <- colorNumeric("Spectral", reverse = TRUE, domain = NULL, na.color = "#eeefff")
leaflet(data = dt, width = "100%", height = "640px") %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(~lon, ~lat, stroke = T, radius = 3.5, weight = 1, color = "black", 
                   fillColor = ~pal(dt[[id.para]]), fillOpacity = 1,
                   popup = ~ sprintf("<p>%s %s (%sm)</p><p>%s</p>", icao, ap_name, elev, metar)) %>%
  addLegend(pal = pal, values = dt[[id.para]], bins = 20)



