library(readxl)
library(usethis)
library(data.table)

# https://r-pkgs.org/data.html

# https://www.aviationweather.gov/docs/metar/stations.txt
#

## --------------------------- Meta Data ------------------------------
file <- "data-raw/metar_config.xlsx"
sheets <- readxl::excel_sheets(file)

l <- lapply(sheets, function(i){
  n <- paste("metar", tolower(i), sep = ".")
  dat <- readxl::read_xlsx(path = file, sheet = i)
  dat <- as.data.table(dat)

  assign(n, dat, envir = rlang::global_env())
  do.call("use_data", list(as.name(n), overwrite = TRUE, internal = FALSE))
})

read_aviationweather <- function(){
  url.station <- "https://www.aviationweather.gov/docs/metar/stations.txt"
  col.names <- c("state", "name", "icao", "iata", "synop", "lat_deg", "lat_min", "lon_deg", "lon_min", "z", "M",  "N",  "V",  "U",  "A", "C", "ctry")
  col.start <- c(1, 4, 21, 27, 33, 40, 43, 48, 52, 56, 63, 66, 69, 72, 75, 80, 82)
  col.pos <- readr::fwf_positions(col.start, c(col.start[-1] - 1, NA), col_names = col.names)

  station.lines <- readr::read_lines(url.station, skip = 39)
  station.lines <- station.lines[sapply(station.lines, nchar) > 80]

  dt.station <- as.data.table(readr::read_fwf(station.lines, col_positions = col.pos))

  # Process lon/lat minutes
  dt.station[, c("lat_num", "ns") := as.data.frame(str_match(lat_min, "([0-9]+)(N|S)")[,2:3]) ]
  dt.station[, c("lon_num", "we") := as.data.frame(str_match(lon_min, "([0-9]+)(W|E)")[,2:3]) ]
  dt.station[, `:=`(
    x = fifelse(we == "E", as.numeric(lon_deg) + as.numeric(lon_num)/60, -(as.numeric(lon_deg) + as.numeric(lon_num)/60)),
    y = fifelse(ns == "N", as.numeric(lat_deg) + as.numeric(lat_num)/60, -(as.numeric(lat_deg) + as.numeric(lat_num)/60))
  )]

  dt.station[, .(ctry, name, icao, synop, iata, x, y, z)]
}

## --------------------------- Airport Data ------------------------------
dt.stn.1 <- read_aviationweather()

setnames(dt.stn.1, old = c("name", "x", "y", "z"), new = c("ap_name", "lon", "lat", "elev"))

dt.stn.2 <- fread("data-raw/master-location-identifier-database-202103_standard.csv", skip = 4, encoding = "Latin-1",
                na.strings = c("", "-9999"))
dt.stn.2 <- dt.stn.2[!is.na(icao), .(icao, ctry = country2, ctry3 = country3, ctry_name = country, region, ap_name_long = place_name, lon, lat, elev )]
dt.stn.2[, ap_name := trimws(tstrsplit(ap_name_long, "\\|")[[1]])]
dt.stn.2 <- dt.stn.2[!is.na(icao) & !is.na(lat)]

# Combine
metar.stn <- rbind(dt.stn.2, dt.stn.1, fill = T)

# Duplicated ICAO
metar.stn <- metar.stn[!duplicated(metar.stn$icao)]

# Find active stn
dat.list <- lapply(0:23, function(h){
  print(h)
  read_metar_noaa(hour = h, latest.only = T)
})
dt <- rbindlist(dat.list)
dt.unique <- data.table(active = TRUE, icao = unique(str_sub(dt$metar, 1, 4)))

metar.stn <- merge(metar.stn, dt.unique, by = "icao", all.x = TRUE)
metar.stn[, active := fifelse(is.na(active), FALSE, TRUE)]

# Test
expression({
  id.icao = "LOWW"
  metar.stn[ctry == "AT" & active == TRUE]
  # dt.stn.1[icao == "EPRA"]
  dt.stn.1[icao == id.icao]
  dt.stn.2[icao == id.icao]
  metar.stn[icao == id.icao]
  metar.stn[, .N, active]
  metar.stn[active == T, .N, ctry]
})

# Save
usethis:::use_data(metar.stn, overwrite = TRUE, internal = FALSE)



