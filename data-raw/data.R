library(readxl)
library(usethis)
library(data.table)

# https://r-pkgs.org/data.html

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


## --------------------------- Airport Data ------------------------------
dt.stn <- fread("data-raw/master-location-identifier-database-202103_standard.csv", skip = 4, encoding = "Latin-1",
                na.strings = c("", "-9999"))

metar.stn <- dt.stn[!is.na(icao), .(icao, ctry = country2, ctry3 = country3, ctry_name = country, region, ap_name_long = place_name, lon, lat, elev )]
metar.stn[, ap_name := trimws(tstrsplit(ap_name_long, "\\|")[[1]])]
metar.stn <- metar.stn[!is.na(icao) & !is.na(lat)]

# Duplicated ICAO
metar.stn <- metar.stn[!duplicated(metar.stn$icao)]

# Find active stn
dat.list <- lapply(0:23, function(h){
  print(h)
  metar_latest(id_icao = "", report.hour = h)
})
dt <- do.call(c, dat.list)
dt <- data.table(active = TRUE, icao = unique(str_extract(dt, "(?<=COR|METAR|\\s|^)\\b([A-Z0-9]{4})\\b")))

metar.stn <- merge(metar.stn, dt, by = "icao", all.x = TRUE)
metar.stn[, active := fifelse(is.na(active), FALSE, TRUE)]

# Find missing
x <- do.call(c, sapply(0:23, function(i) metar_latest(id_icao = "", report.hour = i)))
x <- unique(x)
dat.parsed <- parse_metar(x = x)
xxx <- unique(dat.parsed[is.na(ap_name),1:6])
stn.1 <- read_station(fi.icao = paste(xxx$icao, collapse =  "|"))
setnames(stn.1, c("ctry", "ap_name", "icao", "synop", "iata", "lon", "lat", "elev"))
stn.1[, `:=`(active = TRUE, ap_name_long = ap_name, iata = NULL, synop = NULL)]

stn.1 <- stn.1[!icao %in% metar.stn$icao] # Avoid duplicates

stn.1[icao == "EPRA"]
metar.stn[icao == "EPRA"]


metar.stn <- rbind(metar.stn, stn.1, fill = TRUE)

# Test
metar.stn[ctry == "BA" & active == TRUE]

# Save
usethis:::use_data(metar.stn, overwrite = TRUE, internal = FALSE)

data(metar.stn)



