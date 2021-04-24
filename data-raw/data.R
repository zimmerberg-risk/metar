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

# Test
metar.stn[ctry == "CH" & active == TRUE]

# Save
usethis:::use_data(metar.stn, overwrite = TRUE, internal = FALSE)

data(metar.stn)



