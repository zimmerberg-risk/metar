#' Read METAR reports from mesonet.agron.iastate.edu
#'
#' @author M. Saenger
#' @description Read METAR reports from mesonet.agron.iastate.edu
#' @param id_icao Lorem Ipsum
#' @param date_start Lorem Ipsum
#' @param date_end Lorem Ipsum
#' @param auto Lorem Ipsum
#' @param verbose Lorem Ipsum
#' @export
#' @examples
#' read_mesonet(id_icao = "KDEN", date_start = Sys.time() - 3600*24*1)
#'
read_mesonet <- function(id_icao = "LSZH", date_start = date_end - 3600*24*7, date_end = Sys.time()+3600*24, auto = TRUE, verbose = FALSE){

  date_start <- as.POSIXct(as.character(date_start), tz = "UTC")
  date_end <- as.POSIXct(as.character(date_end), tz = "UTC")

  def <- list(
    station = id_icao,
    data = "metar", #"all",
    year1 = data.table::year(date_start),
    month1 = data.table::month(date_start),
    day1 = data.table::mday(date_start),
    year2 = data.table::year(date_end),
    month2 = data.table::month(date_end),
    day2 = data.table::mday(date_end),
    tz = "UTC", #Etc
    format = "onlycomma",
    missing = "empty"
  )

  url.1 <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?"
  url.2 <- paste0(mapply(def, names(def), FUN = function(i, j) paste0(j, "=", i), SIMPLIFY = F), collapse = "&")

  url <- sprintf("%s%s&tz=UTC&latlon=no&trace=T&direct=no&report_type=1&report_type=2", url.1, url.2)
  if(verbose) print(url)

  # Read
  dt <- data.table::fread(url, colClasses = c("character", "character", "character"))

  # Set TZ UTC
  dt$valid <-  as.POSIXct(format(dt$valid), tz = "UTC")

  dt
}

#' Get airport information from https://www.aviationweather.gov
#'
#' @author M. Saenger
#' @param fi.name Lorem Ipsum
#' @param fi.icao Lorem Ipsum
#' @param fi.ctry Lorem Ipsum
#' @param fi.lat Lorem Ipsum
#' @param fi.lon Lorem Ipsum
#' @export
#' @examples
#' # Central Europe
#' read_station(fi.icao = "^L", fi.lon = c(5, 11), fi.lat = c(46, 49))
#' # South America (Argentina)
#' read_station(fi.icao = "^SA")
#' # Contry filter (New Zealand)
#' read_station(fi.ctry = "NZ")
#'
read_station <- function(fi.name = ".+", fi.icao = ".+", fi.ctry = ".+", fi.lat = c(-90, 90), fi.lon = c(-180, 180)){
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
  dt.station <- dt.station[, .(ctry, name, icao, synop, iata, x, y, z)]

  # Filter
  dt.station[grepl(fi.name, name, ignore.case = TRUE) &
               grepl(fi.ctry, ctry, ignore.case = TRUE) &
               grepl(fi.icao, icao, ignore.case = TRUE) &
               between(x, fi.lon[1], fi.lon[2]) &
               between(y, fi.lat[1], fi.lat[2]
  )]
}

#' Get METARs from NOAA cycle files
#'
#' @author M. Saenger
#' @param id_icao Lorem Ipsum
#' @param latest Lorem Ipsum
#' @export
#' @description
#' metar_latest(id_icao = "LSZH")
#' metar_latest(id_icao = "EG")
#'
metar_latest <- function(id_icao = NULL, latest = TRUE){

  time.local <- Sys.time()
  attr(time.local, "tzone") <- "UTC"
  time.floor <- lubridate::floor_date(time.local, "1 hour")
  url.data <- sprintf("https://tgftp.nws.noaa.gov/data/observations/metar/cycles/%02dZ.TXT", hour(time.floor))
  dt.lines <- readr::read_lines(url.data)
  reports <- dt.lines[grepl("^([A-Z]{4}).+", dt.lines)]

  id <- str_extract(reports,"^([A-Z]{4})") # extract id icao
  #reports <- reports[which(id %in% id_icao)] # Filter
  reports <- reports[grepl(sprintf("^%s", id_icao), id)] # Filter

  # Remove duplicates
  key <- str_extract(reports,"^([A-Z]{4}\\s[0-9]{6}Z)")
  reports <- reports[!duplicated(key)]

  # Filter latest
  if(latest){
    id <- str_extract(reports,"^([A-Z]{4})")
    ind <- tapply(seq_along(id), id, max)
    reports <- reports[ind]
  }
  reports
}
