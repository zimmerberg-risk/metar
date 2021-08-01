## --------------------------------------------------- Read  ---------------------------------------------------


#' Get METARs from NOAA cycle files
#'
#' @author M. Saenger
#' @param hour specific reporting hour (last 24h)
#' @param remote Download file from NOAA server
#' @param path Path to local file
#' @param latest.only latest report per aiport only
#' @export
#' @description
#' read_metar_noaa(hour = 15, latest.only = T)
#'
read_metar_noaa <- function(hour, remote = TRUE, path, latest.only = TRUE){
  url <- sprintf("https://tgftp.nws.noaa.gov/data/observations/metar/cycles/%02dZ.TXT", hour)

  if(remote){
    n <- 0 # Iterator
    download <- FALSE

    # Download (try up to 10 times due to unstable connectivity of server)
    while(n < 10 & download == FALSE){
      txt <- vroom::vroom_lines(url, skip_empty_rows = TRUE)
      if(length(txt) > 100) download <- TRUE
      n <<- n + 1
      Sys.sleep(0.1)
    }
  } else {
    txt <- vroom::vroom_lines(path, skip_empty_rows = TRUE)
  }

  # METAR
  m <- txt[seq_along(txt) %% 2 == 0]
  valid <- which(validUTF8(m)) # Check and exclude invalid UTF-8
  m <- m[valid]

    # Time
  t <- as.POSIXct(txt[seq_along(txt) %% 2 == 1], format = "%Y/%m/%d %H:%M", tz = "GMT")
  t <- t[valid]

  # Remove double spaces
  m <- str_replace(m, "\\s{2,}", "\\s")

  m1a <- str_subset(m, "[0-9A-Z]{4}\\s[0-9]{6}Z")
  m1b <- str_subset(m, "[0-9A-Z]{4}\\s[0-9]{6}Z", negate = TRUE)

  cat(length(m), length(m1a), "\\n", sep = " ")
  cat("Disregarded: ", m1b, "", sep = "\n")

  # Remove duplicates
  key <- str_extract(m,"^([A-Z0-9]{4}\\s[0-9]{6}Z)")
  m <- m[!duplicated(key)]
  t <- t[!duplicated(key)]

  # Filter latest
  if(latest.only){
    id <- str_extract(m, "^([A-Z0-9]{4})")
    ind <- tapply(seq_along(id), id, max)
    m <- m[ind]
    t <- t[ind]
  }
  data.table(time_valid = t, metar = m)
}

#' Read METAR reports from mesonet.agron.iastate.edu
#'
#' @author M. Saenger
#' @description Read METAR reports from mesonet.agron.iastate.edu
#' @param id_icao Lorem Ipsum
#' @param remote Download file from mesonet.agron.iastate.edu
#' @param path Path to local file
#' @param date_start Lorem Ipsum
#' @param date_end Lorem Ipsum
#' @param verbose Lorem Ipsum
#' @export
#' @examples
#' read_metar_mesonet(id_icao = "KDEN", date_start = Sys.time() - 3600*24*1, verbose = T)
#'
read_metar_mesonet <- function(id_icao = "LSZH", remote = TRUE, path, date_start = date_end - 3600*24*7, date_end = Sys.time()+3600*24, verbose = FALSE){

  date_start <- as.POSIXct(as.character(date_start), tz = "UTC")
  date_end <- as.POSIXct(as.character(date_end), tz = "UTC")
  if(date_end == Sys.Date()) date_end <- date_end + 3600*24

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
  if(remote){
    url.1 <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?"
    url.2 <- paste0(mapply(def, names(def), FUN = function(i, j) paste0(j, "=", i), SIMPLIFY = F), collapse = "&")

    url <- sprintf("%s%s&tz=UTC&latlon=no&trace=T&direct=no&report_type=1&report_type=2", url.1, url.2)
    if(verbose) print(url)

    # Read
    dt <- data.table::fread(url, colClasses = c("character", "character", "character"))
  } else {
    dt <- data.table::fread(path, colClasses = c("character", "character", "character"))
  }

  # Move COR flag to right position (after time string)
  dt[, metar := gsub("(^COR\\s)([A-Z0-9]{4}\\s)([0-9]{6}Z\\s)", "\\2\\3\\1", dt$metar, perl = TRUE)]

  # Set TZ UTC
  dt$valid <-  as.POSIXct(format(dt$valid), tz = "UTC")

  dt
}

#' Look up airport information
#'
#' @author M. Saenger
#' @param  id.icao Vector of ICAO identifiers
#' @param fi.name Lorem Ipsum
#' @param fi.icao Lorem Ipsum
#' @param fi.ctry Lorem Ipsum
#' @param fi.lat Lorem Ipsum
#' @param fi.lon Lorem Ipsum
#' @param is.active Filter by active stations
#' @export
#' @examples
#' # By ICAO Identifier
#' get_metar_stn(id.icao = c("LSZH", "YSSY"))
#' # Central Europe
#' get_metar_stn(fi.icao = "^L", fi.lon = c(5, 11), fi.lat = c(46, 49))
#' # South America (Argentina)
#' get_metar_stn(fi.icao = "^SA")
#' # Contry filter (New Zealand)
#' get_metar_stn(fi.ctry = "Brazil", fi.lat = c(-40, -30))
#'
get_metar_stn <- function(id.icao = metar.stn$icao, fi.name = ".+", fi.icao = ".+", fi.ctry = ".+", fi.lat = c(-90, 90),
  fi.lon = c(-180, 180), is.active = TRUE){
  metar.stn[
    icao %in% id.icao &
    grepl(fi.icao, icao, ignore.case = TRUE) &
    grepl(fi.name, ap_name, ignore.case = TRUE) &
    grepl(fi.ctry, ctry_name, ignore.case = TRUE) &
    data.table::between(lon, fi.lon[1], fi.lon[2]) &
    data.table::between(lat, fi.lat[1], fi.lat[2]) &
    active %in% is.active
  ]
}

