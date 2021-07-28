#' Get METARs from NOAA cycle files
#'
#' @author M. Saenger
#' @param hour specific reporting hour (last 24h)
#' @param latest.only latest report per aiport only
#' @export
#' @description
#' metar_read_noaa(hour = 15, latest.only = T)
#'
metar_read_noaa <- function(hour, latest.only = TRUE){
  p <- sprintf("https://tgftp.nws.noaa.gov/data/observations/metar/cycles/%02dZ.TXT", hour)

  n <- 0 # Iterator
  download <- FALSE

  # Download (try up to 6 times)
  while(n < 6 & download == FALSE){
    txt <- vroom_lines(p, skip_empty_rows = TRUE)
    if(length(txt) > 100) download <- TRUE
    n <<- n + 1
    Sys.sleep(0.1)
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

