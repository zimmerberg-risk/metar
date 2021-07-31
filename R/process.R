## --------------------------------------------------- Processing  ---------------------------------------------------

#' Parse present wather groups
#'
#' @author M. Saenger
#' @param x present weather string
#' @export
#' @examples
#' x <- c("TS", "+TSRA VCTS", "SNGRRA VCFG")
#' parse_metar_pw(x)
#' m <- parse_metar_grp(metar.test$code)
#' parse_metar_pw(m$pw)
#'
parse_metar_pw <- function(x){
  # Debug

  #x <- c(NA, "VCSH SHSN RERASN", "RADZ VCSH RERA RESH", "SNRA +SNDZ SHRADZ", "FZRA VCFG", "", "MIFG BR", "BLSN", "VCTS", "+TS", "-RA FZFG", "+TSSHRA", "VCTS +RA BR", "MIFG")

  # Severe
  TS <- str_detect(x, "(?!<VC)(TS|FC|SQ)")
  VCTS <- str_detect(x, "VCTS")
  ST <- str_detect(x, "DS|SS|PO")
  GR <- str_detect(x, "GR")

  # Precip
  ph.solid <- metar.ph[id_ph_subgrp %in% "SOLID"]$id_ph
  ph.liquid <-  metar.ph[id_ph_subgrp %in% "LIQUID"]$id_ph
  ph.pp <- c(ph.solid, ph.liquid)

  SH <- str_detect(x, "(?!<VC)SH")
  VCSH <- str_detect(x, "VCSH")
  PP_HEAVY <- str_detect(x, sprintf("\\+[A-Z]*(%s|TS)", paste(ph.pp, collapse = "|")))
  PP_SOLID <-  str_detect(x, sprintf("(?<!DR|BL)(%s)", paste(ph.solid, collapse = "|")))
  PP_LIQUID <- str_detect(x, sprintf("%s", paste(ph.liquid, collapse = "|")))
  PP_MIXED <- PP_SOLID*PP_LIQUID
  PP_FZ <- str_detect(x, "FZ(?!FG)")
  PP <- pmax(PP_SOLID, PP_LIQUID)

  # Obscuration
  DR <- str_detect(x, "DR|BL")
  FG <- str_detect(x, "(?<!MI|PR|BC|VC)FG")
  FG_FZ <- str_detect(x, "FZFG")
  FG_ANY <- str_detect(x, "FG")
  VCFG <- str_detect(x, "VCFG")

  # Freezing (all)
  FZ <- pmax(PP_FZ, FG_FZ)

  # Column order is strictly by SIGWX priority GR > TS > ST > FZ > PP_HEAVY > PP_SOLID > PP_LiQUID > FG > vc_TS > vc_FG
  pw <- cbind(GR, TS, ST, FZ, PP_HEAVY, PP_SOLID, PP_LIQUID, FG, VCTS, VCFG, NOSIG=1, SH, VCSH, PP_MIXED, PP_FZ, PP, DR, FG_FZ, FG_ANY)
  pw[is.na(pw)] <- 0

  # Significant weather
  SIGWX <- colnames(pw)[unlist(apply(pw[,1:11], 1, which.max))]

  # PW Groups
  grp <- str_split_fixed(x, "\\s", n = 4)
  grp[grp == ""] <- NA_character_
  colnames(grp) <- paste0("pw_grp_", 1:4)

  # Phenomena
  ph <- str_match(x, sprintf("(%s)", paste(metar.ph$id_ph, collapse = "|")))

  data.table(pw = x, pw, grp, sigwx = SIGWX)

}

#' Parse cloud groups
#'
#' @author M. Saenger
#' @param x cloud string
#' @export
#' @examples
#' x <- c("SCT009 BKN029 OVC045CB", "SCT009CB OVC045", "NSC")
#' parse_metar_cld(x)
#' m <- parse_metar_grp(metar.test$code)
#' parse_metar_cld(x = m$cld)
#'
parse_metar_cld <- function(x){

  n <- 5 # Number of cloud groups

  # Meta
  cld.amt <- metar.class[id_para == "cld_amt"]
  cld.amt.num <- cld.amt$num_class
  cld.amt.id <- cld.amt$id_class

  # Cloud groups (max: 5)
  grp <- str_split_fixed(x, "\\s", n = n)
  cld_hgt <- rbind(apply(grp, 2, str_extract, pattern = "[0-9]+"))
  class(cld_hgt) <- "numeric"
  colnames(cld_hgt) <- paste("cld_hgt", 1:n, sep = "_")
  cld_amt <- rbind(apply(grp, 2, str_extract, pattern = "^[A-Z]{3}"))
  colnames(cld_amt) <- paste("cld_amt", 1:n, sep = "_")
  cld_type <- rbind(apply(grp, 2, str_extract, pattern = "(TCU|CB)$"))
  colnames(cld_type) <- paste("cld_type", 1:n, sep = "_")

  # Cloud type
  TCU <- str_detect(x, "TCU")*1
  TCU[is.na(TCU)] <- 0
  CB <- str_detect(x, "CB")
  CB[is.na(CB)] <- 0


  # Heights, ceiling
  ceil <- as.numeric(str_extract(x, "(?<=(BKN|OVC))[0-9]{3}"))
  cld_hgt_min <- apply(cld_hgt, 1, function(i) if(all(is.na(i))) NA else min(i, na.rm = T))
  cld_hgt_max <- apply(cld_hgt, 1, function(i) if(all(is.na(i))) NA else max(i, na.rm = T))

  # Cloud amount
  cld_lay <- apply(cld_amt, 1, function(i) sum(!is.na(i)))
  cld_amt_num <- array(match(cld_amt, cld.amt.id), dim = dim(cld_amt))
  cld_amt_max <- cld.amt.id[apply(cld_amt_num, 1, function(i) if(all(is.na(i))) NA else max(i, na.rm = T))][1:length(cld_lay)]
  cld_amt_min <- cld.amt.id[apply(cld_amt_num, 1, function(i) if(all(is.na(i))) NA else min(i, na.rm = T))][1:length(cld_lay)]

  data.table(cld_grp = x,  cld_lay, cld_hgt_min, cld_hgt_max, cld_amt_min, cld_amt_max, ceil, TCU, CB, cld_amt, cld_hgt, cld_type)
}

#' Parse runway visibility groups
#'
#' @author M. Saenger
#' @param x cloud string
#' @export
#' @examples
#' x <- c("R14/P2000N R16/P2000N R28/P2000N R34/1000VP2000U", "R08R/3000VP6000FT", "R22/0700D", "", "R18L/290050")
#' parse_metar_rvr(x)
#'
parse_metar_rvr <- function(x){

  n <- 5 # Number of cloud groups

  # RVR groups (max: 5)
  grp <- str_split_fixed(x, "\\s", n = n)

  rwy <- rbind(apply(grp, 2, str_extract, pattern = "(?<=R)([0-9]{2}[CLR]?)"))
  colnames(rwy) <- paste("rwy", 1:n, sep = "_")

  rvr_min <- rbind(apply(grp, 2, str_extract, pattern = "(?<=\\/[PP]?)([0-9]{4})(?=[DUN]?)"))
  class(rvr_min) <- "numeric"
  colnames(rvr_min) <- paste("rvr_min", 1:n, sep = "_")

  rvr_max <- rbind(apply(grp, 2, str_extract, pattern = "(?<=\\/[PP]?)([0-9]{4})(?=[DUN]?)"))

  rvr_trend <- rbind(apply(grp, 2, str_extract, pattern = "[DUN]"))

}


m <- parse_metar_grp(dt.noaa$metar)
xxx <- parse_metar_cld(x = rep(m$cld, 1))
yyy <- parse_metar_rvr(x = rep(m$rvr, 1))


#' Parse runway conditions groups
#'
#' @author M. Saenger
#' @param x cloud string
#' @export
#' @examples
#' x <- c("R02/010070 R06/010070", "R88/090060")
#' parse_metar_rwy_cond(x)
#'
parse_metar_rwy_cond <- function(){

}

#' Parse wind shear
#'
#' @author M. Saenger
#' @param x cloud string
#' @export
#' @examples
#' x <- c("WS R25R")
#' parse_metar_ws(x)
#'
parse_metar_ws <- function(){

}

## --------------------------------------------------- Data Sources  ---------------------------------------------------


#' Get METARs from NOAA cycle files
#'
#' @author M. Saenger
#' @param hour specific reporting hour (last 24h)
#' @param latest.only latest report per aiport only
#' @export
#' @description
#' read_metar_noaa(hour = 15, latest.only = T)
#'
read_metar_noaa <- function(hour, latest.only = TRUE){
  p <- sprintf("https://tgftp.nws.noaa.gov/data/observations/metar/cycles/%02dZ.TXT", hour)

  n <- 0 # Iterator
  download <- FALSE

  # Download (try up to 10 times due to unstable connectivity of server)
  while(n < 10 & download == FALSE){
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
#' read_metar_mesonet(id_icao = "KDEN", date_start = Sys.time() - 3600*24*1)
#'
read_metar_mesonet <- function(id_icao = "LSZH", date_start = date_end - 3600*24*7, date_end = Sys.time()+3600*24, auto = TRUE, verbose = FALSE){

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


