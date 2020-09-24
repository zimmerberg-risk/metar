#' Parse a METAR report
#'
#' @author M. Saenger
#' @description Parse a METAR report
#' @param x METAR strings
#' @param date date
#' @import stringr
#' @export
#' @examples parse_metar(x = "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG")
#' @examples parse_metar(x = metar.test)
#'
parse_metar <- function(x, date = NULL){

  icao <- str_extract(x, "^[A-Z0-9]{4}")

  if(is.null(date)){
    time <-  as.POSIXct(strptime(x = paste0(format(Sys.Date(), "%Y"), format(Sys.Date(), "%m"), str_extract(x, "([0-9]{6})(?=Z)")), format = "%Y%m%d%H%M", tz = "UTC"))
  } else {
    time <- as.POSIXct(format(date), tz = "UTC")
  }

  metar <- str_extract(x, "(?<=^[A-Z0-9]{4}\\s[0-9]{6}Z\\s)(.*?)(?=RMK|BECMG|TEMPO|$)")
  rmk <- str_extract(x, "(?<=RMK )(.*?)(?=BECMG|TEMPO|$)")
  cor <- grepl("\\bCOR\\b", x)*1
  speci <- grepl("\\bSPECI\\b", x)*1
  auto <-  grepl("\\bAUTO\\b", x)*1
  wx <- str_extract(x, "(CAVOK|NSC|NCD|WXNIL|CLR|SKC|NSW)")

  wind <- str_match(metar,"([0-9VRB\\/]{3})?([0-9\\/]{2})G?([0-9]{2,3})?(KT|MPH|MPS)")[,-1] %>% rbind %>% as.data.table %>% stats::setNames( c("dir", "ff", "fx", "ff_unit"))
  wind.var <- str_match(metar, "\\b([0-9\\/]{3})V([0-9\\/]{3})\\b")[,-1] %>% rbind %>% as.data.table %>% stats::setNames(c("dir_from", "dir_to"))

  # str_match(c("1/4SM", "1 1/4SM", "3SM", "9999"), "\\b([0-9]{4}|[0-9]{1,2}\\s[0-9]{1}\\/[0-9]{1}(?=SM)|[0-9]{1}\\/[0-9]{1}(?=SM)|(?!<\\/)[0-9]{1,2}(?=SM))(SM)?(NDV)?\\b")
  vis <- str_match(metar, "\\b([0-9]{4}|[0-9]{1,2}(?=KM)|[0-9]{1,2}\\s[0-9]{1}\\/[0-9]{1}(?=SM)|[0-9]{1}\\/[0-9]{1}(?=SM)|(?!<\\/)[0-9]{1,2}(?=SM))(SM|KM)?(NDV)?\\b")[,-1] %>% rbind %>% as.data.table %>% stats::setNames(c("vis", "vis_unit", "ndv"))
  #str_match("27010G20KT 9999 HZ SCT080 /// Q1015", "\\b([0-9]{4}|[0-9]{1,2}\\s[0-9]{1}\\/[0-9]{1}(?=SM)|[0-9]{1}\\/[0-9]{1}(?=SM)|(?!<\\/)[0-9]{1,2}(?=SM))(SM)?(NDV)?\\b")
  #vis[, unique(vis)]

  min.vis <- str_match(metar, "\\s([0-9]{4})(N|NE|E|SE|S|SW|W|NW)\\s")[,-1] %>% rbind %>% as.data.table %>% stats::setNames(c("min_vis", "min_vis_dir"))
  rvr <-  str_match_all(metar, "(R([0-9\\/CRL]{2,3})\\/(P|P|M|\\/)?([0-9\\/]{4})(D|U|N)?V?(VP|P|M)?([0-9]{4})?(FT|D|U|N)?){1,4}") %>% sapply(., function(i) paste(i[,1], collapse = " "))
  vvis <- str_match(metar, "(?<=VV)([0-9|\\/]{3})")[,-1]  %>% cbind %>% as.data.table %>% stats::setNames("vvis")

  cld <- str_match_all(metar, "(FEW|SCT|BKN|OVC|[\\/]{3})([0-9]{3}|[\\/]{3})([A-Z]{2,3}|\\/\\/\\/)?") %>% sapply(., function(i) paste(i[,1], collapse = " "))

  pw <- str_match_all(metar, sprintf("(\\+|\\-|VC|RE)?(%s)?((?:%s){1,3})", paste(pw.dc, collapse = "|"), paste(pw.ph, collapse = "|")))  %>% sapply(., function(i) paste(i[,1], collapse = " "))

  tttd <- str_match(metar, "\\s(M)?([0-9]{2})/(M)?([0-9]{2})?\\s")[,-1]  %>% rbind %>% as.data.table %>% stats::setNames(c("tt_sign", "tt", "td_sign", "td"))
  qnh <- str_match(metar, "(Q|A)([0-9]{4})")[,-1]  %>% rbind %>% as.data.table %>% stats::setNames(c("qnh_unit", "qnh"))

  ws <- str_match(metar, "(?<=WS\\s)(?:R[A-Z0-9]{2,3}|ALL RWY)(?=\\b)")[,1]

  dt <- cbind(icao, time, speci, auto, cor, wx, wind, wind.var, vis, min.vis, rvr, vvis, cld, tttd, pw, qnh, ws, metar, rmk) %>%
    data.table::as.data.table()

  # Assign class and default value
  void <- lapply(intersect(names(dt), names(metar.vars)), function(i){
    # i = "time"
    var.name <- i
    type <- metar.vars[[i]]$type
    fct <- ifelse(type != "POSIXct", paste("as", metar.vars[[i]]$type, sep = "."), "c") # Protect POSIXct (as.POSIXct fails)
    default <- metar.vars[[var.name]]$default
    suppressWarnings(dt[, (var.name) := lapply(.SD, fct), .SDcols = c(var.name)])
    dt[is.na(dt[[var.name]]) & !is.null(default), (var.name) := default]
  })

  wind.conversion = list(KT = 1, MPH = 1.1507767864273, MPS = 0.514444)
  vis.conversion = list(KM = 1000, M = 1, SM = 1609.344) # Statute miles
  qnh.conversion = list(Q = 1, A = 1/2.953)
  tt.conversion = list(M = -1, P = 1)

  # Assign conversion factors
  dt[, wind_factor := unlist(wind.conversion[ff_unit])]
  dt[, vis_factor := unlist(vis.conversion[vis_unit])]
  dt[, qnh_factor := unlist(qnh.conversion[qnh_unit])]
  dt[, tt_factor := unlist(tt.conversion[tt_sign])]
  dt[, td_factor := unlist(tt.conversion[td_sign])]

  # Calculate
  dt[, `:=`(ff = ff*wind_factor, fx = fx*wind_factor, qnh = qnh*qnh_factor, tt = tt*tt_factor, td = td*td_factor,
            vis = unlist(lapply(str_replace(vis, "\\s", "\\+0*"), function(i) eval(parse(text = i))))*vis_factor)]

  dt[, `:=`(vis = data.table::fifelse(`%in%`(wx, "CAVOK"), 9999, vis))]
  dt[, `:=`(cld = data.table::fifelse(`%in%`(wx, c("NCD", "NSC", "SKC")), wx, cld))]
  dt[, `:=`(cld = data.table::fifelse(!`%in%`(vvis, NA), "IMC", cld))]

  #  TZ
  attr(dt$time, 'tzone') <- "UTC"

  # Drop auxiliary columns
  drop.col <-  c(names(metar.vars)[unname(sapply(metar.vars, '[[', "drop")) == F])
  dt[, .SD, .SDcols = intersect(names(dt), drop.col)]

}

#' Parse a METAR PW groups
#'
#' @author M. Saenger
#' @param pw METAR PW string
#' @export
#' @examples metar_pw(pw = "+TSRA BR VCTS")
#' @examples metar_pw(pw = parse_metar(metar.test)$pw)
#'
metar_pw <- function(pw){
  # pw <- c("RADZ VCSH RERA", "SNRA +SNDZ SHRADZ", "FZRA VCFG", "", "MIFG BR", "BLSN", "VCTS", "+TS", "-RA FZFG", "+TSSHRA", "VCTS +RA BR", "MIFG")

  dt.pw <- data.table(pw)
  dt.pw[, `:=`(id = .I, recent = str_extract(pw, "(?<=RE)([A-Z]+)\\b"), pw = str_remove(pw, "(\\s)?RE[A-Z]+\\b"))]
  setkey(dt.pw, id)

  # Lookup
  dt.ph.lut <- rbindlist(metar.ph, idcol = "ph")[ph != "NSW"]

  # Summary
  m <- str_match_all(dt.pw$pw, sprintf("(\\+|\\-)?(VC)?(%s)?((?:%s){1,3})", paste(pw.dc, collapse = "|"), paste(pw.ph, collapse = "|")))
  dt.res <- data.table(do.call(rbind, m)) %>% data.table::setnames(c("pw_grp",  names(metar.vars.pw)))
  dt.res$id <- rep(seq_along(m), sapply(m, nrow))
  dt.res[, id_pw_grp := 1:.N, id]
  dt.res <- data.table::dcast(dt.res, id ~ id_pw_grp, value.var = c("pw_grp",  names(metar.vars.pw)))

  # Phenomena
  m.ph <- str_match(dt.pw$pw, sprintf("((?<!VC)%s)", paste(pw.ph, collapse = ")|(?<!VC)(")))
  m.ph[!is.na(m.ph)] <- 1
  m.ph[is.na(m.ph)] <- 0
  class(m.ph) <- "integer"
  dt.ph <- data.table(m.ph[,-1, drop = FALSE])
  setnames(dt.ph, pw.ph)

  # VC Phenomena
  m.ph.vc <- str_match(dt.pw$pw, sprintf("((?<=VC)%s)", paste(pw.ph, collapse = ")|(?<=VC)(")))
  m.ph.vc[!is.na(m.ph.vc)] <- 1
  m.ph.vc[is.na(m.ph.vc)] <- 0
  class(m.ph.vc) <- "integer"
  dt.ph.vc <- data.table(m.ph.vc[,-1, drop = FALSE])
  setnames(dt.ph.vc,  paste0("VC_", pw.ph))

  # Descriptor
  m.dc <- str_match(dt.pw$pw, sprintf("((?<!VC)%s)", paste(pw.dc, collapse = ")|(?<!VC)(")))
  m.dc[!is.na(m.dc)] <- 1
  m.dc[is.na(m.dc)] <- 0
  class(m.dc) <- "integer"
  dt.dc <- data.table(m.dc[,-1, drop = FALSE])
  setnames(dt.dc, pw.dc)
  setnames(dt.dc, c("SH", "TS"), c("dc_SH", "dc_TS"))

  # m.dc <- str_match_all()
  # dt.dc <- data.table(do.call(rbind, m.dc)[,-1, drop = FALSE])
  # dt.dc$id <- rep(seq_along(m.dc), sapply(m.dc, nrow))
  # dt.dc <- dt.dc[, lapply(.SD, function(i) 1*any(!is.na(i))), id]
  # setnames(dt.dc, c( "id", pw.dc))
  # setnames(dt.dc, c("SH", "TS"), c("dc_SH", "dc_TS"))
  # setkey(dt.dc, id)

  # Combine
  dt.comb <- cbind(dt.pw, dt.ph, dt.ph.vc, dt.dc)
  dt.comb[, id := .I]
  dt.comb <- merge(dt.res, dt.comb, by = "id", all.y = TRUE)

  #dt.comb <- Reduce(function(...) merge(..., all = TRUE), list(dt.pw, dt.res, dt.ph, dt.ph.vc, dt.dc))

  # Derivatives
  dt.comb[, `:=`(is_pp = do.call(pmax,.SD)), .SDcols = dt.ph.lut[type == "Precipitation"]$ph]
  dt.comb[, `:=`(is_obsc = do.call(pmax,.SD)), .SDcols = dt.ph.lut[type == "Obscuration"]$ph]
  dt.comb[, `:=`(is_liquid = do.call(pmax,.SD)), .SDcols = dt.ph.lut[subtype == "Liquid"]$ph]
  dt.comb[, `:=`(is_solid = do.call(pmax,.SD)), .SDcols = dt.ph.lut[subtype == "Solid"]$ph]
  dt.comb[, `:=`(int = str_extract(pw, "([\\+\\-])"))]
  dt.comb[, `:=`(int = dplyr::case_when(int == "+" ~ 3, int == "-" ~ 1, TRUE ~ 2))]
  dt.comb[, `:=`(is_mixed = (is_liquid * is_solid))]
  dt.comb[, `:=`(pw = NULL)][]
  #dt.comb[, .N, int]

  #dt.comb <- Filter(function(x) !all(x == 0), dt.comb)   # Drop empty columns
  dt.comb
}

#' Parse a METAR cloud groups
#'
#' @author M. Saenger
#' @param cld METAR cloud string
#' @export
#' @examples metar_clouds(cld = c("FEW030 FEW032TCU SCT050 BKN075", "SCT050 SCT075 OVC149", "", "SCT003"))
#'
metar_clouds <- function(cld){
  dt.1 <- data.table(cld)
  dt.1[, `:=`(id = .I)]
  setkey(dt.1, id)

  m <- str_match_all(dt.1$cld, "(FEW|SCT|BKN|OVC|[\\/]{3})([0-9]{3}|[\\/]{3})([A-Z]{2,3}|\\/\\/\\/)?")
  dt.cld <- data.table(do.call(rbind, m))
  setnames(dt.cld, c(names(metar.vars.cld)))
  dt.cld$id <- rep(seq_along(m), sapply(m, nrow))
  dt.cld[, cld_level := seq_len(.N), id][order(id, cld_level)]

  void <- lapply(names(metar.vars.cld), function(i){
    fct <- paste("as", metar.vars.cld[[i]]$type, sep = ".")
    dt.cld[, (i) := lapply(.SD, fct), .SDcols = c(i)]
  })
  dt.cld[, cld_hgt := cld_hgt * 1e2]
  setkey(dt.cld, id)

  dt.ceil <- dt.cld[cld_amt %in% c("BKN", "OVC"), .(ceiling = min(cld_hgt)), id]
  setkey(dt.ceil, id)
  dt.agg <- dt.cld[, .(cld_levels = max(cld_level), cld_hgt_min = min(cld_hgt), cld_hgt_max = max(cld_hgt),
    cld_amt_max = cld.amt[max(match(cld_amt, cld.amt))], cld_amt_min = cld.amt[min(match(cld_amt, cld.amt))]), id]

  dt.cast <- data.table::dcast(dt.cld, id ~ cld_level, value.var = names(metar.vars.cld))
  setkey(dt.cast, id)

  dt <- Reduce(function(...) merge(..., all = TRUE), list(dt.1, dt.agg, dt.ceil, dt.cast))
  dt[, `:=`(id = NULL, cld = NULL)][]
}

#' Parse a METAR RVR groups
#'
#' @author M. Saenger
#' @param rvr METAR RVR string
#' @export
#' @examples metar_rvr(rvr = "R14/P2000N R16/P2000N R28/P2000N R34/1000VP2000U")
#'
metar_rvr <- function(rvr){
  m <- str_match_all(rvr, "(R([0-9\\/CRL]{2,3})\\/(P|P|M|\\/)?([0-9\\/]{4})(D|U|N)?V?(VP|P|M)?([0-9]{4})?(FT|D|U|N)?){1,4}")
  dt.rvr <- data.table::rbindlist(lapply(seq_along(m), FUN = function(i){
    dat <- rbind(m[[i]][,-1])
    if(nrow(dat) == 0) dat <- rbind(rep(NA, 8))
    dt <- data.table::setnames(data.frame(dat), c("rvr_str", names(metar.vars.rvr)))
    dt$rvr_group <- seq_len(nrow(dat))
    dt$id <- i
    dt
  }))
  void <- lapply(names(metar.vars.rvr), function(i){
    fct <- paste("as", metar.vars.rvr[[i]]$type, sep = ".")
    dt.rvr[, (i) := lapply(.SD, fct), .SDcols = c(i)]
  })
  data.table::dcast(dt.rvr[], id ~ rvr_group, value.var = names(metar.vars.rvr))
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
#' @import curl
#' @export
#' @examples read_mesonet(id_icao = "KDEN", date_start = Sys.time() - 3600*24*1)
#'
read_mesonet <- function(id_icao = "HSSS", date_start = date_end - 3600*48, date_end = Sys.time()+3600*24, auto = TRUE, verbose = FALSE){

  # Convert TZ UTC
  attr(date_start, 'tzone') <- "UTC"
  attr(date_end, 'tzone') <- "UTC"

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
#' @examples read_station(fi.icao = "^L", fi.lon = c(5, 11), fi.lat = c(46, 49))
#'
read_station <- function(fi.name = ".+", fi.icao = ".+", fi.ctry = ".+", fi.lat = c(-90, 90), fi.lon = c(-180, 180)){
  url.station <- "https://www.aviationweather.gov/docs/metar/stations.txt"
  col.names <- c("state", "name", "icao", "iata", "synop", "lat_deg", "lat_min", "lon_deg", "lon_min", "z", "M",  "N",  "V",  "U",  "A", "C", "ctry")
  col.start <- c(1, 4, 21, 27, 33, 40, 43, 48, 52, 56, 63, 66, 69, 72, 75, 80, 82)
  col.pos <- readr::fwf_positions(col.start, c(col.start[-1] - 1, NA), col_names = col.names)

  station.lines <- readr::read_lines(url.station, skip = 39)
  station.lines <- station.lines[sapply(station.lines, nchar) > 80]
  #station.lines <- station.lines[purrr::map_int(station.lines, nchar) > 80]

  dt.station <- readr::read_fwf(station.lines, col_positions = col.pos)

  dt.station <- dt.station %>%
    tidyr::separate(lat_min, c("lat_num", "ns"), sep = -1, remove = F) %>%
    tidyr::separate(lon_min, c("lon_num", "we"), sep = -1, remove = F) %>%
    dplyr::mutate(
      x = dplyr::case_when(we == "E" ~ as.numeric(lon_deg) + as.numeric(lon_num)/60, TRUE ~ - (as.numeric(lon_deg) + as.numeric(lon_num)/60)),
      y = dplyr::case_when(ns == "N" ~ as.numeric(lat_deg) + as.numeric(lat_num)/60, TRUE ~ - (as.numeric(lat_deg) + as.numeric(lat_num)/60))
    ) %>%
    dplyr::select(ctry, name, icao, synop, iata, x, y, z) %>%
    as.data.table

  dt.station[grepl(fi.name, name, ignore.case = TRUE) & grepl(fi.ctry, ctry, ignore.case = TRUE) & grepl(fi.icao, icao, ignore.case = TRUE) &
               between(x, fi.lon[1], fi.lon[2]) & between(y, fi.lat[1], fi.lat[2])]
}

#' Get METARs from NOAA cycle files
#'
#' @author M. Saenger
#' @param id_icao Lorem Ipsum
#' @param latest Lorem Ipsum
#' @export
#' @description  metar_latest("LSZH")
#'
metar_latest <- function(id_icao = NULL, latest = TRUE){

  time.local <- Sys.time()
  attr(time.local, "tzone") <- "UTC"
  time.floor <- lubridate::floor_date(time.local, "1 hour")
  url.data <- sprintf("https://tgftp.nws.noaa.gov/data/observations/metar/cycles/%02dZ.TXT", hour(time.floor))
  dt.lines <- readr::read_lines(url.data)
  reports <- dt.lines[grepl("^([A-Z]{4}).+", dt.lines)]

  id <- str_extract(reports,"^([A-Z]{4})") # extract id icao
  reports <- reports[which(id %in% id_icao)] # Filter

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

