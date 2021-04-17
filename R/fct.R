#' Parse METAR reports
#'
#' @author M. Saenger
#' @description Parse a METAR report
#' @param x METAR strings
#' @param date date
#' @export
#' @examples
#'  parse_metar(x = "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG")
#'  parse_metar(x = metar.test)
#'
parse_metar <- function(x, date = NULL){

  if(length(x) == 0){
    warning("No data found - returning empty data table")
    return(data.table())
  }

  icao <- str_extract(x, "(?<=COR|METAR|\\s|^)\\b([A-Z0-9]{4})\\b")

  if(is.null(date)){
    time <-  as.POSIXct(strptime(x = paste0(format(Sys.Date(), "%Y"), format(Sys.Date(), "%m"), str_extract(x, "([0-9]{6})(?=Z)")), format = "%Y%m%d%H%M", tz = "UTC"))
  } else {
    time <- as.POSIXct(format(date), tz = "UTC")
  }

  # Meta data
  metar <- trimws(str_extract(x, "(?<=^[A-Z0-9]{4}\\s[0-9]{6}Z\\s)(.*?)(?=RMK|BECMG|TEMPO|$)"))
  rmk <- trimws(str_extract(x, "(?<=RMK )(.*?)(?=BECMG|TEMPO|$)"))
  cor <- grepl("\\bCOR\\b", x)*1
  speci <- grepl("\\bSPECI\\b", x)*1
  auto <-  grepl("\\bAUTO\\b", x)*1
  wx <- str_extract(x, "(CAVOK|NSC|NCD|WXNIL|CLR|SKC|NSW)")

  # Wind
  regex.wind <- "([0-9VRB\\/]{3})?([0-9\\/]{2})G?([0-9]{2,3})?(KT|MPH|MPS)"
  wind <- rbind(str_match(metar, regex.wind)[,-1])
  colnames(wind) <- c("dir", "ff", "fx", "ff_unit")

  regex.wind.var <- "\\b([0-9\\/]{3})V([0-9\\/]{3})\\b"
  wind.var <- rbind(str_match(metar, regex.wind.var)[,-1])
  colnames(wind.var) <- c("dir_from", "dir_to")

  # Meteorological, vertical and runway visibility
  regex.vis <- "\\b([0-9]{4}|[0-9]{1,2}(?=KM)|[0-9]{1,2}\\s[0-9]{1}\\/[0-9]{1}(?=SM)|[0-9]{1}\\/[0-9]{1}(?=SM)|(?!<\\/)[0-9]{1,2}(?=SM))(SM|KM)?(NDV)?\\b"
  vis <- rbind(str_match(metar, regex.vis)[,-1])
  colnames(vis) <- c("vis", "vis_unit", "ndv")

  regex.vis.min <- "\\s([0-9]{4})(N|NE|E|SE|S|SW|W|NW)\\s"
  vis.min <- rbind(str_match(metar, regex.vis.min)[,-1])
  colnames(vis.min) <- c("min_vis", "min_vis_dir")

  #regex.rvr <- "(R([0-9\\/CRL]{2,3})\\/(P|P|M|\\/)?([0-9\\/]{4})(D|U|N)?V?(VP|P|M)?([0-9]{4})?(FT|D|U|N)?){1,4}"
  regex.rvr <- "((R([0-9\\/CRL]{2,3})\\/(P|P|M|\\/)?([0-9\\/]{4})(D|U|N)?V?(VP|P|M)?([0-9]{4})?(FT|D|U|N)?){1,4})"
  rvr <- cbind(sapply(str_extract_all(metar, regex.rvr), paste, collapse = " "))
  colnames(rvr) <- c("rvr")

  regex.vvis <- "(?<=VV)([0-9|\\/]{3})"
  vvis <- cbind(str_match(metar, regex.vvis)[,-1])
  colnames(vvis) <- c("vvis")

  # Clouds
  regex.cld <- "(FEW|SCT|BKN|OVC|[\\/]{3})([0-9]{3}|[\\/]{3})([A-Z]{2,3}|\\/\\/\\/)?"
  cld <- cbind(sapply(str_extract_all(metar, regex.cld), paste, collapse = " "))
  colnames(cld) <- c("cld")

  # Present weather
  regex.pw <- sprintf("(\\+|\\-|VC|RE)?(%s)?((?:%s){1,3})", paste(pw.dc, collapse = "|"), paste(pw.ph, collapse = "|"))
  pw <- cbind(sapply(str_extract_all(metar, regex.pw), paste, collapse = " "))
  colnames(pw) <- c("pw")

  # Temperature/dew point
  regex.tttd <- "\\s(M)?([0-9]{2})/(M)?([0-9]{2})?\\s"
  tttd <- rbind(str_match(metar, regex.tttd)[,-1])
  colnames(tttd) <-c("tt_sign", "tt", "td_sign", "td")

  # Pressure
  regex.qnh <- "(Q|A)([0-9]{4})"
  qnh <- rbind(str_match(metar, regex.qnh)[,-1])
  colnames(qnh) <- c("qnh_unit", "qnh")

  # Wind shear
  regex.ws <- "((?<=WS\\s)(?:R[A-Z0-9]{2,3}|ALL RWY)(?=\\b))"
  ws <- cbind(str_match(metar, regex.ws)[,-1])
  colnames(ws) <- c("ws")

  # Combine
  dt <- as.data.table(cbind(icao, time, speci, auto, cor, wx, wind, wind.var, vis, vis.min, rvr, vvis,
    cld, tttd, pw, qnh, ws, metar, rmk))

  # Assign class and default value
  void <- lapply(intersect(names(dt), names(metar.vars)), function(i){
    var.name <- i
    type <- metar.vars[[i]]$type
    fct <- ifelse(type != "POSIXct", paste("as", metar.vars[[i]]$type, sep = "."), "c") # Protect POSIXct (as.POSIXct fails)
    default <- metar.vars[[var.name]]$default
    suppressWarnings(dt[, (var.name) := lapply(.SD, fct), .SDcols = c(var.name)])
    dt[is.na(dt[[var.name]]) & !is.null(default), (var.name) := default]
  })

  # Covert time to POSIXct
  dt[, time := as.POSIXct(as.integer(time), origin = "1970-01-01", tz = "UTC")]

  # Convert units
  wind.conversion <- list(KT = 1, MPH = 0.868976, MPS = 1.94384) # to knots
  vis.conversion <- list(KM = 1000, M = 1, SM = 1609.344) # to meters,  (statute miles)
  qnh.conversion <- list(Q = 1, A = 1/2.953)
  tt.conversion <- list(M = -1, P = 1)

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
  dt[, `:=`(vvis = vvis*1e2)]

  # Drop auxiliary data columns
  drop.col <-  c(names(metar.vars)[unname(sapply(metar.vars, '[[', "drop")) == F])
  dt[, .SD, .SDcols = intersect(names(dt), drop.col)]
  dt

}
# x <- metar_latest(id_icao = "")
# dat <- parse_metar(x)
# dat[order(-fx)]

#' Parse a METAR PW groups
#'
#' @author M. Saenger
#' @param pw METAR PW string
#' @export
#' @examples
#' metar_pw(pw = "+TSRA BR VCTS")
#' metar_pw(pw = parse_metar(metar.test)$pw)
#'
metar_pw <- function(pw){
  # Debug
  # pw <- c("RADZ VCSH RERA", "SNRA +SNDZ SHRADZ", "FZRA VCFG", "", "MIFG BR", "BLSN", "VCTS", "+TS", "-RA FZFG", "+TSSHRA", "VCTS +RA BR", "MIFG")

  dt.pw <- data.table(pw)
  dt.pw[, `:=`(id = .I, recent = str_extract(pw, "(?<=RE)([A-Z]+)\\b"), pw = str_remove(pw, "(\\s)?RE[A-Z]+\\b"))]
  setkey(dt.pw, id)

  # Lookup
  dt.ph.lut <- rbindlist(metar.ph, idcol = "ph")[ph != "NSW"]

  # Summary
  m <- str_match_all(dt.pw$pw, sprintf("(\\+|\\-)?(VC)?(%s)?((?:%s){1,3})", paste(pw.dc, collapse = "|"), paste(pw.ph, collapse = "|")))
  dt.res <- data.table(id = 1:length(m))
  dt.res <- rbindlist(lapply(m, as.data.frame), idcol = "id")[dt.res, on = "id"]
  setnames(dt.res, new = c("id", "pw_grp",  names(metar.vars.pw)))

  dt.res[, id_pw_grp := 1:.N, id]
  dt.res <- data.table::dcast(dt.res, id ~ id_pw_grp, value.var = c("pw_grp",  names(metar.vars.pw)))

  # Phenomena
  m.ph <- str_match(dt.res$ph_1, sprintf("((?<!VC)%s)", paste(pw.ph, collapse = ")|(?<!VC)(")))
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
  dt.comb[, `:=`(is_sandstorm = do.call(pmax,.SD)), .SDcols = dt.ph.lut[group == "Sand-/Duststorm"]$ph]
  dt.comb[, `:=`(is_storm = do.call(pmax,.SD)), .SDcols = dt.ph.lut[group == "Storm/Hail" & ph != "TS"]$ph]

  dt.comb[, `:=`(int = str_extract(pw, "([\\+\\-])"))]
  dt.comb[, `:=`(int = fifelse(int == "+", 3, fifelse(int == "-", 1, 2)))]
  dt.comb[, `:=`(is_mixed = (is_liquid * is_solid))]
  dt.comb[, `:=`(pw = NULL)][]

  dt.comb
}

#' Parse a METAR cloud groups
#'
#' @author M. Saenger
#' @param cld METAR cloud string
#' @export
#' @examples
#'  metar_clouds(cld = c("FEW030 FEW032TCU SCT050 BKN075", "SCT050 SCT075 OVC149", "", "SCT003"))
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
#' @examples
#' metar_rvr(rvr = "R14/P2000N R16/P2000N R28/P2000N R34/1000VP2000U")
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

# --------------------------------------------------- Formulas --------------------------------------------


#' Vapour pressure saturated (wrapper function)
#'
#' @author M. Saenger
#' @description Vapour pressure saturated (wrapper function)
#' ECMWF IFS Documentation – Cy41r2, Part IV, formula 7.5
#' @param tt °C
#' @param ice true/false
#' @export
#' @examples wx_es(30, TRUE)
#'
wx_es <- function(tt, ice = FALSE){
  if(ice){
    wx_es_i(tt)
  } else {
    wx_es_w(tt)
  }
}

#' Vapour pressure saturated (over water)
#'
#' @author M. Saenger
#' @description ECMWF IFS Documentation – Cy41r2, Part IV, formula 7.5
#' @param tt °C
#' @export
#' @examples wx_es_w(30)
#'
wx_es_w <- function(tt){
  tt <- tt + 273.16
  es <- 611.21 * exp(17.502 * ((tt - 273.16)/(tt + 32.19)))
  es / 100
}

#' Vapour pressure saturated (over ice)
#'
#' @author M. Saenger
#' @description ECMWF IFS Documentation – Cy41r2, Part IV, formula 7.5
#' @param tt °C
#' @export
#' @examples wx_es_i(-30)
#'
wx_es_i <- function(tt){
  tt <- tt + 273.16
  es <- 611.21 * exp(22.587 * ((tt - 273.16)/(tt - 0.7)))
  es / 100
}

#' Relative humidity
#'
#' @author M. Saenger
#' @param tt °C Air temperature
#' @param td °C Dew point
#' @param ice over ice
#' @export
#' @examples
#' wx_rh(5, -35)
#'
wx_rh <- function(tt, td, ice = FALSE){
  100 * wx_es(td, ice)/wx_es(tt, ice)
}

