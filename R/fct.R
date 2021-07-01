#' Parse METAR reports
#'
#' @author M. Saenger
#' @description Parse a METAR report
#' @param x METAR strings
#' @param date date
#' @export
#' @examples
#'  parse_metar(x = "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG")
#'  parse_metar(x = "LSZH 271750Z AUTO /////KT //// ////// 17/// Q//// NOSIG")
#'  parse_metar(x = metar.test$code)
#'
parse_metar <- function(x, date){

  if(length(x) == 0){
    warning("No data found - returning empty data table")
    return(data.table())
  }

  icao <- str_extract(x, "(?<=COR|METAR|\\s|^)\\b([A-Z0-9]{4})\\b")

  if(missing(date)){
    time <-  as.POSIXct(strptime(x = paste0(format(Sys.Date(), "%Y"), format(Sys.Date(), "%m"), str_extract(x, "([0-9]{6})(?=Z)")), format = "%Y%m%d%H%M", tz = "UTC"))
  } else {
    time <- as.POSIXct(format(date), tz = "UTC")
  }

  # Meta data
  dt.stn <- metar_stn(id.icao = icao)

  metar <- str_trim(str_extract(x, "(?<=^[A-Z0-9]{4}\\s[0-9]{6}Z\\s)(.*?)(?=RMK|BECMG|TEMPO|$)"))
  rmk <- str_trim(str_extract(x, "(?<=RMK )(.*?)(?=BECMG|TEMPO|$)"))
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
  regex.vis <- "\\b([0-9]{4}|[0-9]{1,2}(?=KM)|[0-9]{1,2}\\s[0-9]{1}\\/[0-9]{1}(?=SM)|[0-9]{1}\\/[0-9]{1,2}(?=SM)|(?!<\\/)[0-9]{1,2}(?=SM))(SM|KM)?(NDV)?\\b"
  vis <- rbind(str_match(metar, regex.vis)[,-1])
  colnames(vis) <- c("vis", "vis_unit", "ndv")
  # Parse fractions
  vis[,1] <- sapply(str_replace(vis[,1], "\\s", "\\+0*"), function(i) eval(parse(text = i)) )

  regex.vis.min <- "\\s([0-9]{4})(N|NE|E|SE|S|SW|W|NW)\\s"
  vis.min <- rbind(str_match(metar, regex.vis.min)[,-1])
  colnames(vis.min) <- c("min_vis", "min_vis_dir")

  regex.rvr <- "R[0-9]{2}[CRL]?\\/([PM]?[0-9]{4}[DUN]?)(V[PM]?[0-9]{4}[DUN]?)?(FT)?\\b"
  rvr <- cbind(sapply(str_extract_all(metar, regex.rvr), paste, collapse = " "))
  colnames(rvr) <- c("rvr")

  regex.rwc <- "\\bR[0-9]{2}[CRL]?\\/[0-9\\/]{1}[1259\\/]{1}[0-9A-Z\\/]{4}\\b"
  rwc <- cbind(sapply(str_extract_all(metar, regex.rwc), paste, collapse = " "))
  colnames(rwc) <- c("rwc")

  regex.vvis <- "(?<=VV)([0-9|\\/]{3})"
  vvis <- cbind(str_match(metar, regex.vvis)[,-1])
  colnames(vvis) <- c("vvis")

  # Clouds
  regex.cld <- "(FEW|SCT|BKN|OVC|[\\/]{3})([0-9]{3}|[\\/]{3})([A-Z]{2,3}|\\/\\/\\/)?"
  cld <- cbind(sapply(str_extract_all(metar, regex.cld), paste, collapse = " "))
  colnames(cld) <- c("cld")

  # Present weather (up to 4 PW groups (US))
  regex.dc.ph <- paste(c(metar.dc$id_dc, metar.ph$id_ph), collapse = "|")
  regex.pw <- sprintf("((?:\\+|\\-)?\\b(?:VC|RE)?(?:%s){1,4}\\b\\s?){1,4}", regex.dc.ph)
  pw <- trimws(str_match(metar, regex.pw)[,1])

  # regex.dc <- paste(metar.dc$id_dc, collapse = "|")
  # regex.ph <- paste(metar.ph$id_ph, collapse = "|")
  # regex.pw <- sprintf("((\\+|\\-|VC|RE)?(%s)?((?:%s){1,4})\\b)+", regex.dc, regex.ph)
  # pw <- cbind(sapply(str_extract_all(metar, regex.pw), paste, collapse = " "))
  # colnames(pw) <- c("pw")

  # Temperature/dew point
  regex.tttd <- "(?<=\\s)(M)?([0-9]{2})/(M)?([0-9\\/]{2})(?=\\b)"
  tttd <- rbind(str_match(metar, regex.tttd)[,-1])
  colnames(tttd) <-c("tt_unit", "tt", "td_unit", "td")

  # Pressure
  regex.qnh <- "(Q|A)([0-9]{4})"
  qnh <- rbind(str_match(metar, regex.qnh)[,-1])
  colnames(qnh) <- c("qnh_unit", "qnh")

  # Wind shear
  regex.ws <- "((?<=WS\\s)(?:R[A-Z0-9]{2,3}|ALL RWY)(?=\\b))"
  ws <- cbind(str_match(metar, regex.ws)[,-1])
  colnames(ws) <- c("ws")

  # Combine
  dt <- as.data.table(cbind(icao, time, speci, auto, cor, wx, wind, wind.var, vis, vis.min, rvr, rwc, vvis,
    cld, tttd, pw, qnh, ws, metar, rmk))

  # Wind calm -> no wind dir
  dt[dir == "000", `:=`(dir = NA_real_)]

  # Assign class and default value
  para.list <- setdiff(intersect(names(dt), metar.para$id_para), "time")

  void <- lapply(para.list[], function(i){
    def <- metar.para[id_para == i]
    # Convert type
    suppressWarnings(dt[, (i) := do.call(paste0("as.", def$type_para), list(dt[[i]]))])
    # Set defaults
    dt[is.na(dt[[def$id_para]]) & !is.na(def$unit_str), (def$id_para) := def$unit_str]
  })

  # Covert time to POSIXct
  dt[, time := as.POSIXct(as.integer(time), origin = "1970-01-01", tz = "UTC")]

  # Convert units
  para.list.conv <- split(metar.para_conv, by = "id_para")
  void <- lapply(names(para.list.conv), function(i){
    def <- para.list.conv[[i]]
    unit <- dt[[paste(i, "unit", sep = "_")]]
    scale <- def$scale[match(unit, def$id_unit_para)]
    offset <- def$offset[match(unit, def$id_unit_para)]
    dt[, c(i) := get(i) * scale + offset]
  })

  # Processing
  dt[, `:=`(vis = data.table::fifelse(`%in%`(wx, "CAVOK"), 9999, vis))]
  dt[, `:=`(cld = data.table::fifelse(`%in%`(wx, c("NCD", "NSC", "SKC")), wx, cld))]
  dt[, `:=`(cld = data.table::fifelse(!`%in%`(vvis, NA), "IMC", cld))]
  dt[, `:=`(vvis = vvis*1e2)]

  # Drop auxiliary data columns
  cols.drop <- intersect(names(dt), metar.para[drop == TRUE]$id_para)
  dt[, (cols.drop) := NULL]

  merge(dt.stn[, .(icao, ctry, ap_name, lon, lat, elev)], dt, by = "icao", all = TRUE)
}

#' Parse METAR reports
#'
#' @author M. Saenger
#' @description Parse a METAR report
#' @param x METAR strings
#' @param date date
#' @export
#' @examples
#'  parse_metar(x = "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG")
#'  parse_metar(x = metar.test$code)
#'
metar_parse <- function(x, date){
  parse_metar(x, date)
}

#' Validate METAR
#'
#' @author M. Saenger
#' @description Validate METAR reports
#' @param dat Output from `metar_parse`
#' @param set.na set NA if validation fails
#' @export
#'
metar_validate <- function(dat, set.na = TRUE){

  dat$flag <- ""
  cat("Validation failed:\n")

  # Parameter data validation
  para.list <- intersect(names(dat), metar.para[!is.na(min)]$id_para)

  res <- lapply(para.list, function(i){
    para.range <- metar.para[id_para == i, c(min, max)]

    # Parameter range
    check <- !between(dat[[i]], para.range[1], para.range[2])
    if(i == "fx") check <- check | (dat$fx > dat$ff * 5) # Gust factor > 5
    if(i == "td") check <- check | (dat$td > dat$tt) # Dew point > temperature

    ind <- which(check)

    if(length(ind) > 0){
      dat[ind, flag := paste(flag, i, sep = ",")] # Flag

      # Log
      str <- dat[ind, paste("para=", i, " value=", get(i), " ", icao, " ", ctry, " ", metar, sep = "")]
      cat(str, sep = "\n")
      if(set.na) dat[[i]][ind] <- NA_real_ # Set NA
    }
  })
  dat[order(-flag)]
  dat
}

#' Parse METAR PW groups
#'
#' @author M. Saenger
#' @param pw METAR PW string
#' @param DC Add descriptor cross table
#' @param PH Add phenomenon cross table
#' @export
#'
metar_pw <- function(pw, DC = FALSE, PH = FALSE){
  # Debug
  # pw <- c(NA, "VCSH SHSN RERASN", "RADZ VCSH RERA RESH", "SNRA +SNDZ SHRADZ", "FZRA VCFG", "", "MIFG BR", "BLSN", "VCTS", "+TS", "-RA FZFG", "+TSSHRA", "VCTS +RA BR", "MIFG")

  n <- length(pw)

  # Split into PW (Current), RE (Recent) and VC (Vicinity)
  re <- trimws(str_match(pw, "(\\bRE[:alpha:]+\\b\\s?){1,2}")[,1])
  vc <- trimws(str_match(pw, "(\\bVC[:alpha:]+\\b\\s?){1,2}")[,1])
  pw <- trimws(str_remove_all(pw, "\\b(?:RE|VC)[:alpha:]+\\b"))

  # PW classes
  pw.regex <- list(
    GR = "GR",
    TS = "TS|FC|SQ",
    ST = "DS|SS|PO",
    FZ = "FZ",
    PP_HEAVY = sprintf("\\+[:alpha:]+(?:%s)", paste(metar.ph[id_ph_subgrp %in% c("SOLID", "LIQUID")]$id_ph, collapse = "|")),
    PP_SOLID = sprintf("(?<!DR|BL)(?:%s)", paste(metar.ph[id_ph_subgrp == "SOLID"]$id_ph, collapse = "|")),
    PP_LIQUID = sprintf("SH\\b|%s", paste(metar.ph[id_ph_subgrp == "LIQUID"]$id_ph, collapse = "|")),
    DR = "DR|BL",
    FG = "(?<!MI|PR|BC)FG",
    FZ_PP = "FZ(?!FG)",
    FZ_FG = "FZFG",
    FG_any = "FG",
    PP = "RA|DZ",
    SH = "SH"
  )
  re.regex <- pw.regex[c("TS", "SH", "PP")]
  vc.regex <- pw.regex[c("TS", "SH", "ST", "FG")]

  dt.pw <- as.data.table(lapply(pw.regex, str_extract, string = pw))

  # Mixed precipitation (solid & liquid)
  dt.pw[, PP_MIXED := ifelse(!is.na(PP_SOLID) & !is.na(PP_LIQUID), paste0(PP_SOLID, PP_LIQUID), NA_character_)]

  # Recent/vicinity
  dt.re <- as.data.table(lapply(re.regex, str_extract, string = re))
  setnames(dt.re, paste("re", names(re.regex), sep = "_"))
  dt.vc <- as.data.table(lapply(vc.regex, str_extract, string = vc))
  setnames(dt.vc, paste("vc", names(vc.regex), sep = "_"))

  # PW group
  pw.grp.match <- str_match_all(pw[], "([\\+|\\-[:alpha:]]+)")
  dt.pw.grp <- data.table(id = rep(seq_along(pw.grp.match), sapply(pw.grp.match, nrow)), pw_grp = do.call(rbind, pw.grp.match)[,1])
  dt.pw.grp[, `:=`(num_pw_grp = paste("pw_grp", seq_along(.I), sep = "_")), .(id)]

  xt.pw.grp <- dcast(dt.pw.grp, id ~ num_pw_grp, value.var = "pw_grp")

  # Merge
  dt.pw <- cbind(id = seq_along(pw), pw, re, vc, dt.pw, dt.re, dt.vc)
  dt.pw <- merge(dt.pw, xt.pw.grp, by = "id", all.x = TRUE)
  setorder(dt.pw, id)
  setcolorder(dt.pw, c("pw", names(xt.pw.grp)))

  # Significant weather GR > TS > ST > FZ > PP_HEAVY > PP_SOLID > PP_LiQUID > FG > vc_TS > vc_FG
  dt.pw[, NOSIG := 1]
  sig.wx <- c("GR", "TS", "ST", "FZ", "PP_HEAVY", "PP_SOLID", "PP_LIQUID", "DR", "FG", "vc_TS", "vc_FG", "re_TS", "re_PP", "NOSIG")
  dt.pw[, sigwx := sig.wx[apply(!is.na(as.matrix(.SD)), 1, which.max)], .SDcols = sig.wx]

  dt.comb <- dt.pw

  ## ------------------------- PH / DC --------------------------------

  # Phenomena matrix (exclude VCxx)
  ph.match <- str_match_all(pw, sprintf("(%s)", paste(metar.ph$id_ph, collapse = ")|(")))
  ph.matrix <- lapply(ph.match, function(i) (!is.na(i))*1)
  ph.sums <- (do.call(rbind, lapply(ph.matrix, colSums)) > 0)*1
  dt.ph <- data.table(ph.sums[,-1, drop = FALSE])
  setnames(dt.ph, paste("PH", metar.ph$id_ph, sep = "_"))

  # Descriptor
  dc.match <- str_match_all(pw, sprintf("(%s)", paste(metar.dc$id_dc, collapse = ")|(")))
  dc.matrix <- lapply(dc.match, function(i) (!is.na(i))*1)
  dc.sums <- (do.call(rbind, lapply(dc.matrix, colSums)) > 0)*1
  dt.dc <- data.table(dc.sums[,-1, drop = FALSE])
  setnames(dt.dc, paste("DC", metar.dc$id_dc, sep = "_"))

  if(DC) dt.comb <- cbind(dt.comb, dt.dc)
  if(PH) dt.comb <- cbind(dt.comb, dt.ph)

  dt.comb
}


#' Parse METAR cloud groups
#'
#' @author M. Saenger
#' @param cld METAR cloud string
#' @export
#' @examples
#' metar_clouds(cld = c("FEW030 FEW032TCU SCT050 BKN075", "SCT050 SCT075 OVC149", "", "SCT003"))
#'
metar_clouds <- function(cld){
  metar.vars.cld <- c("cld", "cld_amt", "cld_hgt", "cld_type")
  cld.amt <- metar.class[id_para == "cld_amt"]

  dt.1 <- data.table(cld)
  dt.1[, `:=`(id = .I)]
  setkey(dt.1, id)

  m <- str_match_all(dt.1$cld, "(FEW|SCT|BKN|OVC|[\\/]{3})([0-9]{3}|[\\/]{3})([A-Z]{2,3}|\\/\\/\\/)?")
  dt.cld <- data.table(do.call(rbind, m))
  setnames(dt.cld, metar.vars.cld)

  dt.cld$id <- rep(seq_along(m), sapply(m, nrow))

  dt.cld[, cld_level := seq_len(.N), id][order(id, cld_level)]

  void <- lapply(metar.vars.cld, function(i){
    def <- metar.para[id_para == i]
    # Convert type
    suppressWarnings(dt.cld[, (i) := do.call(paste0("as.", def$type_para), list(dt.cld[[i]]))])
  })
  dt.cld[, cld_hgt := cld_hgt * 1e2]
  setkey(dt.cld, id)

  # Ceiling
  dt.ceil <- dt.cld[cld_amt %in% c("BKN", "OVC"), .(ceiling = min(cld_hgt)), id]

  setkey(dt.ceil, id)
  dt.agg <- dt.cld[, .(
    cld_levels = max(cld_level),
    cld_hgt_min = min(cld_hgt),
    cld_hgt_max = max(cld_hgt),
    cld_amt_max = cld.amt[max(match(cld_amt, cld.amt$id_class))]$id_class,
    cld_amt_min = cld.amt[min(match(cld_amt, cld.amt$id_class))]$id_class), id]

  dt.cast <- data.table::dcast(dt.cld, id ~ cld_level, value.var = metar.vars.cld)
  setkey(dt.cast, id)

  dt <- Reduce(function(...) merge(..., all = TRUE), list(dt.1, dt.agg, dt.ceil, dt.cast))
  dt[, `:=`(id = NULL, cld = NULL)][]
}

#' Parse METAR RVR groups
#'
#' @author M. Saenger
#' @param rvr METAR RVR string
#' @export
#' @examples
#' metar_rvr(rvr = c("R14/P2000N R16/P2000N R28/P2000N R34/1000VP2000U", "R14/P2000N",
#' "R08R/3000VP6000FT", "R22/0700D", "", "R18L/290050"))
#'
metar_rvr <- function(rvr){
  metar.vars.rvr.in <- c("rwy", "rvr_min_exc", "rvr_min", "rvr_tend_min", "rvr_max_exc", "rvr_max",  "rvr_tend_max", "rvr_unit")
  metar.vars.rvr <- c("rwy", "rvr_min_exc", "rvr_min", "rvr_max_exc", "rvr_max", "rvr_unit", "rvr_tend")
  # U: increasing, N: Neutral, D: Decreasing

  dt.rvr <- data.table(id = seq_along(rvr))

  m <- str_match_all(rvr, "(?:R([0-9\\/CRL]{2,3})\\/(P|M|\\/)?([0-9\\/]{4})(D|U|N)?V?(P|M)?([0-9]{4})?(D|U|N)?(FT)?){1,4}")
  dt <- data.table(do.call(rbind, m)[,-1])
  setnames(dt, metar.vars.rvr.in)
  dt$id <- rep(seq_along(m), sapply(m, nrow))
  dt[, `:=`(rvr_tend = pmax(rvr_tend_min, rvr_tend_max, na.rm = TRUE), rvr_group = seq_len(.N)), id]
  dt[, `:=`(rvr_tend_min = NULL, rvr_tend_max = NULL)]

  void <- lapply(metar.vars.rvr, function(i){
    def <- metar.para[id_para == i]
    # Convert type
    suppressWarnings(dt[, (i) := do.call(paste0("as.", def$type_para), list(dt[[i]]))])
  })

   # Convert to meters
  dt[, rvr_unit := fifelse(is.na(rvr_unit), "M", rvr_unit)]
  dt[,`:=`(rvr_min = fifelse(rvr_unit == "FT", rvr_min*0.3048, rvr_min))]
  dt[,`:=`(rvr_max = fifelse(rvr_unit == "FT", rvr_max*0.3048, rvr_max))]


  dt.cast <- data.table::dcast(dt, id ~ rvr_group, value.var = metar.vars.rvr)
  dt.out <- merge(dt.rvr, dt.cast, by = "id", all = TRUE)
  dt.out[, `:=`(id = NULL)][]
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

