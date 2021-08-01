## --------------------------------------------------- Process  ---------------------------------------------------

#' Parse present wather groups
#'
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
#' @param x present weather string
#' @export
#' @examples
#' x <- c("TS", "+TSRA VCTS", "SNGRRA VCFG")
#' parse_metar_pw(x)
#' m <- parse_metar(metar.test$code)
#' parse_metar_pw(x = m$pw)
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

  # Phenomena (unique)
  ph <- str_remove_all(x, sprintf("(VC[A-Z]{2}|\\s{2}|\\+|\\-|%s)+", paste(metar.dc$id_dc, collapse = "|")))

  data.table(pw = x, ph, pw, grp, sigwx = SIGWX)

}
#
# m <- parse_metar_grp(dt.noaa$metar)
# xxx <- parse_metar_pw(x = m$pw)
# yyy <- parse_metar_rvr(x = rep(m$rvr, 1))
# parse_metar_rvr(x = NA)

#' Parse cloud groups
#'
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
#' @param x cloud string
#' @export
#' @examples
#' x <- c("SCT009 BKN029 OVC045CB", "SCT009CB OVC045", "NSC")
#' parse_metar_cld(x)
#' m <- parse_metar(metar.test$code)
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
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
#' @param x cloud string
#' @export
#' @examples
#' x <- c("R14/P2000N R16/P2000N R28/P2000N R34/1000VP2000U", "R08R/3000VP6000FT", "R22/0700D", "R01/0900VP2000D")
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

  rvr_max <- rbind(apply(grp, 2, str_extract, pattern = "(?<=V[PM])([0-9]{4})"))
  class(rvr_max) <- "numeric"
  colnames(rvr_max) <- paste("rvr_max", 1:n, sep = "_")

  rvr_unit <- str_match(x, "FT")
  rvr_unit[is.na(rvr_unit)] <- "M"

  rvr_trend <- rbind(apply(grp, 2, str_extract, pattern = "[DUN]"))

  data.table(rwy, rvr_min, rvr_max, rvr_unit)

}

#' Parse runway conditions groups
#'
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
#' @param x cloud string
#' @export
#' @examples
#' x <- c("R02/010070 R06/010070", "R88/090060")
#' parse_metar_rwy_cond(x)
#'
parse_metar_rwy_cond <- function(x){
  warning("Not implemented, yet")
  return(NULL)
}

#' Parse wind shear
#'
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
#' @param x cloud string
#' @export
#' @examples
#' x <- c("WS R25R")
#' parse_metar_ws(x)
#'
parse_metar_ws <- function(x){
  warning("Not implemented, yet")
  return(NULL)
}

## --------------------------------------------------- Validate  ---------------------------------------------------

#' Validate METAR
#'
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
#' @description Validate METAR reports
#' @param dat Output from `parse_metar`
#' @param formulas collection of validation formulas as quosures e.g. `rlang::quos(f1 = tt > tt, f2 = ff > fx)`
#' @param verbose verbose output
#' @export
#'
validate_metar <- function(dat, formulas, verbose = TRUE){

  # Default checks
  if(missing(formulas)){
    formulas <- rlang::quos(
      ff_max = ff > 200,
      fx_max = fx > 300,
      ff_fx = ff > fx,
      ff_fx_gust = fx > ff * 5,
      tt_td = tt < td,
      qnh_min = qnh < 850,
      qnh_max = qnh > 1080
    )
  }
  n <- names(formulas)

  evaluate <- function(table, a) {
    rlang::eval_tidy(a, table)
  }
  arr <- sapply(formulas, evaluate, table = dat)
  failed <- which(arr == T, arr.ind = TRUE)

  dt.valid <- dat[!failed[,1]]
  dt.invalid <- dat[failed[,1]]

  if(verbose){
    cat("Failed validation:\n")
    print(dt.invalid)
  }

  structure(dt.valid, invalid = dt.invalid)

}


