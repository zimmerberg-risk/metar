
library(tidyverse)
library(rlang)


## Usage ---------------------------------------------------------------------------

#' Parse single report
#' "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG" %>% parse_metar()
#'
#' Parse multiple reports
#' metar <- c("LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG", "LFKJ 071130Z AUTO 24011KT CAVOK 27/17 Q1015 NOSIG")
#' sapply(metar, parse_metar) %>% bind_rows()
#'
#' Process single report
#' "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG" %>% parse_metar() %>% process_metar
#'
#' Process multiple reports
#' sapply(metar, parse_metar) %>% bind_rows()  %>% process_metar

## Definitions ---------------------------------------------------------------------------

cld.amt <- exprs(FEW, SCT, BKN, OVC) %>% as.character %>% paste(collapse = "|")
desc <- exprs(MI, BC, PR, DR, BL, SH, TS, FZ) %>% as.character %>% paste(collapse = "|")
ph <- exprs(DZ, RA, SN, SG, PL, GR, GS, UP, FG, BR, FU, VA, DU, SA, HZ, PO, SQ, FC, SS, DS, NSW, TS, SH)
wdir <- exprs(NE, SE, SW,  NW, N, E, S, W) %>% as.character %>% paste(collapse = "|")
ph <- ph %>% as.character %>% paste(collapse = "|") %>% paste0("|\\/\\/")

## Output Columns ---------------------------------------------------------------------------

cols <- list(
  ts = list(name = "Timestamp", type = "integer", required = T),
  type = list(name = "Type", type = "character", required = F),
  cor = list(name = "Correction", type = "character", required = F),
  icao = list(name = "ICAO", type = "character", required = T),
  icao = list(name = "ICAO", type = "character", required = T),
  dd =  list(name = "Day", type = "integer", required = T),
  hh =  list(name = "Hour", type = "integer", required = T),
  mm =  list(name = "Minute", type = "integer", required = T),
  auto = list(name = "AUTO", type = "character", required = F),
  dir = list(name = "Wind Dir.", type = "integer", required = T),
  ff = list(name = "Wind Speed", type = "integer", required = T),
  fx = list(name = "Wind Gusts", type = "integer", required = T),
  ff_unit  = list(name = "Wind Unit", type = "character", required = T),
  dir_from = list(name = "Var. Wind From", type = "integer", required = F),
  dir_to = list(name = "Var. Wind To", type = "integer", required = F),
  vis = list(name = "Visibility", type = "character", required = T),
  vis_unit  = list(name = "Vis. Unit", type = "character", required = T),
  ndv = list(name = "NVD", type = "character", required = F),
  min_vis = list(name = "Min. Vis.", type = "integer", required = F),
  min_vis_dir = list(name = "Min. Vis. Dir.", type = "character", required = F),
  rwy = list(name = "Runway", type = "character", required = F),
  rwr_min_exc = list(name = "RVR Min. Exceed.", type = "character", required = F),
  rvr_min = list(name = "RVR Min.", type = "integer", required = F),
  rwr_max_exc = list(name = "RVR Max Exceed.", type = "character", required = F),
  rvr_tend = list(name = "RVR Tendency", type = "character", required = F),
  rwr_max = list(name = "RVR Max.", type = "integer", required = F),
  rvr_unit = list(name = "RVR Unit", type = "character", required = F),
  int = list(name = "Intensity", type = "character", required = T),
  dc = list(name = "Descriptor", type = "character", required = T),
  ph = list(name = "Phenomena", type = "character", required = T),
  vvis = list(name = "Vertical Vis.", type = "character", required = F),
  wx = list(name = "CAVOK", type = "character", required = F),
  cld_amt = list(name = "Cloud Amount", type = "character", required = F),
  cld_hgt = list(name = "Cloud Base", type = "integer", required = F),
  cld_type = list(name = "Cloud Type", type = "character", required = F),
  tt_sign = list(name = "Temp. Sign", type = "character", required = T),
  tt = list(name = "Temperature", type = "integer", required = T),
  td_sign = list(name = "Dew Sign", type = "character", required = T),
  td = list(name = "Dew Point", type = "integer", required = T),
  qnh_unit  = list(name = "QNH Unit", type = "character", required = T),
  qnh = list(name = "QNH", type = "integer", required = T),
  recent = list(name = "Recent", type = "character", required = F),
  trend_ind = list(name = "Trend Ind.", type = "character", required = F),
  trend_str = list(name = "Trend", type = "character", required = F),
  rmk = list(name = "Remark", type = "character", required = F)
)

## Regular Expressions ---------------------------------------------------------------------------

rules <- list(
  ts = list(regex = "\\[ST\\]([0-9]{14})\\[T\\]", n = 1, names = "ts"),
  type = list(regex = "(METAR|SPECI)", n = 1, names = "type"),
  icao =  list(regex = "([A-Z0-9]{4})", n = 1, names = "icao"),
  time =  list(regex = "([0-9]{2})([0-9]{2})([0-9]{2})Z?", n = 1, names = c("dd", "hh", "mm")),
  cor = list(regex = "(COR)", n = 1, names = "cor"),
  auto =  list(regex = "(AUTO)", n = 1, names = "auto"),
  wind =  list(regex = "([0-9VRB\\/]{3})?([0-9\\/]{2})G?([0-9]{2})?(KT|MPH|MPS)", n = 1, names = c("dir", "ff", "fx", "ff_unit")),
  wind_var =  list(regex = "([0-9\\/]{3})V([0-9\\/]{3})", n = 1, names = c("dir_from", "dir_to")),
  vis =  list(regex = "M?([0-9]{4}(?=\\h?)|[\\/]{4}(?=\\h?)|[0-9]{1,2}(?=SM|KM)|[0-9]{1}\\/[0-9]{1}(?=SM|KM)|[0-9]{1}\\h[0-9]{1}\\/[0-9]{1}(?=SM|KM?))(SM|KM)?(NDV)?", n = 1, names = c("vis", "vis_unit", "ndv")),
  min_vis =  list(regex = sprintf("([0-9]{4})(%s)", wdir), n = 1, names = c("min_vis", "min_vis_dir")),
  rvr =  list(regex = "R([0-9\\/CRL]{2,3})\\/(P|M|\\/)?([0-9\\/]{4})(D|U|N)?V?(P|M)?([0-9]{4})?(FT)?", n = 4, names = c("rwy", "rwr_min_exc", "rvr_min", "rvr_tend", "rwr_max_exc", "rwr_max", "rvr_unit")),
  pw =  list(regex = sprintf("(\\+|\\-|VC)?(%s)?((?:%s){1,3})\\h", desc, ph), n = 3, names = c("int", "dc", "ph")),
  vvis = list(regex = sprintf("VV([0-9|\\/]{3})", desc, ph), n = 1, names = c("vvis")),
  wx = list(regex = "(CAVOK|NSC|NCD|WXNIL|CLR|SKC)", n = 1, names = "wx"),
  cld =  list(regex =  sprintf("(%s|[\\/]{3})([0-9]{3}|[\\/]{3})([A-Z]{2,3}|\\/\\/\\/)?", cld.amt), n = 5, names = c("cld_amt", "cld_hgt", "cld_type")),
  tttd = list(regex = "(M)?([0-9]{2})/(M)?([0-9]{2})?", n = 1, names = c("tt_sign", "tt", "td_sign", "td")),
  qnh = list(regex = "(Q|A)([0-9]{4})", n = 1, names = c("qnh_unit", "qnh")),
  recent = list(regex = sprintf("RE(%s{1,3})\\h", ph), n = 1, names = c("recent")),
  trend = list(regex = "(NOSIG|TEMPO|BECMG)\\h?(.*?)(?=RMK|$)", n = 2, names = c("trend_ind", "trend_str")),
  rmk = list(regex = "RMK\\h(.+)$", n = 1, names = "rmk")
)

## Functions ---------------------------------------------------------------------------

parse_metar <- function(str, verbose = F){
  #' @author M. Saenger
  #' @example  "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG" %>% parse_metar()
  #' @description parse a METAR report
  #' @details

  require(tidyverse)

  str.in <- str
  if(verbose) print(str)

  # Loop rules
  sec.list <- purrr::imap(rules, function(x , name){

    # Loop rule multiple times (pw, cld) if applicable
    el.list <- purrr::map(1:x$n, function(i){

      if(verbose) print(paste0(name, ": ", str))

      # Match regex pattern
      el.match <- stringr::str_match(str, paste0("^", x$regex))

      if(!is.na(el.match[1])){
        # Remove matched section from string
        str <<- stringr::str_remove(str, paste0("^", x$regex, "(\\h)?"))
        # Create named list from matches
        el.result <- el.match[,-1] %>% as.list %>% setNames(x$names)
        # Coerce type
        suppressWarnings(map2(el.result, cols[names(el.result)], ~ exec(paste("as", .y$type, sep = "."), .x)))
      }
    })
    el.list
  })

  if(verbose) str(sec.list)

  # Flatten list, e. g. cld_hgt nested list becomes cld_hgt, cld_hgt1, cld_hgt2, ...
  dt <- map(sec.list, bind_cols) %>% purrr:::flatten() %>% bind_cols()

  # Add required missing columns
  cols.required <- map(keep(cols, ~.x$required), ~exec(.x$type, 0))

  dt %>%
    bind_rows(cols.required) %>%
    mutate(metar = str.in)
}

process_metar <- function(dat, month = NULL, year = NULL, col.drop = NULL, verbose = F){
  #' @author M. Saenger
  #' @example  "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG" %>% parse_metar() %>% process_metar()
  #' @description processes a data.frame of parsed METAR reports
  #' @details if not specified month/year taken from Sys.Date()

  require(tidyverse)
  require(lubridate)

  if(verbose) str(dat)

  # if not specified month/year taken from Sys.Date()
  month <- if(is.null(month)) month(Sys.Date())
  year <- if(is.null(year)) year(Sys.Date())

  # Columns to drop after processing
  col.drop <- c("qnh_unit", "tt_sign", "td_sign", "ff_unit", "vis_unit", "vis_numeric", "dd", "hh", "mm") %>% c(col.drop)

  # Process
  dat %>%
    mutate_if(is.character, ~ str_replace(., "^[\\/]{2,5}$", NA_character_)) %>%
    mutate(
      time = as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00", year, month, dd, hh, mm), tz = "UTC"),
      ff = case_when(ff_unit == "MPS" ~ ff*1.9438, TRUE ~ as.numeric(ff)),
      fx = case_when(ff_unit == "MPS" ~ fx*1.9438, TRUE ~ as.numeric(fx)),
      vis_numeric = map_dbl(vis, ~ eval(parse(text = str_replace(.x, " ", "+")))),
      vis = case_when(vis_unit == "SM" ~ vis_numeric*1609.344, vis_unit == "KM" ~ vis_numeric*1e3, TRUE ~ vis_numeric), #1609.344 1852.216
      tt = case_when(tt_sign == "M" ~ -tt, TRUE ~ tt),
      td = case_when(td_sign == "M" ~ -td, TRUE ~ td),
      qnh = case_when(qnh_unit == "A" ~ as.integer(substr(qnh, 0, 4))/0.02953/100, TRUE ~ as.numeric(qnh)),
    ) %>%
    dplyr::select(icao, time, everything(), -one_of(col.drop))
}





