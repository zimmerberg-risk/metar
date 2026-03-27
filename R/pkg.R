#' @import vroom
#' @import data.table
#' @import stringr
#' @import curl
#' @importFrom graphics abline axis.POSIXct
#' @importFrom stats median setNames time
#' @importFrom rlang quo eval_tidy
#' @importFrom lubridate floor_date ceiling_date
#'
NULL

utils::globalVariables(c(
  ".", ":=", ".data",
  "active", "ap_name", "ctry_name", "denom", "dir_from", "dir_to", "dp",
  "ff", "fx", "icao", "id_para", "id_ph_subgrp", "lat", "lon", "metar",
  "metar.class", "metar.dc", "metar.para", "metar.ph", "metar.stn", "num",
  "pw", "qnh", "qnh_tendency_3h", "qnh_unit", "rh", "sigwx", "td", "td_sign",
  "time_end", "tt", "tt_sign", "value", "vis", "vis_frac", "vis_km",
  "vis_ones", "vis_unit", "vvis", "vvis_hundreds_ft", "id_class", "col",
  "label", "short_class", "sigwx_fill", "temp_high", "temp_low", "wind_fill",
  "wind_sector", "x", "y", "ymax", "ymin"
))
