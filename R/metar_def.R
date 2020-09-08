
#' @name pw.desc
#' @title Lorem Ipsum
#' @docType data
#' @keywords dataset
#' @export
#'
pw.desc = c("MI", "BC", "PR", "DR", "BL", "SH", "TS", "FZ")

#' @name pw.ph
#' @title Lorem Ipsum
#' @docType data
#' @keywords dataset
#' @export
#'
pw.ph = c("DZ", "RA", "SN", "SG", "PL", "GR", "GS", "UP", "FG", "BR", "FU", "VA", "DU", "SA", "HZ", "PO", "SQ", "FC", "SS", "DS", "NSW", "TS", "SH")

#' @name metar.vars
#' @title Lorem Ipsum
#' @docType data
#' @keywords dataset
#' @export
#'
metar.vars <- list(
  metar = list(name = "METAR", type = "character", drop = F),
  cor = list(name = "Correction", type = "character", drop = F),
  icao = list(name = "ICAO", type = "character", drop = F),
  time = list(name = "Time", type = "POSIXct", drop = F),
  # dd =  list(name = "Day", type = "integer", drop = T),
  # hh =  list(name = "Hour", type = "integer", drop = T),
  # mm =  list(name = "Minute", type = "integer", drop = T),
  auto = list(name = "AUTO", type = "character", drop = F),
  dir = list(name = "Wind Dir.", type = "integer", drop = F),
  ff = list(name = "Wind Speed", type = "integer", drop = F),
  fx = list(name = "Wind Gusts", type = "integer", drop = F),
  ff_unit  = list(name = "Wind Unit", type = "character", drop = T, default = "KT"),
  dir_from = list(name = "Var. Wind From", type = "integer", drop = F),
  dir_to = list(name = "Var. Wind To", type = "integer", drop = F),
  vis = list(name = "Visibility", type = "character", drop = F),
  vis_unit  = list(name = "Vis. Unit", type = "character", drop = T, default = "KM"),
  ndv = list(name = "NVD", type = "character", drop = F),
  min_vis = list(name = "Min. Vis.", type = "integer", drop = F),
  min_vis_dir = list(name = "Min. Vis. Dir.", type = "character", drop = F),

  cld = list(name = "Cloud Groups", type = "character", drop = F),
  rvr = list(name = "RVR Groups", type = "character", drop = F),
  pw = list(name = "Present Weather Groups", type = "character", drop = F),

  vvis = list(name = "Vertical Vis.", type = "character", drop = F),
  wx = list(name = "CAVOK", type = "character", drop = F),
  tt_sign = list(name = "Temp. Sign", type = "character", drop = T, default = "P"),
  tt = list(name = "Temperature", type = "integer", drop = F),
  td_sign = list(name = "Dew Sign", type = "character", drop = T, default = "P"),
  td = list(name = "Dew Point", type = "integer", drop = F),
  qnh_unit  = list(name = "QNH Unit", type = "character", drop = T, default = "Q"),
  qnh = list(name = "QNH", type = "integer", drop = F),
  recent = list(name = "Recent", type = "character", drop = F),
  trend_ind = list(name = "Trend Ind.", type = "character", drop = F),
  trend_str = list(name = "Trend", type = "character", drop = F),
  rmk = list(name = "Remark", type = "character", drop = F)
)

#' @name metar.vars.pw
#' @title Lorem Ipsum
#' @docType data
#' @keywords dataset
#' @export
#'
metar.vars.pw <- list(
  int = list(name = "Intensity", type = "character", drop = F),
  dc = list(name = "Descriptor", type = "character", drop = F),
  ph = list(name = "Phenomena", type = "character", drop = F)
)

#' @name metar.vars.cld
#' @title Lorem Ipsum
#' @docType data
#' @keywords dataset
#' @export
#'
metar.vars.cld <- list(
  cld_amt = list(name = "Cloud Amount", type = "character", drop = F),
  cld_hgt = list(name = "Cloud Base", type = "integer", drop = F),
  cld_type = list(name = "Cloud Type", type = "character", drop = F)
)
#' @name metar.vars.rvr
#' @title Lorem Ipsum
#' @docType data
#' @keywords dataset
#' @export
#'
metar.vars.rvr <- list(
  rwy = list(name = "Runway", type = "character", drop = F),
  rwr_min_exc = list(name = "RVR Min. Exceed.", type = "character", drop = F),
  rvr_min = list(name = "RVR Min.", type = "integer", drop = F),
  rwr_max_exc = list(name = "RVR Max Exceed.", type = "character", drop = F),
  rvr_tend = list(name = "RVR Tendency", type = "character", drop = F),
  rwr_max = list(name = "RVR Max.", type = "integer", drop = F),
  rvr_unit = list(name = "RVR Unit", type = "character", drop = F)
)
