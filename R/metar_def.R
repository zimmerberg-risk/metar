
#' @name cld.amt
#' @title Lorem Ipsum
#' @docType data
#' @keywords dataset
#' @export
#'
cld.amt = c("NSC", "FEW", "SCT", "BKN", "OVC")

#' @name pw.dc
#' @title Lorem Ipsum
#' @docType data
#' @keywords dataset
#' @export
#'
pw.dc = c("MI", "BC", "PR", "DR", "BL", "SH", "TS", "FZ")

#' @name pw.ph
#' @title Lorem Ipsum
#' @docType data
#' @keywords dataset
#' @export
#'
pw.ph = c("DZ", "RA", "SN", "SG", "PL", "GR", "GS", "UP", "FG", "BR", "FU", "VA", "DU", "SA", "HZ", "PO", "SQ", "FC", "SS", "DS", "NSW", "TS", "SH")

#' @name metar.ph
#' @title Lorem Ipsum
#' @docType data
#' @keywords dataset
#' @export
#'
metar.ph <- list(
  "TS" = list(name = "Thunderstorm", type = "Storm", subtype = "", group = "Storm/Hail"),
  "SQ" = list(name = "Squalls", type = "Storm", subtype = "", group = "Storm/Hail"),
  "FC" = list(name = "Funnel Cloud, Tornado", type = "Storm", subtype = "", group = "Storm/Hail"),
  "GR" = list(name = "Hail", type = "Precipitation", subtype = "Solid", group = "Storm/Hail"),
  "SN" = list(name = "Snow", type = "Precipitation", subtype = "Solid", group = "Snow/Ice"),
  "SG" = list(name = "Snow Grains", type = "Precipitation", subtype = "Solid", group = "Snow/Ice"),
  "PL" = list(name = "Ice Pellets", type = "Precipitation", subtype = "Solid", group = "Snow/Ice"),
  "GS" = list(name = "Snow Pellets", type = "Precipitation", subtype = "Solid", group = "Snow/Ice"),
  "RA" = list(name = "Rain", type = "Precipitation", subtype = "Liquid", group = "Rain/Shower"),
  "DZ" = list(name = "Drizzle", type = "Precipitation", subtype = "Liquid", group = "Rain/Shower"),
  "SH" = list(name = "Shower", type = "Other", subtype = "Unknown", group = "Rain/Shower"),
  "UP" = list(name = "Unknown Precipitation", type = "Precipitation", subtype = "Unknown", group = "Rain/Shower"),
  "PO" = list(name = "Dust/Sand Whirls", type = "Storm", subtype = "", group = "Sand-/Duststorm"),
  "SS" = list(name = "Sandstorm", type = "Storm", subtype = "", group = "Sand-/Duststorm"),
  "DS" = list(name = "Duststorm", type = "Storm", subtype = "", group = "Sand-/Duststorm"),
  "FG" = list(name = "Fog", type = "Obscuration", subtype = "", group = "Fog"),
  "SA" = list(name = "Sand", type = "Obscuration", subtype = "", group = "Sand/Ash/Dust"),
  "BR" = list(name = "Mist", type = "Obscuration", subtype = "", group = "Mist/Haze"),
  "FU" = list(name = "Smoke", type = "Obscuration", subtype = "", group = "Sand/Ash/Dust"),
  "VA" = list(name = "Volcanic Ash", type = "Obscuration", subtype = "", group = "Sand/Ash/Dust"),
  "DU" = list(name = "Dust", type = "Obscuration", subtype = "", group = "Sand/Ash/Dust"),
  "HZ" = list(name = "Haze", type = "Obscuration", subtype = "", group = "Mist/Haze"),
  "NSW" = list(name = "No Significant Weather", type = "Other", subtype = "", group = "NSW")
)


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
  vis_unit  = list(name = "Vis. Unit", type = "character", drop = T, default = "M"),
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
  ws = list(name = "Wind Shear", type = "character", drop = F),
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

  # re = list(name = "Recent", type = "character", drop = F),
  int = list(name = "Intensity", type = "character", drop = F),
  vc = list(name = "Vicinity", type = "character", drop = F),
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
  cld_str = list(name = "Cloud Group", type = "character", drop = F),
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

#' @name metar.test
#' @title Lorem Ipsum
#' @docType data
#' @keywords dataset
#' @export
#'
metar.test <- c(
  # 5 cloud groups
  "KFFO 271946Z 25006KT 10SM FEW009 SCT015 BKN027 BKN035 OVC044 25/22 A2986 RMK AO2A CIG 015V027 CIG 023 RWY05L SLP109",
  # Remark
  "RJTT 241900Z 02016G37KT 6000 -SHRA FEW015 BKN025 BKN040 23/22 Q1008 NOSIG RMK 1CU015 5CU025 7SC040 A",
  # Empty cloud group
  "EGLL 081420Z AUTO 29006KT 260V340 9999 0400SE VCTS SCT038/// //////CB 25/15 Q1023 TEMPO 4000 +TSRA",
  # COR Correction
  "KBBG 271945Z COR 33008KT 10SM SCT029 BKN250 27/19 A3003",
  # VIS: statute miles
  "KAUS 092135Z 26018G25KT 8SM -TSRA BR SCT045CB BKN060 OVC080 30/21 A2992",
  # VIS: factional statute miles
  "KCLT 232327Z VRB03G17KT 1/2SM R18C/1800V4000FT +TSRA FG SCT009 BKN029 OVC045CB 22/21 A3007 RMK",
  "KCLT 232327Z VRB03G17KT 1 1/2SM R18C/1800V4000FT +TSRA FG SCT009 BKN029 OVC045CB 22/21 A3007 RMK",
  # Missing cloud section fragments
  "LFKB 241300Z AUTO 10007KT 050V140 9999 BKN030/// BKN042/// ///TCU 28/21 Q1017 TEMPO FEW035CB BKN090",
  # Trend: TEMPO and NOSIG
  "VTUW 241300Z VRB02KT 9999 -RA VCTS FEW018CB SCT020 BKN100 25/25 Q1006 TEMPO FM1420 TL1450 5000 TSRA",
  "LSZH 241120Z 33004KT 280V020 9999 FEW018 FEW019 BKN/// 21/16 Q1022 NOSIG",
  # Missing vertical visibility value
  "LIMH 240355Z 05004KT 0300 FG VV/// 01/M02 Q1035 RMK MON INVIS VAL INVIS VIS MIN 0300",
  # Runway visibility RVR
  "EKKA 240406Z AUTO 17001KT 0200 R09R/0900N R27L/0750N FG VV002 14/13 Q1026",
  # Various missing sections, recent RE
  "LGKZ 241750Z AUTO 00000KT //// // ////// 25/07 Q1020 RE//",
  # CAVOK
  "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG",
  # All combined (fictional)
  "XXXX 271750Z AUTO VRB03G17KT 050V140 0200NDV R09R/0900N R27L/0750N +TSRA VCFG SCT009 BKN029 OVC045CB 25/18 Q1017 TEMPO FM1420 TL1450 5000 TSRA",
  # Complex
  "LSZH 250350Z 32003KT 290V350 2000 R14/P2000N R16/P2000N R28/P2000N R34/1000VP2000U BCFG FEW001 SCT003 BKN140 16/16 Q1016 TEMPO BKN003",
  #BECMG
  "ZGGG 071800Z 06002MPS 340V120 7000 FEW023 SCT040 26/24 Q1009 BECMG AT1840 -TSRA SCT011 FEW026CB BKN030",
  #Cloud
  "LSZH 250350Z VRB01KT 9000 -SHRA FEW007 BKN009 BKN022 02/00 Q1007",
  # Recent
  "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 RERA",
  # Mixed Precipitation
  "LSZH 271750Z 14002KT CAVOK +SNRA 25/18 Q1017 ",
  # Wind shear
  "VHHH 110530Z 23009KT 9999 FEW018 33/24 Q1008 WS R25R NOSIG"
)
