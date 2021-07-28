library(data.table)
library(metar)
library(vroom)
library(stringr)
library(metar)
#library(stringdist)

# https://tgftp.nws.noaa.gov/data/observations/metar/cycles/10Z.TXT

dir.base <- "C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Data/metar"
dir.data <- file.path(dir.base, "data")
p <- file.path(dir.data, "10Z.TXT")
dt.noaa <- metar_read_noaa(hour = 15, latest.only = T)
grep("BECMG", dt.noaa$metar, value = T)

#' Parse METAR Groups
#'
#' @author M. Saenger
#' @description
#' Parse a METAR report into groups
#' Performance: ~ 60s for 1 million records on average machine
#' @param x METAR reports (character vector)
#' @param t time (POSIXct vector of same length as `x`)
#' @param yyyy year
#' @param mm month
#' @export
#' @examples
#' parse_metar_grp(metar.test$code)
#' parse_metar_grp(x = "EGVP 281509Z 27011KT 5000 1000SW +SHRA BKN022CB 15/13 Q1007 BECMG 9999 NSW SCT030 RMK AMB BECMG BLU")
#'
parse_metar_grp <- function(x, t = NULL, yyyy = year(Sys.Date()), mm = month(Sys.Date())){
  regex.grp <- "^([0-9A-Z]{4})\\s([0-9]{6})Z\\s(COR(?:\\s))?(AUTO(?:\\s))?(.*?)(RE.*?)?((?:TEMPO\\s.*|NOSIG)?)?(RMK\\s.*?)?(BECMG\\s.*?)?$"

  # Apply regex
  grp <- as.data.table(rbind(str_match(x, regex.grp)[,-1]))
  setnames(grp, c("id_icao", "time_valid", "cor", "auto", "wx", "recent", "trend", "rmk", "bcmg"))

  # Process
  grp[, `:=`(
    auto = !is.na(auto),
    cor = !is.na(cor),
    time_valid = if(!is.null(t)) t else as.POSIXct(paste0(yyyy, sprintf("%02d", mm), time_valid), format = "%Y%m%d%H%M", tz = "GMT")
  )]
  setcolorder(grp, c("id_icao", "time_valid"))

  #WX
  wx <- parse_metar_wx(x = grp$wx)

  cbind(grp, wx)
}
x <- metar.test[1]$code

#' Parse METAR Message
#'
#' @author M. Saenger
#' @description
#' Parse a METAR report into groups
#' Performance: ~ 60s for 1 million records on average machine
#' @param x METAR reports (character vector)
#' @param t time (POSIXct vector of same length as `x`)
#' @param yyyy year
#' @param mm month
#' @export
#' @examples
#' parse_metar_wx(parse_metar_grp(metar.test)$wx)
#'
parse_metar_wx <- function(x){

  regex.dc.ph <- paste(c(metar.dc$id_dc, metar.ph$id_ph), collapse = "|")

  regex.list <- list(
    wind = list(
      regex = "(?:([0-9VRB\\/]{3})([0-9]{2})G?([0-9]{2})?(KT|MPH|MPS))",
      id_para = c("dir", "ff", "fx", "ff_unit")
    ),
    wind_var = list(
      regex = "(?:([0-9\\/]{3})V([0-9\\/]{3}))",
      id_para = c("dir_from", "dir_to"),
      ex = c("12444P99")
    ),
    cavok = list(
      regex = "(CAVOK)",
      id_para = "cavok"
    ),
    vis = list(
      regex = "(?:([0-9]{4}|[0-9]{1,2}(?=KM)|[0-9]{1,2}\\s[0-9]{1}\\/[0-9]{1}(?=SM)|[0-9]{1}\\/[0-9]{1,2}(?=SM)|(?!<\\/)[0-9]{1,2}(?=SM))(SM|KM)?(NDV)?)", #////SM
      id_para = c("vis", "vis_unit", "ndv")
    ),
    vis_min = list(
      regex = "(?:([0-9]{4})(N|NE|E|SE|S|SW|W|NW))",
      id_para = c("vis_min", "vis_min_dir")
    ),
    rvr = list(
      regex = "((?:R[0-9]{2}[CRL]?\\/(?:[PM]?[0-9]{4}[DUN])(?:V[PM]?[0-9]{4}[DUN])?(?:FT)?\\s?){1,4})",
      id_para = "rvr"
    ),
    rwy_cond = list(
      regex = "((?:R[0-9]{2}[CRL]?\\/[0-9\\/]{1}[1259\\/]{1}[0-9\\/]{4}\\s?){1,4})", # R88/CLRD//
      id_para = "rwy_cond"
    ),
    pw = list(
      regex = sprintf("((?:(?:\\+|\\-)?\\b(?:VC)?(?:%s){1,4}\\b\\s?){1,4})", regex.dc.ph), #"(\\+|\\-)?(TS)?(RA)", #//
      id_para = "pw"
    ),
    vvis = list(
      regex = "(?:((?:VV)[0-9|\\/]{3}))",
      id_para = "vvis"
    ),
    cld =  list(
      regex = "((?:CLR|NCD|NSC|SKC|(?:FEW|SCT|BKN|OVC|[\\/]{3})[0-9\\/]{3}(?:[A-Z]{2,3}|[\\/]{3})?(?:\\s)?){1,4})",
      id_para = "cld"
    ),
    tt_td = list(
      regex = "(?:(M)?([0-9]{2})/(M)?([0-9\\/]{2}))", # M17/
      id_para = c("tt_sign", "tt", "td_sign", "td")
    ),
    qnh =  list(
      regex = "(?:(Q|A)([0-9]{4}))",
      id_para = c("qnh_unit", "qnh")
    ),
    ws = list(
      regex = "(?:((?:WS\\s)R[A-Z0-9]{2,3}|ALL RWY))", # WS R26
      id_para = "ws"
    )
  )
  regex <- sprintf("%s.*$", paste0(sprintf("%s?(?:\\s)?", lapply(regex.list, "[[", "regex")), collapse = ""))
  #print(regex)

  # Apply Regex
  m <- as.data.table(str_match(string = x, regex)[,-1])
  setnames(m, do.call(c, sapply(regex.list, "[[", "id_para")) )

  # Parse visibility fractions (statute miles)
  m[, vis_ones := str_match(vis, "([0-9]+(?!\\/))")[,-1]]
  m[, vis_frac := str_match(vis, "([0-9]{1,2}\\/[0-9]{1})")[,-1]]
  m[, c("denom", "num") := tstrsplit(vis_frac, "\\/")]

  # Process
  m[, `:=`(
    dir = fifelse(dir == "VRB", 360, as.numeric(dir)),
    ff = as.numeric(ff),
    fx = as.numeric(fx),
    dir_from = as.numeric(dir_from),
    dir_to = as.numeric(dir_to),
    vis = pmax(as.numeric(vis_ones), 0, na.rm = TRUE) + pmax(as.numeric(denom)/as.numeric(num), 0, na.rm = TRUE),
    cavok = !is.na(cavok),
    tt = as.numeric(tt)*fifelse(!is.na(tt_sign), -1, 1),
    td = as.numeric(td)*fifelse(!is.na(td_sign), -1, 1),
    qnh =  as.numeric(qnh)
  )]

  # Units
  ff.unit <- c(MPS = 1.9438400, KT = 1, MPH = 0.8689760)
  m[!is.na(ff), ff := round(ff * unname(ff.unit[match(ff_unit, names(ff.unit))]), 0)]
  m[!is.na(fx), fx := round(fx * unname(ff.unit[match(ff_unit, names(ff.unit))]), 0)]
  vis.unit <- c(KM = 1000, SM = 1609.3440000, M = 1.0000000)
  m[!is.na(vis_unit), vis := round(vis * unname(vis.unit[match(vis_unit, names(vis.unit))]), 0)]
  m[qnh_unit == "A", qnh := round(qnh * 0.3386387, 1)]

  # m[!is.na(tt_sign)]
  # m[grepl("\\/", vis)]
  #
  # grep("17\\/", x, value = T)
  # unique(m$ff)
  # unique(m$vis)

  # Remove columns
  set(m, NULL, c("ff_unit", "vis_ones", "vis_frac", "vis_unit", "denom", "num", "tt_sign", "td_sign", "qnh_unit"), NULL)
  m[]
}





system.time({  dt.grp <- parse_metar_grp(x = dt.noaa$metar, t = dt.noaa$time_valid) })
dt.grp[id_icao %in% c("LSZH", "YSSY", "KDEN", "LOWI", "EGGW", "BIAR", "DRRN")]

system.time({  dt.grp.1 <- parse_metar(x = dt.noaa$metar, dt.noaa$time_valid) })
system.time({  dt.grp.2 <- parse_metar_grp(x = rep(dt.noaa$metar, 200)) })





test <- "XXXX 271750Z AUTO VRB03G17KT 050V140 0200NDV 4000SE R09R/0900N R27L/0750N +TSRA SNGSRA VCFG SCT009 BKN029 OVC045CB 25/18 Q1017" # TEMPO FM1420 TL1450 5000 TSRA RMK AO2A CIG 015V027 BECMG AT1840 -TSRA SCT011 FEW026CB BKN030
dt.grp[!is.na(recent)]


# CAVOK CLR
"(CAVOK|NSC|NCD|WXNIL|CLR|SKC|NSW)"


lapply(regex.list, function(i) str_match(string = test, sprintf("(%s)", i$regex)))

test <- c("VRB03G17KT 050V140 0200NDV 4000SE R09R/0900N R27L/0750N R01/019591 R02C/019591 +TSRA SNGSRA VCFG SCT009 BKN029 OVC045CB 25/18 Q1017 WS R08", "29006KT CAVOK 10SM 28/18 A3008")
regex <- sprintf("%s.*$", paste0(sprintf("%s?(?:\\s)?", lapply(regex.list, "[[", "regex")), collapse = ""))
print(regex)
str_match(string = test, regex)[,-1]


dt.metar <- cbind(dt.grp[, .(id_icao, time_valid, msg)], dt.metar)

parse_metar_cld <- function(x){
  # x <- c("SCT009 BKN029 OVC045CB", "SCT009CB OVC045")
  m <- str_match_all(str_trim(x), "((?:CLR|NCD|NSC|SKC)|(?:FEW|SCT|BKN|OVC|[\\/]{3}))([0-9\\/]{3})?([A-Z]{2,3}|[\\/]{3})?")
  m
  id <- seq_along(m)
  id <- rep(id, lapply(m, nrow))
  cld_level <- do.call(c, lapply(m, function(i) 1:nrow(i)))

  dt.cld <- data.table(do.call(rbind, m))
  setnames(dt.cld,  c("cld_grp", "cld_amt", "cld_hgt", "cld_type"))
  dt.cld[, `:=`(id = id, cld_level = cld_level)]
  data.table::dcast(dt.cld, id ~ cld_level, value.var = c("cld_grp", "cld_amt", "cld_hgt", "cld_type"))
}
dt.cld <- parse_metar_cld(x = dt.metar$cld[])

parse_metar_pw <- function(){}
parse_metar_rvr <- function(){}
parse_metar_rwy_cond <- function(){}
parse_metar_ws <- function(){}


dt.metar[id_icao %in% c("LSZH", "YSSY", "KDEN", "LOWI"), .SD[which.max(time_valid)], id_icao]
dt.metar[!is.na(vvis)]



stringr::str_match("111 AAAA AAAA 111", "((?:A{4}(?:\\s)?){1,4}(?:\\s)?)")
stringr::str_match(rep("AAAA AAAA XXXX XXXX XXXX 111", 2), "((?:A{4}(?:\\s)){1,4})?((?:X{4}(?:\\s)){1,4})?((?:Y{4}(?:\\s)){1,4})?")
stringr::str_match("SCT009 SCT/// BKN029 OVC045CB", "((?:(?:FEW|SCT|BKN|OVC|[0-9\\/]{3})[0-9\\/]{3}[A-Z]{0,2}(?:\\s)?){1,4}(?:\\s)?)")
str_match(c("WS R08", "WS ALL RWY"), "((?<=WS\\s)(?:R[A-Z0-9]{2,3}|ALL RWY))")

str_match("EGVP 281509Z 27011KT 5000 1000SW +SHRA BKN022CB 15/13 Q1007 BECMG 9999 NSW SCT030 RMK AMB",
          "^([0-9A-Z]{4})\\s([0-9]{6})Z\\s(.+?)\\s((?<=BECMG\\s).+?)((?<=RMK\\s).+?)$")

str_match_all("Aaa Bbbb Eccc e", "[ABCE].+?(?=\\s)")
str_match("Aaa Bbbb Dddd", "(A.+(?=\\sB))\\s(B.+(?=\\sC|D))?\\s?(C.+(?=\\sD))?")
str_match("Aaa Bbbb Dddd", "(A.+)*(B.+)*(C.+)*(D.+)*")

"^([0-9A-Z]{4})\\s([0-9]{6})Z\\s(COR(?:\\s))?(AUTO(?:\\s))?(.*?)(RE.*?)?((?:TEMPO\\s.*|NOSIG)?)?(RMK\\s.*?)?(BECMG\\s.*?)?$"
