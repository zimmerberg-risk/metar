#' Parse METAR Message
#'
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
#' @description
#' Parse a METAR current weather section
#' @param x METAR reports (character vector)
#' @export
#' @examples
#' x <- parse_metar(metar.test$code)$wx
#' parse_metar_wx(x)
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
    ),
    rwy_cond = list(
      regex = "((?:R[0-9]{2}[CRL]?\\/[0-9\\/]{1}[1259\\/]{1}[0-9\\/]{4}\\s?){1,4})", # R88/CLRD// R07C/CLRD95 R07C/SNOCLO
      id_para = "rwy_cond"
    ),
    re = list(
      regex = "(?:RE)(.+)",
      id_para = "re"
    )
  )
  #x <- c("VRB03G17KT 050V140 0200NDV 4000SE R09R/0900N R27L/0750N  +TSRA SNGSRA VCFG SCT009 BKN029 OVC045CB 25/18 Q1017 WS R08 R01/019591 R02C/019591",   "01509Z 29006KT CAVOK 28/18 A3008")
  #x <- "18003MPS 150V220 7000 FEW006 SCT020CB 24/22 Q1009 R20/210370"

  #regex <- sprintf("%s.*$", paste0(sprintf("%s?(?:\\s)?", lapply(regex.list, "[[", "regex")), collapse = ""))
  #print(regex)

  # # Apply Regex
  # m <- as.data.table(str_match(string = x, regex)[,-1])
  # setnames(m, do.call(c, sapply(regex.list, "[[", "id_para")) )

  rwy_cond <- str_split_fixed(x, "\\s(?=(R[0-9]{2}[CLR]?\\/)(([0-9\\/]{1}[1259\\/]{1}[0-9\\/]{2}[0-9\\/]{1}[0-9\\/]{1})|SNOCLO|CLRD[0-9\\/]{2})\\b)", 2)
  # R88/CLRD// R07C/CLRD95 R07C/SNOCLO  R23L/CLRD70

  ws <- str_split_fixed(rwy_cond[,1], "\\sWS\\s", 2)
  re <- str_split_fixed(ws[,1], "\\s(?<=\\s)RE", 2)
  qnh <- str_split_fixed(re[,1], "\\s(?=[AQ][0-9\\/]{4})", 2)
  tt <- str_split_fixed(qnh[,1], "\\s(?=M?[0-9]{2}\\/(M?[0-9]{2})?)", 2)
  vvis <- str_split_fixed(tt[,1], "\\sVV", 2)
  cld <- str_split_fixed(vvis[,1], "\\s(?=NCD|SKC|CLR|NSC|FEW|SCT|BKN|OVC)", 2)
  pw <- str_split_fixed(cld[,1], sprintf("\\s(?=\\+?\\-?(VC)?(%s))", regex.dc.ph), 2)
  rvr <- str_split_fixed(pw[,1],"(?=\\sR[0-9]{2}[CLR]?\\/)\\s", 2)
  vis_min <- str_split_fixed(rvr[,1],"(?=\\s[0-9]{4}(N|NE|E|SE|S|SW|W|NW)\\b)\\s", 2)
  ndv <- str_split_fixed(vis_min[,1],"(?=NDV\\b)", 2)
  vis <- str_split_fixed(ndv[,1],"\\s(?=([0-9]{4}\\b)|([0-9]{1,2}(?=KM))|([0-9]{1,2}\\s[0-9]{1}\\/[0-9]{1,2}(?=SM))|([0-9]{1}\\/[0-9]{1,2}(?=SM))|((?!<\\/)[0-9]{1,2}(?=SM)))", 2)

  cavok <-  str_split_fixed(vis[,1], "\\s(?=CAVOK)", 2)
  wind_var <- str_split_fixed(cavok[,1],"(?=[0-9]{3}V[0-9])", 2)
  wind <- str_split_fixed(wind_var[,1],"(?<=([0-9VRB\\/]{3}[0-9]{2}(G[0-9]{2})?(KT|MPH|MPS)?)\\b)", 2)

  ff_unit = str_split_fixed(wind[,1], "(?=KT|MPS|MPH)", n = 2)
  fx = str_split_fixed(ff_unit[,1], "G", n = 2)
  ff = str_sub(fx[,1], 4,5)

  wind_comp <- str_match(wind[,1], "([0-9VRB\\/]{3})([0-9]{2})G?([0-9]{2})?(KT|MPH|MPS)")[,-1]
  wind_var_comp <- str_split_fixed(wind_var[,2], "V", n = 2)
  vis_comp <- str_split_fixed(vis[,2], "(?=[A-Z]{2})", n = 2)
  tt_comp <- str_match(tt[,2], "(M)?([0-9]{2})/(M)?([0-9\\/]{2})?")[,-1]
  qnh_comp <- str_split_fixed(qnh[,2], "(?<=[AQ])", n = 2)
  vis_min_comp <- str_split_fixed(vis_min[,2], "(?=[A-Z])", n = 2)

  arr <- cbind(rbind(wind_comp), rbind(wind_var_comp), cavok[,2], rbind(vis_comp), ndv[,2], rbind(vis_min_comp), rvr[,2], pw[,2], cld[,2], vvis[,2],
               rbind(tt_comp), rbind(qnh_comp), re[,2], ws[,2], rwy_cond[,2])
  arr[arr == ""] <- NA
  m <- as.data.table(arr)
  setnames(m, c("dir", "ff", "fx", "ff_unit", "dir_from", "dir_to", "cavok", "vis", "vis_unit", "ndv", "vis_min", "vis_min_dir", "rvr",
         "pw", "cld", "vvis",  "tt_sign", "tt", "td_sign", "td", "qnh_unit", "qnh", "re", "ws",  "rwy_cond"))

  # Parse visibility fractions (statute miles)
  m[, vis_ones := str_match(vis, "([0-9]+(?!\\/))")[,-1]]
  m[, vis_frac := str_match(vis, "([0-9]{1,2}\\/[0-9]{1})")[,-1]]
  m[, c("denom", "num") := tstrsplit(vis_frac, "\\/")]

  m[dir == "VRB", dir := 360]

  # Process
  m[, `:=`(
    dir = as.numeric(dir),
    ff = as.numeric(ff),
    fx = as.numeric(fx),
    dir_from = as.numeric(dir_from),
    dir_to = as.numeric(dir_to),
    vis = pmax(as.numeric(vis_ones), 0, na.rm = FALSE) + pmax(as.numeric(denom)/as.numeric(num), 0, na.rm = TRUE),
    vvis = as.numeric(vvis),
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


#' Parse METAR Groups
#'
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
#' @description
#' Parse a METAR report into groups
#' Performance: ~ 60s for 1 million records on average machine
#' @param x METAR reports (character vector)
#' @param t time (POSIXct vector of same length as `x`)
#' @param yyyy year
#' @param mm month
#' @export
#' @examples
#' x <- metar.test$code
#' parse_metar(x)
#' parse_metar(x = "EGVP 281509Z 27011KT 5000 1000SW +SHRA BKN022CB 15/13 Q1007 BECMG 9999 NSW SCT030 RMK AMB BECMG BLU")
#'
parse_metar <- function(x, t = NULL, yyyy = year(Sys.Date()), mm = month(Sys.Date())){

  if(class(x)[1] != "character") stop("x needs to character vector. Did you accidentialy pass a data frame?")

  rmk <- str_split_fixed(x, "(\\s(RMK)\\s)|\\sRMK(?=$)|\\=(?=$)", 2) # |\\=
  becmg <- str_split_fixed(rmk[,1], "((?<=\\s)BECMG)|(\\s(?=NOSIG))", 2)
  tempo <- str_split_fixed(becmg[,1], "\\s(?<=\\s)TEMPO\\s", 2)
  code <- str_split_fixed(tempo[,1], "\\s(?=(BLU|GRN|WHT|AMB|WHITE|BLACK))", 2)
  wx <- str_split_fixed(code[,1], "\\s(?<=[0-9]{6}Z\\s)", 2)
  auto <- str_split_fixed(wx[,2], "(?<=AUTO)\\s|(^(?!AUTO))", 2)
  cor <- str_split_fixed(auto[,2], "(?<=COR)\\s|(^(?!COR))", 2)

  time <- str_split_fixed(wx[,1], "(?<=[0-9A-Z]{4})\\s", 2)

  grp <- as.data.table(cbind(time[,1], time[,2], cor[,1], auto[,1], cor[,2], code[,2], tempo[,2], becmg[,2],  rmk[,2]))
  setnames(grp, c("icao", "time", "cor", "auto", "wx", "code", "tempo", "becmg", "rmk")) #

  # Process
  grp[, `:=`(
    auto = auto != "",
    cor = cor != "",
    time = if(!is.null(t)) t else as.POSIXct(paste0(yyyy, sprintf("%02d", mm), time), format = "%Y%m%d%H%M", tz = "GMT")
  )]
  setcolorder(grp, c("icao", "time"))

  #WX
  wx <- parse_metar_wx(x = grp$wx)

  cbind(grp, wx, metar = x)
}
