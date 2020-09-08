# library(metar)
library(data.table)
library(tidyverse)

dt.in <- read_mesonet(id_icao = "KDEN", date_start = Sys.time() - 3600*24*2, date_end = Sys.time() + 3600*24) #"RKPK"


test <- c(
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
  "LSZH 250350Z VRB01KT 9000 -SHRA FEW007 BKN009 BKN022 02/00 Q1007"
)

dt.in <- read_csv("C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Kunden/05 MeteoSchweiz/AutoMETAR/metar_LSZH_2000_2020.txt", guess_max = 1e5)

dt <- parse_metar(x = test)
dt <- parse_metar(x = dt.in$metar, date = dt.in$valid)


plot(dt$time, dt$td, type = "l")
dt$pw
dt[, unique(tt)]
dt[, unique(pw)]
dt[is.na(qnh)]

metar_clouds <- function(cld, long = TRUE){
  #cld <- dt$cld
  m <- str_match_all(cld, "(FEW|SCT|BKN|OVC|[\\/]{3})([0-9]{3}|[\\/]{3})([A-Z]{2,3}|\\/\\/\\/)?")
  dt.cloud <- data.table::rbindlist(lapply(seq_along(m), FUN = function(i){
    dat <- m[[i]]
    if(nrow(dat) == 0) dat <- rbind(rep(NA, 4))
    dt <- data.table::setnames(data.frame(dat), c("cld_str", "cld_amt","cld_hgt", "cld_type"))
    dt$cld_group <- seq_len(nrow(dat))
    dt$id <- i
    dt
  }))
  dt.melt <- data.table::melt(dt.cloud, id.vars = c("id", "cld_group"))
  data.table::dcast(dt.melt, id ~ ...)

}
dt <- cbind(dt, metar_clouds(dt$cld))


