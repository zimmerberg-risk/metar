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
dt.noaa <- read_metar_noaa(hour = 5, latest.only = T)
grep("SN\\b", dt.noaa$metar, value = T) # CAVOK|NSC|NCD|WXNIL|CLR|SKC|NSW

## --------------------------------------------------- Speed  ---------------------------------------------------
dt.test <- parse_metar_grp(x = metar.test$code)

microbenchmark::microbenchmark({  dt.grp <- parse_metar_grp(x = dt.noaa$metar, t = dt.noaa$time_valid) }, times = 1)
dt.grp[id_icao %in% c("LSZH", "YSSY", "KDEN", "LOWI", "EGGW", "BIAR", "DRRN", "ETMN", "RJAA", "UUMO")]
vroom::vroom_write(dt.grp, "C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Data/metar/test.csv", delim = ",", na = "")

# microbenchmark::microbenchmark({  dt.grp.1 <- parse_metar(x = dt.noaa$metar, dt.noaa$time_valid) }, times = 1)
# microbenchmark::microbenchmark({  dt.grp.2 <- parse_metar_grp(x = rep(dt.noaa$metar, 200)) }, times = 1)

parse_metar_cld(x = dt.test$cld)

x1 <- read_metar_mesonet("SBPA", date_start = "2021-07-15")
x2 <- parse_metar_grp(x1$metar, x1$valid) #dt.noaa$metar
x3 <- parse_metar_pw(x2$pw)
x2 <- metar.stn[x2, on = "icao"]
plot_metargram(dat = cbind(x2, x3))

## --------------------------------------------------- Tests  ---------------------------------------------------
expression({
  test.wx <- c("EGVP 281509Z VRB03G17KT 050V140 0200NDV 4000SE R09R/0900N R27L/0750N R01/019591 R02C/019591 +TSRA SNGSRA VCFG SCT009 BKN029 OVC045CB 25/18 Q1017 WS R08",
          "EGVP 281509Z 29006KT CAVOK 10SM 28/18 A3008")
  test.grp <- "EGVP 281509Z 27011KT 5000 1000SW +SHRA BKN022CB 15/13 Q1007 TEMPO +TS BECMG 9999 NSW SCT030 RMK AMB"

  stringr::str_match("111 AAAA AAAA 111", "((?:A{4}(?:\\s)?){1,4}(?:\\s)?)")
  stringr::str_match(rep("AAAA AAAA XXXX XXXX XXXX 111", 2), "((?:A{4}(?:\\s)){1,4})?((?:X{4}(?:\\s)){1,4})?((?:Y{4}(?:\\s)){1,4})?")
  stringr::str_match("SCT009 SCT/// BKN029 OVC045CB", "((?:(?:FEW|SCT|BKN|OVC|[0-9\\/]{3})[0-9\\/]{3}[A-Z]{0,2}(?:\\s)?){1,4}(?:\\s)?)")
  str_match(c("WS R08", "WS ALL RWY"), "((?<=WS\\s)(?:R[A-Z0-9]{2,3}|ALL RWY))")

  re <- str_match(metar.test$code, "(?<=RE).+?(?=(?:\\s(?:TEMP|BECMG|RMK))|$)")
  tempo <- str_match(metar.test$code, "(?<=TEMPO\\s).+?(?=(?:\\s(?:BECMG|RMK))|$)")
  becmg <- str_match(metar.test$code, "(?<=BECMG\\s).+?(?=(?:\\s(?:RMK))|$)")
  rmk <- str_match(metar.test$code, "(?<=RMK\\s).+?$")


  str_detect(metar.test$code, fixed("(RMK|TEMPO)"))
  str_locate_all(metar.test$code, fixed(c("RMK", "TEMPO")))


  test.grp <- c("EGVP 281509Z 27011KT 5000 BKN022CB 15/13 Q1007", "EGVP 281509Z 27011KT 5000 BKN022CB 15/13 Q1007 TEMPO +TS RMK AMB", "EGVP 281509Z 27011KT 5000 BKN022CB 15/13 Q1007 TEMPO +TS BECMG 9999 NSW SCT030 RMK AMB")

  system.time({
    str <-  dt.noaa$metar #  metar.test$code
    rmk <- str_split_fixed(str, "(?<=\\s)RMK(?=\\s)", 2)
    becmg <- str_split_fixed(rmk[,1], "(?<=\\s)BECMG", 2)
    tempo <- str_split_fixed(becmg[,1], "(?<=\\s)TEMPO", 2)
    re <- str_split_fixed(tempo[,1], "\\s(?<=\\s)RE", 2)

    wx <- str_split_fixed(re[,1], "\\s(?<=[0-9]{6}Z\\s)", 2)
    auto <- str_split_fixed(wx[,2], "(?<=AUTO)\\s|(^(?!AUTO))", 2)
    cor <- str_split_fixed(auto[,2], "(?<=COR)\\s|(^(?!COR))", 2)

    time <- str_split_fixed(wx[,1], "(?<=[0-9A-Z]{4})\\s", 2)

    res <- as.data.table(cbind(time[,1], time[,2], cor[,1], auto[,1], wx[,2], re[,2], tempo[,2], becmg[,2],  rmk[,2]))
    setnames(res, c("id_icao", "time_valid", "cor", "auto", "wx", "re", "tempo", "becmg", "rmk")) #
  })
  res

})


