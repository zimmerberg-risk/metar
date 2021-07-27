library(data.table)
library(metar)
library(vroom)
library(stringr)
#library(stringdist)

# https://tgftp.nws.noaa.gov/data/observations/metar/cycles/10Z.TXT

dir.base <- "C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Data/metar"
dir.data <- file.path(dir.base, "data")
p <- file.path(dir.data, "10Z.TXT")
p <- "https://tgftp.nws.noaa.gov/data/observations/metar/cycles/18Z.TXT"

txt <- vroom_lines(p, skip_empty_rows = TRUE)
t <- txt[seq_along(txt) %% 2 == 1]
m <- txt[seq_along(txt) %% 2 == 0]

# Remove double spaces
m <- str_replace(m, "\\s{2,}", "\\s")

m1a <- str_subset(m, "[0-9A-Z]{4}\\s[0-9]{6}Z")
m1b <- str_subset(m, "[0-9A-Z]{4}\\s[0-9]{6}Z", negate = TRUE)

cat(length(m), length(m1a), "\\n", sep = " ")
cat("Disregarded: ", m1b, "", sep = "\n")

test <- "XXXX 271750Z AUTO VRB03G17KT 050V140 0200NDV 4000SE R09R/0900N R27L/0750N +TSRA SNGSRA VCFG SCT009 BKN029 OVC045CB 25/18 Q1017" # TEMPO FM1420 TL1450 5000 TSRA RMK AO2A CIG 015V027 BECMG AT1840 -TSRA SCT011 FEW026CB BKN030

regex.blocks <- "^([0-9A-Z]{4})\\s([0-9]{6})Z\\s(COR(?:\\s))?(AUTO(?:\\s))?(.*?)(RE.*?)?((?:TEMPO\\s.*|NOSIG)?)?(RMK\\s.*?)?(BECMG\\s.*?)?$"
system.time({
 blocks <- as.data.table(str_match(m1a, regex.blocks)[,-1])
 setnames(blocks, c("id_icao", "time_valid", "cor", "auto", "msg", "recent", "trend", "rmk", "bcmg"))
})

head(blocks)
blocks[!is.na(recent)]
blocks[id_icao %in% c("LSZH", "YSSY", "KDEN", "LOWI"), .SD[which.max(time_valid)], id_icao]

regex.dc.ph <- paste(c(metar.dc$id_dc, metar.ph$id_ph), collapse = "|")

regex.list <- list(
  wind = "([0-9VRB\\/]{3})([0-9]{2})(?:G)?([0-9]{2})?(?:KT|MPH|MPS)",
  wind_var = "([0-9\\/]{3})V([0-9\\/]{3})",
  vis = "([0-9]{4}|[0-9]{1,2}(?=KM)|[0-9]{1,2}\\s[0-9]{1}\\/[0-9]{1}(?=SM)|[0-9]{1}\\/[0-9]{1,2}(?=SM)|(?!<\\/)[0-9]{1,2}(?=SM))(SM|KM)?(NDV)?",

  vis_min = "([0-9]{4})(N|NE|E|SE|S|SW|W|NW)",
  rvr = "(?:R[0-9]{2}[CRL]?\\/(?:[PM]?[0-9]{4}[DUN])(?:V[PM]?[0-9]{4}[DUN])?(?:FT)?\\s?){1,4}",
  rwy_cond = "(?:R[0-9]{2}[CRL]?\\/[0-9\\/]{1}[1259\\/]{1}[0-9\\/]{4}\\s?){1,4}",

  vvis = "(?<=VV)[0-9|\\/]{3}",
  pw = sprintf("((?:\\+|\\-)?\\b(?:VC)?(?:%s){1,4}\\b\\s?){1,4}", regex.dc.ph), #"(\\+|\\-)?(TS)?(RA)",
  cld =  "(?:(?:FEW|SCT|BKN|OVC|[0-9\\/]{3})[0-9\\/]{3}[A-Z]{0,2}(?:\\s)?){1,4}",

  tt_td = "(M)?([0-9]{2})/(M)?([0-9\\/]{2})",
  qnh =  "(Q|A)([0-9]{4})",
  ws = "(?<=WS\\s)(?:R[A-Z0-9]{2,3}|ALL RWY)"
)
lapply(regex.list, function(i) str_match(string = test, sprintf("(%s)", i)))

test <- "VRB03G17KT 050V140 0200NDV 4000SE R09R/0900N R27L/0750N R01/019591 R02C/019591 +TSRA SNGSRA VCFG SCT009 BKN029 OVC045CB 25/18 Q1017 WS R08"
regex <- sprintf("%s", paste0(sprintf("(%s)?(?:\\s)?", regex.list[1:12]), collapse = ""))
print(regex)
str_match(string = test, regex)[,-1]

na.omit(as.data.table(str_match(string = blocks$msg[], regex)[,-1]), cols = 12)

grep("SE", blocks$msg, value = T)

stringr::str_match("111 AAAA AAAA 111", "((?:A{4}(?:\\s)?){1,4}(?:\\s)?)")
stringr::str_match(rep("AAAA AAAA XXXX XXXX XXXX 111", 2), "((?:A{4}(?:\\s)){1,4})?((?:X{4}(?:\\s)){1,4})?((?:Y{4}(?:\\s)){1,4})?")
stringr::str_match("SCT009 SCT/// BKN029 OVC045CB", "((?:(?:FEW|SCT|BKN|OVC|[0-9\\/]{3})[0-9\\/]{3}[A-Z]{0,2}(?:\\s)?){1,4}(?:\\s)?)")

str_match("WS R08 cc", "(?<=WS\\s)(?:R[A-Z0-9]{2,3}|ALL RWY)")
