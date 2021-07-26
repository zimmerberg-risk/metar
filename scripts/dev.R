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

test <- "XXXX 271750Z AUTO VRB03G17KT 050V140 0200NDV R09R/0900N R27L/0750N +TSRA VCFG SCT009 BKN029 OVC045CB 25/18 Q1017" # TEMPO FM1420 TL1450 5000 TSRA RMK AO2A CIG 015V027 BECMG AT1840 -TSRA SCT011 FEW026CB BKN030

regex.blocks <- "([0-9A-Z]{4})\\s([0-9]{6})Z\\s(COR(?:\\s))?(AUTO(?:\\s))?(.*?)((?:TEMPO\\s.*|NOSIG)?)?(RMK\\s.*?)?(BECMG\\s.*?)?$" # (RMK|BECMG|TEMPO|$
system.time({
 blocks <- as.data.table(str_match(m1a, regex.blocks)[,-1])
 setnames(blocks, c("id_icao", "time_valid", "cor", "auto", "msg", "trend", "rmk", "bcmg"))
})

head(blocks)
blocks[id_icao %in% c("LSZH", "YSSY", "KDEN", "LOWI"), .SD[which.max(time_valid)], id_icao]

regex.list <- list(
  wind = "([0-9VRB\\/]{3})([0-9]{2})(?:G)?([0-9]{2})?((?:KT|MPH|MPS)(?=\\s))",
  wind_var = "([0-9\\/]{3})V([0-9\\/]{3}(?=\\s))",
  vis = "(([0-9]{4}|[0-9]{1,2}(?=KM)|[0-9]{1,2}\\s[0-9]{1}\\/[0-9]{1}(?=SM)|[0-9]{1}\\/[0-9]{1,2}(?=SM)|(?!<\\/)[0-9]{1,2}(?=SM))(SM|KM)?(NDV)?)",
  vis_min = "([0-9]{4})((?:N|NE|E|SE|S|SW|W|NW)(?=\\s))",
  rvr = "(R[0-9]{2}[CRL]?\\/([PM]?[0-9]{4}[DUN]?)(V[PM]?[0-9]{4}[DUN]?)?(FT)?)",
  rwy_cond = "R[0-9]{2}[CRL]?\\/[0-9\\/]{1}[1259\\/]{1}[0-9A-Z\\/]{4}",
  vvis = "(?<=VV)([0-9|\\/]{3})",
  cld =  "(FEW|SCT|BKN|OVC|[\\/]{3})([0-9]{3}|[\\/]{3})([A-Z]{2,3}|\\/\\/\\/)?",
  pw = "(\\+|\\-)?(TS)?(RA)",
  tt_td = "(?<=\\s)(M)?([0-9]{2})/(M)?([0-9\\/]{2})",
  qnh =  "(Q|A)([0-9]{4})",
  ws = "((?<=WS\\s)(?:R[A-Z0-9]{2,3}|ALL RWY))"
)
regex <- sprintf("%s", paste(sprintf("(?:%s).+", regex.list[1:3]), collapse = "", sep = ""), sep = "", collapse = "")
print(regex)
str_match(string = test, regex)[,-1]

lapply(regex.list, function(i) str_match(string = test, sprintf("\\b%s\\b", i)))

str_match(test, "(?=<\\b)([0-9VRB\\/]{3})([0-9\\/]{2})([0-9]{2})(?:G)?([0-9]{2})?(KT|MPH|MPS)(?=\\b)")
str_match(c("VRB03G17KT ", "03317MPS ", "12503G17KT ", "271750Z"), "([0-9VRB\\/]{3})([0-9]{2})(?:G)?([0-9]{2})?((?:KT|MPH|MPS)(?=\\s))")

