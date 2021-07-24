library(data.table)
library(metar)
library(vroom)
library(stringr)
#library(stringdist)

# https://tgftp.nws.noaa.gov/data/observations/metar/cycles/10Z.TXT

dir.base <- "C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/Data/metar"
dir.data <- file.path(dir.base, "data")


txt <- vroom_lines(file.path(dir.data, "10Z.TXT"), skip_empty_rows = TRUE)
t <- txt[seq_along(txt) %% 2 == 1]
m <- txt[seq_along(txt) %% 2 == 0]

# Remove double spaces
m <- str_replace(m, "\\s{2,}", "\\s")

m1a <- str_subset(m, "[0-9A-Z]{4}\\s[0-9]{6}Z")
m1b <- str_subset(m, "[0-9A-Z]{4}\\s[0-9]{6}Z", negate = TRUE)

cat(length(m), length(m1), "\\n", sep = " ")
cat("Disregarded: ", m1b, "", sep = "\n")

# test <- "XXXX 271750Z AUTO VRB03G17KT 050V140 0200NDV R09R/0900N R27L/0750N +TSRA VCFG SCT009 BKN029 OVC045CB 25/18 Q1017 TEMPO FM1420 TL1450 5000 TSRA RMK AO2A CIG 015V027 BECMG AT1840 -TSRA SCT011 FEW026CB BKN030"

regex.blocks <- "([0-9A-Z]{4})\\s([0-9]{6})Z\\s(COR(?:\\s))?(AUTO(?:\\s))?(.*?)((?:TEMPO\\s.*|NOSIG)?)?(RMK\\s.*?)?(BECMG\\s.*?)?$" # (RMK|BECMG|TEMPO|$
system.time({
 blocks <- as.data.table(str_match(m1a, regex.blocks)[,-1])
 setnames(blocks, c("id_icao", "time_valid", "cor", "auto", "msg", "trend", "rmk", "bcmg"))
})

head(blocks)
blocks[id_icao %in% c("LSZH", "YSSY", "KDEN", "LOWI"), .SD[which.max(time_valid)], id_icao]

regex.list <- list(
  wind = "([0-9VRB\\/]{3})([0-9\\/]{2})(?:G)([0-9]{2,3})?(KT|MPH|MPS)(?=\\b)",
  wind.var = "((?:[0-9\\/]{3})V(?:[0-9\\/]{3}))",
  vis = "(\\b([0-9]{4}|[0-9]{1,2}(?=KM)|[0-9]{1,2}\\s[0-9]{1}\\/[0-9]{1}(?=SM)|[0-9]{1}\\/[0-9]{1,2}(?=SM)|(?!<\\/)[0-9]{1,2}(?=SM))(SM|KM)?(NDV)?)\\b"
)
regex.msg <- paste(regex.list$wind, "?", sep = "", collapse = "?")
parse <- as.data.table(str_match(test, regex.msg)[,-1])
parse[!is.na(V4)]



str_match(test, "(?=<\\b)([0-9VRB\\/]{3})([0-9\\/]{2})([0-9]{2})(?:G)?([0-9]{2})?(KT|MPH|MPS)(?=\\b)")
str_match(c("VRB03G17KT", "03317MPS", "12503G17KT"), "([0-9VRB\\/]{3})([0-9]{2})(?:G)?([0-9]{2})?(KT|MPH|MPS)")

