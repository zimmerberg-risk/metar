
## Usage ---------------------------------------------------------------------------

#' Parse single report
#' "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG" %>% parse_metar()
#'
#' Parse multiple reports
#' metar <- c("LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG", "LFKJ 071130Z AUTO 24011KT CAVOK 27/17 Q1015 NOSIG")
#' sapply(metar, parse_metar) %>% bind_rows()
#'
#' Process single report
#' "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG" %>% parse_metar() %>% process_metar
#'
#' Process multiple reports
#' sapply(metar, parse_metar) %>% bind_rows()  %>% process_metar

## Definitions ---------------------------------------------------------------------------

library(metar)
library(tidyverse)
library(lubridate)

## Test Data Set ---------------------------------------------------------------------------

test <- c(
  # 5 cloud groups
  "KFFO 271946Z 25006KT 10SM FEW009 SCT015 BKN027 BKN035 OVC044 25/22 A2986 RMK AO2A CIG 015V027 CIG 023 RWY05L SLP109",
  # Remark
  "RJTT 241900Z 02016G37KT 6000 -SHRA FEW015 BKN025 BKN040 23/22 Q1008 NOSIG RMK 1CU015 5CU025 7SC040 A",
  # Empty cloud group
  "EGLL 081420Z AUTO 29006KT 260V340 9999 VCTS SCT038/// //////CB 25/15 Q1023 TEMPO 4000 +TSRA",
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
  "LIMH 240355Z 05004KT 0300 FG VV/// 01/M00 Q1035 RMK MON INVIS VAL INVIS VIS MIN 0300",
  # Runway visibility RVR
  "EKKA 240406Z AUTO 17001KT 0200 R09R/0900N R27L/0750N FG VV002 14/13 Q1026",
  # Various missing sections, recent RE
  "LGKZ 241750Z AUTO 00000KT //// // ////// 25/07 Q1020 RE//",
  # CAVOK
  "LSZH 271750Z 14002KT CAVOK 25/18 Q1017 NOSIG",
  # All combined (fictional)
  "XXXX 271750Z AUTO VRB03G17KT 050V140 0200 R09R/0900N R27L/0750N +TSRA VCFG SCT009 BKN029 OVC045CB 25/18 Q1017 TEMPO FM1420 TL1450 5000 TSRA"
)

## Run Test ---------------------------------------------------------------------------

dat <- sapply(test, parse_metar, verbose = F) %>% bind_rows()
process_metar(dat = dat, col.drop = c("metar", "rmk"), verbose = F)

## Run Live Data ---------------------------------------------------------------------------

# Station filter (REGEX)
station.filter <- "^LSZ" # ^LS (Swiss stations) ^LF (French stations) ^K (US stations)

# Get time
time.local <- Sys.time()
attr(time.local, "tzone") <- "UTC"
time.floor <- lubridate::floor_date(time.local, "1 hour")

# Station meta data
url.station <- "https://www.aviationweather.gov/docs/metar/stations.txt"

# Latest cycle file from NOAA
url.data <- sprintf("https://tgftp.nws.noaa.gov/data/observations/metar/cycles/%02dZ.TXT", hour(time.floor))
dt.lines <- read_lines(url.data)
dt.metar <- dt.lines[grepl("^([A-Z]{4}).+", dt.lines)]

str <- dt.metar[grepl(station.filter, dt.metar)]
dat <- lapply(str, parse_metar, verbose = F) %>% bind_rows()
res <- process_metar(dat = dat, col.drop = c("metar"), verbose = F)

# Pick latest report by station
res %>%
  arrange(time) %>%
  distinct(icao, .keep_all = T) %>%
  filter(!is.na(ff)) %>%
  ggplot(aes(icao, ff)) + geom_bar(stat = "identity")


## Run Live Data ---------------------------------------------------------------------------

col.names <- exprs(state, name, icao, iata, synop, lat_deg, lat_min, lon_deg, lon_min, z, M,  N,  V,  U,  A, C, ctry)
col.start <- c(1, 4, 21, 27, 33, 40, 43, 48, 52, 56, 63, 66, 69, 72, 75, 80, 82)
col.pos <- fwf_positions(col.start, c(col.start[-1] - 1, NA), col_names = col.names)

station.lines <- read_lines(url.station, skip = 39)
station.lines <- station.lines[map_int(station.lines, nchar) > 80]
dt.station <- read_fwf(station.lines, col_positions = col.pos)

dt.station %>% filter(ctry == "VI")

dt.station <- dt.station %>%
  #filter(icao == "LSZH") %>%
  separate(lat_min, c("lat_num", "ns"), sep = -1, remove = F) %>%
  separate(lon_min, c("lon_num", "we"), sep = -1, remove = F) %>%
  mutate(
    x = case_when(we == "E" ~ as.numeric(lon_deg) + as.numeric(lon_num)/60, TRUE ~ - (as.numeric(lon_deg) + as.numeric(lon_num)/60)),
    y = case_when(ns == "N" ~ as.numeric(lat_deg) + as.numeric(lat_num)/60, TRUE ~ - (as.numeric(lat_deg) + as.numeric(lat_num)/60))
  ) %>%
  dplyr::select(name, icao, x, y, z)

# Worldwide
system.time({dat <- sapply(dt.metar[grepl("^.*", dt.metar)], parse_metar, verbose = F) %>% bind_rows()})
res <- process_metar(dat, verbose = F)
write_delim(res, file.path(dir, "out.csv"), delim = ";")

res %>%
  left_join(dt.station, by = "icao") %>%
  ggplot(aes(x, y)) +
  geom_text(aes(label = fx)) +
  coord_quickmap()
