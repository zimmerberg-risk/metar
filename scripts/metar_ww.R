# 
# https://github.com/alessandro-gentilini/R-METAR-decoder/blob/master/metar_decoder.R
# https://github.com/tomp/python-metar
# https://www.badbadweather.com/other-2/snow-world/
# https://stringr.tidyverse.org/articles/regular-expressions.html

# cld.test <- c("//////TCU ", "BKN100", "BKN100CB")
# str_match(cld.test, sprintf("(%s|[\\/]{3})([0-9]{3}|[\\/]{3})([A-Z]{2,3}|\\/\\/\\/)?", cld.amt))

# vis.test <- c("2 3/4SM", "1/2SM ", "5000", "M0400", "10SM", "11KM", "////NDV", "9999NDV", "////", "22/01")
# str_match(vis.test, "M?([0-9]{4}(?=\\h?)|[\\/]{4}(?=\\h?)|[0-9]{1,2}(?=SM|KM)|[0-9]{1}\\/[0-9]{1}(?=SM|KM)|[0-9]{1}\\h[0-9]{1}\\/[0-9]{1}(?=SM|KM?))(SM|KM)?(NDV)?")

# time.test <- c("280830", "280830Z")

url.station <- "https://www.aviationweather.gov/docs/metar/stations.txt"
url.data <- "https://tgftp.nws.noaa.gov/data/observations/metar/cycles/08Z.TXT"

dt.lines <- read_lines(url.data)
dt.metar <- dt.lines[grepl("^([A-Z]{4}).+", dt.lines)]

col.names <- exprs(state, name, icao, iata, synop, lat_deg, lat_min, lon_deg, lon_min, z, M,  N,  V,  U,  A, C, ctry)
col.start <- c(1, 4, 21, 27, 33, 40, 43, 48, 52, 56, 63, 66, 69, 72, 75, 80, 82)
col.pos <- fwf_positions(col.start, c(col.start[-1] - 1, NA), col_names = col.names)

station.lines <- read_lines(url.station, skip = 39)
station.lines <- station.lines[map_int(station.lines, nchar) > 80]
dt.station <- read_fwf(station.lines, col_positions = col.pos) 

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