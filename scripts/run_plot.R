library(metar)
library(maps)
library(data.table)
library(maps)

dir.base <- "C:/Temp/metar"

# ---------------------------------------- Selection -----------------------------------------------
get_metar_stn(fi.name = "^berli")

stn <- get_metar_stn(fi.lat = c(40, 50), fi.lon = c(5, 15))$icao
stn <- get_metar_stn(fi.ctry = "Switz")$icao

# World cicties
data(world.cities)
dt.cities <- as.data.table(world.cities)
setorder(dt.cities, -pop)
dt.cities <- dt.cities[pop>1e4, .SD[1:5], country.etc]
id.icao <- sapply(unique(dt.cities$name[]), function(i) metar.stn[active == T]$icao[grepl(i, metar.stn[active == T]$ap_name_long)][1])

# ---------------------------------------- Selection -----------------------------------------------

id.icao <- c("YAYE", "YSSY", "YMML", "YPPH", "YMLT", "YPDN", "NZAA", "NZCH", "NFNA", "NVVV")
id.icao <- c("PTRO", "PTYA", "BIKF", "YGEL", "LSZH", "LSZA", "FTTJ", "GABS", "GCXO", "NZSP", "LOWI",
         "UUDD", "GCXO", "WSSS", "BGTL", "SBMN", "OMDB", "KBOS")

gl <- c("BGBW", "BGKK")

# ---------------------------------------- Plot Metargram -----------------------------------------------

id.icao <- "BGBW" #"LSZH" #CYYT RCFN RCKH ROYN ROIG    URSS URKK LTFH LICZ URKA

folder <- "gl"
date.end <- Sys.Date() #  "2021-04-01"
date.start <- date.end - 2 #Sys.Date() - 14  "2021-04-01"

dir.plot <- file.path(dir.base, folder)
dir.create(dir.plot, showWarnings = F)

void <- lapply(id.icao, function(id){
  # id <- "LSZH" #CYYT RCFN  RCKH   URSS URKK LTFH LICZ URKA
  cat(id, " ", metar.stn[icao == id, ap_name], as.character(date.start), as.character(date.end), "\n")

  dat.metar <- read_metar_mesonet(id_icao = id, date_start = date.start, date_end = date.end)
  if(nrow(dat.metar) == 0) return(NULL)

  dat.parsed <- parse_metar(x = dat.metar$metar, t = dat.metar$valid)
  dat.parsed <- validate_metar(dat.parsed)
  dat.plot <- cbind(dat.parsed, dat.parsed[, parse_metar_pw(pw)])
  dat.plot <- metar.stn[dat.plot, on = "icao"]

  file.name <- file.path(dir.plot, sprintf("%s_%s_%s.png", id, stringr::str_remove(dat.plot$ap_name[1], "\\/|\\?"), substr(date.end, 1, 10)))

  png(file.name, width = 1600, height = 900, units = "px", res = 96)
  plot_metargram(dat = dat.plot, cex = 1.3)
  dev.off()

})

