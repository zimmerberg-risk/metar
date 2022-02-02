library(metar)
library(maps)
library(data.table)
library(maps)

if(Sys.info()[["sysname"]] == "Darwin"){
  dir.base <- "~/Temp/metar"
} else {
  dir.base <- "C:/Temp/metar"
}

# ---------------------------------------- Selection -----------------------------------------------
get_metar_stn(fi.name = "^berli")

# World cicties
data(world.cities)
dt.cities <- as.data.table(world.cities)
setorder(dt.cities, -pop)
dt.cities <- dt.cities[pop>1e4, .SD[1:5], country.etc]
id.icao <- sapply(unique(dt.cities$name[]), function(i) metar.stn[active == T]$icao[grepl(i, metar.stn[active == T]$ap_name_long)][1])

# ---------------------------------------- Selection -----------------------------------------------

au <- c("YAYE", "YSSY", "YMML", "YPPH", "YMLT", "YPDN", "NZAA", "NZCH", "NFNA", "NVVV")
gl <- c("BGBW", "BGKK")

cn <- c("ZSPD", "ZSNB")
fr <- c("LFTW", "LFMT", "LFMV")
es <- "GCLA"
ww <- c("PGSN", "PGRO", "PGUM", "RJAW", "PTRO", "PTYA", "BIKF", "YGEL", "LSZH", "LSZA", "FTTJ", "GABS", "GCXO", "NZSP", "LOWI",
         "UUDD", "GCXO", "WSSS", "BGTL", "SBMN", "OMDB", "KBOS")
jp <- c("RJTT", "RJAA", "RJTE", "RJNS")
gr <- c("LGKR", "LGZA", "LGKF")
gb <- c( "EGPB", "EGNT")
ie <- c("EIKY", "EINN")
no <- c("ENBR", "ENGM")
ch <- c("LSZH", "LSGG", "LSZA", "LSZG", "LSZS", "LSZB")
fr <- c("FMEE")

# ---------------------------------------- Plot Metargram -----------------------------------------------
folder <- "fr"
id.icao <-  get(folder) #"LSZH" #CYYT RCFN RCKH ROYN ROIG    URSS URKK LTFH LICZ URKA

date.end <- Sys.Date() #  "2021-04-01"
date.start <- date.end - 5 #Sys.Date() - 14  "2021-04-01"

dir.plot <- file.path(dir.base, folder)
dir.create(dir.plot, showWarnings = F)

void <- lapply(id.icao, function(id.icao){
  # id <- "LSZH" #CYYT RCFN  RCKH   URSS URKK LTFH LICZ URKA
  cat(id.icao, " ", metar.stn[icao == id.icao, ap_name], as.character(date.start), as.character(date.end), "\n")

  dat.metar <- read_metar_mesonet(id_icao = id.icao, date_start = date.start, date_end = date.end)
  if(nrow(dat.metar) == 0) return(NULL)

  dat.parsed <- parse_metar(x = dat.metar$metar, t = dat.metar$valid)
  dat.parsed <- validate_metar(dat.parsed)
  dat.plot <- cbind(dat.parsed, dat.parsed[, parse_metar_pw(pw)])
  dat.plot <- metar.stn[dat.plot, on = "icao"]

  file.name <- file.path(dir.plot, sprintf("%s_%s_%s.png", id.icao, stringr::str_remove(dat.plot$ap_name[1], "\\/|\\?"), substr(date.end, 1, 10)))

  png(file.name, width = 1600, height = 900, units = "px", res = 96)
  plot_metargram(dat = dat.plot, cex = 1.3)
  dev.off()

})

