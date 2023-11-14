library(metar)
library(maps)
library(data.table)
library(maps)

dir.base <- file.path(dirs$prod, "demo", "metar")

# ---------------------------------------- Selection -----------------------------------------------
get_metar_stn(fi.ctry = "Ice")

# # World cicties
# data(world.cities)
# dt.cities <- as.data.table(world.cities)
# setorder(dt.cities, -pop)
# dt.cities <- dt.cities[pop>1e4, .SD[1:5], country.etc]
# id.icao <- sapply(unique(dt.cities$name[]), function(i) metar.stn[active == T]$icao[grepl(i, metar.stn[active == T]$ap_name_long)][1])

# ---------------------------------------- Selection -----------------------------------------------

ww <- c("RJAW", "PTRO", "PTYA", "BIKF", "YGEL", "LSZH", "LSZA", "FTTJ", "GABS", "GCXO", "NZSP", "LOWI",
        "UUDD", "GCXO", "WSSS", "BGTL", "SBMN", "OMDB", "KBOS")

gu <- c("PGSN", "PGRO", "PGUM")
au <- c("YAYE", "YSSY", "YMML", "YPPH", "YMLT", "YPDN", "NZAA", "NZCH", "NFNA", "NVVV", "YBAS")
nz <- c("NZAA", "NZWN", "YSNF")
cn <- c("ZJSY", "ZJHK", "VMMC", "VHHH")
jp <- c("RJTT", "RJAA", "RJNS", "RJAW", "RODN", "RODE", "ROTM", "ROIG", "ROYN", "RORS", "ROMY", "ROAH", "RJFE", "RJDT", "RJDB", "RJNK", "RJNT",
        "RJKA", "RJKI", "RJFG", "RJFK", "RJFM", "RJFY", "RJFT", "RJFU")
kr = c("RKPM", "RKPC", "RKPK", "RKJB")
mt = c("FIMP", "FMEE", "FMEP")

fr <- c("LFTW", "LFMT", "LFMV", "LFKJ", "LFKC", "LFRB", "LFRL", "LFRH", "EGJJ", "LFBZ")
gb <- c( "EGPB", "EGNT", "EGLL", "EGHQ", "EGDR", "EGHQ")
it <- c("LIPY", "LIVF")
es <- c("GCLA", "LESO", "LEBB")
gl <- c("BGBW", "BGKK")
gr <- c("LGKR", "LGZA", "LGKF")
ie <- c("EINN")
no <- c("ENBR", "ENGM")
ch <- c("LSZH", "LSGG", "LSZA", "LSZG", "LSZS", "LSZR", "LSZB", "LSGS")
ic <- c("BIKF", "BIEG", "BIAR", "BIRK")
de <- c("EDDH")

ind <- c("VIDP")
dz = c("DAAG", "DAOO", "DAUG")
ma <- c("GMFK")
sa <- c("FALE", "FBSK", "FAMM")
uae <- c("OMDB", "OMAD", "OMRK", "OMFJ", "OOKB")
qa <- c("OTHH", "OTBH")
pk <- c("OPKC", "OPNH", "OPIS")


ca <- c("CWSA", "CYQY", "CWRA", "CYYT", "CYQX")
ca <- c("CYQY", "CYHZ", "CWBV", "CYHZ", "CWEF", "CWGR", "CXMI", "CWRN", "CWEP")
bm <- c("TXKF")
cl <- c("SCNT", "SCFM", "SCCI")
ak <- c("PACZ", "PASN", "PAOO", "PAKI", "PADK")
us <- c("KSFO")
us.fl <- c("KEYW", "KSRQ", "KSPG","KVNC", "KPGD", "KRSW", "KAPF")
us.tx <- c("KSAT", "KDFW", "KAUS")
# ---------------------------------------- Plot Metargram -----------------------------------------------

folder <- "es"
id.icao <-  get(folder) #"LSZH" #CYYT RCFN RCKH ROYN ROIG    URSS URKK LTFH LICZ URKA

date.end <- Sys.Date()#  "2021-04-01" as.Date("2000-04-29")
date.start <- date.end - 1 #Sys.Date() - 14  "2021-04-01"

dir.plot <- file.path(dir.base, folder)
dir.create(dir.plot, showWarnings = F)

void <- lapply(id.icao, function(id.icao){
  # id.icao <- "PGUM"
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

