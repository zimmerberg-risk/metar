suppressPackageStartupMessages({
  library(data.table)
  library(metar)
  library(stringr)
})

args <- commandArgs(trailingOnly = TRUE)
args.full <- commandArgs(trailingOnly = FALSE)
arg.file <- args.full[grepl("^--file=", args.full)][1]
dir.repo <- if(length(arg.file) && !is.na(arg.file)) {
  normalizePath(file.path(dirname(sub("^--file=", "", arg.file)), ".."), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

if(!exists("plot_metargram_gg", mode = "function")) {
  file.local.plot <- file.path(dir.repo, "R", "plot-gg.R")
  if(file.exists(file.local.plot)) {
    sys.source(file.local.plot, envir = .GlobalEnv)
    message(sprintf("Loaded local plotting code from %s", file.local.plot))
  } else {
    stop("`plot_metargram_gg()` is not available in the installed package and local R/plot-gg.R was not found.", call. = FALSE)
  }
}

station_groups <- list(
  demo = "KFFO",
  ch = c("LSZH", "LSGG", "LSZA", "LSZG", "LSZS", "LSZR", "LSZB", "LSGS"),
  au_west = c("YPLM", "YBAS", "YMLT", "YPDN", "YPPH"),
  ww = c("RJAW", "PTRO", "PTYA", "BIKF", "YGEL", "LSZH", "LSZA", "FTTJ", "GABS", "GCXO", "NZSP", "LOWI")
)

selection <- if(length(args) >= 1L) args[[1]] else "au_west"
date_end <- if(length(args) >= 2L) as.Date(args[[2]]) else Sys.Date()
days_back <- if(length(args) >= 3L) as.integer(args[[3]]) else 2L
dir_base_arg <- if(length(args) >= 4L) args[[4]] else NA_character_
folder <- gsub("[^A-Za-z0-9_-]+", "_", selection)

id_icao <- station_groups[[selection]]
if(is.null(id_icao)) {
  id_icao <- unique(trimws(unlist(strsplit(selection, ","))))
}
id_icao <- toupper(id_icao[nzchar(id_icao)])
date_start <- date_end - days_back

default_base <- if(exists("dirs", inherits = TRUE) && "prod" %in% names(dirs)) {
  file.path(dirs$prod, "demo", "metar-gg", folder)
} else {
  file.path(dir.repo, "out", "metar-gg", folder)
}
dir_base <- if(!is.na(dir_base_arg) && nzchar(dir_base_arg)) dir_base_arg else default_base
dir_panels <- file.path(dir_base, "panels")
dir_combined <- file.path(dir_base, "combined")
dir.create(dir_panels, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_combined, recursive = TRUE, showWarnings = FALSE)
message(sprintf("Writing plots under %s", dir_base))

save_plot_png <- function(plot, filename, width, height, dpi = 144) {
  device <- if(requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "white",
    device = device
  )
}

build_demo_metar <- function(id = "KFFO") {
  if(!exists("metar.test", inherits = TRUE)) data("metar.test", package = "metar", envir = environment())
  dat_demo <- parse_metar(metar.test$code)
  dat_checked <- try(validate_metar(dat_demo), silent = TRUE)
  if(!inherits(dat_checked, "try-error")) dat_demo <- dat_checked

  keep_cols <- c("time", "icao", "tt", "td", "ff", "fx", "dir", "qnh", "vis", "vvis", "pw", "sigwx")
  keep_cols <- intersect(keep_cols, names(dat_demo))
  dat_demo <- dat_demo[complete.cases(dat_demo[, .(tt, td, ff, qnh, vis)]), ..keep_cols]
  if(!nrow(dat_demo)) {
    stop("Bundled demo data could not be prepared.", call. = FALSE)
  }

  dat_demo <- dat_demo[seq_len(min(16L, .N))]
  dat_demo[, time := seq(
    from = as.POSIXct("2026-03-26 00:00:00", tz = "UTC"),
    by = "3 hour",
    length.out = .N
  )]
  dat_demo[, icao := id]
  dat_demo
}

save_plot_set <- function(plots, id, suffix = format(date_end, "%Y-%m-%d")) {
  station_name <- plots$metadata$title
  station_stub <- str_remove_all(station_name, "[^A-Za-z0-9_-]+")
  file_stub <- sprintf("%s_%s_%s", id, station_stub, suffix)

  fn_combined <- file.path(dir_combined, sprintf("%s_combined.png", file_stub))
  save_plot_png(
    plot = plots$combined,
    filename = fn_combined,
    width = 12,
    height = 12
  )
  message(sprintf("Saved combined chart for %s -> %s", id, fn_combined))

  for(panel_name in names(plots$panels)) {
    save_plot_png(
      plot = plots$panels[[panel_name]],
      filename = file.path(dir_panels, sprintf("%s_%s.png", file_stub, panel_name)),
      width = 7,
      height = 7
    )
  }
  message(sprintf("Saved %s square panels for %s", length(plots$panels), id))
}

success_count <- 0L

for(id in id_icao) {
  cat(sprintf("%s | %s -> %s\n", id, as.character(date_start), as.character(date_end)))

  dat_metar <- try(read_metar_mesonet(id_icao = id, date_start = date_start, date_end = date_end), silent = TRUE)
  if(inherits(dat_metar, "try-error") || !nrow(dat_metar)) {
    err <- if(inherits(dat_metar, "try-error")) {
      conditionMessage(attr(dat_metar, "condition"))
    } else {
      "no METAR rows returned"
    }
    message(sprintf("Skipping %s: %s", id, err))
    next
  }

  dat_parsed <- parse_metar(x = dat_metar$metar, t = dat_metar$valid)
  dat_valid <- try(validate_metar(dat_parsed), silent = TRUE)
  if(inherits(dat_valid, "try-error")) dat_valid <- dat_parsed

  plots <- try(plot_metargram_gg(dat_valid), silent = TRUE)
  if(inherits(plots, "try-error")) {
    message(sprintf("Skipping %s: plotting failed: %s", id, conditionMessage(attr(plots, "condition"))))
    next
  }

  save_plot_set(plots = plots, id = id)
  success_count <- success_count + 1L
}

if(identical(selection, "demo") || success_count == 0L) {
  message("Building bundled demo plot set.")
  dat_demo <- build_demo_metar(if(length(id_icao)) id_icao[[1]] else "KFFO")
  plots_demo <- plot_metargram_gg(dat_demo, attribution = "Bundled metar.test demo (synthetic time sequence)")
  save_plot_set(
    plots = plots_demo,
    id = unique(dat_demo$icao)[1],
    suffix = "demo"
  )
}
