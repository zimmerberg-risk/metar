#' Plot METARgram with ggplot2
#'
#' @author M. Saenger, Zimmerberg Risk Analytics GmbH
#' @param dat data set
#' @param attribution data source
#' @param base_size base text size passed to ggplot theme helpers
#' @param base_family font family passed to ggplot theme helpers
#' @return A named list with `panels`, `combined`, `data`, and `metadata`.
#' @export
plot_metargram_gg <- function(
  dat,
  attribution = "Data: Iowa State Univ. mesonet.agron.iastate.edu/",
  base_size = 15,
  base_family = "Oswald"
) {
  metargram_require_namespaces()
  prep <- metargram_prepare_data(
    dat = dat,
    attribution = attribution,
    base_size = base_size,
    base_family = base_family
  )

  panels <- list(
    overview = metargram_panel_overview(prep, show_x = TRUE, square = TRUE),
    wind = metargram_panel_wind(prep, show_x = TRUE, square = TRUE),
    pressure = metargram_panel_pressure(prep, show_x = TRUE, square = TRUE),
    visibility = metargram_panel_visibility(prep, show_x = TRUE, square = TRUE)
  )
  panels_combined <- list(
    overview = metargram_panel_overview(prep, show_x = TRUE, square = FALSE),
    wind = metargram_panel_wind(prep, show_x = TRUE, square = FALSE),
    pressure = metargram_panel_pressure(prep, show_x = TRUE, square = FALSE),
    visibility = metargram_panel_visibility(prep, show_x = TRUE, square = FALSE)
  )

  combined <- (
    (panels_combined$overview | panels_combined$wind) /
      (panels_combined$pressure | panels_combined$visibility)
  ) +
    patchwork::plot_layout(widths = c(1, 1), heights = c(1, 1)) +
    patchwork::plot_annotation(
      title = prep$metadata$title,
      subtitle = prep$metadata$subtitle,
      caption = prep$metadata$caption,
      theme = ggplot2::theme(
        text = ggplot2::element_text(family = prep$style$font_family),
        plot.title = ggplot2::element_text(
          size = prep$style$base_size * 1.45,
          face = "bold",
          colour = prep$style$col_dark,
          margin = ggplot2::margin(b = 2)
        ),
        plot.subtitle = ggplot2::element_text(
          size = prep$style$base_size * 0.86,
          colour = "#555555",
          lineheight = 1.02,
          margin = ggplot2::margin(b = 10)
        ),
        plot.caption = ggplot2::element_text(
          size = prep$style$base_size * 0.72,
          colour = "#666666",
          hjust = 0,
          lineheight = 1.02
        ),
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
    ) &
    ggplot2::theme(plot.margin = ggplot2::margin(4, 6, 4, 6))

  list(
    panels = panels,
    combined = combined,
    data = prep$data,
    metadata = prep$metadata
  )
}

metargram_require_namespaces <- function() {
  pkgs <- c("ggplot2", "patchwork")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if(length(missing)) {
    stop(
      sprintf(
        "plot_metargram_gg() requires the following packages: %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

metargram_style <- function(base_size, base_family) {
  list(
    base_size = base_size,
    font_family = base_family,
    col_bg = "#ffffff",
    col_panel = "#ffffff",
    col_dark = "#111111",
    col_mid = "#666666",
    col_grid = "#e8e8e8",
    col_highlight = "#c1121f",
    col_temp = "#111111",
    col_dew = "#c1121f",
    col_band = "#bdbdbd",
    col_tend_low = "#111111",
    col_tend_mid = "#f5f5f5",
    col_tend_high = "#c1121f",
    fill_air = "#ffffff",
    fill_wind = "#ffffff",
    fill_pressure = "#ffffff",
    fill_visibility = "#ffffff",
    wind_cols = c(
      N = "#111111",
      NE = "#111111",
      E = "#111111",
      SE = "#111111",
      S = "#111111",
      SW = "#111111",
      W = "#111111",
      NW = "#111111",
      Calm = "#999999"
    ),
    vis_band_cols = c(
      LIFR = "#ffffff",
      IFR = "#ffffff",
      MVFR = "#ffffff",
      VFR = "#ffffff"
    )
  )
}

metargram_prepare_data <- function(dat, attribution, base_size, base_family) {
  dat <- data.table::as.data.table(data.table::copy(dat))

  if(!all(c("time", "icao") %in% names(dat))) {
    stop("`dat` must include at least `time` and `icao` columns.", call. = FALSE)
  }
  if(length(unique(stats::na.omit(dat$icao))) > 1) {
    stop("`plot_metargram_gg()` expects data for a single ICAO station.", call. = FALSE)
  }

  if(!"sigwx" %in% names(dat) && "pw" %in% names(dat)) {
    pw_has_content <- !is.na(dat$pw) & nzchar(trimws(dat$pw))
    if(any(pw_has_content)) {
      pw_parsed <- try(parse_metar_pw(dat$pw), silent = TRUE)
      if(!inherits(pw_parsed, "try-error")) {
        dat <- cbind(dat, pw_parsed)
        dat <- data.table::as.data.table(dat)
      } else {
        dat[, sigwx := NA_character_]
      }
    } else {
      dat[, sigwx := NA_character_]
    }
  }

  data.table::setorderv(dat, "time")
  dat <- unique(dat, by = c("icao", "time"))

  station_cols <- c("ap_name", "ctry", "ctry_name", "elev", "lon", "lat")
  missing_station_cols <- setdiff(station_cols, names(dat))
  if(length(missing_station_cols)) {
    station_lookup <- metar.stn[, c("icao", missing_station_cols), with = FALSE]
    dat <- station_lookup[dat, on = "icao"]
  }

  obs_time <- sort(unique(stats::na.omit(dat$time)))
  step_secs <- if(length(obs_time) > 1) {
    as.numeric(stats::median(diff(obs_time), na.rm = TRUE), units = "secs")
  } else {
    3600
  }
  if(!is.finite(step_secs) || step_secs <= 0) step_secs <- 3600

  full_time <- data.table::data.table(time = fill_time_gaps(dat$time))
  full_time[, icao := dat$icao[1]]
  dat <- dat[full_time, on = c("time", "icao")]

  dat[, time_end := data.table::shift(time, type = "lead")]
  dat[.N, time_end := time + step_secs]

  missing_station_cols <- setdiff(station_cols, names(dat))
  if(length(missing_station_cols)) {
    station_lookup <- metar.stn[, c("icao", missing_station_cols), with = FALSE]
    dat <- station_lookup[dat, on = "icao"]
  }

  if(!"rh" %in% names(dat)) {
    dat[, rh := ifelse(!is.na(tt) & !is.na(td), wx_rh(tt, td), NA_real_)]
  }

  dat[, qnh_tendency_3h := metargram_pressure_tendency(time, qnh, step_secs = step_secs, hours_back = 3)]
  dat[, vis_km := vis / 1e3]
  dat[, vis_km := ifelse(!is.na(vis_km) & vis_km > 0, vis_km, NA_real_)]
  dat[, vvis_hundreds_ft := vvis / 1e2]
  dat[, temp_low := pmin(tt, td, na.rm = TRUE)]
  dat[, temp_high := pmax(tt, td, na.rm = TRUE)]
  dat[!is.finite(temp_low), temp_low := NA_real_]
  dat[!is.finite(temp_high), temp_high := NA_real_]
  dat[, wind_sector := metargram_wind_sector(dir, ff)]
  dat[, wind_fill := unname(metargram_style(base_size, base_family)$wind_cols[wind_sector])]

  time_breaks <- metargram_time_breaks(range(dat$time, na.rm = TRUE))
  time_format <- metargram_time_format(range(dat$time, na.rm = TRUE))
  style <- metargram_style(base_size = base_size, base_family = base_family)

  station <- dat[which.max(!is.na(ap_name))]
  if(nrow(station) == 0L) station <- dat[1]

  latest <- dat[which.max(time)]
  latest_time <- latest$time[1]
  latest_summary <- sprintf(
    "Latest %s UTC | T/Td %s/%s C | Wind %s/%s kt | QNH %s | Vis %s km",
    format(latest_time, "%d %b %Y %H:%M"),
    metargram_fmt_num(latest$tt[1], 1),
    metargram_fmt_num(latest$td[1], 1),
    metargram_fmt_dir_plain(latest$dir[1]),
    metargram_fmt_gust(latest$ff[1], latest$fx[1]),
    metargram_fmt_num(latest$qnh[1], 1),
    metargram_fmt_num(latest$vis_km[1], 1)
  )

  title <- sprintf(
    "%s | %s",
    station$icao[1],
    metargram_value_or(station$ap_name[1], "Unknown airport")
  )
  subtitle <- sprintf(
    "%s | %sm | lon %.2f | lat %.2f\n%s",
    metargram_value_or(station$ctry[1], metargram_value_or(station$ctry_name[1], "NA")),
    metargram_value_or(station$elev[1], "NA"),
    metargram_value_or(station$lon[1], NA_real_),
    metargram_value_or(station$lat[1], NA_real_),
    latest_summary
  )
  caption <- sprintf("METARgram gg | Source: %s", attribution)

  list(
    data = dat,
    step_secs = step_secs,
    time_breaks = time_breaks,
    time_format = time_format,
    style = style,
    metadata = list(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x_min = min(dat$time, na.rm = TRUE),
      x_max = max(dat$time, na.rm = TRUE)
    )
  )
}

metargram_pressure_tendency <- function(time, qnh, step_secs, hours_back = 3) {
  out <- rep(NA_real_, length(time))
  valid <- which(!is.na(time) & !is.na(qnh))
  if(length(valid) < 2) return(out)

  time_num <- as.numeric(time[valid])
  qnh_valid <- qnh[valid]
  target <- time_num - hours_back * 3600
  ref_idx <- findInterval(target, time_num)
  tol <- max(step_secs * 1.5, 1800)

  ok <- ref_idx > 0
  if(any(ok)) {
    age <- target[ok] - time_num[ref_idx[ok]]
    ok2 <- age <= tol
    out_idx <- valid[ok][ok2]
    ref_qnh <- qnh_valid[ref_idx[ok][ok2]]
    out[out_idx] <- qnh[out_idx] - ref_qnh
  }

  out
}

metargram_time_format <- function(time_range) {
  span_hours <- as.numeric(diff(time_range), units = "hours")

  if(!is.finite(span_hours)) return("%Y-%m-%d\n%H:%M")
  if(span_hours <= 18) return("%H:%M")
  if(span_hours <= 72) return("%d %b\n%H:%M")
  if(span_hours <= 24 * 45) return("%d\n%b")
  if(span_hours <= 24 * 550) return("%b\n%Y")
  "%Y"
}

metargram_time_breaks <- function(time_range) {
  span_hours <- as.numeric(diff(time_range), units = "hours")
  if(!is.finite(span_hours) || span_hours <= 0) return(time_range[1])

  by_secs <- if(span_hours <= 18) {
    3 * 3600
  } else if(span_hours <= 36) {
    6 * 3600
  } else if(span_hours <= 72) {
    12 * 3600
  } else if(span_hours <= 24 * 10) {
    24 * 3600
  } else if(span_hours <= 24 * 45) {
    24 * 3600
  } else if(span_hours <= 24 * 120) {
    2 * 24 * 3600
  } else if(span_hours <= 24 * 550) {
    7 * 24 * 3600
  } else {
    30 * 24 * 3600
  }

  from <- metargram_floor_time(time_range[1], by_secs)
  to <- metargram_ceil_time(time_range[2], by_secs)
  seq(from = from, to = to, by = by_secs)
}

metargram_floor_time <- function(x, by_secs) {
  as.POSIXct(floor(as.numeric(x) / by_secs) * by_secs, origin = "1970-01-01", tz = attr(x, "tzone"))
}

metargram_ceil_time <- function(x, by_secs) {
  as.POSIXct(ceiling(as.numeric(x) / by_secs) * by_secs, origin = "1970-01-01", tz = attr(x, "tzone"))
}

metargram_value_or <- function(x, fallback) {
  if(length(x) == 0L || is.na(x)) fallback else x
}

metargram_fmt_num <- function(x, digits = 0) {
  if(length(x) == 0L || is.na(x)) return("NA")
  formatC(x, digits = digits, format = "f")
}

metargram_fmt_gust <- function(ff, fx) {
  ff_txt <- if(is.na(ff)) "NA" else formatC(ff, digits = 0, format = "f")
  if(is.na(fx) || fx <= ff) return(ff_txt)
  sprintf("%sG%s", ff_txt, formatC(fx, digits = 0, format = "f"))
}

metargram_fmt_dir <- function(dir) {
  if(length(dir) == 0L || is.na(dir)) return("NA")
  sprintf("%03d%s", as.integer(round(dir)), metargram_wind_sector(dir, ff = 1))
}

metargram_fmt_dir_plain <- function(dir) {
  if(length(dir) == 0L || is.na(dir)) return("VRB")
  sprintf("%03d", as.integer(round(dir)))
}

metargram_wind_sector <- function(dir, ff) {
  out <- rep(NA_character_, length(dir))
  out[!is.na(ff) & ff == 0] <- "Calm"
  idx <- !is.na(dir) & is.na(out)
  if(any(idx)) {
    labs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    bins <- ((round(dir[idx] / 45) %% 8) + 1)
    out[idx] <- labs[bins]
  }
  out
}

metargram_theme <- function(prep, show_x = TRUE, square = FALSE, panel_fill = NULL, panel_grid = NULL) {
  if(is.null(panel_fill)) panel_fill <- prep$style$col_panel
  if(is.null(panel_grid)) panel_grid <- prep$style$col_grid
  title_element <- if(requireNamespace("ggtext", quietly = TRUE)) {
    ggtext::element_markdown(
      size = prep$style$base_size * 1.04,
      face = "bold",
      colour = prep$style$col_dark,
      margin = ggplot2::margin(b = 3)
    )
  } else {
    ggplot2::element_text(
      size = prep$style$base_size * 1.04,
      face = "bold",
      colour = prep$style$col_dark,
      margin = ggplot2::margin(b = 3)
    )
  }
  out <- ggplot2::theme_minimal(base_size = prep$style$base_size, base_family = prep$style$font_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = prep$style$col_bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = panel_fill, colour = NA),
      panel.border = ggplot2::element_rect(fill = NA, colour = grDevices::adjustcolor(prep$style$col_dark, alpha.f = 0.12), linewidth = 0.7),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = panel_grid, linewidth = 0.4),
      axis.ticks = ggplot2::element_line(colour = "#cccccc", linewidth = 0.3),
      axis.ticks.length = grid::unit(3, "pt"),
      axis.text.x = ggplot2::element_text(colour = prep$style$col_mid, size = prep$style$base_size * 0.72, angle = 0, hjust = 0.5, vjust = 1, lineheight = 0.88),
      axis.text.y = ggplot2::element_text(colour = prep$style$col_mid, size = prep$style$base_size * 0.75),
      axis.text = ggplot2::element_text(colour = prep$style$col_mid),
      axis.title = ggplot2::element_text(colour = prep$style$col_dark, size = prep$style$base_size * 0.85),
      plot.title = title_element,
      plot.subtitle = ggplot2::element_text(
        size = prep$style$base_size * 0.76,
        colour = "#666666",
        lineheight = 1.03,
        margin = ggplot2::margin(b = 4)
      ),
      plot.margin = ggplot2::margin(4, 5, 4, 5),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 3)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 3)),
      legend.position = "none"
    )

  if(!show_x) {
    out <- out + ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  }
  if(square) {
    out <- out + ggplot2::theme(aspect.ratio = 1)
  }

  out
}

metargram_scale_x <- function(prep, show_x) {
  x_limits <- as.POSIXct(
    c(as.numeric(prep$metadata$x_min), as.numeric(prep$metadata$x_max)),
    origin = "1970-01-01",
    tz = attr(prep$metadata$x_min, "tzone")
  )
  ggplot2::scale_x_datetime(
    limits = x_limits,
    breaks = prep$time_breaks,
    labels = if(show_x) {
      function(x) format(x, prep$time_format)
    } else {
      function(x) rep("", length(x))
    },
    expand = ggplot2::expansion(mult = c(0, 0.01))
  )
}

metargram_line_or_point <- function(data, y_col, colour, linewidth = 0.9, point_size = 2.2) {
  n_valid <- sum(!is.na(data[[y_col]]))
  if(n_valid > 1) {
    ggplot2::geom_line(
      data = data,
      ggplot2::aes(x = time, y = !!rlang::sym(y_col), group = 1),
      colour = colour,
      linewidth = linewidth,
      na.rm = TRUE,
      inherit.aes = FALSE
    )
  } else {
    ggplot2::geom_point(
      data = data[!is.na(data[[y_col]])],
      ggplot2::aes(x = time, y = !!rlang::sym(y_col)),
      colour = colour,
      size = point_size,
      na.rm = TRUE,
      inherit.aes = FALSE
    )
  }
}

metargram_step_or_point <- function(data, y_col, colour, linewidth = 0.8, point_size = 2.2) {
  n_valid <- sum(!is.na(data[[y_col]]))
  if(n_valid > 1) {
    ggplot2::geom_step(
      data = data,
      ggplot2::aes(x = time, y = !!rlang::sym(y_col), group = 1),
      direction = "hv",
      colour = colour,
      linewidth = linewidth,
      na.rm = TRUE,
      inherit.aes = FALSE
    )
  } else {
    ggplot2::geom_point(
      data = data[!is.na(data[[y_col]])],
      ggplot2::aes(x = time, y = !!rlang::sym(y_col)),
      colour = colour,
      size = point_size,
      na.rm = TRUE,
      inherit.aes = FALSE
    )
  }
}

metargram_empty_panel <- function(prep, title, subtitle, y_lab, show_x, square, y_limits = c(0, 1)) {
  mid_time <- prep$metadata$x_min + (prep$metadata$x_max - prep$metadata$x_min) / 2

  ggplot2::ggplot(data.frame(time = prep$metadata$x_min, value = 0), ggplot2::aes(time, value)) +
    ggplot2::annotate(
      "text",
      x = mid_time,
      y = mean(y_limits),
      label = "No data available",
      colour = "#777777",
      size = prep$style$base_size / 3.4,
      family = prep$style$font_family
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = y_lab) +
    metargram_scale_x(prep, show_x = show_x) +
    ggplot2::scale_y_continuous(limits = y_limits, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    metargram_theme(prep, show_x = show_x, square = square, panel_fill = prep$style$col_panel)
}

metargram_label_layer <- function(data, x_col, y_col, label_col, colour, prep, colour_col = NULL) {
  if(!nrow(data)) return(NULL)
  data <- data.table::as.data.table(data.table::copy(data))
  data[['.eff_col']] <- if(!is.null(colour_col)) data[[colour_col]] else rep(colour, nrow(data))

  if(requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(
      data = data,
      ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], label = .data[[label_col]], colour = I(.eff_col)),
      family = prep$style$font_family,
      size = prep$style$base_size / 3.05,
      min.segment.length = 0.1,
      seed = 42,
      box.padding = 0.55,
      point.padding = 0.45,
      force = 6,
      force_pull = 0.3,
      direction = "both",
      show.legend = FALSE,
      segment.colour = "#00000044",
      segment.size = 0.3,
      inherit.aes = FALSE
    )
  } else {
    ggplot2::geom_text(
      data = data,
      ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], label = .data[[label_col]], colour = I(.eff_col)),
      family = prep$style$font_family,
      size = prep$style$base_size / 3.4,
      vjust = -0.5,
      show.legend = FALSE,
      inherit.aes = FALSE
    )
  }
}

metargram_marquee_layer <- function(prep, y, labels, colours, x_start = 0.04, x_step = 0.19) {
  if(!length(labels)) return(NULL)
  span_secs <- as.numeric(prep$metadata$x_max) - as.numeric(prep$metadata$x_min)
  if(!is.finite(span_secs) || span_secs <= 0) span_secs <- 3600

  out <- vector("list", length(labels))
  for(i in seq_along(labels)) {
    x_pos <- as.POSIXct(
      as.numeric(prep$metadata$x_min) + span_secs * (x_start + (i - 1) * x_step),
      origin = "1970-01-01",
      tz = attr(prep$metadata$x_min, "tzone")
    )
    out[[i]] <- ggplot2::annotate(
      "label",
      x = x_pos,
      y = y,
      label = labels[[i]],
      family = prep$style$font_family,
      fontface = "bold",
      size = prep$style$base_size / 4.25,
      colour = colours[[i]],
      fill = "#ffffff",
      label.size = 0.25,
      label.padding = grid::unit(0.12, "lines"),
      label.r = grid::unit(0.12, "lines"),
      hjust = 0
    )
  }
  out
}

metargram_panel_overview <- function(prep, show_x, square) {
  dat <- prep$data
  has_panel_data <- any(!is.na(dat$tt) | !is.na(dat$td))
  if(!has_panel_data) {
    return(metargram_empty_panel(
      prep = prep,
      title = "Air Mass",
      subtitle = "Temperature, dew point and humidity spread",
      y_lab = "Temperature [C]",
      show_x = show_x,
      square = square
    ))
  }

  band_dat <- dat[!is.na(temp_low) & !is.na(temp_high),
    .(time, temp_low, temp_high)
  ]
  line_dat <- dat[, .(time, tt, td)]
  valid_y <- c(line_dat$tt, line_dat$td)
  y_lim <- range(valid_y, na.rm = TRUE)
  temp_extrema <- unique(data.table::rbindlist(list(
    dat[!is.na(tt)][which.max(tt), .(time, value = tt, label = sprintf("Tmax %s", metargram_fmt_num(tt, 1)))],
    dat[!is.na(tt)][which.min(tt), .(time, value = tt, label = sprintf("Tmin %s", metargram_fmt_num(tt, 1)))]
  ), fill = TRUE))
  zero_layer <- if(0 >= y_lim[1] && 0 <= y_lim[2]) {
    ggplot2::geom_hline(yintercept = 0, colour = "#bdbdbd", linetype = 2, linewidth = 0.35)
  } else {
    NULL
  }
  ggplot2::ggplot(line_dat, ggplot2::aes(x = time)) +
    zero_layer +
    ggplot2::geom_ribbon(
      data = band_dat,
      ggplot2::aes(ymin = temp_low, ymax = temp_high),
      fill = "#efefef",
      alpha = 1,
      colour = NA
    ) +
    metargram_line_or_point(line_dat, "tt", prep$style$col_temp, linewidth = 1.05) +
    metargram_line_or_point(line_dat, "td", prep$style$col_dew, linewidth = 0.95) +
    ggplot2::geom_point(
      data = dat[!is.na(tt)][.N],
      ggplot2::aes(y = tt),
      size = 2.8,
      shape = 21,
      stroke = 0.7,
      fill = prep$style$col_temp,
      colour = "#ffffff"
    ) +
    ggplot2::geom_point(
      data = dat[!is.na(td)][.N],
      ggplot2::aes(y = td),
      size = 2.8,
      shape = 21,
      stroke = 0.7,
      fill = prep$style$col_dew,
      colour = "#ffffff"
    ) +
    ggplot2::geom_point(
      data = temp_extrema,
      ggplot2::aes(x = time, y = value),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.8,
      stroke = 0.75,
      fill = "#ffffff",
      colour = prep$style$col_dark
    ) +
    metargram_label_layer(temp_extrema, "time", "value", "label", prep$style$col_dark, prep) +
    ggplot2::labs(
      title = sprintf(
        "<b>AIR MASS</b> <span style='color:%s'>T</span> <span style='color:%s'>Td</span> <span style='color:%s'>Spread</span>",
        prep$style$col_temp, prep$style$col_dew, prep$style$col_mid
      ),
      subtitle = NULL,
      x = NULL,
      y = "Temperature [C]"
    ) +
    metargram_scale_x(prep, show_x = show_x) +
    ggplot2::scale_y_continuous(limits = y_lim, expand = ggplot2::expansion(mult = c(0.12, 0.18))) +
    metargram_theme(prep, show_x = show_x, square = square, panel_fill = prep$style$fill_air, panel_grid = "#ededed")
}

metargram_panel_wind <- function(prep, show_x, square) {
  dat <- prep$data
  has_panel_data <- any(!is.na(dat$ff) | !is.na(dat$fx))
  if(!has_panel_data) {
    return(metargram_empty_panel(
      prep = prep,
      title = "Wind Structure",
      subtitle = "Sustained wind, gust envelope",
      y_lab = "Wind [kt]",
      show_x = show_x,
      square = square
    ))
  }

  line_dat <- dat[!is.na(ff) | !is.na(fx), .(time, ff, fx, wind_sector)]
  gust_dat <- dat[!is.na(ff) & !is.na(fx) & fx > ff, .(time, ff, fx)]
  latest <- dat[!is.na(ff) | !is.na(fx)][.N]
  peak_dat <- unique(data.table::rbindlist(list(
    line_dat[!is.na(ff)][which.max(ff), .(time, value = ff, label = sprintf("max ff %s", metargram_fmt_num(ff, 0)), col = prep$style$col_dark)],
    line_dat[!is.na(fx)][which.max(fx), .(time, value = fx, label = sprintf("max fx %s", metargram_fmt_num(fx, 0)), col = prep$style$col_highlight)]
  ), fill = TRUE))
  y_lim <- range(get_range(line_dat$ff, "ff"), get_range(gust_dat$fx, "fx"), na.rm = TRUE)
  if(!all(is.finite(y_lim))) y_lim <- c(0, 40)

  ggplot2::ggplot(line_dat, ggplot2::aes(x = time)) +
    ggplot2::geom_ribbon(
      data = gust_dat,
      ggplot2::aes(x = time, ymin = ff, ymax = fx),
      inherit.aes = FALSE,
      fill = prep$style$col_highlight,
      alpha = 0.12,
      colour = NA
    ) +
    metargram_line_or_point(line_dat, "ff", prep$style$col_dark, linewidth = 1.05, point_size = 2) +
    metargram_line_or_point(line_dat, "fx", prep$style$col_highlight, linewidth = 0.85, point_size = 2) +
    ggplot2::geom_point(
      data = latest,
      ggplot2::aes(y = ff),
      shape = 21,
      size = 2.4,
      stroke = 0.65,
      fill = "#111111",
      colour = "#ffffff"
    ) +
    ggplot2::geom_point(
      data = peak_dat,
      ggplot2::aes(x = time, y = value),
      inherit.aes = FALSE,
      shape = 21,
      size = 3,
      stroke = 0.8,
      fill = "#ffffff",
      colour = peak_dat$col
    ) +
    metargram_label_layer(peak_dat, "time", "value", "label", prep$style$col_dark, prep, colour_col = "col") +
    ggplot2::labs(
      title = sprintf(
        "<b>WIND STRUCTURE</b> <span style='color:%s'>Sustained</span> <span style='color:%s'>Gust</span> <span style='color:%s'>Spread</span>",
        prep$style$col_dark, prep$style$col_highlight, prep$style$col_highlight
      ),
      subtitle = NULL,
      x = NULL,
      y = "Wind [kt]"
    ) +
    metargram_scale_x(prep, show_x = show_x) +
    ggplot2::scale_y_continuous(limits = c(0, y_lim[2]), expand = ggplot2::expansion(mult = c(0, 0.18))) +
    metargram_theme(prep, show_x = show_x, square = square, panel_fill = prep$style$fill_wind, panel_grid = "#ededed")
}

metargram_panel_pressure <- function(prep, show_x, square) {
  dat <- prep$data
  has_panel_data <- any(!is.na(dat$qnh))
  if(!has_panel_data) {
    return(metargram_empty_panel(
      prep = prep,
      title = "Pressure Pulse",
      subtitle = "QNH and 3h tendency",
      y_lab = "Pressure [hPa]",
      show_x = show_x,
      square = square
    ))
  }

  qnh_dat <- dat[!is.na(qnh), .(time, qnh)]
  tend_dat <- dat[!is.na(qnh_tendency_3h), .(time, qnh_tendency_3h)]
  tend_lim <- if(nrow(tend_dat)) max(2, ceiling(max(abs(tend_dat$qnh_tendency_3h), na.rm = TRUE))) else 4
  qnh_lim <- range(c(get_range(qnh_dat$qnh, "qnh"), 1013.25), na.rm = TRUE)
  qnh_span <- diff(qnh_lim)
  if(!is.finite(qnh_span) || qnh_span == 0) qnh_span <- 8

  # Tendency zone sits above the QNH data range (visually separated by a rule)
  tend_zone_height <- qnh_span * 0.30
  tend_zone_lo <- qnh_lim[2]                           # separator line y
  tend_zone_hi <- qnh_lim[2] + tend_zone_height
  base_band    <- tend_zone_lo + tend_zone_height / 2  # centre of tendency zone
  amp_band     <- tend_zone_height * 0.43
  qnh_lim_extended <- c(qnh_lim[1], tend_zone_hi)

  if(nrow(tend_dat)) {
    tend_dat[, y0 := base_band]
    tend_dat[, y1 := base_band + (qnh_tendency_3h / tend_lim) * amp_band]
    tend_dat[, fill_col := ifelse(qnh_tendency_3h >= 0, prep$style$col_highlight, prep$style$col_dark)]
  }
  qnh_extrema <- unique(data.table::rbindlist(list(
    qnh_dat[which.min(qnh), .(time, value = qnh, label = sprintf("min %s", metargram_fmt_num(qnh, 1)))],
    qnh_dat[which.max(qnh), .(time, value = qnh, label = sprintf("max %s", metargram_fmt_num(qnh, 1)))]
  ), fill = TRUE))
  span_secs <- as.numeric(prep$metadata$x_max) - as.numeric(prep$metadata$x_min)
  tick_x0 <- as.POSIXct(as.numeric(prep$metadata$x_min) + span_secs * 0.03, origin = "1970-01-01", tz = attr(prep$metadata$x_min, "tzone"))
  tick_x1 <- as.POSIXct(as.numeric(prep$metadata$x_min) + span_secs * 0.08, origin = "1970-01-01", tz = attr(prep$metadata$x_min, "tzone"))
  tend_scale <- data.table::data.table(
    y = c(base_band + amp_band, base_band, base_band - amp_band),
    label = c(sprintf("+%s", tend_lim), "0", sprintf("-%s", tend_lim))
  )
  tendency_layer <- if(nrow(tend_dat)) {
    ggplot2::geom_segment(
      data = tend_dat,
      ggplot2::aes(x = time, xend = time, y = y0, yend = y1),
      inherit.aes = FALSE,
      linewidth = 3,
      lineend = "butt",
      colour = tend_dat$fill_col,
      alpha = 0.9
    )
  } else {
    NULL
  }
  x_min_pos <- prep$metadata$x_min
  x_max_pos <- prep$metadata$x_max
  ggplot2::ggplot(qnh_dat, ggplot2::aes(x = time, y = qnh)) +
    # Shaded background for tendency zone
    ggplot2::annotate("rect",
      xmin = x_min_pos, xmax = x_max_pos,
      ymin = tend_zone_lo, ymax = tend_zone_hi,
      fill = "#f5f5f5", colour = NA, alpha = 1
    ) +
    # Separator rule between QNH and tendency
    ggplot2::geom_hline(yintercept = tend_zone_lo, colour = "#bbbbbb", linewidth = 0.45) +
    ggplot2::geom_hline(yintercept = 1013.25, colour = "#d9d9d9", linetype = 2, linewidth = 0.35) +
    tendency_layer +
    metargram_line_or_point(qnh_dat, "qnh", prep$style$col_dark, linewidth = 1.1) +
    ggplot2::geom_point(
      data = qnh_extrema,
      ggplot2::aes(x = time, y = value),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.8,
      stroke = 0.75,
      fill = "#ffffff",
      colour = prep$style$col_dark
    ) +
    metargram_label_layer(qnh_extrema, "time", "value", "label", prep$style$col_dark, prep) +
    ggplot2::geom_segment(
      data = tend_scale,
      ggplot2::aes(x = tick_x0, xend = tick_x1, y = y, yend = y),
      inherit.aes = FALSE,
      colour = prep$style$col_mid,
      linewidth = 0.3
    ) +
    ggplot2::geom_text(
      data = tend_scale,
      ggplot2::aes(x = tick_x1, y = y, label = label),
      inherit.aes = FALSE,
      hjust = -0.15,
      family = prep$style$font_family,
      size = prep$style$base_size / 4.5,
      colour = prep$style$col_mid
    ) +
    ggplot2::labs(
      title = sprintf(
        "<b>PRESSURE PULSE</b> <span style='color:%s'>QNH</span> <span style='color:%s'>3h tendency</span>",
        prep$style$col_dark, prep$style$col_highlight
      ),
      subtitle = NULL,
      x = NULL,
      y = "QNH [hPa]"
    ) +
    metargram_scale_x(prep, show_x = show_x) +
    ggplot2::scale_y_continuous(limits = qnh_lim_extended, expand = ggplot2::expansion(mult = c(0.10, 0.02))) +
    metargram_theme(prep, show_x = show_x, square = square, panel_fill = prep$style$fill_pressure, panel_grid = "#ededed")
}

metargram_panel_visibility <- function(prep, show_x, square) {
  dat <- prep$data
  has_panel_data <- any(!is.na(dat$vis_km) | !is.na(dat$vvis_hundreds_ft))
  if(!has_panel_data) {
    return(metargram_empty_panel(
      prep = prep,
      title = "Visibility / Significant Weather",
      subtitle = "Flight rules backdrop with weather stripe",
      y_lab = "Visibility [km]",
      show_x = show_x,
      square = square,
      y_limits = c(0.1, 30)
    ))
  }

  sigwx_lookup <- metar.class[id_para == "sigwx"][1:10, .(id_class, short_class, col)]
  weather_dat <- dat[, .(time, time_end, sigwx)]
  weather_dat[is.na(sigwx) | sigwx == "", sigwx := "NOSIG"]
  sigwx_lookup[, sigwx_label := ifelse(is.na(short_class) | short_class == "", id_class, short_class)]
  sigwx_cols <- c(stats::setNames(sigwx_lookup$col, sigwx_lookup$sigwx_label), "No sigwx" = "#ffffff")
  weather_dat[, sigwx_label := sigwx_lookup$sigwx_label[match(sigwx, sigwx_lookup$id_class)]]
  weather_dat[is.na(sigwx_label), sigwx_label := "No sigwx"]

  vis_dat <- dat[, .(time, vis_km, vvis_hundreds_ft)]
  stripe_ymin <- 16
  stripe_ymax <- 30
  vis_extrema <- vis_dat[!is.na(vis_km)][which.min(vis_km), .(time, value = vis_km, label = sprintf("min %s", metargram_fmt_num(vis_km, 1)))]

  ggplot2::ggplot(vis_dat, ggplot2::aes(x = time)) +
    ggplot2::geom_hline(yintercept = c(0.3, 1, 5, 10), colour = "#e5e5e5", linewidth = 0.45, linetype = c(1, 2, 2, 1)) +
    ggplot2::geom_rect(
      data = weather_dat,
      ggplot2::aes(xmin = time, xmax = time_end, ymin = stripe_ymin, ymax = stripe_ymax, fill = sigwx_label),
      inherit.aes = FALSE,
      colour = NA,
      alpha = 0.95,
      show.legend = TRUE
    ) +
    metargram_step_or_point(vis_dat, "vis_km", prep$style$col_dark, linewidth = 1.15) +
    ggplot2::geom_point(ggplot2::aes(y = vvis_hundreds_ft), colour = prep$style$col_highlight, shape = 15, size = 2.3, alpha = 0.95, na.rm = TRUE) +
    ggplot2::geom_point(
      data = vis_extrema,
      ggplot2::aes(x = time, y = value),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.8,
      stroke = 0.75,
      fill = "#ffffff",
      colour = prep$style$col_dark
    ) +
    metargram_label_layer(vis_extrema, "time", "value", "label", prep$style$col_dark, prep) +
    ggplot2::annotate("text", x = prep$metadata$x_max, y = c(0.35, 1.2, 6, 12), label = c("LIFR", "IFR", "MVFR", "VFR"), hjust = 1.05, family = prep$style$font_family, size = prep$style$base_size / 4.2, colour = prep$style$col_mid) +
    ggplot2::labs(
      title = sprintf(
        "<b>VISIBILITY / WEATHER</b> <span style='color:%s'>Vis</span> <span style='color:%s'>VV</span> <span style='color:%s'>SigWx</span>",
        prep$style$col_dark, prep$style$col_highlight, prep$style$col_mid
      ),
      subtitle = NULL,
      x = NULL,
      y = "Visibility [km]",
      fill = "SigWx"
    ) +
    metargram_scale_x(prep, show_x = show_x) +
    ggplot2::scale_y_log10(
      limits = c(0.1, 30),
      breaks = c(0.1, 0.3, 1, 3, 10, 30),
      labels = c("0.1", "0.3", "1", "3", "10", "30")
    ) +
    ggplot2::scale_fill_manual(
      values = sigwx_cols,
      drop = FALSE,
      guide = ggplot2::guide_legend(
        nrow = 1,
        byrow = TRUE,
        override.aes = list(alpha = 1),
        title.position = "left"
      )
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    metargram_theme(prep, show_x = show_x, square = square, panel_fill = prep$style$fill_visibility, panel_grid = "#ededed") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(6, 10, 6, 8),
      legend.position = "bottom",
      legend.justification = c(0, 0),
      legend.direction = "horizontal",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.spacing.x = grid::unit(2, "pt"),
      legend.text = ggplot2::element_text(size = prep$style$base_size * 0.65, colour = prep$style$col_mid),
      legend.title = ggplot2::element_text(size = prep$style$base_size * 0.7, colour = prep$style$col_dark, face = "bold"),
      legend.key.width = grid::unit(10, "pt"),
      legend.key.height = grid::unit(8, "pt"),
      legend.key.spacing.x = grid::unit(1, "pt")
    )
}
