# --------------------------------------------------- Formulas --------------------------------------------


#' Vapour pressure saturated (wrapper function)
#'
#' @author M. Saenger
#' @description Vapour pressure saturated (wrapper function)
#' ECMWF IFS Documentation – Cy41r2, Part IV, formula 7.5
#' @param tt °C
#' @param ice true/false
#' @export
#' @examples wx_es(30, TRUE)
#'
wx_es <- function(tt, ice = FALSE){
  if(ice){
    wx_es_i(tt)
  } else {
    wx_es_w(tt)
  }
}

#' Vapour pressure saturated (over water)
#'
#' @author M. Saenger
#' @description ECMWF IFS Documentation – Cy41r2, Part IV, formula 7.5
#' @param tt °C
#' @export
#' @examples wx_es_w(30)
#'
wx_es_w <- function(tt){
  tt <- tt + 273.16
  es <- 611.21 * exp(17.502 * ((tt - 273.16)/(tt + 32.19)))
  es / 100
}

#' Vapour pressure saturated (over ice)
#'
#' @author M. Saenger
#' @description ECMWF IFS Documentation – Cy41r2, Part IV, formula 7.5
#' @param tt °C
#' @export
#' @examples wx_es_i(-30)
#'
wx_es_i <- function(tt){
  tt <- tt + 273.16
  es <- 611.21 * exp(22.587 * ((tt - 273.16)/(tt - 0.7)))
  es / 100
}

#' Relative humidity
#'
#' @author M. Saenger
#' @param tt °C Air temperature
#' @param td °C Dew point
#' @param ice over ice
#' @export
#' @examples
#' wx_rh(5, -35)
#'
wx_rh <- function(tt, td, ice = FALSE){
  100 * wx_es(td, ice)/wx_es(tt, ice)
}

