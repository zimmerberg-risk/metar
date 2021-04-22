#' METAR Data Sources
#'
#' @docType data
#' @keywords dataset
#' @export
#'
metar.src <- list(
  mesonet = "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py",
  stn = "https://www.aviationweather.gov/docs/metar/stations.txt",
  noaa = "https://tgftp.nws.noaa.gov/data/observations/metar/cycles/%02dZ.TXT"
)

#' Station meta data
#'
#' @author http://www.weathergraphics.com/identifiers/
#'
"metar.stn"

#' Present Weather Descriptor
#'
"metar.dc"

#' Present Weather Phenomena
#'
"metar.ph"

#' Present Weather Phenomena Group
#'
"metar.ph_grp"

#' Present Weather Phenomena Sub-Group
#'
"metar.ph_subgrp"

#' Present Weather Phenomena List
#'
"metar.ph_list"

#' Present Weather Intensity
#'
"metar.int"

#' METAR Parameters
#'
"metar.para"

#' METAR Parameter Conversion
#'
"metar.para_conv"

#' METAR Parameter Classes
#'
"metar.class"

#' METAR Test Data Set
#'
"metar.test"
