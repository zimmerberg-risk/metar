% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process.R
\name{parse_metar_cld}
\alias{parse_metar_cld}
\title{Parse cloud groups}
\usage{
parse_metar_cld(x)
}
\arguments{
\item{x}{cloud string}
}
\description{
Parse cloud groups
}
\examples{
x <- c("SCT009 BKN029 OVC045CB", "SCT009CB OVC045", "NSC")
parse_metar_cld(x)
m <- parse_metar(metar.test$code)
parse_metar_cld(x = m$cld)

}
\author{
M. Saenger, Zimmerberg Risk Analytics GmbH
}
