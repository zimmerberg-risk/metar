
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metar

The goal of **metar** is to provide an R-built parser for METAR reports.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools") 
devtools::install_github("m-saenger/metar")
```

This package depends on **data.table**, **stringr**, **readr** and
**lubridate**.

## Example

### Data retrieval and parsing

``` r
library(metar)

## read raw data from Mesonet website
dat <- read_mesonet("LSZH", as.POSIXct("2021-03-15"), as.POSIXct("2021-03-28"))

## parse METAR code
dat.parsed <- parse_metar(dat$metar, date = dat$valid)

# Structure  (subset of columns)
print(dat.parsed[1:10, .(icao, metar, tt, ff, qnh)])
#>     icao                                                              metar tt
#>  1: LSZH                     25005KT 220V280 9999 FEW008 BKN012 02/01 Q1017  2
#>  2: LSZH                             VRB02KT 9999 FEW008 BKN010 02/01 Q1017  2
#>  3: LSZH 19003KT 120V230 9999 -SHSNRA FEW007 SCT011 BKN016 02/01 Q1017 RESN  2
#>  4: LSZH              VRB03KT 9999 -SHSNRA FEW007 SCT012 BKN017 02/01 Q1016  2
#>  5: LSZH         21005KT 9999 -SHSNRA FEW011 SCT014 BKN019 02/01 Q1016 RESN  2
#>  6: LSZH              22007KT 9999 -SHSNRA FEW009 SCT015 BKN060 02/01 Q1015  2
#>  7: LSZH                    20006KT 9999 FEW010 BKN013 02/01 Q1015 RESHSNRA  2
#>  8: LSZH         22006KT 9999 -SHSNRA FEW009 SCT012 BKN017 03/01 Q1015 RESN  3
#>  9: LSZH                     22007KT 9999 -SHSNRA FEW009 BKN012 02/01 Q1014  2
#> 10: LSZH          22006KT 180V280 6000 -RASN BKN010 BKN015 02/01 Q1014 RESN  2
#>     ff  qnh
#>  1:  5 1017
#>  2:  2 1017
#>  3:  3 1017
#>  4:  3 1016
#>  5:  5 1016
#>  6:  7 1015
#>  7:  6 1015
#>  8:  6 1015
#>  9:  7 1014
#> 10:  6 1014
```

### Example plot

``` r
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.0.4
ggplot(dat.parsed, aes(time, tt)) +
  geom_path() +
  labs(title = "Recent Temperature at LSZH Airport") +
  theme_bw()
```

<img src="man/figures/README-plot1-1.png" width="100%" />

### Data processing (cloud groups)

``` r
# Process cloud groups (1-4)
dat.cld <- metar_clouds(dat.parsed$cl)    
dat.cld$time <- dat.parsed$time

# Structure (subset of columns)
print(dat.cld[1:10, .(time, cld_levels, cld_str_1 , cld_amt_1, cld_hgt_1, cld_type_1 )])
#>                    time cld_levels cld_str_1 cld_amt_1 cld_hgt_1 cld_type_1
#>  1: 2021-03-15 00:50:00          2    FEW008       FEW       800       <NA>
#>  2: 2021-03-15 01:20:00          2    FEW008       FEW       800       <NA>
#>  3: 2021-03-15 01:50:00          3    FEW007       FEW       700       <NA>
#>  4: 2021-03-15 02:20:00          3    FEW007       FEW       700       <NA>
#>  5: 2021-03-15 02:50:00          3    FEW011       FEW      1100       <NA>
#>  6: 2021-03-15 03:20:00          3    FEW009       FEW       900       <NA>
#>  7: 2021-03-15 03:50:00          2    FEW010       FEW      1000       <NA>
#>  8: 2021-03-15 04:20:00          3    FEW009       FEW       900       <NA>
#>  9: 2021-03-15 04:50:00          2    FEW009       FEW       900       <NA>
#> 10: 2021-03-15 05:20:00          2    BKN010       BKN      1000       <NA>

ggplot(dat.cld, aes(time, cld_hgt_1)) +
  geom_path() +
  labs(title = "Altitude of Lowest Cloud Level at LSZH Airport") +
  theme_bw()
```

<img src="man/figures/README-plot2-1.png" width="100%" />

### METARgram - Timeseries

``` r
# Parse present weather group
dat.pw <- dat.parsed[, metar_pw(pw)]

# Structure (subset of columns)
print(dat.pw[1:10, .(pw_grp_1, pw_grp_2, is_pp, RA, SN, DZ, is_solid, is_liquid, is_mixed)])
#>     pw_grp_1 pw_grp_2 is_pp RA SN DZ is_solid is_liquid is_mixed
#>  1:     <NA>     <NA>     0  0  0  0        0         0        0
#>  2:     <NA>     <NA>     0  0  0  0        0         0        0
#>  3:  -SHSNRA     <NA>     1  1  1  0        1         1        1
#>  4:  -SHSNRA     <NA>     1  1  1  0        1         1        1
#>  5:  -SHSNRA     <NA>     1  1  1  0        1         1        1
#>  6:  -SHSNRA     <NA>     1  1  1  0        1         1        1
#>  7:     <NA>     <NA>     0  0  0  0        0         0        0
#>  8:  -SHSNRA     <NA>     1  1  1  0        1         1        1
#>  9:  -SHSNRA     <NA>     1  1  1  0        1         1        1
#> 10:    -RASN     <NA>     1  1  1  0        1         1        1

# Bind data
dat.plot <- cbind(dat.parsed, dat.pw)

plot_metargram(dat = dat.plot, cex = 0.7)
```

<img src="man/figures/README-plot3-1.png" width="100%" />
