## metar 0.9.0
* Complete rebuild
* File reading based on `vroom` package
* Performance improvements: Parse functions almost completely based on vectorised functions


## metar 0.4.0

* Station meta data available within package: `metar.stn`. Column `active` indicates whether station reports on regular basis (Source: http://www.weathergraphics.com/identifiers/)
* All meta data is available as `data.table` objects
* Performance improvements: `metar_parse`, `metar_pw`
* `metar_parse` returns station meta data
* `metar_parse` splits up PW string into PW (Current), RE (Recent) and VC (Vicinity)
* `metar_pw` returns more significant weather classes: see `metar.class` meta data
* `plot_metargram` is able to handle data gaps
* `metar_validation` performs simple validation checks (e. g. dismisses dew point > temperature) 

## metar 0.3.1
* Intermediate update
