library(devtools)
library(roxygen2)
library(usethis)
library(available)


#' https://usethis.r-lib.org/articles/articles/usethis-setup.html
#' https://happygitwithr.com/ssh-keys.html
#'
package.name <- "metar"

available(package.name)
create_package(sprintf("C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/R/%s", package.name))

usethis::use_git_config(user.name = "m-saenger", user.email = "matthias.saenger@myweather.ch")
usethis::git_sitrep()
usethis::git_vaccinate()

# usethis::use_github(private = T, credentials = git2r::cred_ssh_key(), auth_token = usethis::github_token())
# edit_r_environ()

use_readme_rmd()

# once
use_r(package.name)
use_test(package.name)
usethis::use_ccby_license(name = "Zimmerberg Risk Analytics GmbH")
usethis::use_namespace(roxygen = TRUE)


# before checks/install

usethis:::use_data(
  #metar.groups,
  metar.vars,
  metar.vars.pw,
  metar.vars.cld,
  metar.vars.rvr,
  metar.test,
  pw.dc,
  pw.ph,
  metar.ph,
  cld.amt,
  internal=FALSE, overwrite=TRUE
)
usethis::use_data_table()
usethis::use_pipe()
usethis::use_package("data.table")
usethis::use_package("purrr")
usethis::use_package("readr")
usethis::use_package("stringr")

devtools::document()
devtools::check()

p <- devtools::build()

# https://github.com/m-saenger/metar
devtools::install_local(p, force = TRUE, upgrade = "never")
devtools::install_github("m-saenger/metar", upgrade = "never")
