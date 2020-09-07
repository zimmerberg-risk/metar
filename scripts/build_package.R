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

# before checks/install
devtools::document()
#devtools::load_all()
devtools::check()
devtools::build()

# https://github.com/m-saenger/parseDate
devtools::install_github("m-saenger/parseDate")
