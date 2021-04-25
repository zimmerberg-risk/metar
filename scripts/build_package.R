library(devtools)
library(roxygen2)
library(usethis)
library(available)


#' https://usethis.r-lib.org/articles/articles/usethis-setup.html
#' https://happygitwithr.com/ssh-keys.html

package.name <- "metar"

available(package.name)
create_package(sprintf("C:/Users/mat/OneDrive - Zimmerberg Risk Analytics GmbH/R/%s", package.name))

usethis::use_git_config(user.name = "m-saenger", user.email = "matthias.saenger@myweather.ch")
usethis::git_sitrep()
usethis::git_vaccinate()

# usethis::use_github(private = T, credentials = git2r::cred_ssh_key(), auth_token = usethis::github_token())
# edit_r_environ()

usethis::use_readme_rmd()

# once
use_r(package.name)
use_test(package.name)
usethis::use_ccby_license()
usethis::use_namespace(roxygen = TRUE)
usethis::use_news_md()

# before checks/install
usethis::use_data_table()


devtools::load_all(reset = TRUE)
usethis:::use_data(
  metar.src,
  internal=FALSE, overwrite=TRUE
)
devtools::document()
devtools::check()
# devtools::build_readme()
# devtools::build_vignettes()

detach("package:metar", unload = TRUE)
remove.packages("metar")
p <- devtools::build()
devtools::install_local(p, force = TRUE, upgrade = "never")
library(metar)

# https://github.com/m-saenger/metar
devtools::install_github("m-saenger/metar", upgrade = "never")

# git tag 0.3.1
# git push --tags
# git tags


