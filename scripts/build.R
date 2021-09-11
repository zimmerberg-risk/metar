## ------------------------------------- Def --------------------------------------

pkg.name <- "metar"
github <- "m-saaenger"
version <- "0.9.0"

## ------------------------------------- Build --------------------------------------

devtools::load_all()

# Register data sets
usethis:::use_data(
  metar.src,
  internal=FALSE, overwrite=TRUE
)

# Document and check
devtools::document()
devtools::check()
devtools::build_readme()
devtools::build_vignettes()

# Build and install local
p <- devtools::build()
detach(sprintf("package:%s", pkg.name), unload = TRUE, character.only = TRUE)
devtools::install_local(p, force = TRUE, upgrade = "never", build_manual = T, build_vignettes = T)
devtools::build_site()

## ------------------------------------- Install from Github --------------------------------------
expression({

  # Install tagged version
  devtools::install_github(sprintf("%s/%s@%s", github, pkg.name, version))
  # Install latest
  sprintf("devtools::install_github(\"%s/%s\")", github, pkg.name)
  devtools::install_github(sprintf("%s/%s", github, pkg.name))


})



