## ------------------------------------- Def --------------------------------------

pkg.name <- "metar"
github <- "zimmerberg-risk"
version <- "0.9.1"

## ------------------------------------- Build --------------------------------------

devtools::load_all()

# Register data sets
usethis:::use_data(
  metar.src,
  internal=FALSE, overwrite=TRUE
)

# Document and check
devtools::document()
# devtools::check()
# devtools::build_readme()
# devtools::build_vignettes()

# Build and install local
p <- devtools::build(vignettes = F)
detach(name = sprintf("package:%s", pkg.name), character.only = TRUE,  unload = TRUE)
devtools::install_local(p, force = TRUE, upgrade = "never", build_manual = T, build_vignettes = F)

stop("OK")
devtools::build_site()

## ------------------------------------- Create tag/release --------------------------------------
# List
system(sprintf("git tag -l"), intern = T)

# Create
system(sprintf("git tag %s", version), intern = F)

# Push
system(sprintf("git push origin %s", version), intern=T)


## ------------------------------------- Install from Github --------------------------------------
expression({

  # Install tagged version
  devtools::install_github(sprintf("%s/%s@%s", github, pkg.name, version))
  # Install latest
  sprintf("devtools::install_github(\"%s/%s\")", github, pkg.name)
  devtools::install_github(sprintf("%s/%s", github, pkg.name))


})



