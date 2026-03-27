pkg.name <- "metar"
github <- "zimmerberg-risk"

# Paths
p <- file.path(dirs$r, pkg.name)
version <- gsub("Version: ", "",grep("^Version:", readLines(file.path(p, "DESCRIPTION")) , value = TRUE))
dest <- file.path(dirs$r, "builds")

# Check in a temp dir
tmpdir <- tempdir()
# system(sprintf("R CMD check --no-manual --no-build-vignettes %s --output=%s", p, tmpdir))
# system(sprintf("R CMD INSTALL %s", p))

# Build and install local
p.zip <- pkgbuild::build(p, dest)
pak::local_install(p, upgrade=FALSE, dependencies=NA, ask=FALSE)