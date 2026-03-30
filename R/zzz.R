.onLoad <- function(libname, pkgname) {
  op <- options()

  op.dataforge <- list(
    dataforge.verbose = TRUE,
    dataforge.plot    = TRUE,
    dataforge.long    = FALSE,
    dataforge.seed    = NULL,
    dataforge.sep     = "_"
  )

  toset <- !(names(op.dataforge) %in% names(op))
  if (any(toset)) options(op.dataforge[toset])

  invisible()
}

.opt <- function(name) {
  getOption(paste0("dataforge.", name))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "\n************",
    "Welcome to dataforge.",
    "https://github.com/Sagars314/dataforge",
    "- Get and set global package options with: dataforge_options()",
    "************",
    sep = "\n"
  ))
}

