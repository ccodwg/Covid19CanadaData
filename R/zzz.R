covid_ds_env <- NULL
.onLoad <- function(libname, pkgname)
{
  # create environment for package objects
  covid_ds_env <<- new.env(parent = emptyenv())

  # prevent R CMD check from complaining about "."
  utils::globalVariables(".")
}
