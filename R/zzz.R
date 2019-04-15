#.onAttach <- function(libname, pkgname) {
#  packageStartupMessage("Package SocEpi depends on dplyr and tidyr")
#}

#.onLoad <- function(libname, pkgname) {
#  data("R/sysdata.rda", package=pkgname, envir=parent.env(environment()))
#}

.onUnload <- function(libpath) {
  library.dynam.unload("SocEpi", libpath)
}
