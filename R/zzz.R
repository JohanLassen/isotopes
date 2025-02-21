

.onLoad <- function(libname, pkgname) {
  options(shiny.maxRequestSize = 500 * 1024^2)
}
