.onAttach <- function(libname, pkgname) {
  packageStartupMessage("enter mldrGUI() to launch mldr's web-based GUI")
}

.onLoad <- function(libname, pkgname) {
  options(shiny.error=FALSE)
  options(shiny.try = FALSE)
}
