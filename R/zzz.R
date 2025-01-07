### Start up messages and actions to run for use of satf
# Start up message

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Thanks for using asar! Please visit this GitHub Discussions page to offer feedback on the package: https://github.com/nmfs-ost/asar/discussions/categories/feedback-for-v1-0.")
}

# Things to load on start up
# .onLoad <- function(libname, pkgname){
#   # install satf
#   install.packages('remotes')
#   remotes::install_github("nmfs-ost/satf")
# }
