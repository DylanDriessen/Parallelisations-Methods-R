import <- function(packages) {
  for (p in packages) {
    if(!require(p, character.only = TRUE)){
      print(paste0("Package ", p, " is not installed."))
      install.packages(p)
      require(p, character.only = TRUE)
    } else {
      print(paste0("Package ", p, " is installed."))
    }
  }
}