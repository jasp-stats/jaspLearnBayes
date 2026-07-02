.onLoad <- function(libname, pkgname) {
  if (isTRUE(try(jaspBase::jaspResultsCalledFromJasp()))) {
    jagsHome <- Sys.getenv("JAGS_HOME")
    if (jagsHome != "") {
      if (jaspBase:::getOS() == "osx") {
        options(jags.moddir = file.path(jagsHome, "modules-4"))
      }
      jagspath <- if (jaspBase:::getOS() == "win") file.path(jagsHome, "x64") else jagsHome
      runjags::runjags.options(jagspath = jagspath)
    }
  }
}
