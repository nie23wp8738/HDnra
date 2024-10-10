#' @importFrom utils packageVersion

.pkgenv <- new.env(parent = emptyenv())

.onAttach <- function(...) {
  ## Retrieve Year Information
  date <- date()
  x <- regexpr("[0-9]{4}", date)
  this.year <- substr(date, x[1], x[1] + attr(x, "match.length") - 1)

  ## Retrieve Current Version
  this.version <- packageVersion("HDNRA")

  ## Print on Screen
  packageStartupMessage("**------------------------------------------------------**")
  packageStartupMessage("** HHH   HHH  DDDDDDDD  NNNN    NN  RRRRRR     AAAA     **")
  packageStartupMessage("** HHH   HHH  DD    DD  NNNNN   NN  RR   RR  AA    AA   **")
  packageStartupMessage("** HHHHHHHHH  DD    DD  NN  NN  NN  RRRRRR   AAAAAAAA   **")
  packageStartupMessage("** HHH   HHH  DD    DD  NN   NN NN  RR  RR  AA      AA  **")
  packageStartupMessage("** HHH   HHH  DDDDDDDD  NN    NNNN  RR   RR AA      AA  **")
  packageStartupMessage("** ")
  packageStartupMessage("**     High-Dimensional Location Testing Toolbox")
  packageStartupMessage("**")
  packageStartupMessage("** Version   :", this.version, "      (", this.year, ")", sep = "")
  packageStartupMessage("** Authors   :Pengfei Wang,Shuqi Luo,Tianming Zhu,Bu Zhou")
  packageStartupMessage("** Maintainer:Pengfei Wang (nie23.wp8738@e.ntu.edu.sg)")
  packageStartupMessage("**")
  packageStartupMessage("** This package provides a comprehensive set of tools for")
  packageStartupMessage("** high-dimensional location testing, including classical")
  packageStartupMessage("** and state-of-the-art normal-reference approaches for")
  packageStartupMessage("** two-sample and general linear hypothesis testing (GLHT).")
  packageStartupMessage("**")
  packageStartupMessage("** Please report any bugs or suggestions to the maintainer.")
  packageStartupMessage("**------------------------------------------------------**")
}

.onUnload <- function(libpath) {
  library.dynam.unload("HDNRA", libpath)
}
