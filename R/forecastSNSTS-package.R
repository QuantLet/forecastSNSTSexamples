#' @include forecastSNSTS-package.R
NULL

################################################################################
#' Data examples for the R package 'forecastSNSTS'
#'
#' Three demos that replicate the data analysis examples from Section 5 of
#' Kley et al (2017).
#'
#' @details
#'  \tabular{ll}{
#'    \cr Package: \tab forecastSNSTSexamples
#'    \cr Type:    \tab Package
#'    \cr Version: \tab 1.1-0
#'    \cr Date:    \tab 2017-01-18
#'    \cr License: \tab GPL (>= 2) (for R code),
#'    \cr          \tab Open Government License (for data set)
#'  }
#'
#' @name forecastSNSTSexamples-package
#' @aliases forecastSNSTSexamples
#' @docType package
#' @author Tobias Kley
#' 
#' @import forecastSNSTS
#'
#' @references
#' Kley, T., Preuss, P. & Fryzlewicz, P. (2017).
#' Predictive, finite-sample model choice for time series under stationarity and non-stationarity.
#' [cf. \url{https://arxiv.org/abs/1611.04460}]
#'
NULL

# Taken from quantreg-package and adapted.
".onAttach" <- function(lib, pkg) {
  if(interactive() || getOption("verbose"))
    packageStartupMessage("Package forecastSNSTSexamples loaded.\n     To cite, see citation(\"forecastSNSTSexamples\").\n     For demos, see demo(package = \"forecastSNSTSexamples\").")
}
