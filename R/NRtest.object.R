#' @title
#' S3 Class "NRtest"

#' @details
#'
#' A class of objects returned by high-dimensional hypothesis testing functions in the \pkg{HDNRA} package,
#' designed to encapsulate detailed results from statistical hypothesis tests.
#' These objects are structured similarly to \pkg{htest} objects in the package \pkg{EnvStats} but are tailored
#' to the needs of the \pkg{HDNRA} package.
#'
#' @description
#' The \code{"NRtest"} objects provide a comprehensive summary of hypothesis test outcomes,
#' including test statistics, p-values, parameter estimates, and confidence intervals, if applicable.
#'
#' @return An object of class \code{"NRtest"} containing both required and optional components depending on the specifics of the hypothesis test,
#' shown as follows:
#'
#' @section Required Components:
#' These components must be present in every \code{"NRtest"} object:
#' \describe{
#'   \item{\code{statistic}}{numeric scalar containing the value of the test statistic, with a \code{names} attribute indicating the name of the test statistic.}
#'   \item{\code{p.value}}{numeric scalar containing the p-value for the test.}
#'   \item{\code{null.value}}{character string indicating the null hypothesis.}
#'   \item{\code{alternative}}{character string indicating the alternative hypothesis.}
#'   \item{\code{method}}{character string giving the name of the test.}
#' }
#'
#' @section Optional Components:
#' These components are included depending on the specifics of the hypothesis test performed:
#' \describe{
#'   \item{\code{parameter}}{numeric vector containing the estimated approximation parameter(s) associated with the approximation method.  This vector has a \code{names} attribute describing its element(s).}
#'   \item{\code{sample.size}}{numeric vector containing the number of observations in each group used for the hypothesis test.}
#'   \item{\code{sample.dimension}}{numeric scalar containing the dimension of the dataset used for the hypothesis test.}
#'   \item{\code{estimation.method}}{character string giving the name of approximation approach used to approximate the null distribution of the test statstic.}
#'   \item{\code{data.name}}{character string describing the data set used in the hypothesis test.}
#' }
#'

#' @section Methods:
#' The class has the following methods:
#' \itemize{
#'   \item{\code{\link{print.NRtest}}} {Printing the contents of the NRtest object in a human-readable form.}
#' }

#' @examples
#' # Example 1: Using Bai and Saranadasa (1996)'s test (two-sample problem)
#' NRtest.obj1 <- NRtest.object(
#'   statistic = c("T[BS]" = 2.208),
#'   p.value = 0.0136,
#'   method = "Bai and Saranadasa (1996)'s test",
#'   data.name = "group1 and group2",
#'   null.value = c("Difference between two mean vectors is o"),
#'   alternative = "Difference between two mean vectors is not 0",
#'   parameter = NULL,
#'   sample.size = c(n1 = 24, n2 = 26),
#'   sample.dimension = 20460,
#'   estimation.method = "Normal approximation"
#' )
#' print(NRtest.obj1)
#'
#' # Example 2: Using Fujikoshi et al. (2004)'s test (GLHT problem)
#' NRtest.obj2 <- NRtest.object(
#'   statistic = c("T[FHW]" = 6.4015),
#'   p.value = 0,
#'   method = "Fujikoshi et al. (2004)'s test",
#'   data.name = "Y",
#'   null.value  = "The general linear hypothesis is true",
#'   alternative = "The general linear hypothesis is not true",
#'   parameter = NULL,
#'   sample.size = c(n1 = 43, n2 = 14, n3 = 21, n4 = 72),
#'   sample.dimension = 2000,
#'   estimation.method = "Normal approximation"
#' )
#' print(NRtest.obj2)
#'
#' @concept object
#'
#' @export
NRtest.object <- function(statistic,
                          p.value,
                          method,
                          null.value,
                          alternative,
                          parameter=NULL,
                          sample.size = NULL,
                          sample.dimension = NULL,
                          estimation.method = NULL,
                          data.name=NULL,...)  {
  structure(
    list(
      statistic = statistic,
      p.value = p.value,
      null.value = null.value,
      alternative = alternative,
      method = method,
      parameter = parameter,
      sample.size = sample.size,
      sample.dimension = sample.dimension,
      estimation.method = estimation.method,
      data.name = data.name,
      ...
    ),
    class = "NRtest"
  )
}
