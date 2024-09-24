#' @title
#' Normal-approximation-based test for two-sample problem proposed by Srivastava and Du (2008)
#' @description
#' Srivastava and Du (2008)'s test for testing equality of two-sample high-dimensional mean vectors with assuming that two covariance matrices are the same.

#' @usage SD2008.TS.NABT(y1, y2)
#' @param y1 The data matrix (\eqn{n_1 \times p}) from the first population. Each row represents a \eqn{p}-dimensional observation.
#' @param y2 The data matrix (\eqn{n_2 \times p}) from the second population. Each row represents a \eqn{p}-dimensional observation.
#'
#' @details
#' Suppose we have two independent high-dimensional samples:
#' \deqn{
#' \boldsymbol{y}_{i1},\ldots,\boldsymbol{y}_{in_i}, \;\operatorname{are \; i.i.d. \; with}\; \operatorname{E}(\boldsymbol{y}_{i1})=\boldsymbol{\mu}_i,\; \operatorname{Cov}(\boldsymbol{y}_{i1})=\boldsymbol{\Sigma},i=1,2.
#' }
#' The primary object is to test
#' \deqn{H_{0}: \boldsymbol{\mu}_1 = \boldsymbol{\mu}_2\; \operatorname{versus}\; H_{1}: \boldsymbol{\mu}_1 \neq \boldsymbol{\mu}_2.}
#' Srivastava and Du (2008) proposed the following test statistic:
#'  \deqn{T_{SD} = \frac{n^{-1}n_1n_2(\bar{\boldsymbol{y}}_1 - \bar{\boldsymbol{y}}_2)^\top \boldsymbol{D}_S^{-1}(\bar{\boldsymbol{y}}_1 - \bar{\boldsymbol{y}}_2) - \frac{(n-2)p}{n-4}}{\sqrt{2 \left[\operatorname{tr}(\boldsymbol{R}^2) - \frac{p^2}{n-2}\right] c_{p, n}}},}
#' where \eqn{\bar{\boldsymbol{y}}_{i},i=1,2} are the sample mean vectors,  \eqn{\boldsymbol{D}_S} is the diagonal matrix of sample variance, \eqn{\boldsymbol{R}} is the sample correlation matrix and \eqn{c_{p, n}} is the adjustment coefficient proposed by Srivastava and Du (2008).

#' They showed that under the null hypothesis, \eqn{T_{SD}} is asymptotically normally distributed.

#'
#' @references
#' \insertRef{Srivastava_2008}{HDNRA}
#'
#' @return A list of class \code{"NRtest"} containing the results of the hypothesis test. See the help file for \code{\link{NRtest.object}} for details.


#' @examples
#' library("HDNRA")
#' data("COVID19")
#' dim(COVID19)
#' group1 <- as.matrix(COVID19[c(2:19, 82:87), ]) ## healthy group
#' group2 <- as.matrix(COVID19[-c(1:19, 82:87), ]) ## COVID-19 patients
#' SD2008.TS.NABT(group1,group2)
#'

#'
#' @concept ts
#' @export
SD2008.TS.NABT <- function(y1, y2) {
  if (ncol(y1) != ncol(y2)) {
    stop("y1 and y2 must have the same dimension!")
  }

  # Calculate test statistics using the provided C++ function
  stats <- sd2008_ts_nabt_cpp(y1, y2)
  stat <- stats[1]
  cpn <- stats[2] # Assuming cpn represents some additional parameters

  # Calculate p-value
  pvalue <- pnorm(stat, 0, 1, lower.tail = FALSE, log.p = FALSE)

  # Prepare the result as an NRtest object using consistent naming style
  hname <- paste("Srivastava and Du (2008)'s test", sep = "")
  hname1 <- paste("Normal approximation", sep = "")

  null.value  <- "0"
  attr(null.value, "names") <- "Difference between two mean vectors"
  alternative <- "two.sided"

  out <- list(
    statistic = c("T[SD]" = round(stat,4)),
    parameter = c("Adjustment coefficient" = round(cpn,4)), # Include additional parameters as needed
    p.value = pvalue,
    method = hname,
    estimation.method = hname1,
    data.name = paste(deparse(substitute(y1)), " and ", deparse(substitute(y2)), sep = ""),
    null.value = null.value,
    sample.size = c(n1 = nrow(y1), n2 = nrow(y2)),
    sample.dimension = ncol(y1),
    alternative = alternative
  )

  class(out) <- "NRtest"
  return(out)
}

