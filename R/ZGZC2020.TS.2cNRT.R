#' @title
#' Normal-reference-test with two-cumulant (2-c) matched $\\chi^2$-approximation for two-sample problem proposed by Zhang et al. (2020)
#' @description
#' Zhang et al. (2020)'s test for testing equality of two-sample high-dimensional mean vectors with assuming that two covariance matrices are the same.

#' @usage ZGZC2020.TS.2cNRT(y1, y2)
#' @param y1 The data matrix (\eqn{n_1 \times p}) from the first population. Each row represents a \eqn{p}-dimensional observation.
#' @param y2 The data matrix (\eqn{n_2 \times p}) from the second population. Each row represents a \eqn{p}-dimensional observation.
#' @details
#'  Suppose we have two independent high-dimensional samples:
#' \deqn{
#' \boldsymbol{y}_{i1},\ldots,\boldsymbol{y}_{in_i}, \;\operatorname{are \; i.i.d. \; with}\; \operatorname{E}(\boldsymbol{y}_{i1})=\boldsymbol{\mu}_i,\; \operatorname{Cov}(\boldsymbol{y}_{i1})=\boldsymbol{\Sigma},i=1,2.
#' }
#' The primary object is to test
#' \deqn{H_{0}: \boldsymbol{\mu}_1 = \boldsymbol{\mu}_2\; \operatorname{versus}\; H_{1}: \boldsymbol{\mu}_1 \neq \boldsymbol{\mu}_2.}
#' Zhang et al.(2020) proposed the following test statistic:
#' \deqn{T_{ZGZC} = \frac{n_1n_2}{n} \|\bar{\boldsymbol{y}}_1 - \bar{\boldsymbol{y}}_2\|^2,}
#' where  \eqn{\bar{\boldsymbol{y}}_{i},i=1,2} are the sample mean vectors.
#' They showed that under the null hypothesis, \eqn{T_{ZGZC}} and a chi-squared-type mixture have the same normal or non-normal limiting distribution.



#' @references
#' \insertRef{zhang2020simple}{HDNRA}
#'
#' @return A list of class \code{"NRtest"} containing the results of the hypothesis test. See the help file for \code{\link{NRtest.object}} for details.
#'
#' @examples
#' library("HDNRA")
#' data("COVID19")
#' dim(COVID19)
#' group1 <- as.matrix(COVID19[c(2:19, 82:87), ]) ## healthy group
#' group2 <- as.matrix(COVID19[-c(1:19, 82:87), ]) ## COVID-19 patients
#' ZGZC2020.TS.2cNRT(group1, group2)

#'
#' @concept nrats
#' @export
ZGZC2020.TS.2cNRT <- function(y1, y2) {
  if (ncol(y1) != ncol(y2)) {
    stop("y1 and y2 must have the same dimension!")
  }

  # Calculate test statistics using the provided C++ function
  stats <- zgzc2020_ts_2cnrt_cpp(y1, y2)
  stat <- stats[1]
  statn <- stats[2]
  beta <- stats[3]
  df <- stats[4]

  # Calculate p-value
  pvalue <- pchisq(q = statn, df = df, ncp = 0, lower.tail = FALSE, log.p = FALSE)

  # Prepare the result as an NRtest object
  hname <- paste("Zhang et al. (2020)'s test", sep = "")
  hname1 <- paste("2-c matched chi^2-approximation", sep = "")

  null.value  <- "0"
  attr(null.value, "names") <- "Difference between two mean vectors"
  alternative <- "two.sided"

  out <- list(
    statistic = c("T[ZGZC]" = round(stat,4)),
    parameter = c("df" = round(df,4), "beta" = round(beta,4)), # Include additional parameters as needed
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
