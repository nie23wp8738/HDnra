#' @title
#' Normal-approximation-based test for two-sample BF problem proposed by Chen and Qin (2010)
#' @description
#' Chen and Qin (2010)'s test for testing equality of two-sample high-dimensional mean vectors without assuming that two covariance matrices are the same.
#'
#' @usage CQ2010.TSBF.NABT(y1, y2)
#' @param y1 The data matrix (\eqn{n_1 \times p}) from the first population. Each row represents a \eqn{p}-dimensional observation.
#' @param y2 The data matrix (\eqn{n_2 \times p}) from the second population. Each row represents a \eqn{p}-dimensional observation.
#'
#' @details
#' Suppose we have two independent high-dimensional samples:
#' \deqn{
#' \boldsymbol{y}_{i1},\ldots,\boldsymbol{y}_{in_i}, \;\operatorname{are \; i.i.d. \; with}\; \operatorname{E}(\boldsymbol{y}_{i1})=\boldsymbol{\mu}_i,\; \operatorname{Cov}(\boldsymbol{y}_{i1})=\boldsymbol{\Sigma}_i,i=1,2.
#' }
#' The primary object is to test
#' \deqn{H_{0}: \boldsymbol{\mu}_1 = \boldsymbol{\mu}_2\; \operatorname{versus}\; H_{1}: \boldsymbol{\mu}_1 \neq \boldsymbol{\mu}_2.}
#' Chen and Qin (2010) proposed the following test statistic:
#'  \deqn{T_{CQ} = \frac{\sum_{i \neq j}^{n_1} \boldsymbol{y}_{1i}^\top \boldsymbol{y}_{1j}}{n_1 (n_1 - 1)} + \frac{\sum_{i \neq j}^{n_2} \boldsymbol{y}_{2i}^\top \boldsymbol{y}_{2j}}{n_2 (n_2 - 1)} - 2 \frac{\sum_{i = 1}^{n_1} \sum_{j = 1}^{n_2} \boldsymbol{y}_{1i}^\top \boldsymbol{y}_{2j}}{n_1 n_2}.}
#'  They showed that under the null hypothesis, \eqn{T_{CQ}} is asymptotically normally distributed.
#'
#' @references
#' \insertRef{Chen_2010}{HDNRA}
#'
#' @return A list of class \code{"NRtest"} containing the results of the hypothesis test. See the help file for \code{\link{NRtest.object}} for details.
#'

#' @examples
#' library("HDNRA")
#' data("COVID19")
#' dim(COVID19)
#' group1 <- as.matrix(COVID19[c(2:19, 82:87), ]) ## healthy group
#' group2 <- as.matrix(COVID19[-c(1:19, 82:87), ]) ## COVID-19 patients
#' CQ2010.TSBF.NABT(group1,group2)
#'
#'
#' @concept ts
#' @export

CQ2010.TSBF.NABT<- function(y1, y2) {
  if (ncol(y1) != ncol(y2)) {
    stop("y1 and y2 must have the same dimension!")
  }

  # Calculate test statistics using the provided C++ function
  stats <- cq2010_tsbf_nabt_cpp(y1, y2)
  statn <- stats[1]

  # Calculate p-value
  pvalue <- pnorm(q = statn, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)

  # Prepare the result as an NRtest object
  hname <- paste("Chen and Qin (2010)'s test", sep = "")
  hname1 <- paste("Normal approximation", sep = "")

  null.value  <- "0"
  attr(null.value, "names") <- "Difference between two mean vectors"
  alternative <- "two.sided"


  out <- list(
    statistic = c("T[CQ]"= round(statn,4)),
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
