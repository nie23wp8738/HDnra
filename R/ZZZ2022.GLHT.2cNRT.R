#' @title
#' Normal-reference-test with two-cumulant (2-c) matched $\\chi^2$-approximation for GLHT problem proposed by Zhu et al. (2022)
#' @description
#' Zhu et al. (2022)'s test for general linear hypothesis testing (GLHT) problem for high-dimensional data with assuming that underlying covariance matrices are the same.
#'
#' @usage ZZZ2022.GLHT.2cNRT(Y,X,C,n,p)
#' @param Y A list of \eqn{k} data matrices.  The \eqn{i}th element represents the data matrix (\eqn{n_i \times p}) from the \eqn{i}th population with each row representing a \eqn{p}-dimensional observation.
#' @param X A known \eqn{n\times k} full-rank design matrix with \eqn{\operatorname{rank}(\boldsymbol{X})=k<n}.
#' @param C A known matrix of size \eqn{q\times k} with \eqn{\operatorname{rank}(\boldsymbol{C})=q<k}.
#' @param n A vector of \eqn{k} sample sizes. The \eqn{i}th element represents the sample size of group \eqn{i}, \eqn{n_i}.
#' @param p The dimension of data.
#'
#' @details
#' A high-dimensional linear regression model can be expressed as
#' \deqn{\boldsymbol{Y}=\boldsymbol{X\Theta}+\boldsymbol{\epsilon},}
#' where \eqn{\Theta} is a \eqn{k\times p} unknown parameter matrix and \eqn{\boldsymbol{\epsilon}} is an \eqn{n\times p} error matrix.
#'
#' It is of interest to test the following GLHT problem
#' \deqn{H_0: \boldsymbol{C\Theta}=\boldsymbol{0}, \quad \text { vs. } H_1: \boldsymbol{C\Theta} \neq \boldsymbol{0}.}
#'
#' Zhu et al. (2022) proposed the following test statistic:
#' \deqn{T_{ZZZ}=\frac{(n-k-2)}{(n-k)pq}\operatorname{tr}(\boldsymbol{S}_h\boldsymbol{D}^{-1}),}
#' where \eqn{\boldsymbol{S}_h} and \eqn{\boldsymbol{S}_e} are the variation matrices due to the hypothesis and error, respectively, and \eqn{\boldsymbol{D}} is the diagonal matrix with the diagonal elements of \eqn{\boldsymbol{S}_e/(n-k)}.
#' They showed that under the null hypothesis, \eqn{T_{ZZZ}} and a chi-squared-type mixture have the same limiting distribution.

#' @references
#' \insertRef{Zhu_2023}{HDNRA}
#'
#' @return A list of class \code{"NRtest"} containing the results of the hypothesis test. See the help file for \code{\link{NRtest.object}} for details.
#'
#' @examples
#' library("HDNRA")
#' data("corneal")
#' dim(corneal)
#' group1 <- as.matrix(corneal[1:43, ]) ## normal group
#' group2 <- as.matrix(corneal[44:57, ]) ## unilateral suspect group
#' group3 <- as.matrix(corneal[58:78, ]) ## suspect map group
#' group4 <- as.matrix(corneal[79:150, ]) ## clinical keratoconus group
#' p <- dim(corneal)[2]
#' Y <- list()
#' k <- 4
#' Y[[1]] <- group1
#' Y[[2]] <- group2
#' Y[[3]] <- group3
#' Y[[4]] <- group4
#' n <- c(nrow(Y[[1]]),nrow(Y[[2]]),nrow(Y[[3]]),nrow(Y[[4]]))
#' X <- matrix(c(rep(1,n[1]),rep(0,sum(n)),rep(1,n[2]), rep(0,sum(n)),
#'             rep(1,n[3]),rep(0,sum(n)),rep(1,n[4])),ncol=k,nrow=sum(n))
#' q <- k-1
#' C <- cbind(diag(q),-rep(1,q))
#' ZZZ2022.GLHT.2cNRT(Y,X,C,n,p)
#'
#' @concept nraglht
#' @export
ZZZ2022.GLHT.2cNRT <- function(Y, X, C, n, p) {
  stats <- zzz2022_glht_2cnrt_cpp(Y, X, C, n, p)
  stat <- stats[1]
  df <- stats[2]
  # Obtain sample size n and number of predictors k
  ss <- sum(n)
  k <- length(n)

  # Adjusted statistic and degrees of freedom calculations
  statnew <- stat * (ss - k - 2) / (ss - k)
  dhatnew <- df * (ss - k)^2 / ((ss - k - 2)^2)
  pvalue <- 1 - pchisq(dhatnew * statnew, dhatnew)

  # Prepare the result as an NRtest object

  hname <- paste("Zhu et al. (2022)'s test",sep = "")
  hname1 <-paste("2-c matched chi^2-approximation",sep = "")

  null.value  <- "true"
  attr(null.value, "names") <- "The general linear hypothesis"
  alternative <- "two.sided"

  out <- list(
    statistic = c("T[ZZZ]" = round(statnew,4)),
    parameter = c("df" = round(dhatnew,4)),
    p.value = pvalue,
    method = hname,
    estimation.method = hname1,
    data.name = deparse(substitute(Y)),
    null.value = null.value,
    sample.size = c(n = n),
    sample.dimension = p,
    alternative = alternative
  )
  class(out) <- "NRtest"
  return(out)
}

