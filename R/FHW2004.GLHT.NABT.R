#' @title
#' Normal-approximation-based test for GLHT problem proposed by Fujikoshi et al. (2004)
#' @description
#' Fujikoshi et al. (2004)'s test for general linear hypothesis testing (GLHT) problem for high-dimensional data with assuming that underlying covariance matrices are the same.
#'
#' @usage FHW2004.GLHT.NABT(Y,X,C,n,p)
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
#' \deqn{H_0: \boldsymbol{C\Theta}=\boldsymbol{0}, \quad \text { vs. } \quad H_1: \boldsymbol{C\Theta} \neq \boldsymbol{0}.}
#'
#' Fujikoshi et al. (2004) proposed the following test statistic:
#' \deqn{T_{FHW}=\sqrt{p}\left[(n-k)\frac{\operatorname{tr}(\boldsymbol{S}_h)}{\operatorname{tr}(\boldsymbol{S}_e)}-q\right],}
#' where \eqn{\boldsymbol{S}_h} and \eqn{\boldsymbol{S}_e} are the matrices of sums of squares and products due to the hypothesis and the error, respecitively.
#'
#' They showed that under the null hypothesis, \eqn{T_{FHW}} is asymptotically normally distributed.


#' @references
#' \insertRef{fujikoshi_2004_asymptotic}{HDNRA}
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
#' FHW2004.GLHT.NABT(Y,X,C,n,p)



#' @concept glht
#' @export
FHW2004.GLHT.NABT <- function(Y, X, C, n, p) {
  stats <- fhw2004_glht_nabt_cpp(Y, X, C,n,p)
  stat <- stats[1]

  # Adjust for standardization if needed, similar to other GLHT functions:
  # Calculate p-value
  pvalue <- pnorm(stat, 0, 1, lower.tail = FALSE, log.p = FALSE)

  # Prepare the result as an NRtest object
  hname <- paste("Fujikoshi et al. (2004)'s test", sep = "")
  hname1 <- paste("Normal approximation", sep = "")

  null.value  <- "true"
  attr(null.value, "names") <- "The general linear hypothesis"
  alternative <- "two.sided"

  sample.size <- setNames(n, paste0("n", seq_along(n)))

  out <- list(
    statistic = c("T[FHW]" = round(stat,4)),
    parameter = NULL,
    p.value = pvalue,
    method = hname,
    estimation.method = hname1,
    data.name = deparse(substitute(Y)),
    null.value = null.value,
    sample.size = sample.size,
    sample.dimension = p,
    alternative = alternative
  )

  class(out) <- "NRtest"
  return(out)
}

