#' @title
#' Normal-approximation-based test for one-way MANOVA problem proposed by Schott (2007)
#' @description
#' Schott, J. R. (2007)'s test for one-way MANOVA problem for high-dimensional data with assuming that underlying covariance matrices are the same.

#' @usage S2007.ks.NABT(Y, n, p)
#' @param Y A list of \eqn{k} data matrices.  The \eqn{i}th element represents the data matrix (\eqn{n_i \times p}) from the \eqn{i}th population with each row representing a \eqn{p}-dimensional observation.
#' @param n A vector of \eqn{k} sample sizes. The \eqn{i}th element represents the sample size of group \eqn{i}, \eqn{n_i}.
#' @param p The dimension of data.

#'
#' @details
#' Suppose we have the following \eqn{k} independent high-dimensional samples:
#' \deqn{
#' \boldsymbol{y}_{i1},\ldots,\boldsymbol{y}_{in_i}, \;\operatorname{are \; i.i.d. \; with}\; \operatorname{E}(\boldsymbol{y}_{i1})=\boldsymbol{\mu}_i,\; \operatorname{Cov}(\boldsymbol{y}_{i1})=\boldsymbol{\Sigma},i=1,\ldots,k.
#' }
#' It is of interest to test the following one-way MANOVA problem:
#' \deqn{H_0: \boldsymbol{\mu}_1=\cdots=\boldsymbol{\mu}_k, \quad \text { vs. }\; H_1: H_0 \;\operatorname{is \; not\; ture}.}
#' Schott (2007) proposed the following test statistic:
#' \deqn{
#'  T_{S}=[\operatorname{tr}(\boldsymbol{H})/h-\operatorname{tr}(\boldsymbol{E})/e]/\sqrt{N-1},
#'  }
#'  where \eqn{\boldsymbol{H}=\sum_{i=1}^kn_i(\bar{\boldsymbol{y}}_i-\bar{\boldsymbol{y}})(\bar{\boldsymbol{y}}_i-\bar{\boldsymbol{y}})^\top}, \eqn{\boldsymbol{E}=\sum_{i=1}^k\sum_{j=1}^{n_i}(\boldsymbol{y}_{ij}-\bar{\boldsymbol{y}}_{i})(\boldsymbol{y}_{ij}-\bar{\boldsymbol{y}}_{i})^\top}, \eqn{h=k-1}, and \eqn{e=N-k}, with \eqn{N=n_1+\cdots+n_k}.
#' They showed that under the null hypothesis, \eqn{T_{S}} is asymptotically normally distributed.
#'
#' @references
#' \insertRef{schott2007some}{HDNRA}
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
#' Y[[1]] <- group1
#' Y[[2]] <- group2
#' Y[[3]] <- group3
#' Y[[4]] <- group4
#' n <- c(nrow(Y[[1]]),nrow(Y[[2]]),nrow(Y[[3]]),nrow(Y[[4]]))
#' S2007.ks.NABT(Y, n, p)

#'
#' @concept glht
#' @export
S2007.ks.NABT <- function(Y, n, p) {
  stats <- s2007_ks_nabt_cpp(Y, n, p)
  stat <- stats[1]
  sigma <- stats[2]
  statstd <- stat / sigma
  pvalue <- pnorm(
    q = stat / sigma, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE
  )

  hname <- paste("Schott (2007)'s test",sep = "")
  hname1 <-paste("Normal approximation",sep = "")

  null.value  <- "0"
  attr(null.value, "names") <- "Difference between k mean vectors"
  alternative <- "two.sided"

  sample.size <- setNames(n, paste0("n", seq_along(n)))

  out <- list(
    statistic = c("T[S]" = round(statstd,4)),
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
