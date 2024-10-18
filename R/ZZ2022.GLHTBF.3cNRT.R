#' @title
#' Normal-reference-test with three-cumulant (3-c) matched $\\chi^2$-approximation for GLHT problem under heteroscedasticity proposed by Zhang and Zhu (2022)
#' @description
#' Zhang and Zhu (2022)'s test for general linear hypothesis testing (GLHT) problem for high-dimensional data under heteroscedasticity.

#' @usage ZZ2022.GLHTBF.3cNRT(Y,G,n,p)
#' @param Y A list of \eqn{k} data matrices.  The \eqn{i}th element represents the data matrix (\eqn{n_i\times p}) from the \eqn{i}th population with each row representing a \eqn{p}-dimensional observation.
#' @param G A known full-rank coefficient matrix (\eqn{q\times k}) with \eqn{\operatorname{rank}(\boldsymbol{G})< k}.
#' @param n A vector of \eqn{k} sample sizes. The \eqn{i}th element represents the sample size of group \eqn{i}, \eqn{n_i}.
#' @param p The dimension of data.
#'
#' @details
#' Suppose we have the following \eqn{k} independent high-dimensional samples:
#' \deqn{
#' \boldsymbol{y}_{i1},\ldots,\boldsymbol{y}_{in_i}, \;\operatorname{are \; i.i.d. \; with}\; \operatorname{E}(\boldsymbol{y}_{i1})=\boldsymbol{\mu}_i,\; \operatorname{Cov}(\boldsymbol{y}_{i1})=\boldsymbol{\Sigma}_i,i=1,\ldots,k.
#' }
#' It is of interest to test the following GLHT problem:
#' \deqn{H_0: \boldsymbol{G M}=\boldsymbol{0}, \quad \text { vs. } H_1: \boldsymbol{G M} \neq \boldsymbol{0},}
#' where
#' \eqn{\boldsymbol{M}=(\boldsymbol{\mu}_1,\ldots,\boldsymbol{\mu}_k)^\top} is a \eqn{k\times p} matrix collecting \eqn{k} mean vectors and \eqn{\boldsymbol{G}:q\times k} is a known full-rank coefficient matrix with \eqn{\operatorname{rank}(\boldsymbol{G})<k}.
#'
#' Let  \eqn{\bar{\boldsymbol{y}}_{i},i=1,\ldots,k} be the sample mean vectors and \eqn{\hat{\boldsymbol{\Sigma}}_i,i=1,\ldots,k} be the sample covariance matrices.
#'
#' Zhang and Zhu (2022) proposed the following U-statistic based test statistic:
#' \deqn{
#' T_{ZZ}=\|\boldsymbol{C \hat{\mu}}\|^2-\sum_{i=1}^kh_{ii}\operatorname{tr}(\hat{\boldsymbol{\Sigma}}_i)/n_i,
#' }
#' where \eqn{\boldsymbol{C}=[(\boldsymbol{G D G}^\top)^{-1/2}\boldsymbol{G}]\otimes\boldsymbol{I}_p}, \eqn{\boldsymbol{D}=\operatorname{diag}(1/n_1,\ldots,1/n_k)}, and \eqn{h_{ij}} is the \eqn{(i,j)}th entry of the \eqn{k\times k} matrix \eqn{\boldsymbol{H}=\boldsymbol{G}^\top(\boldsymbol{G}\boldsymbol{D}\boldsymbol{G}^\top)^{-1}\boldsymbol{G}}.
#'
#'
#' @references
#' \insertRef{Zhang_2022heteroscedastic}{HDNRA}
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
#' G <- cbind(diag(k-1),rep(-1,k-1))
#' ZZ2022.GLHTBF.3cNRT(Y,G,n,p)
#'
#' @concept nraglht
#' @export
ZZ2022.GLHTBF.3cNRT <- function(Y, G, n, p) {
  stats <- zz2022_glhtbf_3cnrt_cpp(Y, G, n, p)
  stat <- stats[1]
  beta0 <- stats[2]
  beta1 <- stats[3]
  df <- stats[4]

  # Standardize the statistic
  standardized_stat = (stat - beta0) / beta1

  # Calculate p-value
  pvalue <- pchisq(
    q = standardized_stat, df = df, ncp = 0, lower.tail = FALSE, log.p = FALSE
  )

  # Prepare the result as an NRtest object
  hname <- paste("Zhang and Zhu (2022)'s test", sep = "")
  hname1 <- paste("3-c matched chi^2-approximation", sep = "")

  null.value  <- "true"
  attr(null.value, "names") <- "The general linear hypothesis"
  alternative <- "two.sided"

  out <- list(
    statistic = c("T[ZZ]" = round(stat,4)),
    parameter = c("df" = round(df,4), "beta0" = round(beta0,4), "beta1" = round(beta1,4)),
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

