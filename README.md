
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HDNRA

<!-- badges: start -->

[![License:
GPL-3.0](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/nie23wp8738/HDNRA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nie23wp8738/HDNRA/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The R package **HDNRA** includes the latest methods based on
normal-reference approach to test the equality of the mean vectors of
high-dimensional samples with possibly different covariance matrices.
`HDNRA` is also used to demonstrate the implementation of these tests,
catering not only to the two-sample problem, but also to the general
linear hypothesis testing (GLHT) problem. This package provides easy and
user-friendly access to these tests. Both coded in C++ to allow for
reasonable execution time using
[Rcpp](https://github.com/RcppCore/Rcpp). Besides
[Rcpp](https://github.com/RcppCore/Rcpp), the package has no strict
dependencies in order to provide a stable self-contained toolbox that
invites re-use.

There are:

Two real data sets in `HDNRA`

- [COVID19](https://nie23wp8738.github.io/HDNRA/reference/COVID19.html)
- [corneal](https://nie23wp8738.github.io/HDNRA/reference/corneal.html)

Seven normal-reference tests for the two-sample problem

- [ZGZC2020.TS.2cNRT](https://nie23wp8738.github.io/HDNRA/reference/ZGZC2020.TS.2cNRT.html)
- [ZZ2022.TS.3cNRT](https://nie23wp8738.github.io/HDNRA/reference/ZZ2022.TS.3cNRT.html)
- [ZZZ2020.TS.2cNRT](https://nie23wp8738.github.io/HDNRA/reference/ZZZ2020.TS.2cNRT.html)
- [ZWZ2023.TSBF.2cNRT](https://nie23wp8738.github.io/HDNRA/reference/ZWZ2023.TSBF.2cNRT.html)
- [ZZ2022.TSBF.3cNRT](https://nie23wp8738.github.io/HDNRA/reference/ZZ2022.TSBF.3cNRT.html)
- [ZZGZ2021.TSBF.2cNRT](https://nie23wp8738.github.io/HDNRA/reference/ZZGZ2021.TSBF.2cNRT.html)
- [ZZZ2023.TSBF.2cNRT](https://nie23wp8738.github.io/HDNRA/reference/ZZZ2023.TSBF.2cNRT.html)

Five normal-reference tests for the GLHT problem in `HDNRA`

- [ZGZ2017.GLHT.2cNRT](https://nie23wp8738.github.io/HDNRA/reference/ZGZ2017.GLHT.2cNRT.html)
- [ZZ2022.GLHT.3cNRT](https://nie23wp8738.github.io/HDNRA/reference/ZZ2022.GLHT.3cNRT.html)
- [ZZZ2022.GLHT.2cNRT](https://nie23wp8738.github.io/HDNRA/reference/ZZZ2022.GLHT.2cNRT.html)
- [ZZ2022.GLHTBF.3cNRT](https://nie23wp8738.github.io/HDNRA/reference/ZZ2022.GLHTBF.3cNRT.html)
- [ZZG2022.GLHTBF.2cNRT](https://nie23wp8738.github.io/HDNRA/reference/ZZG2022.GLHTBF.2cNRT.html)

Four existing tests for the two-sample problem in `HDNRA`

- [BS1996.TS.NART](https://nie23wp8738.github.io/HDNRA/reference/BS1996.TS.NART.html)
- [SD2008.TS.NABT](https://nie23wp8738.github.io/HDNRA/reference/SD2008.TS.NABT.html)
- [CQ2010.TSBF.NABT](https://nie23wp8738.github.io/HDNRA/reference/CQ2010.TSBF.NABT.html)
- [SKK2013.TSBF.NABT](https://nie23wp8738.github.io/HDNRA/reference/SKK2013.TSBF.NABT.html)

Five existing tests for the GLHT problem in `HDNRA`

- [FHW2004.GLHT.NABT](https://nie23wp8738.github.io/HDNRA/reference/FHW2004.GLHT.NABT.html)
- [SF2006.GLHT.NABT](https://nie23wp8738.github.io/HDNRA/reference/SF2006.GLHT.NABT.html)
- [YS2012.GLHT.NABT](https://nie23wp8738.github.io/HDNRA/reference/YS2012.GLHT.NABT.html)
- [ZGZ2017.GLHTBF.NABT](https://nie23wp8738.github.io/HDNRA/reference/ZGZ2017.GLHTBF.NABT.html)
- [S2007.ks.NABT](https://nie23wp8738.github.io/HDNRA/reference/S2007.ks.NABT.html)

## Installation

You can install and load the most recent development version of `HDNRA`
from [GitHub](https://github.com/) with:

``` r
# Installing from GitHub requires you first install the devtools or remotes package
install.packages("devtools")
# Or
install.packages("remotes")

# install the most recent development version from GitHub
devtools::install_github("nie23wp8738/HDNRA")
# Or
remotes::install_github("nie23wp8738/HDNRA")
# load the most recent development version from GitHub
library(HDNRA)
```

## Usage

### Load the package

``` r
library(HDNRA)
```

### Example data

Package `HDNRA` comes with two real data sets:

``` r
# A COVID19 data set from NCBI with ID GSE152641 for the two-sample problem.
?COVID19
data(COVID19)
dim(COVID19)
#> [1]    87 20460
group1 <- as.matrix(COVID19[c(2:19, 82:87), ]) ## healthy group
dim(group1)
#> [1]    24 20460
group2 <- as.matrix(COVID19[-c(1:19, 82:87), ]) ## COVID-19 patients
dim(group2)
#> [1]    62 20460

# A corneal data set acquired during a keratoconus study for the GLHT problem.
?corneal
data(corneal)
dim(corneal)
#> [1]  150 2000
group1 <- as.matrix(corneal[1:43, ]) ## normal group
dim(group1)
#> [1]   43 2000
group2 <- as.matrix(corneal[44:57, ]) ## unilateral suspect group
dim(group2)
#> [1]   14 2000
group3 <- as.matrix(corneal[58:78, ]) ## suspect map group
dim(group3)
#> [1]   21 2000
group4 <- as.matrix(corneal[79:150, ]) ## clinical keratoconus group
dim(group4)
#> [1]   72 2000
```

### Example for two-sample problem

A simple example of how to use one of the normal-reference tests
`ZWZ2023.TSBF.2cNRT` using data set `COVID19`:

``` r
data("COVID19")
group1 <- as.matrix(COVID19[c(2:19, 82:87), ]) # healthy group1
group2 <- as.matrix(COVID19[-c(1:19, 82:87), ]) # patients group2
# The data matrix for tsbf_zwz2023 should be p by n, sometimes we should transpose the data matrix
ZWZ2023.TSBF.2cNRT(group1, group2)
#> 
#> Results of Hypothesis Test
#> --------------------------
#> 
#> Test name:                       Zhu et al. (2023)'s test
#> 
#> Null Hypothesis:                 Difference between two mean vectors is 0
#> 
#> Alternative Hypothesis:          Difference between two mean vectors is not 0
#> 
#> Data:                            group1 and group2
#> 
#> Sample Sizes:                    n1 = 24
#>                                  n2 = 62
#> 
#> Sample Dimension:                20460
#> 
#> Test Statistic:                  T[ZWZ] = 4.1877
#> 
#> Approximation method to the      2-c matched chi^2-approximation
#> null distribution of T[ZWZ]: 
#> 
#> Approximation parameter(s):      df1 =   2.7324
#>                                  df2 = 171.7596
#> 
#> P-value:                         0.008672887
```

### Example for GLHT problem

A simple example of how to use one of the normal-reference tests
`ZZG2022.GLHTBF.2cNRT` using data set `corneal`:

``` r
data("corneal")
dim(corneal)
#> [1]  150 2000
group1 <- as.matrix(corneal[1:43, ]) ## normal group
group2 <- as.matrix(corneal[44:57, ]) ## unilateral suspect group
group3 <- as.matrix(corneal[58:78, ]) ## suspect map group
group4 <- as.matrix(corneal[79:150, ]) ## clinical keratoconus group
p <- dim(corneal)[2]
Y <- list()
k <- 4
Y[[1]] <- group1
Y[[2]] <- group2
Y[[3]] <- group3
Y[[4]] <- group4
n <- c(nrow(Y[[1]]),nrow(Y[[2]]),nrow(Y[[3]]),nrow(Y[[4]]))
G <- cbind(diag(k-1),rep(-1,k-1))
ZZG2022.GLHTBF.2cNRT(Y,G,n,p)
#> 
#> Results of Hypothesis Test
#> --------------------------
#> 
#> Test name:                       Zhang et al. (2022)'s test
#> 
#> Null Hypothesis:                 The general linear hypothesis is true
#> 
#> Alternative Hypothesis:          The general linear hypothesis is not true
#> 
#> Data:                            Y
#> 
#> Sample Sizes:                    n1 = 43
#>                                  n2 = 14
#>                                  n3 = 21
#>                                  n4 = 72
#> 
#> Sample Dimension:                2000
#> 
#> Test Statistic:                  T[ZZG] = 159.7325
#> 
#> Approximation method to the      2-c matched chi^2-approximation
#> null distribution of T[ZZG]: 
#> 
#> Approximation parameter(s):      df   = 6.1652
#>                                  beta = 6.1464
#> 
#> P-value:                         0.0002577084
```

## Code of Conduct

Please note that the HDNRA project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms
