pkgname <- "HDNRA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "HDNRA-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('HDNRA')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("COVID19")
### * COVID19

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: COVID19
### Title: HDNRA_data COVID19
### Aliases: COVID19
### Keywords: datasets

### ** Examples

library(HDNRA)
data(COVID19)
dim(COVID19)
group1 <- as.matrix(COVID19[c(2:19, 82:87), ])
dim(group1)
group2 <- as.matrix(COVID19[-c(1:19, 82:87), ])
dim(group2)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("COVID19", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("corneal")
### * corneal

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: corneal
### Title: HDNRA_data corneal
### Aliases: corneal
### Keywords: datasets

### ** Examples

library(HDNRA)
data(corneal)
dim(corneal)
group1 <- as.matrix(corneal[1:43, ])
dim(group1)
group2 <- as.matrix(corneal[44:57, ])
dim(group2)
group3 <- as.matrix(corneal[58:78, ])
dim(group3)
group4 <- as.matrix(corneal[79:150, ])
dim(group4)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("corneal", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("glht_fhw2004")
### * glht_fhw2004

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: glht_fhw2004
### Title: Test proposed by Fujikoshi et al. (2004)
### Aliases: glht_fhw2004

### ** Examples

set.seed(1234)
k <- 3
q <- k-1
p <- 50
n <- c(25,30,40)
rho <- 0.01
Theta <- matrix(rep(0,k*p),nrow=k)
X <- matrix(c(rep(1,n[1]),rep(0,sum(n)),rep(1,n[2]),rep(0,sum(n)),rep(1,n[3])),ncol=k,nrow=sum(n))
y <- (-2*sqrt(1-rho)+sqrt(4*(1-rho)+4*p*rho))/(2*p)
x <- y+sqrt((1-rho))
Gamma <- matrix(rep(y,p*p),nrow=p)
diag(Gamma) <- rep(x,p)
U <- matrix(ncol = sum(n),nrow=p)
for(i in 1:sum(n)){
U[,i] <- rnorm(p,0,1)
}
Y <- X%*%Theta+t(U)%*%Gamma
C <- cbind(diag(q),-rep(1,q))
glht_fhw2004(Y,X,C)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("glht_fhw2004", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("glht_sf2006")
### * glht_sf2006

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: glht_sf2006
### Title: Test proposed by Srivastava and Fujikoshi (2006)
### Aliases: glht_sf2006

### ** Examples

set.seed(1234)
k <- 3
q <- k-1
p <- 50
n <- c(25,30,40)
rho <- 0.01
Theta <- matrix(rep(0,k*p),nrow=k)
X <- matrix(c(rep(1,n[1]),rep(0,sum(n)),rep(1,n[2]),rep(0,sum(n)),rep(1,n[3])),ncol=k,nrow=sum(n))
y <- (-2*sqrt(1-rho)+sqrt(4*(1-rho)+4*p*rho))/(2*p)
x <- y+sqrt((1-rho))
Gamma <- matrix(rep(y,p*p),nrow=p)
diag(Gamma) <- rep(x,p)
U <- matrix(ncol = sum(n),nrow=p)
for(i in 1:sum(n)){
U[,i] <- rnorm(p,0,1)
}
Y <- X%*%Theta+t(U)%*%Gamma
C <- cbind(diag(q),-rep(1,q))
glht_sf2006(Y,X,C)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("glht_sf2006", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("glht_ys2012")
### * glht_ys2012

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: glht_ys2012
### Title: Test proposed by Yamada and Srivastava (2012)
### Aliases: glht_ys2012

### ** Examples

set.seed(1234)
k <- 3
q <- k-1
p <- 50
n <- c(25,30,40)
rho <- 0.01
Theta <- matrix(rep(0,k*p),nrow=k)
X <- matrix(c(rep(1,n[1]),rep(0,sum(n)),rep(1,n[2]),rep(0,sum(n)),rep(1,n[3])),ncol=k,nrow=sum(n))
y <- (-2*sqrt(1-rho)+sqrt(4*(1-rho)+4*p*rho))/(2*p)
x <- y+sqrt((1-rho))
Gamma <- matrix(rep(y,p*p),nrow=p)
diag(Gamma) <- rep(x,p)
U <- matrix(ncol = sum(n),nrow=p)
for(i in 1:sum(n)){
U[,i] <- rnorm(p,0,1)
}
Y <- X%*%Theta+t(U)%*%Gamma
C <- cbind(diag(q),-rep(1,q))
glht_ys2012(Y,X,C)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("glht_ys2012", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("glht_zgz2017")
### * glht_zgz2017

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: glht_zgz2017
### Title: Test proposed by Zhang et al. (2017)
### Aliases: glht_zgz2017

### ** Examples

set.seed(1234)
k <- 3
p <- 50
n <- c(25, 30, 40)
rho <- 0.1
M <- matrix(rep(0, k * p), nrow = k, ncol = p)
y <- (-2 * sqrt(1 - rho) + sqrt(4 * (1 - rho) + 4 * p * rho)) / (2 * p)
x <- y + sqrt((1 - rho))
Gamma <- matrix(rep(y, p * p), nrow = p)
diag(Gamma) <- rep(x, p)
Y <- list()
for (g in 1:k) {
  Z <- matrix(rnorm(n[g] * p, mean = 0, sd = 1), p, n[g])
  Y[[g]] <- Gamma %*% Z + t(t(M[g, ])) %*% (rep(1, n[g]))
}
G <- cbind(diag(k - 1), rep(-1, k - 1))
glht_zgz2017(Y, G, n, p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("glht_zgz2017", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("glht_zz2022")
### * glht_zz2022

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: glht_zz2022
### Title: Test proposed by Zhu and Zhang (2022)
### Aliases: glht_zz2022

### ** Examples

set.seed(1234)
k <- 3
p <- 50
n <- c(25, 30, 40)
rho <- 0.1
M <- matrix(rep(0, k * p), nrow = k, ncol = p)
y <- (-2 * sqrt(1 - rho) + sqrt(4 * (1 - rho) + 4 * p * rho)) / (2 * p)
x <- y + sqrt((1 - rho))
Gamma <- matrix(rep(y, p * p), nrow = p)
diag(Gamma) <- rep(x, p)
Y <- list()
for (g in 1:k) {
  Z <- matrix(rnorm(n[g] * p, mean = 0, sd = 1), p, n[g])
  Y[[g]] <- Gamma %*% Z + t(t(M[g, ])) %*% (rep(1, n[g]))
}
G <- cbind(diag(k - 1), rep(-1, k - 1))
glht_zz2022(Y, G, n, p)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("glht_zz2022", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("glht_zzz2022")
### * glht_zzz2022

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: glht_zzz2022
### Title: Test proposed by Zhu et al. (2022)
### Aliases: glht_zzz2022

### ** Examples

set.seed(1234)
k <- 3
q <- k - 1
p <- 50
n <- c(25, 30, 40)
rho <- 0.01
Theta <- matrix(rep(0, k * p), nrow = k)
X <- matrix(c(rep(1, n[1]), rep(0, sum(n)), rep(1, n[2]), rep(0, sum(n)), rep(1, n[3])),
  ncol = k, nrow = sum(n)
)
y <- (-2 * sqrt(1 - rho) + sqrt(4 * (1 - rho) + 4 * p * rho)) / (2 * p)
x <- y + sqrt((1 - rho))
Gamma <- matrix(rep(y, p * p), nrow = p)
diag(Gamma) <- rep(x, p)
U <- matrix(ncol = sum(n), nrow = p)
for (i in 1:sum(n)) {
  U[, i] <- rnorm(p, 0, 1)
}
Y <- X %*% Theta + t(U) %*% Gamma
C <- cbind(diag(q), -rep(1, q))
glht_zzz2022(Y, X, C)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("glht_zzz2022", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("glhtbf_zgz2017")
### * glhtbf_zgz2017

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: glhtbf_zgz2017
### Title: Test proposed by Zhou et al. (2017)
### Aliases: glhtbf_zgz2017

### ** Examples

set.seed(1234)
k <- 3
p <- 50
n <- c(25, 30, 40)
rho <- 0.1
M <- matrix(rep(0, k * p), nrow = k, ncol = p)
avec <- seq(1, k)
Y <- list()
for (g in 1:k) {
  a <- avec[g]
  y <- (-2 * sqrt(a * (1 - rho)) + sqrt(4 * a * (1 - rho) + 4 * p * a * rho)) / (2 * p)
  x <- y + sqrt(a * (1 - rho))
  Gamma <- matrix(rep(y, p * p), nrow = p)
  diag(Gamma) <- rep(x, p)
  Z <- matrix(rnorm(n[g] * p, mean = 0, sd = 1), p, n[g])
  Y[[g]] <- Gamma %*% Z + t(t(M[g, ])) %*% (rep(1, n[g]))
}
G <- cbind(diag(k - 1), rep(-1, k - 1))
glhtbf_zgz2017(Y, G, n, p)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("glhtbf_zgz2017", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("glhtbf_zz2022")
### * glhtbf_zz2022

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: glhtbf_zz2022
### Title: Test proposed by Zhang and Zhu (2022)
### Aliases: glhtbf_zz2022

### ** Examples

set.seed(1234)
k <- 3
p <- 50
n <- c(25, 30, 40)
rho <- 0.1
M <- matrix(rep(0, k * p), nrow = k, ncol = p)
avec <- seq(1, k)
Y <- list()
for (g in 1:k) {
  a <- avec[g]
  y <- (-2 * sqrt(a * (1 - rho)) + sqrt(4 * a * (1 - rho) + 4 * p * a * rho)) / (2 * p)
  x <- y + sqrt(a * (1 - rho))
  Gamma <- matrix(rep(y, p * p), nrow = p)
  diag(Gamma) <- rep(x, p)
  Z <- matrix(rnorm(n[g] * p, mean = 0, sd = 1), p, n[g])
  Y[[g]] <- Gamma %*% Z + t(t(M[g, ])) %*% (rep(1, n[g]))
}
G <- cbind(diag(k - 1), rep(-1, k - 1))
glhtbf_zz2022(Y, G, n, p)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("glhtbf_zz2022", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("glhtbf_zzg2022")
### * glhtbf_zzg2022

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: glhtbf_zzg2022
### Title: Test proposed by Zhang et al. (2022)
### Aliases: glhtbf_zzg2022

### ** Examples

set.seed(1234)
k <- 3
p <- 50
n <- c(25, 30, 40)
rho <- 0.1
M <- matrix(rep(0, k * p), nrow = k, ncol = p)
avec <- seq(1, k)
Y <- list()
for (g in 1:k) {
  a <- avec[g]
  y <- (-2 * sqrt(a * (1 - rho)) + sqrt(4 * a * (1 - rho) + 4 * p * a * rho)) / (2 * p)
  x <- y + sqrt(a * (1 - rho))
  Gamma <- matrix(rep(y, p * p), nrow = p)
  diag(Gamma) <- rep(x, p)
  Z <- matrix(rnorm(n[g] * p, mean = 0, sd = 1), p, n[g])
  Y[[g]] <- Gamma %*% Z + t(t(M[g, ])) %*% (rep(1, n[g]))
}
G <- cbind(diag(k - 1), rep(-1, k - 1))
glhtbf_zzg2022(Y, G, n, p)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("glhtbf_zzg2022", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ks_s2007")
### * ks_s2007

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ks_s2007
### Title: Test proposed by Schott (2007)
### Aliases: ks_s2007

### ** Examples

set.seed(1234)
k <- 3
p <- 50
n <- c(25, 30, 40)
rho <- 0.1
M <- matrix(rep(0, k * p), nrow = k, ncol = p)
y <- (-2 * sqrt(1 - rho) + sqrt(4 * (1 - rho) + 4 * p * rho)) / (2 * p)
x <- y + sqrt((1 - rho))
Gamma <- matrix(rep(y, p * p), nrow = p)
diag(Gamma) <- rep(x, p)
Y <- list()
for (g in 1:k) {
  Z <- matrix(rnorm(n[g] * p, mean = 0, sd = 1), p, n[g])
  Y[[g]] <- Gamma %*% Z + t(t(M[g, ])) %*% (rep(1, n[g]))
}
ks_s2007(Y, n, p)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ks_s2007", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ts_bs1996")
### * ts_bs1996

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ts_bs1996
### Title: Test proposed by Bai and Saranadasa (1996)
### Aliases: ts_bs1996

### ** Examples

set.seed(1234)
n1 <- 20
n2 <- 30
p <- 50
mu1 <- t(t(rep(0, p)))
mu2 <- mu1
rho <- 0.1
y <- (-2 * sqrt(1 - rho) + sqrt(4 * (1 - rho) + 4 * p * rho)) / (2 * p)
x <- y + sqrt((1 - rho))
Gamma <- matrix(rep(y, p * p), nrow = p)
diag(Gamma) <- rep(x, p)
Z1 <- matrix(rnorm(n1 * p, mean = 0, sd = 1), p, n1)
Z2 <- matrix(rnorm(n2 * p, mean = 0, sd = 1), p, n2)
y1 <- Gamma %*% Z1 + mu1 %*% (rep(1, n1))
y2 <- Gamma %*% Z2 + mu2 %*% (rep(1, n2))
ts_bs1996(y1, y2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ts_bs1996", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ts_sd2008")
### * ts_sd2008

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ts_sd2008
### Title: Test proposed by Srivastava and Du (2008)
### Aliases: ts_sd2008

### ** Examples

set.seed(1234)
n1 <- 20
n2 <- 30
p <- 50
mu1 <- t(t(rep(0, p)))
mu2 <- mu1
rho <- 0.1
y <- (-2 * sqrt(1 - rho) + sqrt(4 * (1 - rho) + 4 * p * rho)) / (2 * p)
x <- y + sqrt((1 - rho))
Gamma <- matrix(rep(y, p * p), nrow = p)
diag(Gamma) <- rep(x, p)
Z1 <- matrix(rnorm(n1 * p, mean = 0, sd = 1), p, n1)
Z2 <- matrix(rnorm(n2 * p, mean = 0, sd = 1), p, n2)
y1 <- Gamma %*% Z1 + mu1 %*% (rep(1, n1))
y2 <- Gamma %*% Z2 + mu2 %*% (rep(1, n2))
ts_sd2008(y1, y2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ts_sd2008", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ts_zgzc2020")
### * ts_zgzc2020

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ts_zgzc2020
### Title: Test proposed by Zhang et al. (2020)
### Aliases: ts_zgzc2020

### ** Examples

set.seed(1234)
n1 <- 20
n2 <- 30
p <- 50
mu1 <- t(t(rep(0, p)))
mu2 <- mu1
rho <- 0.1
y <- (-2 * sqrt(1 - rho) + sqrt(4 * (1 - rho) + 4 * p * rho)) / (2 * p)
x <- y + sqrt((1 - rho))
Gamma <- matrix(rep(y, p * p), nrow = p)
diag(Gamma) <- rep(x, p)
Z1 <- matrix(rnorm(n1 * p, mean = 0, sd = 1), p, n1)
Z2 <- matrix(rnorm(n2 * p, mean = 0, sd = 1), p, n2)
y1 <- Gamma %*% Z1 + mu1 %*% (rep(1, n1))
y2 <- Gamma %*% Z2 + mu2 %*% (rep(1, n2))
ts_zgzc2020(y1, y2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ts_zgzc2020", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ts_zz2022")
### * ts_zz2022

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ts_zz2022
### Title: Test proposed by Zhang and Zhu (2022)
### Aliases: ts_zz2022

### ** Examples

set.seed(1234)
n1 <- 20
n2 <- 30
p <- 50
mu1 <- t(t(rep(0, p)))
mu2 <- mu1
rho <- 0.1
y <- (-2 * sqrt(1 - rho) + sqrt(4 * (1 - rho) + 4 * p * rho)) / (2 * p)
x <- y + sqrt((1 - rho))
Gamma <- matrix(rep(y, p * p), nrow = p)
diag(Gamma) <- rep(x, p)
Z1 <- matrix(rnorm(n1 * p, mean = 0, sd = 1), p, n1)
Z2 <- matrix(rnorm(n2 * p, mean = 0, sd = 1), p, n2)
y1 <- Gamma %*% Z1 + mu1 %*% (rep(1, n1))
y2 <- Gamma %*% Z2 + mu2 %*% (rep(1, n2))
ts_zz2022(y1, y2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ts_zz2022", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ts_zzz2020")
### * ts_zzz2020

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ts_zzz2020
### Title: Test proposed by Zhang et al. (2020)
### Aliases: ts_zzz2020

### ** Examples

set.seed(1234)
n1 <- 20
n2 <- 30
p <- 50
mu1 <- t(t(rep(0, p)))
mu2 <- mu1
rho <- 0.1
y <- (-2 * sqrt(1 - rho) + sqrt(4 * (1 - rho) + 4 * p * rho)) / (2 * p)
x <- y + sqrt((1 - rho))
Gamma <- matrix(rep(y, p * p), nrow = p)
diag(Gamma) <- rep(x, p)
Z1 <- matrix(rnorm(n1 * p, mean = 0, sd = 1), p, n1)
Z2 <- matrix(rnorm(n2 * p, mean = 0, sd = 1), p, n2)
y1 <- Gamma %*% Z1 + mu1 %*% (rep(1, n1))
y2 <- Gamma %*% Z2 + mu2 %*% (rep(1, n2))
ts_zzz2020(y1, y2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ts_zzz2020", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tsbf_cq2010")
### * tsbf_cq2010

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tsbf_cq2010
### Title: Test proposed by Chen and Qin (2010)
### Aliases: tsbf_cq2010

### ** Examples

set.seed(1234)
n1 <- 20
n2 <- 30
p <- 50
mu1 <- t(t(rep(0, p)))
mu2 <- mu1
rho1 <- 0.1
rho2 <- 0.2
a1 <- 1
a2 <- 2
w1 <- (-2 * sqrt(a1 * (1 - rho1)) + sqrt(4 * a1 * (1 - rho1) + 4 * p * a1 * rho1)) / (2 * p)
x1 <- w1 + sqrt(a1 * (1 - rho1))
Gamma1 <- matrix(rep(w1, p * p), nrow = p)
diag(Gamma1) <- rep(x1, p)
w2 <- (-2 * sqrt(a2 * (1 - rho2)) + sqrt(4 * a2 * (1 - rho2) + 4 * p * a2 * rho2)) / (2 * p)
x2 <- w2 + sqrt(a2 * (1 - rho2))
Gamma2 <- matrix(rep(w2, p * p), nrow = p)
diag(Gamma2) <- rep(x2, p)
Z1 <- matrix(rnorm(n1*p,mean = 0,sd = 1), p, n1)
Z2 <- matrix(rnorm(n2*p,mean = 0,sd = 1), p, n2)
y1 <- Gamma1 %*% Z1 + mu1%*%(rep(1,n1))
y2 <- Gamma2 %*% Z2 + mu2%*%(rep(1,n2))
tsbf_cq2010(y1, y2)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tsbf_cq2010", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tsbf_skk2013")
### * tsbf_skk2013

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tsbf_skk2013
### Title: Test proposed by Srivastava et al. (2013)
### Aliases: tsbf_skk2013

### ** Examples

set.seed(1234)
n1 <- 20
n2 <- 30
p <- 50
mu1 <- t(t(rep(0, p)))
mu2 <- mu1
rho1 <- 0.1
rho2 <- 0.2
a1 <- 1
a2 <- 2
w1 <- (-2 * sqrt(a1 * (1 - rho1)) + sqrt(4 * a1 * (1 - rho1) + 4 * p * a1 * rho1)) / (2 * p)
x1 <- w1 + sqrt(a1 * (1 - rho1))
Gamma1 <- matrix(rep(w1, p * p), nrow = p)
diag(Gamma1) <- rep(x1, p)
w2 <- (-2 * sqrt(a2 * (1 - rho2)) + sqrt(4 * a2 * (1 - rho2) + 4 * p * a2 * rho2)) / (2 * p)
x2 <- w2 + sqrt(a2 * (1 - rho2))
Gamma2 <- matrix(rep(w2, p * p), nrow = p)
diag(Gamma2) <- rep(x2, p)
Z1 <- matrix(rnorm(n1*p,mean = 0,sd = 1), p, n1)
Z2 <- matrix(rnorm(n2*p,mean = 0,sd = 1), p, n2)
y1 <- Gamma1 %*% Z1 + mu1%*%(rep(1,n1))
y2 <- Gamma2 %*% Z2 + mu2%*%(rep(1,n2))
tsbf_skk2013(y1, y2)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tsbf_skk2013", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tsbf_zwz2023")
### * tsbf_zwz2023

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tsbf_zwz2023
### Title: Test proposed by Zhu et al. (2023)
### Aliases: tsbf_zwz2023

### ** Examples

set.seed(1234)
n1 <- 20
n2 <- 30
p <- 50
mu1 <- t(t(rep(0, p)))
mu2 <- mu1
rho1 <- 0.1
rho2 <- 0.2
a1 <- 1
a2 <- 2
w1 <- (-2 * sqrt(a1 * (1 - rho1)) + sqrt(4 * a1 * (1 - rho1) + 4 * p * a1 * rho1)) / (2 * p)
x1 <- w1 + sqrt(a1 * (1 - rho1))
Gamma1 <- matrix(rep(w1, p * p), nrow = p)
diag(Gamma1) <- rep(x1, p)
w2 <- (-2 * sqrt(a2 * (1 - rho2)) + sqrt(4 * a2 * (1 - rho2) + 4 * p * a2 * rho2)) / (2 * p)
x2 <- w2 + sqrt(a2 * (1 - rho2))
Gamma2 <- matrix(rep(w2, p * p), nrow = p)
diag(Gamma2) <- rep(x2, p)
Z1 <- matrix(rnorm(n1*p,mean = 0,sd = 1), p, n1)
Z2 <- matrix(rnorm(n2*p,mean = 0,sd = 1), p, n2)
y1 <- Gamma1 %*% Z1 + mu1%*%(rep(1,n1))
y2 <- Gamma2 %*% Z2 + mu2%*%(rep(1,n2))
tsbf_zwz2023(y1, y2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tsbf_zwz2023", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tsbf_zz2022")
### * tsbf_zz2022

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tsbf_zz2022
### Title: Test proposed by Zhang and Zhu (2022)
### Aliases: tsbf_zz2022

### ** Examples

set.seed(1234)
n1 <- 20
n2 <- 30
p <- 50
mu1 <- t(t(rep(0, p)))
mu2 <- mu1
rho1 <- 0.1
rho2 <- 0.2
a1 <- 1
a2 <- 2
w1 <- (-2 * sqrt(a1 * (1 - rho1)) + sqrt(4 * a1 * (1 - rho1) + 4 * p * a1 * rho1)) / (2 * p)
x1 <- w1 + sqrt(a1 * (1 - rho1))
Gamma1 <- matrix(rep(w1, p * p), nrow = p)
diag(Gamma1) <- rep(x1, p)
w2 <- (-2 * sqrt(a2 * (1 - rho2)) + sqrt(4 * a2 * (1 - rho2) + 4 * p * a2 * rho2)) / (2 * p)
x2 <- w2 + sqrt(a2 * (1 - rho2))
Gamma2 <- matrix(rep(w2, p * p), nrow = p)
diag(Gamma2) <- rep(x2, p)
Z1 <- matrix(rnorm(n1 * p, mean = 0, sd = 1), p, n1)
Z2 <- matrix(rnorm(n2 * p, mean = 0, sd = 1), p, n2)
y1 <- Gamma1 %*% Z1 + mu1 %*% (rep(1, n1))
y2 <- Gamma2 %*% Z2 + mu2 %*% (rep(1, n2))
tsbf_zz2022(y1, y2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tsbf_zz2022", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tsbf_zzgz2021")
### * tsbf_zzgz2021

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tsbf_zzgz2021
### Title: Test proposed by Zhang et al. (2021)
### Aliases: tsbf_zzgz2021

### ** Examples

set.seed(1234)
n1 <- 20
n2 <- 30
p <- 50
mu1 <- t(t(rep(0, p)))
mu2 <- mu1
rho1 <- 0.1
rho2 <- 0.2
a1 <- 1
a2 <- 2
w1 <- (-2 * sqrt(a1 * (1 - rho1)) + sqrt(4 * a1 * (1 - rho1) + 4 * p * a1 * rho1)) / (2 * p)
x1 <- w1 + sqrt(a1 * (1 - rho1))
Gamma1 <- matrix(rep(w1, p * p), nrow = p)
diag(Gamma1) <- rep(x1, p)
w2 <- (-2 * sqrt(a2 * (1 - rho2)) + sqrt(4 * a2 * (1 - rho2) + 4 * p * a2 * rho2)) / (2 * p)
x2 <- w2 + sqrt(a2 * (1 - rho2))
Gamma2 <- matrix(rep(w2, p * p), nrow = p)
diag(Gamma2) <- rep(x2, p)
Z1 <- matrix(rnorm(n1*p,mean = 0,sd = 1), p, n1)
Z2 <- matrix(rnorm(n2*p,mean = 0,sd = 1), p, n2)
y1 <- Gamma1 %*% Z1 + mu1%*%(rep(1,n1))
y2 <- Gamma2 %*% Z2 + mu2%*%(rep(1,n2))
tsbf_zzgz2021(y1, y2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tsbf_zzgz2021", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tsbf_zzz2023")
### * tsbf_zzz2023

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tsbf_zzz2023
### Title: Test proposed by Zhang et al. (2023)
### Aliases: tsbf_zzz2023

### ** Examples

set.seed(1234)
n1 <- 20
n2 <- 30
p <- 50
mu1 <- t(t(rep(0, p)))
mu2 <- mu1
rho1 <- 0.1
rho2 <- 0.2
a1 <- 1
a2 <- 2
w1 <- (-2 * sqrt(a1 * (1 - rho1)) + sqrt(4 * a1 * (1 - rho1) + 4 * p * a1 * rho1)) / (2 * p)
x1 <- w1 + sqrt(a1 * (1 - rho1))
Gamma1 <- matrix(rep(w1, p * p), nrow = p)
diag(Gamma1) <- rep(x1, p)
w2 <- (-2 * sqrt(a2 * (1 - rho2)) + sqrt(4 * a2 * (1 - rho2) + 4 * p * a2 * rho2)) / (2 * p)
x2 <- w2 + sqrt(a2 * (1 - rho2))
Gamma2 <- matrix(rep(w2, p * p), nrow = p)
diag(Gamma2) <- rep(x2, p)
Z1 <- matrix(rnorm(n1*p,mean = 0,sd = 1), p, n1)
Z2 <- matrix(rnorm(n2*p,mean = 0,sd = 1), p, n2)
y1 <- Gamma1 %*% Z1 + mu1%*%(rep(1,n1))
y2 <- Gamma2 %*% Z2 + mu2%*%(rep(1,n2))
tsbf_zzz2023(y1,y2,cutoff=1.2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tsbf_zzz2023", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
