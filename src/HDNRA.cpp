// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(openmp)]]
// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include <RcppArmadillo.h>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace arma;

// Helper function to compute inverse using Cholesky decomposition
arma::mat cholesky_inverse(const arma::mat &X) {
  arma::mat L = arma::chol(X, "lower");  // Performs Cholesky decomposition, returns lower triangular matrix
  arma::mat Linv = arma::inv(L);  // Computes the inverse of the lower triangular matrix L
  return Linv.t() * Linv;  // Returns the inverse of the input matrix X
}

// Test proposed by Bai and Saranadasa (1996)
// [[Rcpp::export]]
double bs1996_ts_nart_cpp(const arma::mat &y1, const arma::mat &y2) {
  int n1 = y1.n_rows;
  int n2 = y2.n_rows;
  int p = y1.n_cols;

  arma::rowvec mu1 = arma::mean(y1, 0);
  arma::mat z1 = y1.t() - arma::repmat(mu1.t(), 1, n1);
  arma::rowvec mu2 = arma::mean(y2, 0);
  arma::mat z2 = y2.t() - arma::repmat(mu2.t(), 1, n2);

  int n = n1 + n2 - 2;
  double invtau = double(n1 * n2) / (n1 + n2);
  double stat = arma::as_scalar((mu1 - mu2) * (mu1 - mu2).t());
  arma::mat z = arma::join_horiz(z1, z2);
  arma::mat S;

  if (p <= n) {
    S = (z * z.t()) / n; // tr(Sigmahat)
  } else {
    S = (z.t() * z) / n;
  }

  double trSn = arma::trace(S); // tr(Sigmahat)
  double trSn2 = arma::accu(S % S); // tr(Sigmahat^2)
  double Bn2 = double(n * n) / ((n + 2) * (n - 1)) * (trSn2 - trSn * trSn / n); // unbiased estimator for B
  double statn = (invtau * stat - trSn) / sqrt(2 * double(n + 1) / n * Bn2);

  return statn;
}

// Test proposed by Chen and Qin (2010)
// [[Rcpp::export]]
arma::vec cq2010_tsbf_nabt_cpp(const arma::mat &y1, const arma::mat &y2) {
  int n1 = y1.n_rows;
  int n2 = y2.n_rows;

  arma::mat y1xy1t = y1 * y1.t();
  double sumx1tx1inej = arma::accu(y1xy1t) - arma::trace(y1xy1t);
  arma::mat y2xy2t = y2 * y2.t();
  double sumx2tx2inej = arma::accu(y2xy2t) - arma::trace(y2xy2t);
  arma::mat y1xy2t = y1 * y2.t();
  double sumx1tx2 = arma::accu(y1xy2t);
  double Tn = sumx1tx1inej / (n1 * (n1 - 1)) + sumx2tx2inej / (n2 * (n2 - 1)) - 2 * sumx1tx2 / (n1 * n2);

  double tempsum = 0.0;
  arma::rowvec y1sum = arma::sum(y1, 0); // add columns
  int n1_1 = n1 - 1;
  int n2_1 = n2 - 1;
  int n1_2 = n1 - 2;
  int n2_2 = n2 - 2;

  // Parallelize the first loop using OpenMP
#pragma omp parallel for reduction(+:tempsum)
  for (int j = 0; j < n1; j++) {
    arma::rowvec y1_j = y1.row(j);
    for (int k = j + 1; k < n1; k++) {
      arma::rowvec y1_k = y1.row(k);
      arma::rowvec meanxiexjk = (y1sum - y1_j - y1_k) / n1_2;
      tempsum += arma::as_scalar(y1_k * (y1_j - meanxiexjk).t() * y1_j * (y1_k - meanxiexjk).t());
    }
  }

  double trsigma12hat = (tempsum + tempsum) / (n1 * n1_1);
  tempsum = 0.0;
  arma::rowvec y2sum = arma::sum(y2, 0); // add columns

  // Parallelize the second loop using OpenMP
#pragma omp parallel for reduction(+:tempsum)
  for (int j = 0; j < n2; j++) {
    arma::rowvec y2_j = y2.row(j);
    for (int k = j + 1; k < n2; k++) {
      arma::rowvec y2_k = y2.row(k);
      arma::rowvec meanxiexjk = (y2sum - y2_j - y2_k) / n2_2;
      tempsum += arma::as_scalar(y2_k * (y2_j - meanxiexjk).t() * y2_j * (y2_k - meanxiexjk).t());
    }
  }

  double trsigma22hat = (tempsum + tempsum) / (n2 * n2_1);
  tempsum = 0.0;

  // Parallelize the third loop using OpenMP
#pragma omp parallel for reduction(+:tempsum)
  for (int l = 0; l < n1; l++) {
    arma::rowvec y1_l = y1.row(l);
    arma::rowvec meanx1exl = (y1sum - y1_l) / n1_1;
    for (int k = 0; k < n2; k++) {
      arma::rowvec y2_k = y2.row(k);
      arma::rowvec meanx2exk = (y2sum - y2_k) / n2_1;
      tempsum += arma::as_scalar(y2_k * (y1_l - meanx1exl).t() * y1_l * (y2_k - meanx2exk).t());
    }
  }

  double trsigma1sigma2hat = tempsum / (n1 * n2);
  double sigman12hat = 2.0 / (n1 * n1_1) * trsigma12hat + 2.0 / (n2 * n2_1) * trsigma22hat + 4.0 / (n1 * n2) * trsigma1sigma2hat;
  double stat = Tn / sqrt(sigman12hat); // Qn

  arma::vec stats(2);
  stats(0) = stat;
  stats(1) = Tn;
  return stats;
}

// Test proposed by Zhang et al. (2020)
// [[Rcpp::export]]
arma::vec zgzc2020_ts_2cnrt_cpp(const arma::mat &y1, const arma::mat &y2) {
  int n1 = y1.n_rows;
  int n2 = y2.n_rows;
  int p = y1.n_cols;

  arma::rowvec mu1 = arma::mean(y1, 0);
  arma::mat z1 = y1.t() - arma::repmat(mu1.t(), 1, n1);
  arma::rowvec mu2 = arma::mean(y2, 0);
  arma::mat z2 = y2.t() - arma::repmat(mu2.t(), 1, n2);
  int n = n1 + n2;
  double stat = double(n1 * n2) / n * arma::as_scalar((mu1 - mu2) * (mu1 - mu2).t());

  arma::mat z = join_horiz(z1, z2);
  arma::mat S;
  if (p <= n) {
    S = (z * z.t()) / (n - 2); // tr(Sigmahat)
  } else {
    S = (z.t() * z) / (n - 2);
  }

  double B = arma::accu(S % S); // tr(Sigmahat^2)
  double A = arma::trace(S); // tr(Sigmahat)
  double uA0 = A;
  double uA = double((n - 1) * (n - 2)) / (n * (n - 3)) * (A * A - 2 * B / (n - 1)); // unbiased estimator for A^2
  double uB = double((n - 2) * (n - 2)) / (n * (n - 3)) * (B - A * A / (n - 2)); // unbiased estimator for B
  double beta = uB / uA0;
  double df = uA / uB;
  double statn = stat / beta; // normalizing
  double statstd = (stat - beta * df) / sqrt(2 * beta * beta * df);

  arma::vec stats(5);
  stats(0) = stat;
  stats(1) = statn;
  stats(2) = beta;
  stats(3) = df;
  stats(4) = statstd;
  return stats;
}

// Test proposed by Zhang et al. (2021).
// [[Rcpp::export]]
arma::vec zzgz2021_tsbf_2cnrt_cpp(const arma::mat &y1, const arma::mat &y2) {
  int n1 = y1.n_rows;
  int n2 = y2.n_rows;
  int p = y1.n_cols;

  arma::rowvec mu1 = arma::mean(y1, 0);
  arma::mat z1 = y1.t() - arma::repmat(mu1.t(), 1, n1);
  arma::rowvec mu2 = arma::mean(y2, 0);
  arma::mat z2 = y2.t() - arma::repmat(mu2.t(), 1, n2);
  int n = n1 + n2;
  double stat = double(n1 * n2) / n * arma::as_scalar((mu1 - mu2) * (mu1 - mu2).t());

  arma::mat S1, S2;
  double B1, B2, B12;
  if (p < n) {
    S1 = (z1 * z1.t()) / (n1 - 1);
    B1 = arma::accu(S1 % S1);
    S2 = (z2 * z2.t()) / (n2 - 1);
    B2 = arma::accu(S2 % S2);
    B12 = arma::trace(z1 * z1.t() * z2 * z2.t()) / ((n1 - 1) * (n2 - 1));
  } else {
    S1 = (z1.t() * z1) / (n1 - 1);
    B1 = arma::accu(S1 % S1);
    S2 = (z2.t() * z2) / (n2 - 1);
    B2 = arma::accu(S2 % S2);
    B12 = arma::trace(z1.t() * z2 * z2.t() * z1) / ((n1 - 1) * (n2 - 1));
  }

  double A1 = arma::trace(S1);
  double A2 = arma::trace(S2);
  double uA0 = (n2 * A1 + n1 * A2) / n; // tr(Sigma)
  double A12 = A1 * A2;
  double uA1 = double(n1 * (n1 - 1)) / ((n1 + 1) * (n1 - 2)) * (A1 * A1 - 2 * B1 / n1); // unbiased estimator for A1^2
  double uB1 = double((n1 - 1) * (n1 - 1)) / ((n1 + 1) * (n1 - 2)) * (B1 - A1 * A1 / (n1 - 1)); // unbiased estimator for B1
  double uA2 = double(n2 * (n2 - 1)) / ((n2 + 1) * (n2 - 2)) * (A2 * A2 - 2 * B2 / n2); // unbiased estimator for A2^2
  double uB2 = double((n2 - 1) * (n2 - 1)) / ((n2 + 1) * (n2 - 2)) * (B2 - A2 * A2 / (n2 - 1)); // unbiased estimator for B2

  double uA = (double(n2 * n2) / (n * n) * uA1 + 2 * double(n1 * n2) / (n * n) * A12 + double(n1 * n1) / (n * n) * uA2); // tr(Sigma)^2
  double uB = (double(n2 * n2) / (n * n) * uB1 + 2 * double(n1 * n2) / (n * n) * B12 + double(n1 * n1) / (n * n) * uB2); // tr(Sigma^2)

  double beta = uB / uA0;
  double df = uA / uB;
  double statn = stat / beta; // normalizing
  double statstd = (stat - beta * df) / sqrt(2 * beta * beta * df);

  arma::vec stats(5);
  stats(0) = stat;
  stats(1) = statn;
  stats(2) = beta;
  stats(3) = df;
  stats(4) = statstd;
  return stats;
}

// Test proposed by Zhang and Zhu (2022)
// [[Rcpp::export]]
arma::vec zz2022_ts_3cnrt_cpp(const arma::mat &y1, const arma::mat &y2) {
  int n1 = y1.n_rows;
  int n2 = y2.n_rows;
  int p = y1.n_cols;

  arma::rowvec mu1 = arma::mean(y1, 0);
  arma::mat z1 = y1.t() - arma::repmat(mu1.t(), 1, n1);
  arma::rowvec mu2 = arma::mean(y2, 0);
  arma::mat z2 = y2.t() - arma::repmat(mu2.t(), 1, n2);
  int n = n1 + n2;
  double invtau = double(n1 * n2) / n;
  double stat = arma::as_scalar((mu1 - mu2) * (mu1 - mu2).t());

  arma::mat z = join_horiz(z1, z2);
  arma::mat S;
  if (p <= n) {
    S = (z * z.t()) / (n - 2); // tr(Sigmahat)
  } else {
    S = (z.t() * z) / (n - 2);
  }

  double trSn = arma::trace(S); // tr(Sigmahat)
  double trSn2 = arma::accu(S % S); // tr(Sigmahat^2)
  double trSn3 = arma::trace(S * S * S); // tr(Sigmahat^3)
  double htrSn2 = pow((n - 2), 2) * (trSn2 - trSn * trSn / (n - 2)) / (n - 3) / n;
  double htrSn3 = pow((n - 2), 4) * (trSn3 - 3 * trSn * trSn2 / (n - 2) + 2 * pow(trSn, 3) / pow((n - 2), 2)) / (n * n - n - 6) / (n * (n - 4));

  double statn = invtau * stat - trSn;
  double beta0 = -(n - 1) * htrSn2 * htrSn2 / htrSn3 / (n - 3);
  double beta1 = (n - 3) * htrSn3 / htrSn2 / (n - 2);
  double d = (n - 1) * (n - 2) * pow(htrSn2, 3) / pow(htrSn3, 2) / pow((n - 3), 2);
  double statstd = statn / sqrt(2 * (n - 1) * htrSn2 / (n - 2));

  arma::vec stats(5);
  stats(0) = statn;
  stats(1) = beta0;
  stats(2) = beta1;
  stats(3) = d;
  stats(4) = statstd;
  return stats;
}

// Test proposed by Zhang and Zhu (2022)
// [[Rcpp::export]]
arma::vec zz2022_tsbf_3cnrt_cpp(const arma::mat &y1, const arma::mat &y2) {
  int n1 = y1.n_rows;
  int n2 = y2.n_rows;
  int p = y1.n_cols;

  arma::rowvec mu1 = arma::mean(y1, 0);
  arma::mat z1 = y1.t() - arma::repmat(mu1.t(), 1, n1);
  arma::rowvec mu2 = arma::mean(y2, 0);
  arma::mat z2 = y2.t() - arma::repmat(mu2.t(), 1, n2);
  int n = n1 + n2;
  arma::mat S1, S2;
  double B1, B2, B12, D12, D21;

  if (p < n) {
    S1 = (z1 * z1.t()) / (n1 - 1);
    B1 = arma::accu(S1 % S1);
    S2 = (z2 * z2.t()) / (n2 - 1);
    B2 = arma::accu(S2 % S2);
    B12 = arma::trace(z1 * z1.t() * z2 * z2.t()) / ((n1 - 1) * (n2 - 1));
    D12 = arma::trace(z1 * z1.t() * z1 * z1.t() * z2 * z2.t()) / ((n1 - 1) * (n1 - 1) * (n2 - 1));
    D21 = arma::trace(z1 * z1.t() * z2 * z2.t() * z2 * z2.t()) / ((n1 - 1) * (n2 - 1) * (n2 - 1));
  } else {
    S1 = (z1.t() * z1) / (n1 - 1);
    B1 = arma::accu(S1 % S1);
    S2 = (z2.t() * z2) / (n2 - 1);
    B2 = arma::accu(S2 % S2);
    B12 = arma::trace(z1.t() * z2 * z2.t() * z1) / ((n1 - 1) * (n2 - 1));
    D12 = arma::trace(z1.t() * z1 * z1.t() * z2 * z2.t() * z1) / ((n1 - 1) * (n1 - 1) * (n2 - 1));
    D21 = arma::trace(z1.t() * z2 * z2.t() * z2 * z2.t() * z1) / ((n1 - 1) * (n2 - 1) * (n2 - 1));
  }

  double A1 = arma::trace(S1);
  double A2 = arma::trace(S2);
  double stat = arma::as_scalar((mu1 - mu2) * (mu1 - mu2).t()) - (A1 / n1 + A2 / n2);

  double uB1 = double((n1 - 1) * (n1 - 1)) / ((n1 + 1) * (n1 - 2)) * (B1 - A1 * A1 / (n1 - 1));
  double uB2 = double((n2 - 1) * (n2 - 1)) / ((n2 + 1) * (n2 - 2)) * (B2 - A2 * A2 / (n2 - 1));

  double K2 = 2 * (uB1 / (n1 * (n1 - 1)) + 2 * B12 / (n1 * n2) + uB2 / (n2 * (n2 - 1)));

  double C1 = arma::trace(S1 * S1 * S1);
  double C2 = arma::trace(S2 * S2 * S2);

  double c1 = pow((n1 - 1), 4) / (n1 * n1 + n1 - 6) / (n1 * n1 - 2 * n1 - 3);
  double c2 = pow((n2 - 1), 4) / (n2 * n2 + n2 - 6) / (n2 * n2 - 2 * n2 - 3);

  double uC1 = c1 * (C1 - 3 * A1 * B1 / (n1 - 1) + 2 * pow(A1, 3) / ((n1 - 1) * (n1 - 1)));
  double uC2 = c2 * (C2 - 3 * A2 * B2 / (n2 - 1) + 2 * pow(A2, 3) / ((n2 - 1) * (n2 - 1)));

  double uD12 = (n1 - 1) * ((n1 - 1) * D12 - B12 * A1) / (n1 - 2) / (n1 + 1);
  double uD21 = (n2 - 1) * ((n2 - 1) * D21 - B12 * A2) / (n2 - 2) / (n2 + 1);

  double K3 = 8 * ((n1 - 2) * uC1 / pow(n1 * (n1 - 1), 2) + 3 * uD12 / (n1 * n1 * n2) + 3 * uD21 / (n1 * n2 * n2) + (n2 - 2) * uC2 / pow(n2 * (n2 - 1), 2));

  double beta0 = -2 * K2 * K2 / K3;
  double beta1 = K3 / (4 * K2);
  double d = 8 * pow(K2, 3) / (K3 * K3);

  double statstd = stat / sqrt(K2);

  arma::vec stats(5);
  stats(0) = stat;
  stats(1) = beta0;
  stats(2) = beta1;
  stats(3) = d;
  stats(4) = statstd;
  return stats;
}

// Test proposed by Zhu et al.(2023)
// [[Rcpp::export]]
arma::vec zwz2023_tsbf_2cnrt_cpp(const arma::mat &y1, const arma::mat &y2) {
  int n1 = y1.n_rows;
  int n2 = y2.n_rows;
  int p = y1.n_cols;

  arma::rowvec mu1 = arma::mean(y1, 0);
  arma::mat z1 = y1.t() - arma::repmat(mu1.t(), 1, n1);
  arma::rowvec mu2 = arma::mean(y2, 0);
  arma::mat z2 = y2.t() - arma::repmat(mu2.t(), 1, n2);
  int n = n1 + n2;
  arma::mat S1, S2;
  double B1, B2, B12;

  if (p < n) {
    S1 = (z1 * z1.t()) / (n1 - 1);
    B1 = arma::accu(S1 % S1);
    S2 = (z2 * z2.t()) / (n2 - 1);
    B2 = arma::accu(S2 % S2);
    B12 = arma::trace(z1 * z1.t() * z2 * z2.t()) / ((n1 - 1) * (n2 - 1));
  } else {
    S1 = (z1.t() * z1) / (n1 - 1);
    B1 = arma::accu(S1 % S1);
    S2 = (z2.t() * z2) / (n2 - 1);
    B2 = arma::accu(S2 % S2);
    B12 = arma::trace(z1.t() * z2 * z2.t() * z1) / ((n1 - 1) * (n2 - 1));
  }

  double A1 = arma::trace(S1);
  double A2 = arma::trace(S2);
  double stat = arma::as_scalar((mu1 - mu2) * (mu1 - mu2).t()) / (A1 / n1 + A2 / n2);

  double A12 = A1 * A2;
  double uA1 = double(n1 * (n1 - 1)) / ((n1 + 1) * (n1 - 2)) * (A1 * A1 - 2 * B1 / n1); // unbiased estimator for A1^2
  double uB1 = double((n1 - 1) * (n1 - 1)) / ((n1 + 1) * (n1 - 2)) * (B1 - A1 * A1 / (n1 - 1)); // unbiased estimator for B1
  double uA2 = double(n2 * (n2 - 1)) / ((n2 + 1) * (n2 - 2)) * (A2 * A2 - 2 * B2 / n2); // unbiased estimator for A2^2
  double uB2 = double((n2 - 1) * (n2 - 1)) / ((n2 + 1) * (n2 - 2)) * (B2 - A2 * A2 / (n2 - 1)); // unbiased estimator for B2

  double uA = uA1 / (n1 * n1) + 2 * A12 / (n1 * n2) + uA2 / (n2 * n2);
  double uB = uB1 / (n1 * n1) + 2 * B12 / (n1 * n2) + uB2 / (n2 * n2);
  double d1 = uA / uB;
  double uC = uB1 / (n1 * n1 * (n1 - 1)) + uB2 / (n2 * n2 * (n2 - 1));
  double d2 = uA / uC;

  arma::vec stats(3);
  stats(0) = stat;
  stats(1) = d1;
  stats(2) = d2;
  return stats;
}

// Two-sample scale-invariant tests
// Test proposed by Srivastava and Du (2008)
// [[Rcpp::export]]
arma::vec sd2008_ts_nabt_cpp(const arma::mat &y1, const arma::mat &y2) {
  int n1 = y1.n_rows;
  int n2 = y2.n_rows;
  int n = n1 + n2 - 2;
  int p = y1.n_cols;

  arma::rowvec bary1 = arma::mean(y1, 0);
  arma::rowvec bary2 = arma::mean(y2, 0);

  arma::mat x1 = y1.each_row() - bary1;
  arma::mat x2 = y2.each_row() - bary2;

  arma::vec diagSvec1 = arma::var(x1.t(), 0, 1); // variance of x1
  arma::vec diagSvec2 = arma::var(x2.t(), 0, 1); // variance of x2

  arma::mat x = join_horiz(x1.t(), x2.t());

  arma::vec sqrtdiagSvec = arma::sqrt((diagSvec1 * (n1 - 1) + diagSvec2 * (n2 - 1)) / n); // D^{-1/2}
  sqrtdiagSvec.elem(find(sqrtdiagSvec < pow(10, -10))).fill(pow(10, -10));
  arma::vec meandiff = (bary1.t() - bary2.t());
  meandiff.each_col() /= sqrtdiagSvec; // D^{-1/2}(bary1-bary2)

  double Tnp = double(n1 * n2) / (n1 + n2) * arma::dot(meandiff, meandiff) / p;
  x.each_col() /= sqrtdiagSvec;
  arma::mat R;
  double trR2;

  if (n1 < p || n2 < p) {
    R = (x.t() * x) / n; // tr(Sigmahat)
    trR2 = arma::accu(R % R);
  } else {
    R = (x * x.t()) / n;
    trR2 = arma::accu(R % R);
  }

  double cpn = 1 + trR2 / pow(sqrt(p), 3);
  double TSD = (p * Tnp - n * p / (n - 2)) / sqrt(2 * (trR2 - p * p / n) * cpn);

  arma::vec values(2);
  values(0) = TSD;
  values(1) = cpn;
  return values;
}

// Test proposed by Srivastava et al.(2013)
// [[Rcpp::export]]
arma::vec skk2013_tsbf_nabt_cpp(const arma::mat &y1, const arma::mat &y2) {
  int n1 = y1.n_rows;
  int n2 = y2.n_rows;
  int n = n1 + n2 - 2;
  int p = y1.n_cols;

  arma::rowvec bary1 = arma::mean(y1, 0);
  arma::rowvec bary2 = arma::mean(y2, 0);

  arma::mat x1 = y1.each_row() - bary1;
  arma::mat x2 = y2.each_row() - bary2;

  arma::vec diagSvec1 = arma::var(x1.t(), 0, 1); // variance of x1
  arma::vec diagSvec2 = arma::var(x2.t(), 0, 1); // variance of x2

  arma::vec sqrtdiagSvec = arma::sqrt((diagSvec1 * n2 + diagSvec2 * n1) / n); // D^{-1/2}
  sqrtdiagSvec.elem(find(sqrtdiagSvec < pow(10, -10))).fill(pow(10, -10));
  arma::vec meandiff = (bary1.t() - bary2.t());
  meandiff.each_col() /= sqrtdiagSvec; // D^{-1/2}(bary1-bary2)

  double Tnp = double(n1 * n2) / n * arma::dot(meandiff, meandiff) / p;
  arma::mat x1t = x1.t();
  arma::mat x2t = x2.t();
  arma::mat w1 = x1t.each_col() / sqrtdiagSvec;
  arma::mat w2 = x2t.each_col() / sqrtdiagSvec;
  arma::mat R1;
  arma::mat R2;
  double B1, B2, B12;

  if (n1 < p || n2 < p) {
    R1 = (w1.t() * w1) / (n1 - 1); // tr(Sigmahat)
    R2 = (w2.t() * w2) / (n2 - 1);
    B1 = arma::accu(R1 % R1);
    B2 = arma::accu(R2 % R2);
    B12 = arma::trace(w2.t() * w1 * w1.t() * w2) / ((n1 - 1) * (n2 - 1));
  } else {
    R1 = (w1 * w1.t()) / (n1 - 1); // tr(Sigmahat)
    R2 = (w2 * w2.t()) / (n2 - 1);
    B1 = arma::accu(R1 % R1);
    B2 = arma::accu(R2 % R2);
    B12 = arma::trace(w1 * w1.t() * w2 * w2.t()) / ((n1 - 1) * (n2 - 1));
  }

  arma::vec vecw1 = arma::vectorise(w1);
  arma::vec vecw2 = arma::vectorise(w2);
  double trinvDS1 = arma::dot(vecw1, vecw1) / (n1 - 1); // = arma::trace(invDs*S1)
  double trinvDS2 = arma::dot(vecw2, vecw2) / (n2 - 1); // = arma::trace(invDs*S2)

  double trR2 = (n2 * n2 * B1 + n1 * n1 * B2 + 2 * n1 * n2 * B12) / (n * n);
  double cpn = 1 + trR2 / pow(sqrt(p), 3);
  double sigma2 = ((n2 * n2 * trinvDS1 * trinvDS1) / (n1 - 1) + (n1 * n1 * trinvDS2 * trinvDS2) / (n2 - 1)) / (n * n);
  sigma2 = 2 * (trR2 - sigma2);

  double TSKK = (p * Tnp - p) / sqrt(sigma2 * cpn);

  arma::vec values(3);
  values(0) = TSKK;
  values(1) = sigma2;
  values(2) = cpn;
  return values;
}

// Test proposed by Zhang et al. (2020)
// [[Rcpp::export]]
arma::vec zzz2020_ts_2cnrt_cpp(const arma::mat &y1, const arma::mat &y2) {
  int n1 = y1.n_rows;
  int n2 = y2.n_rows;
  int n = n1 + n2 - 2;
  int p = y1.n_cols;

  arma::rowvec bary1 = arma::mean(y1, 0);
  arma::rowvec bary2 = arma::mean(y2, 0);

  arma::mat x1 = y1.each_row() - bary1;
  arma::mat x2 = y2.each_row() - bary2;

  arma::vec diagSvec1 = arma::var(x1.t(), 0, 1); // variance of x1
  arma::vec diagSvec2 = arma::var(x2.t(), 0, 1); // variance of x2

  arma::mat x = join_horiz(x1.t(), x2.t());

  arma::vec sqrtdiagSvec = arma::sqrt((diagSvec1 * (n1 - 1) + diagSvec2 * (n2 - 1)) / n); // D^{-1/2}
  sqrtdiagSvec.elem(find(sqrtdiagSvec < pow(10, -10))).fill(pow(10, -10));
  arma::vec meandiff = (bary1.t() - bary2.t());
  meandiff.each_col() /= sqrtdiagSvec; // D^{-1/2}(bary1-bary2)

  double Tnp = double(n1 * n2) / (n1 + n2) * arma::dot(meandiff, meandiff) / p;
  x.each_col() /= sqrtdiagSvec;
  arma::mat R;
  double trR2;

  if (n1 < p || n2 < p) {
    R = (x.t() * x) / n; // tr(Sigmahat)
    trR2 = arma::accu(R % R);
  } else {
    R = (x * x.t()) / n;
    trR2 = arma::accu(R % R);
  }

  double trR2hat = pow(n, 2) * (trR2 - pow(trace(R), 2) / n) / (n1 + n2) / (n - 1);
  double dhat = p * p / trR2hat;

  arma::vec values(2);
  values(0) = Tnp;
  values(1) = dhat;
  return values;
}

// Test proposed by Zhang et al. (2023)
// [[Rcpp::export]]
arma::vec zzz2023_tsbf_2cnrt_cpp(const arma::mat &y1, const arma::mat &y2) {
  int n1 = y1.n_rows;
  int n2 = y2.n_rows;
  int n = n1 + n2 - 2;
  int p = y1.n_cols;

  arma::rowvec bary1 = arma::mean(y1, 0);
  arma::rowvec bary2 = arma::mean(y2, 0);

  arma::mat x1 = y1.each_row() - bary1;
  arma::mat x2 = y2.each_row() - bary2;

  arma::vec diagSvec1 = arma::var(x1.t(), 0, 1); // variance of x1
  arma::vec diagSvec2 = arma::var(x2.t(), 0, 1); // variance of x2

  arma::vec sqrtdiagSvec = arma::sqrt((diagSvec1 * n2 + diagSvec2 * n1) / n); // D^{-1/2}
  sqrtdiagSvec.elem(find(sqrtdiagSvec < pow(10, -10))).fill(pow(10, -10));
  arma::vec meandiff = (bary1.t() - bary2.t());
  meandiff.each_col() /= sqrtdiagSvec; // D^{-1/2}(bary1-bary2)

  double Tnp = double(n1 * n2) / n * arma::dot(meandiff, meandiff) / p;
  arma::mat x1t = x1.t();
  arma::mat x2t = x2.t();
  arma::mat w1 = x1t.each_col() / sqrtdiagSvec;
  arma::mat w2 = x2t.each_col() / sqrtdiagSvec;
  arma::mat R1;
  arma::mat R2;
  double B1, B2, B12;

  if (n1 < p || n2 < p) {
    R1 = (w1.t() * w1) / (n1 - 1); // tr(Sigmahat)
    R2 = (w2.t() * w2) / (n2 - 1);
    B1 = arma::accu(R1 % R1);
    B2 = arma::accu(R2 % R2);
    B12 = arma::trace(w2.t() * w1 * w1.t() * w2) / ((n1 - 1) * (n2 - 1));
  } else {
    R1 = (w1 * w1.t()) / (n1 - 1); // tr(Sigmahat)
    R2 = (w2 * w2.t()) / (n2 - 1);
    B1 = arma::accu(R1 % R1);
    B2 = arma::accu(R2 % R2);
    B12 = arma::trace(w1 * w1.t() * w2 * w2.t()) / ((n1 - 1) * (n2 - 1));
  }

  double trR12 = (n1 - 1) * (n1 - 1) * (B1 - pow(trace(R1), 2) / (n1 - 1)) / (n1 - 2) / (n1 + 1);
  double trR22 = (n2 - 1) * (n2 - 1) * (B2 - pow(trace(R2), 2) / (n2 - 1)) / (n2 - 2) / (n2 + 1);

  double trR2 = (n2 * n2 * B1 + n1 * n1 * B2 + 2 * n1 * n2 * B12) / (n * n);
  double cpn = 1 + trR2 / pow(sqrt(p), 3);

  double trRn2 = (n2 * n2 * trR12 + n1 * n1 * trR22 + 2 * n1 * n2 * B12) / (n * n);
  double dhat = p * p / trRn2;

  arma::vec values(3);
  values(0) = Tnp;
  values(1) = dhat;
  values(2) = cpn;
  return values;
}

// One-way MANOVA
// Test proposed by Schott (2007)
// [[Rcpp::export]]
arma::vec s2007_ks_nabt_cpp(List Y, const arma::vec &n, int p) {
  int g = Y.size(); // number of classes
  int h = g - 1;
  int ss = sum(n);
  int e = ss - g;
  arma::mat ybar(p, g);

  // Precompute necessary values
  arma::vec index = arma::cumsum(n);
  arma::vec ind = arma::zeros(g + 1);
  std::copy(index.begin(), index.end(), ind.begin() + 1);
  arma::mat Ymat(ss, p, arma::fill::zeros);

  #pragma omp parallel for
  for (int i = 0; i < g; i++) {
    arma::mat yi = Y[i];
    Ymat.rows(ind[i], ind[i + 1] - 1) = yi;
    arma::colvec mu = arma::mean(yi.t(), 1);
    ybar.col(i) = mu;
  }

  arma::mat P(ss,ss,fill::zeros);
  arma::mat X(ss,g,fill::zeros);
  arma::vec index1 = arma::cumsum(n);

  arma::vec index0(index1.n_elem + 1);
  index0(0) = 0;
  index0.subvec(1, index1.n_elem) = index1;

  for(int i=0;i<g;++i){
    arma::mat I (n(i), n(i),arma::fill::ones);
    P.submat(index0(i), index0(i), (index0(i + 1)-1), (index0(i + 1)-1)) = I / n(i);
    arma::colvec v(n(i), arma::fill::ones);
    X.submat(index0(i),i,(index0(i + 1)-1),i) = v;
  }


  arma::mat identity = arma::eye(g - 1, g - 1);
  arma::mat column = -arma::ones<arma::mat>(g - 1, 1);
  arma::mat C = join_horiz(identity, column);
  arma::mat XtXinv = diagmat(1/n);
  arma::mat XtXinvCt = X*XtXinv*C.t();

  arma::mat Hmat = XtXinvCt*cholesky_inverse(C*XtXinv*C.t())*XtXinvCt.t();


  arma::mat H1 = arma::eye(ss, ss) - P;


  double trH, trE,trE2;
  if (p < ss) {
    arma::mat E = Ymat.t() * H1 * Ymat;
    arma::mat H = Ymat.t() * Hmat * Ymat;
    trH = arma::trace(H);
    trE= arma::trace(E);
    trE2 = arma::trace(E*E);
  }else{
    arma::mat E1 =  H1 * Ymat*Ymat.t();
    arma::mat H1 =  Hmat * Ymat*Ymat.t();
    trH = arma::trace(H1);
    trE= arma::trace(E1);
    trE2 = arma::trace(E1*E1);
  }


  double tnp = (trH / h - trE / e) / sqrt(ss - 1);
  double a = (trE2 - pow(trE, 2) / e) / (e + 2) / (e - 1);
  double sigmahat2 = 2 * a / e / h;
  double sigmahat = sqrt(sigmahat2);

  arma::vec stats(2);
  stats(0) = tnp;
  stats(1) = sigmahat;
  return stats;
}

// General linear hypothesis testing (GLHT) problem
// Test proposed by Fujikoshi et al. (2004)
// [[Rcpp::export]]
double fhw2004_glht_nabt_cpp(List Y, const arma::mat &X, const arma::mat &C, const arma::vec &n, int p) {
  int k = Y.size(); // number of classes
  int q = rank(C);
  int ss = sum(n);
  arma::mat Ymat(ss, p);

  arma::vec index = cumsum(n);
  arma::vec ind = zeros(k + 1);
  std::copy(index.begin(), index.end(), ind.begin() + 1);
  for (int i = 0; i < k; i++) {
    arma::mat yi = Y[i];
    Ymat.rows(ind[i], ind[i + 1] - 1) = yi;
  }

  arma::mat XtXinv = cholesky_inverse(X.t() * X);
  arma::mat H = X * XtXinv * C.t() * cholesky_inverse(C * XtXinv * C.t()) * C * XtXinv * X.t();

  arma::mat P = X * XtXinv * X.t();
  arma::mat I = eye(ss, ss);

  double trSh,trSe, trSe2;
  if (p < ss) {
    // Calculate Tnp
    trSh = arma::trace(Ymat.t() * H * Ymat);
    arma::mat Se = Ymat.t() * (I - P) * Ymat;
    trSe = arma::trace(Se);
    trSe2 = arma::trace(Se*Se);
  }else{
    trSh = arma::trace(H * Ymat*Ymat.t());
    arma::mat H1 = arma::eye(ss, ss) - P;
    arma::mat R1 = H1*Ymat*Ymat.t();
    trSe = arma::trace((I - P) * Ymat*Ymat.t());
    trSe2 = arma::trace(R1*R1);
  }

  double sigmaD = sqrt(2 * q * (trSe2 / pow(ss - k, 2) - pow(trSe, 2) / pow(ss - k, 3)) / p) / (trSe / (ss - k) / p);
  double TFHW = sqrt(p) * ((ss - k) * trSh / trSe - q) / sigmaD;
  return TFHW;
}

// Test proposed by Srivastava and Fujikoshi (2006)
// [[Rcpp::export]]
double sf2006_glht_nabt_cpp(List Y, const arma::mat &X, const arma::mat &C, const arma::vec &n, int p) {
  int k = Y.size(); // number of classes
  int q = rank(C);
  int ss = sum(n); // total sample size
  arma::mat Ymat(ss, p); // initialize Y matrix

  // Cumulative index for splitting Y
  arma::vec index = cumsum(n);
  arma::vec ind = arma::zeros(k + 1);
  std::copy(index.begin(), index.end(), ind.begin() + 1);

  // Fill Ymat with class data from list Y
  for (int i = 0; i < k; i++) {
    arma::mat yi = Y[i];
    Ymat.rows(ind[i], ind[i + 1] - 1) = yi;
  }

  // Apply regularization to XtX for numerical stability
  arma::mat XtXinv = cholesky_inverse(X.t() * X);
  arma::mat H = X * XtXinv * C.t() * cholesky_inverse(C * XtXinv * C.t()) * C * XtXinv * X.t();
  arma::mat P = X * XtXinv * X.t();
  arma::mat I = arma::eye(ss, ss); // Identity matrix

  double trSh, trSe, trSe2;

  if (p < ss) {
    // If the number of features is less than total samples
    trSh = arma::trace(Ymat.t() * H * Ymat);
    arma::mat Se = Ymat.t() * (I - P) * Ymat;
    trSe = arma::trace(Se);
    trSe2 = arma::trace(Se * Se);
  } else {
    // If the number of features is greater than or equal to total samples
    trSh = arma::trace(H * Ymat * Ymat.t());
    arma::mat H1 = I - P;
    arma::mat R1 = H1 * Ymat * Ymat.t();
    trSe = arma::trace((I - P) * Ymat * Ymat.t());
    trSe2 = arma::trace(R1 * R1);
  }

  // Compute a2 and TSF
  double a2 = (trSe2 - std::pow(trSe, 2) / (ss - k)) / (ss - k - 1) / (ss - k + 2) / p;
  double TSF = (trSh / std::sqrt(p) - q * trSe / std::sqrt(ss - k) / std::sqrt((ss - k) * p)) /
    std::sqrt(2 * q * a2 * (1 + q / (ss - k)));

  return TSF;
}


// Test proposed by Yamada and Srivastava (2012)
// [[Rcpp::export]]
arma::vec ys2012_glht_nabt_cpp(const Rcpp::List& Y, const arma::mat& X, const arma::mat& C, const arma::vec& n, int p) {
  int k = Y.size(); // number of classes
  int q = C.n_rows; // rank of C should be its row number
  int ss = arma::sum(n);

  // Precompute necessary values
  arma::vec index = arma::cumsum(n);
  arma::vec ind = arma::zeros(k + 1);
  std::copy(index.begin(), index.end(), ind.begin() + 1);

  arma::mat Ymat(ss, p, arma::fill::zeros);

  // Parallelize data filling for performance optimization
#pragma omp parallel for
  for (int i = 0; i < k; i++) {
    arma::mat yi = Y[i];
    Ymat.rows(ind[i], ind[i + 1] - 1) = yi;
  }

  // Compute XtX and its inverse using Cholesky decomposition with regularization
  arma::mat XtX = X.t() * X;
  arma::mat XtXinv = cholesky_inverse(XtX); // Regularization to avoid singular matrix

  // Precompute XtXinv * C.t() and C * XtXinv * C.t() * inv
  arma::mat XtXinvC = XtXinv * C.t();
  arma::mat invC_XtXinvC = cholesky_inverse(C * XtXinvC); // Regularization for stability

  // Compute H matrix
  arma::mat H = X * XtXinvC * invC_XtXinvC * XtXinvC.t() * X.t();

  // Calculate Sh and Se (Error and projection matrices)
  arma::mat Ymat_t = Ymat.t();
  arma::mat Sh = Ymat_t * H * Ymat;
  arma::mat P = X * XtXinv * X.t();
  arma::mat Se = Ymat_t * (arma::eye(ss, ss) - P) * Ymat;

  // Calculate Sigma and its inverse diagonal (if needed)
  arma::mat Sigma = Se / (ss - k);
  arma::vec Sigma_diag = Sigma.diag();

  // Prevent very small values in Sigma_diag to avoid NaN issues
  Sigma_diag = arma::clamp(Sigma_diag, 1e-10, arma::datum::inf);

  arma::vec invSigma_diag = 1 / Sigma_diag;
  arma::mat invD = arma::diagmat(invSigma_diag);

  double Tnp, trRhat2, TYS;

  // Different paths based on p and ss
  if (p < ss) {
    // Path for p < ss
    Tnp = arma::trace(Sh.each_col() % invSigma_diag) / (p * q);
    arma::mat invDSigma = invD * Sigma;
    trRhat2 = arma::trace(invDSigma * invDSigma);
  } else {
    // Path for p >= ss
    Tnp = arma::trace(H * Ymat * invD * Ymat_t) / (p * q);
    arma::mat H1 = arma::eye(ss, ss) - P;
    arma::mat R1 = H1 * Ymat * invD * Ymat_t;
    trRhat2 = arma::trace(R1 * R1) / ((ss - k) * (ss - k));
  }

  // Calculate cpn based on trRhat2 (common part)
  double cpn = 1 + trRhat2 / sqrt(pow(p, 3));

  // Calculate TYS statistic (using trRhat2 and cpn)
  TYS = (p * q * Tnp - (ss - k) * p * q / (ss - k - 2)) / sqrt(2 * q * (trRhat2 - p * p / (ss - k)) * cpn);

  // Return the results
  arma::vec values(2);
  values(0) = TYS;
  values(1) = cpn;
  return values;
}

// Test proposed by Zhou et al. (2017)
// [[Rcpp::export]]
arma::vec zgz2017_glhtbf_nabt_cpp(List Y, const arma::mat &tG, const arma::vec &n, int p) {
  int k = Y.size(); // number of classes
  int ss = sum(n);
  arma::mat D = diagmat(1 / n);
  arma::mat H = tG.t() * cholesky_inverse(tG * D * tG.t()) * tG;
  arma::mat hatM(p, k);
  double trOmegan = 0;
  arma::vec A(k);
  arma::vec B(k);
  arma::vec Q(k, fill::zeros);
  arma::mat Bij(k, k);

  if (p < ss) {
    for (int i = 0; i < k; ++i) {
      arma::mat y1 = Y[i];
      arma::mat yi = y1.t();
      arma::colvec mui = arma::mean(yi, 1);
      hatM.col(i) = mui;
      arma::mat zi = yi - arma::repmat(mui, 1, n(i));
      arma::mat Si = zi * zi.t() / (n(i) - 1);
      A(i) = arma::trace(Si); // tr(hbSigma_i)
      B(i) = arma::accu(Si % Si); // tr(hbSigma_i^2)
      for (int f = 0; f < n(i); ++f) {
        double cXtcX = arma::as_scalar(zi.col(f).t() * zi.col(f));
        Q(i) += cXtcX * cXtcX;
      }
      Q(i) = Q(i) / (n(i) - 1);
      trOmegan += H(i, i) * A(i) / n(i);
      for (int j = 0; j < k; ++j) {
        arma::mat y2 = Y[j];
        arma::mat yj = y2.t();
        arma::colvec muj = arma::mean(yj, 1);
        arma::mat zj = yj - arma::repmat(muj, 1, n(j));
        Bij(i, j) = arma::trace(zi * zi.t() * zj * zj.t()) / ((n(i) - 1) * (n(j) - 1)); // tr(hbSigma1*hbSigma2)
      }
    }
  } else {
    for (int i = 0; i < k; ++i) {
      arma::mat y1 = Y[i];
      arma::mat yi = y1.t();
      arma::colvec mui = arma::mean(yi, 1);
      hatM.col(i) = mui;
      arma::mat zi = yi - arma::repmat(mui, 1, n(i));
      arma::mat Si = zi.t() * zi / (n(i) - 1);
      A(i) = arma::trace(Si); // tr(hbSigma_i)
      B(i) = arma::accu(Si % Si); // tr(hbSigma_i^2)
      for (int f = 0; f < n(i); ++f) {
        double cXtcX = arma::as_scalar(zi.col(f).t() * zi.col(f));
        Q(i) += cXtcX * cXtcX;
      }
      Q(i) = Q(i) / (n(i) - 1);
      trOmegan += H(i, i) * A(i) / n(i);
      for (int j = 0; j < k; ++j) {
        arma::mat y2 = Y[j];
        arma::mat yj = y2.t();
        arma::colvec muj = arma::mean(yj, 1);
        arma::mat zj = yj - arma::repmat(muj, 1, n(j));
        Bij(i, j) = arma::trace(zi.t() * zj * zj.t() * zi) / ((n(i) - 1) * (n(j) - 1));
      }
    }
  }

  double Tnp = trace(hatM * H * hatM.t()) - trOmegan;
  arma::vec uB(k, fill::zeros); // unbiased estimates of tr(bSigma_i^2)
  double K2s1 = 0;
  arma::mat K2s2mat(k, k);

  for (int i = 0; i < k; ++i) {
    uB(i) = (n(i) - 1) * ((n(i) - 1) * (n(i) - 2) * B(i) + A(i) * A(i) - n(i) * Q(i)) / (n(i) * (n(i) - 2) * (n(i) - 3));
    K2s1 += H(i, i) * H(i, i) * uB(i) / (n(i) * (n(i) - 1));
    for (int j = 0; j < k; ++j) {
      K2s2mat(i, j) = H(i, j) * H(i, j) * Bij(i, j) / (n(i) * n(j));
    }
  }

  double K2s2 = accu(K2s2mat) - sum(K2s2mat.diag());
  double sigma2 = 2 * (K2s1 + K2s2);

  arma::vec stats(2);
  stats(0) = Tnp;
  stats(1) = sigma2;
  return stats;
}

// Test proposed by Zhang et al. (2017)
// [[Rcpp::export]]
arma::vec zgz2017_glht_2cnrt_cpp(List Y, const arma::mat &tG, const arma::vec &n, int p) {
  int k = Y.size(); // number of classes
  int q = rank(tG);
  int ss = sum(n);
  arma::mat D = diagmat(1 / n);
  arma::mat H = tG.t() * cholesky_inverse(tG * D * tG.t()) * tG;
  arma::mat hatM(p, k);

  // Precompute necessary values
  arma::vec index = arma::cumsum(n);
  arma::vec ind = arma::zeros(k + 1);
  std::copy(index.begin(), index.end(), ind.begin() + 1);
  arma::mat Ymat(ss, p, arma::fill::zeros);

#pragma omp parallel for
  for (int i = 0; i < k; i++) {
    arma::mat yi = Y[i];
    Ymat.rows(ind[i], ind[i + 1] - 1) = yi;
    arma::colvec mu = arma::mean(yi.t(), 1);
    hatM.col(i) = mu;
  }


  arma::mat P(ss,ss,fill::zeros);
  arma::vec index1 = arma::cumsum(n);

  arma::vec index0(index1.n_elem + 1);
  index0(0) = 0;
  index0.subvec(1, index1.n_elem) = index1;

  for(int i=0;i<k;++i){
    arma::mat I (n(i), n(i),arma::fill::ones);
    P.submat(index0(i), index0(i), (index0(i + 1)-1), (index0(i + 1)-1)) = I / n(i);
  }

  arma::mat H1 = arma::eye(ss, ss) - P;

  double Tnp, trSn,trSn2;
  if (p < ss) {
    // Calculate Tnp
    Tnp = trace(hatM * H * hatM.t());
    // Calculate trRhat2 and trR2
    arma::mat S = Ymat.t() * H1 * Ymat/(ss-k);
    trSn= arma::trace(S);
    trSn2 = arma::trace(S*S);
  }else{
    Tnp = trace(H * hatM.t()*hatM);
    arma::mat S1 =  H1 * Ymat*Ymat.t()/(ss-k);
    trSn= arma::trace(S1);
    trSn2 = arma::trace(S1*S1);
  }
  double uA = (ss - k) * (ss - k + 1) * (pow(trSn, 2) - 2 * trSn2 / (ss - k + 1)) / (ss - k - 1) / (ss - k + 2);
  double uB = pow(ss - k, 2) * (trSn2 - pow(trSn, 2) / (ss - k)) / (ss - k - 1) / (ss - k + 2);

  double beta = uB / trSn;
  double d = q * uA / uB;
  arma::vec stats(4);
  stats(0) = Tnp;
  stats(1) = beta;
  stats(2) = d;
  return stats;
}

// Test proposed by Zhang et al. (2022)
// [[Rcpp::export]]
arma::vec zzg2022_glhtbf_2cnrt_cpp(List Y, const arma::mat &tG, const arma::vec &n, int p) {
  int k = Y.size(); // number of classes
  int ss = sum(n);
  arma::mat D = diagmat(1 / n);
  arma::mat D2 = diagmat(1 / sqrt(n));
  arma::mat H = tG.t() * cholesky_inverse(tG * D * tG.t()) * tG;
  arma::mat hatM(p, k);
  arma::mat Amat = D2 * H * D2;
  arma::vec A(k);
  arma::vec B(k);
  arma::mat Bij(k, k);

  if (p < ss) {
    for (int i = 0; i < k; ++i) {
      arma::mat y1 = Y[i];
      arma::mat yi = y1.t();
      arma::colvec mui = arma::mean(yi, 1);
      hatM.col(i) = mui;
      arma::mat zi = yi - arma::repmat(mui, 1, n(i));
      arma::mat Si = zi * zi.t() / (n(i) - 1);
      A(i) = arma::trace(Si); // tr(hbSigma_i)
      B(i) = arma::accu(Si % Si); // tr(hbSigma_i^2)
      for (int j = 0; j < k; ++j) {
        arma::mat y2 = Y[j];
        arma::mat yj = y2.t();
        arma::colvec muj = arma::mean(yj, 1);
        arma::mat zj = yj - arma::repmat(muj, 1, n(j));
        Bij(i, j) = arma::trace(zi * zi.t() * zj * zj.t()) / ((n(i) - 1) * (n(j) - 1)); // tr(hbSigma1*hbSigma2)
      }
    }
  } else {
    for (int i = 0; i < k; ++i) {
      arma::mat y1 = Y[i];
      arma::mat yi = y1.t();
      arma::colvec mui = arma::mean(yi, 1);
      hatM.col(i) = mui;
      arma::mat zi = yi - arma::repmat(mui, 1, n(i));
      arma::mat Si = zi.t() * zi / (n(i) - 1);
      A(i) = arma::trace(Si); // tr(hbSigma_i)
      B(i) = arma::accu(Si % Si); // tr(hbSigma_i^2)
      for (int j = 0; j < k; ++j) {
        arma::mat y2 = Y[j];
        arma::mat yj = y2.t();
        arma::colvec muj = arma::mean(yj, 1);
        arma::mat zj = yj - arma::repmat(muj, 1, n(j));
        Bij(i, j) = arma::trace(zi.t() * zj * zj.t() * zi) / ((n(i) - 1) * (n(j) - 1));
      }
    }
  }

  double Tnp = trace(hatM * H * hatM.t());

  arma::vec uB(k); // unbiased estimates of tr(bSigma_i^2)
  arma::vec uC(k); // unbiased estimates of tr^2(bSigma_i)
  arma::mat S1(k, k);
  arma::mat S2(k, k);
  double trOmegan = 0;
  double tr2Omegans1 = 0;
  double trOmegan2s1 = 0;

  for (int i = 0; i < k; ++i) {
    uB(i) = (n(i) - 1) * (n(i) - 1) * (B(i) - A(i) * A(i) / (n(i) - 1)) / ((n(i) - 2) * (n(i) + 1));
    uC(i) = n(i) * (n(i) - 1) * (A(i) * A(i) - 2 * B(i) / n(i)) / ((n(i) - 2) * (n(i) + 1));
    trOmegan += Amat(i, i) * A(i);
    tr2Omegans1 += Amat(i, i) * Amat(i, i) * uC(i);
    trOmegan2s1 += Amat(i, i) * Amat(i, i) * uB(i);
    for (int j = 0; j < k; ++j) {
      S1(i, j) = Amat(i, i) * Amat(j, j) * A(i) * A(j);
      S2(i, j) = Amat(i, j) * Amat(i, j) * Bij(i, j);
    }
  }

  double tr2Omegan = tr2Omegans1 + accu(S1) - sum(S1.diag());
  double trOmegan2 = trOmegan2s1 + accu(S2) - sum(S2.diag());
  double beta = trOmegan2 / trOmegan;
  double d = tr2Omegan / trOmegan2;

  arma::vec stats(3);
  stats(0) = Tnp;
  stats(1) = beta;
  stats(2) = d;
  return stats;
}

// Test proposed by Zhang and Zhu (2022)
// [[Rcpp::export]]
arma::vec zz2022_glht_3cnrt_cpp(List Y, const arma::mat &tG, const arma::vec &n, int p) {
  int k = Y.size(); // number of classes
  int q = rank(tG);
  int ss = sum(n);
  arma::mat D = diagmat(1 / n);
  arma::mat H = tG.t() * cholesky_inverse(tG * D * tG.t()) * tG;
  arma::mat hatM(p, k);
  arma::mat z;
  arma::mat S;

  for (int i = 0; i < k; ++i) {
    arma::mat yi = Y[i];
    arma::mat y = yi.t();
    arma::colvec mu = arma::mean(y, 1);
    hatM.col(i) = mu;
    z.insert_cols(0, y - arma::repmat(mu, 1, n(i)));
  }

  if (p < ss) {
    S = z * z.t() / (ss - k);
  } else {
    S = z.t() * z / (ss - k);
  }

  double Tnp = trace(hatM * H * hatM.t()) - q * trace(S);

  double trSn = arma::trace(S); // tr(Sigmahat^2)
  double trSn2 = arma::accu(S % S); // tr(Sigmahat^2) ##element-wise multiplication
  double trSn3 = arma::trace(S * S * S); // tr(Sigmahat^3)
  double htrSn2 = pow((ss - k), 2) * (trSn2 - trSn * trSn / (ss - k)) / (ss - k - 1) / (ss - k + 2);
  double htrSn3 = pow((ss - k), 4) * (trSn3 - 3 * trSn * trSn2 / (ss - k) + 2 * pow(trSn, 3) / pow((ss - k), 2)) / (pow(ss - k, 2) + 2 * (ss - k) - 3) / (pow(ss - k, 2) - 4);

  double beta0 = -q * (ss - k + q) * htrSn2 * htrSn2 / htrSn3 / (ss - k - q);
  double beta1 = (ss - k - q) * htrSn3 / htrSn2 / (ss - k);
  double d = q * (ss - k) * (ss - k + q) * pow(htrSn2, 3) / pow(htrSn3, 2) / pow((ss - k - q), 2);

  arma::vec stats(4);
  stats(0) = Tnp;
  stats(1) = beta0;
  stats(2) = beta1;
  stats(3) = d;
  return stats;
}

//Test proposed by Zhang and Zhu (2022)
// [[Rcpp::export]]
arma::vec zz2022_glhtbf_3cnrt_cpp(const Rcpp::List& Y, const arma::mat& tG, const arma::vec& n, int p) {
  int k = Y.size(); // number of classes
  int ss = sum(n);
  arma::mat D = diagmat(1/n);
  arma::mat H = tG.t() * cholesky_inverse(tG * D * tG.t()) * tG;
  arma::mat hatM(p, k);
  double trOmegan = 0;
  arma::vec A(k), B(k), C(k);
  arma::mat Bij(k, k), Dij(k, k);

  // Parallelize the computation for each class
#pragma omp parallel for reduction(+:trOmegan)
  for (int i = 0; i < k; ++i) {
    arma::mat y1 = Y[i];
    arma::mat yi = y1.t();
    arma::colvec mui = arma::mean(yi, 1);
    hatM.col(i) = mui;
    arma::mat zi = yi - arma::repmat(mui, 1, n(i));

    arma::mat Si;
    if (p < ss) {
      Si = zi * zi.t() / (n[i] - 1);
    } else {
      Si = zi.t() * zi / (n[i] - 1);
    }

    A(i) = arma::trace(Si); // tr(hbSigma_i)
    B(i) = arma::accu(Si % Si); // tr(hbSigma_i^2)
    C(i) = arma::trace(Si * Si * Si); // tr(hbSigma_i^3)
    trOmegan += H(i, i) * A(i) / n(i);

#pragma omp parallel for
    for (int j = 0; j < k; ++j) {
      arma::mat y2 = Y[j];
      arma::mat yj = y2.t();
      arma::colvec muj = arma::mean(yj, 1);
      arma::mat zj = yj - arma::repmat(muj, 1, n(j));

      if (p < ss) {
        Bij(i, j) = arma::trace(zi * zi.t() * zj * zj.t()) / ((n(i) - 1) * (n(j) - 1));
        Dij(i, j) = arma::trace(zi * zi.t() * zi * zi.t() * zj * zj.t()) / ((n(i) - 1) * (n(i) - 1) * (n(j) - 1));
      } else {
        Bij(i, j) = arma::trace(zi.t() * zj * zj.t() * zi) / ((n(i) - 1) * (n(j) - 1));
        Dij(i, j) = arma::trace(zi.t() * zi * zi.t() * zj * zj.t() * zi) / ((n(i) - 1) * (n(i) - 1) * (n(j) - 1));
      }
    }
  }

  double Tnp = trace(hatM * H * hatM.t()) - trOmegan;

  arma::vec uB(k), uC(k); // unbiased estimates of tr(bSigma_i^2) and tr(bSigma_i^3)
  uB.zeros();
  uC.zeros();
  double K2s1 = 0, K3s1 = 0, K3s3 = 0;

  // Parallelize the computation for uB and uC
#pragma omp parallel for reduction(+:K2s1, K3s1)
  for (int i = 0; i < k; ++i) {
    double c = pow((n(i) - 1), 4) / ((n(i) * n(i) + n(i) - 6) * (n(i) * n(i) - 2 * n(i) - 3));
    uB(i) = (n(i) - 1) * (n(i) - 1) * (B(i) - A(i) * A(i) / (n(i) - 1)) / ((n(i) - 2) * (n(i) + 1));
    uC(i) = c * (C(i) - 3 * A(i) * B(i) / (n(i) - 1) + 2 * pow(A(i), 3) / ((n(i) - 1) * (n(i) - 1)));
    K2s1 += H(i, i) * H(i, i) * uB(i) / (n(i) * (n(i) - 1));
    K3s1 += pow(H(i, i), 3) * (n(i) - 2) * uC(i) / (n(i) * n(i) * (n(i) - 1) * (n(i) - 1));
  }

  arma::mat K2s2mat(k, k), K3s2mat(k, k);

  // Parallelize the computation for K2s2mat and K3s2mat
#pragma omp parallel for
  for (int i = 0; i < k; ++i) {
    for (int j = 0; j < k; ++j) {
      double Cij = (n(i) - 1) * ((n(i) - 1) * Dij(i, j) - Bij(i, j) * A(i)) / ((n(i) - 2) * (n(i) + 1));
      K2s2mat(i, j) = H(i, j) * H(i, j) * Bij(i, j) / (n(i) * n(j));
      K3s2mat(i, j) = H(i, i) * H(i, j) * H(i, j) * Cij / (n(i) * n(i) * n(j));
    }
  }
  double K2s2 = accu(K2s2mat) - sum(K2s2mat.diag());
  double K3s2 = accu(K3s2mat) - sum(K3s2mat.diag());

  // Parallelize the computation for K3s3
#pragma omp parallel for reduction(+:K3s3)
  for (int i = 2; i < k; ++i) {
    for (int j = 1; j < i; ++j) {
      for (int r = 0; r < j; ++r) {
        arma::mat y3 = Y[i], y4 = Y[j], y5 = Y[r];
        arma::mat yi = y3.t(), yj = y4.t(), yr = y5.t();
        arma::mat zi = yi - arma::repmat(hatM.col(i), 1, n(i));
        arma::mat zj = yj - arma::repmat(hatM.col(j), 1, n(j));
        arma::mat zr = yr - arma::repmat(hatM.col(r), 1, n(r));
        if (p < ss) {
          K3s3 += H(i, j) * H(j, r) * H(r, i) * arma::trace(zi * zi.t() * zj * zj.t() * zr * zr.t()) / ((n(i) - 1) * (n(j) - 1) * (n(r) - 1)) / (n(i) * n(j) * n(r));
        } else {
          K3s3 += H(i, j) * H(j, r) * H(r, i) * arma::trace(zi.t() * zj * zj.t() * zr * zr.t() * zi) / ((n(i) - 1) * (n(j) - 1) * (n(r) - 1)) / (n(i) * n(j) * n(r));
        }
      }
    }
  }

  double K2 = 2 * (K2s1 + K2s2);
  double K3 = 8 * (K3s1 + 3 * K3s2 + 6 * K3s3);

  double beta0 = -2 * K2 * K2 / K3;
  double beta1 = K3 / (4 * K2);
  double d = 8 * K2 * K2 * K2 / (K3 * K3);

  arma::vec stats(4);
  stats(0) = Tnp;
  stats(1) = beta0;
  stats(2) = beta1;
  stats(3) = d;
  return stats;
}


// Test proposed by Zhang and Zhu (2022)
// [[Rcpp::export]]
arma::vec zzz2022_glht_2cnrt_cpp(const Rcpp::List& Y, const arma::mat& X, const arma::mat& C, const arma::vec& n, int p) {
  int k = Y.size(); // number of classes
  int q = C.n_rows; // rank of C should be its row number
  int ss = arma::sum(n);

  // Precompute necessary values
  arma::vec index = arma::cumsum(n);
  arma::vec ind = arma::zeros(k + 1);
  std::copy(index.begin(), index.end(), ind.begin() + 1);

  arma::mat Ymat(ss, p, arma::fill::zeros);

  // Fill Ymat with data
#pragma omp parallel for
  for (int i = 0; i < k; i++) {
    arma::mat yi = Y[i];
    Ymat.rows(ind[i], ind[i + 1] - 1) = yi;
  }

  // Calculate XtXinv using Cholesky decomposition
  arma::mat XtX = X.t() * X;
  arma::mat XtXinv = cholesky_inverse(XtX); // Regularization removed

  // Precompute XtXinv * C.t() and C * XtXinv * C.t() * inv
  arma::mat XtXinvC = XtXinv * C.t();
  arma::mat invC_XtXinvC = cholesky_inverse(C * XtXinvC); // Regularization removed

  // Compute H matrix
  arma::mat H = X * XtXinvC * invC_XtXinvC * XtXinvC.t() * X.t();

  // Calculate Sh and Se
  arma::mat Ymat_t = Ymat.t();
  arma::mat Sh = Ymat_t * H * Ymat;
  arma::mat P = X * XtXinv * X.t();
  arma::mat Se = Ymat_t * (arma::eye(ss, ss) - P) * Ymat;

  // Calculate Sigma and its inverse diagonal
  arma::mat Sigma = Se / (ss - k);
  arma::vec Sigma_diag = Sigma.diag();
  Sigma_diag = arma::clamp(Sigma_diag, 1e-10, arma::datum::inf);// Prevent very small values
  arma::vec invSigma_diag = 1 / Sigma_diag;
  arma::mat invD = arma::diagmat(invSigma_diag);

  double Tnp, trRhat2;
  if (p < ss) {
    // Calculate Tnp
    Tnp = arma::trace(Sh.each_col() % invSigma_diag) / (p * q);
    // Calculate trRhat2 and trR2
    arma::mat invDSigma = invD * Sigma;
    trRhat2 = arma::trace(invDSigma * invDSigma);
  } else {
    Tnp = arma::trace(H * Ymat * invD * Ymat_t) / (p * q);
    arma::mat H1 = arma::eye(ss, ss) - P;
    arma::mat R1 = H1 * Ymat * invD * Ymat_t;
    trRhat2 = arma::trace(R1 * R1) / ((ss - k) * (ss - k));
  }

  // Calculate trR2 and ensure it is positive
  double trR2 = (ss - k) * (ss - k) * (trRhat2 - p * p / (ss - k)) / ((ss - k - 1) * (ss - k + 2));
  if (trR2 <= 1e-6) {
    double adjustment = std::max(1e-6, 0.01 * fabs(trRhat2));
    Rcpp::Rcout << "Warning: trR2 is very small or non-positive. Applying adaptive adjustment.\n";
    trR2 += adjustment; // Apply adaptive adjustment to ensure trR2 is sufficiently positive
  }



  // Calculate degrees of freedom and ensure it is positive
  double hatd = p * p * q / trR2;
  if (hatd <= 1e-6) {
    Rcpp::Rcout << "Warning: Degrees of freedom (hatd) is non-positive. Applying adjustment.\n";
    hatd = std::max(1e-6, 0.01 * fabs(hatd)); // Ensure hatd is positive, with an adaptive adjustment
  }


  // Return the results
  arma::vec values(2);
  values(0) = Tnp;
  values(1) = hatd;
  return values;
}


