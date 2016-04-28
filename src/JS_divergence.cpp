#include <Rcpp.h>
using namespace Rcpp;

// underflow prevention: if the argument to log is so small
// we get -Inf, we just give back 0.
inline double xlogy(double x, double y) {
    double lg = log(y);
    return (lg == R_NegInf) ? 0 : x * lg;
}

// [[Rcpp::export]]
double jsdiv_v(NumericVector P, NumericVector Q) {
    int n = P.size();
    if (Q.size() != n) {
        stop("P and Q must be of same length");
    }
    
    double total = 0;
    double PQ_mean;
    for (int i = 0; i < n; i++) {
        PQ_mean = (P[i] + Q[i]) / 2;
        if (P[i] != 0) {
            total += xlogy(P[i], P[i] / PQ_mean);
        }
        if (Q[i] != 0) {
            total += xlogy(Q[i], Q[i] / PQ_mean);
        }
    }
    return total / 2;
}

// [[Rcpp::export]]
NumericMatrix jsdiv_m(NumericMatrix x, NumericMatrix y) {
    int n = x.nrow(), m = y.nrow();
    if (y.ncol() != x.ncol()) {
        stop("x and y must have the same number of columns");
    }

    NumericMatrix result(n, m);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            result(i, j) = jsdiv_v(x(i, _), y(j, _));
        }
    }
    return result;
}

// TODO an RcppEigen version for sparse matrices would be nice
