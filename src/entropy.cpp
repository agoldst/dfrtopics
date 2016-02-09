#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

// Calculate entropies of the rows of a sparse matrix. Wrapped by R function
// row_entropies().
//
// [[Rcpp::export]]
NumericVector calc_row_entropies(const Eigen::MappedSparseMatrix<double> m) {
    NumericVector result(m.rows());
    // TODO parallelize
    for (int j = 0; j < m.outerSize(); ++j) {
        for (Eigen::MappedSparseMatrix<double>::InnerIterator iter(m, j);
                iter; ++iter) {
            result[iter.row()] += iter.value() * log2(iter.value());
        }
    }
    return -result;
}

//' Entropy of a vector
//'
//' This function computes the entropy of a vector, understood as a discrete
//' distribution over the index.
//'
//' @param x vector representing the distribution. Not checked to see whether it is properly normalized.
//'
//' @return \eqn{sum_i x_i \log x_i}, where the log is base 2 and the sum is taken only over non-zero elements of \eqn{x}.
//'
//' @export
// [[Rcpp::export]] 
double entropy(NumericVector x) {
    double total = 0;
    for (int i = 0; i < x.size(); i++) {
        if (x[i] != 0) {
            total += x[i] * log2(x[i]);
        }
    }
    return -total;
}


