#include <Rcpp.h>
using namespace Rcpp;

//' Jensen-Shannon divergence between two vectors
//'
//' This function computes the Jensen-Shannon divergence between two vectors,
//' understood as distributions over the index.
//'
//' @param P,Q vectors representing the distributions. Must be of same length.
//'
//' @return \deqn{\sum_j \frac{1}{2}P(j) log\left(\frac{2P(j)}{P(j) +
//' Q(j)}\right) + \frac{1}{2}Q(j) log\left(\frac{2P(j)}{P(j) +
//' Q(j)}\right)}
//'
//' @seealso \code{\link{topic_divergences}}, \code{\link{row_dists}}
//'
//' @export
// [[Rcpp::export]]
double JS_divergence(NumericVector P, NumericVector Q) {
    int n = P.size();
    if (Q.size() != n) {
        stop("P and Q must be of same length");
    }
    
    double total = 0;
    double PQ_mean;
    for (int i = 0; i < n; i++) {
        PQ_mean = (P[i] + Q[i]) / 2;
        if (P[i] != 0) {
            total += P[i] * log(P[i] / PQ_mean);
        }
        if (Q[i] != 0) {
            total += Q[i] * log(Q[i] / PQ_mean);
        }
    }
    return total / 2;
}
