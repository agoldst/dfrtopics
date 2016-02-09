#include <Rcpp.h>

using namespace Rcpp;

void multinom_sparse_trial(int N, const std::vector<double> p,
        std::vector<std::vector<int> >& result);

// [[Rcpp::export]]
List draw_multinom(const IntegerVector nn, const NumericVector probs) {
    // p := probs, normalized to sum to 1
    double p_sum = sum(probs);
    std::vector<double> p = as<std::vector<double> >(probs);
    int last_nz = 0;
    for (std::vector<double>::iterator it = p.begin();
            it != p.end(); ++it) {
        if (*it != 0) {
            *it /= p_sum;
            last_nz = std::distance(p.begin(), it);
        }
    }
    // and truncate to the last non-zero entry
    p.resize(last_nz + 1);

    std::vector<int> i;     // sparse matrix row (word) indices
    std::vector<int> x;     // sparse matrix values (weights)
    std::vector<int> col_p(nn.size() + 1); // column pointer
    col_p[0] = 0;

    std::vector<std::vector<int> > trial(2);
    for (int j = 0; j < nn.size(); ++j) {
        if (nn[j] != 0) {
            multinom_sparse_trial(nn[j], p, trial);
            i.insert(i.end(), trial[0].begin(), trial[0].end());
            x.insert(x.end(), trial[1].begin(), trial[1].end());
            col_p[j + 1] = col_p[j] + trial[0].size();
        } else {
            // a column of zeroes
            col_p[j + 1] = col_p[j];
        }
    }
    return List::create(
        _["i"] = i,
        _["p"] = col_p,
        _["x"] = x
    );
}

// Closely based on src/nmath/rmultinom.c in the R source
// found at https://github.com/wch/r-source/

// TODO should we keep track of p_total in a long double as in rmultinom.c?
void multinom_sparse_trial(int N, const std::vector<double> p,
        std::vector<std::vector<int> >& result) {
    result[0].clear();
    result[1].clear();

    if (N <= 0) {
        return;
    }

    double p_left = 1.0;
    double n_left = N;
    double pb;
    int n;
    for (int k = 0; k < p.size() - 1; ++k) {
        if (p[k] != 0.) {
            pb = p[k] / p_left;
            if (pb >= 1.) {   // > 1 possible by rounding error
                n = n_left;
            } else {
                n = (int) R::rbinom(n_left, pb);
            }

            if (n > 0) {
                n_left -= n;
                result[0].push_back(k);
                result[1].push_back(n);

                if (n_left <= 0) {
                    return;
                }
                p_left -= p[k];
            }
        }
    }
    // if any left, they go in the last slot
    // which, N.B., is assumed to have non-zero probability.
    if (n_left > 0) {
        result[0].push_back(p.size() - 1);
        result[1].push_back(n_left);
    }
}
