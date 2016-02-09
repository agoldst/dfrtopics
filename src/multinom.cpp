#include <Rcpp.h>

using namespace Rcpp;

void multinom_sparse_trial(int N, const NumericVector p, double p_total,
        std::vector<std::vector<int> >& result);

// [[Rcpp::export]]
List draw_multinom(const IntegerVector nn, const NumericVector probs) {
    std::vector<int> col_p(nn.size() + 1);
    std::vector<int> i;
    std::vector<int> x;
    std::vector<std::vector<int> > trial(2);
    NumericVector p = probs / sum(probs);
    double p_total = sum(p);

    col_p[0] = 0;
    for (int j = 0; j < nn.size(); ++j) {
        if (nn[j] != 0) {
            multinom_sparse_trial(nn[j], p, p_total, trial);
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

// TODO should p_total be a long double as in rmultinom.c?
void multinom_sparse_trial(int N, const NumericVector p, double p_total,
        std::vector<std::vector<int> >& result) {
    result[0].clear();
    result[1].clear();

    if (N <= 0) {
        return;
    }

    double p_left = p_total;
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
    if (n_left > 0) {
        result[0].push_back(p.size() - 1);
        result[1].push_back(n_left);
    }
}
