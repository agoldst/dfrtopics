#include <Rcpp.h>
using namespace Rcpp;

// #define __NAIVE_CLUSTER_LOG__

// used to keep track when we sort our distances
struct pair_dist {
    double d;
    int i;
    int j;
};

// custom comparator for sorting pair-distances
// (C++11 would let us use a lambda instead, but...)
struct pair_dist_cmp {
    bool operator() (pair_dist a, pair_dist b) {
        return a.d < b.d;
    }
};

// [[Rcpp::export]]
List naive_cluster(NumericVector D, int M, int K, double threshold) {
    IntegerVector result(M * K);
    NumericVector result_distances(M * K);
    std::vector<pair_dist> dst(D.size());
    std::map<int, std::set<int> > clusters;

    if (M * (M - 1) / 2 * K * K != D.size()) {
        stop("The length of D is not consistent with M and K");
    }

    // initialization
    std::iota(result.begin(), result.end(), 0);
    for (int i = 0; i < K * M; ++i) {
        clusters[i].insert(i);
    }

    // brute force (1): we'll just write down which index in D
    // corresponds to which pair of topics, copying over D like space
    // is cheap or something. D is assumed to go block-by-block row-wise
    // along a block-upper-triangular matrix whose (I, J) block is the
    // K x K matrix of distances between topics in models I and J.
    // Within each block D runs rowwise along the block.
    // Zero blocks are omitted.
    // We code topic k in model m by its index in `result`, m * K + k.
    int d = 0; // runs along D
    for (int m1 = 0; m1 < M - 1; ++m1) {
        for (int m2 = m1 + 1; m2 < M; ++m2) {
            for (int k1 = 0; k1 < K; ++k1) {
                for (int k2 = 0; k2 < K; ++k2) {
                    if (d >= dst.size()) {
                        stop("Something's wrong: counted past D");
                    }
                    dst[d].d = D[d];
                    dst[d].i = m1 * K + k1;
                    dst[d].j = m2 * K + k2;
                    d += 1;
                }
            }
        }
    }
    if (d != D.size()) {
        stop("Something's wrong: didn't finish counting D.");
    }

    pair_dist_cmp pdc;
    std::sort(dst.begin(), dst.end(), pdc);

    int r1, r2;
    std::set<int> sect;
    bool allow;
    for (std::vector<pair_dist>::iterator d = dst.begin();
            d != dst.end(); ++d) {
        if (d->d > threshold) { // then we're done
            break;
        }
        // topic pair's positions in the sequence of all topics
        d->i = d->i;
        d->j = d->j;  // guaranteed to be > d->i

#ifdef __NAIVE_CLUSTER_LOG__
        Rcout << "Consider: " << d->i << " (" << d->i / K << " ";
        Rcout << d->i % K << ") ";
        Rcout << d->j << " (" << d->j / K << " ";
        Rcout << d->j % K << ") [";
        Rcout << d->d << "]";
#endif
        // get current cluster assignments
        r1 = result[d->i];
        r2 = result[d->j];
        if (r1 == r2) {
#ifdef __NAIVE_CLUSTER_LOG__
            Rcout << "...already merged." << std::endl;
#endif
            continue; // they're already merged
        }
        if (r1 > r2) {
            std::swap(r1, r2);  // guarantee r1 < r2
        }
        std::set<int> &c1 = clusters[r1];
        std::set<int> &c2 = clusters[r2];

        if (c1.size() == M || c2.size() == M) {
            // if either cluster is full, merge is impossible
#ifdef __NAIVE_CLUSTER_LOG__
            Rcout << "...cluster full." << std::endl;
#endif
            continue;
        }
        // Check whether the two clusters share topics from the same model
        sect.clear();
        allow = true;
#ifdef __NAIVE_CLUSTER_LOG__
        Rcout << " c1 " << "(" << r1 << "):";
#endif
        for (std::set<int>::iterator t = c1.begin(); t != c1.end(); ++t) {
#ifdef __NAIVE_CLUSTER_LOG__
            Rcout <<  " " << *t / K;
#endif
            sect.insert(*t / K);
        }
#ifdef __NAIVE_CLUSTER_LOG__
        Rcout << " c2:" << "(" << r2 << "):";
#endif
        for (std::set<int>::iterator t = c2.begin();
                    t != c2.end() && allow; ++t) {
            // if same model: cluster disallowed
#ifdef __NAIVE_CLUSTER_LOG__
            Rcout << " " << *t / K;
#endif
            allow = (sect.count(*t / K) == 0);
        }
        if (!allow) {
#ifdef __NAIVE_CLUSTER_LOG__
            Rcout << "...disallowed." << std::endl;
#endif
            continue;
        }
#ifdef __NAIVE_CLUSTER_LOG__
        Rcout << "...merge." << std::endl;
#endif
        // otherwise: merge
        for (std::set<int>::iterator t = c2.begin(); t != c2.end(); ++t) {
            c1.insert(*t);
            result[*t] = r1;
            result_distances[*t] = d->d;
        }
        clusters.erase(r2); // also deletes the associated set object
    }

    return List::create(
        _["clusters"] = result,
        _["distances"] = result_distances
    );
}
