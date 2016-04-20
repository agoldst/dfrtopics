#include <Rcpp.h>
using namespace Rcpp;

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
IntegerVector naive_cluster(NumericVector D, int M, int K,
        double threshold=1.0) {
    IntegerVector result(M * K);
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
    
    int d = 0; // runs along D
    for (int i = 0; i < (M - 1) * K; ++i) {

        // brute force: we'll just write down which index in D
        // corresponds to which pair of topics. D is assumed to go rowwise
        // along a block-lower-triangular matrix whose (I, J) block is the
        // K x K matrix of distances between topics in models I and J. And
        // blocks are omitted.
        // 1.1:2.1 1.1:2.2 ... 1.1:M.K 
        // 1.2:2.1 1.1:2.1 ... 1.2:M.K
        // ...
        // 2.1:3.1 ...
        // ...
        // (M - 1).1:M.1 ... (M - 1):M.K
        // where a.b:c.d = distance from topic a in model b to topic c in 
        // model d.
        for (int j = i / K + K; j < M * K; ++j) {
            dst[d].d = D[d]; // copying D like space is cheap or something
            dst[d].i = i; // a.b coded as (a - 1) * K + (b - 1)
            dst[d].j = j;
            d += 1;
        }
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
        if (d->i >= d->j) {
            stop("Something's wrong: d->i >= d->j");
        }
        Rcout << "Consider: " << d->i << " (" << d->i / K << " ";
        Rcout << d->i % K << ") ";
        Rcout << d->j << " (" << d->j / K << " ";
        Rcout << d->j % K << ")";

        r1 = result[d->i];
        r2 = result[d->j];
        std::set<int> &c1 = clusters[r1];
        std::set<int> &c2 = clusters[r2];

        if (c1.size() == M || c2.size() == M) {
            // if either cluster is full, merge is impossible
            Rcout << "...cluster full." << std::endl;
            continue;
        }
        // Check whether the two clusters share topics from the same model
        sect.clear();
        allow = true;
        Rcout << " c1:";
        for (std::set<int>::iterator t = c1.begin(); t != c1.end(); ++t) {
            Rcout <<  " " << *t / K;
            sect.insert(*t / K);
        } 
        Rcout << " c2:";
        for (std::set<int>::iterator t = c2.begin();
                    t != c2.end() && allow; ++t) {
            // if same model: cluster disallowed
            Rcout << " " << *t / K;
            allow = (sect.count(*t / K) == 0);
        }
        if (!allow) {
            Rcout << "...disallowed." << std::endl;
            continue;
        }
        Rcout << "...merge." << std::endl;
        // otherwise: merge 
        for (std::set<int>::iterator t = c2.begin(); t != c2.end(); ++t) {
            c1.insert(*t);
            result[*t] = r1;
        }
        clusters.erase(r2); // also deletes the associated set object
    }

    return result;
}
