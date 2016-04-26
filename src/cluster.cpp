#include <Rcpp.h>
using namespace Rcpp;

// #define __NAIVE_CLUSTER_LOG__

// topics are indexed by model and topic
typedef std::pair<int, int> topic_index;

// used to keep track when we sort our distances
struct pair_dist {
    double d;
    topic_index t1;
    topic_index t2;
};


// custom comparator for sorting pair-distances
// (C++11 would let us use a lambda instead, but...)
struct pair_dist_cmp {
    bool operator() (pair_dist a, pair_dist b) {
        return a.d < b.d;
    }
};

// [[Rcpp::export]]
List naive_cluster(NumericVector D, IntegerVector K, double threshold) {
    int M = K.size();
    std::vector<std::vector<int> > result(M);
    std::vector<std::vector<double> > result_distances(M);
    std::vector<pair_dist> dst(D.size());
    std::map<int, std::set<topic_index> > clusters;

    // if (M * (M - 1) / 2 * K * K != D.size()) {
   //// }

    // initialization

    int c = 0;
    for (int m = 0; m < M; ++m) {
        result[m].resize(K[m]);
        result_distances[m].resize(K[m]);
        for (int k = 0; k < K[m]; ++k) {
            result[m][k] = c;
            clusters[c].insert(topic_index(m, k));
            ++c;
        }
    }

    // brute force: we'll just write down which index in D
    // corresponds to which pair of topics, copying over D like space
    // is cheap or something. D is assumed to go block-by-block row-wise
    // along a block-upper-triangular matrix whose (I, J) block is the
    // K_I x K_J matrix of distances between topics in models I and J.
    // Within each block D runs rowwise along the block.
    // Zero blocks are omitted.
    int d = 0; // runs along D
    for (int m1 = 0; m1 < M - 1; ++m1) {
        for (int m2 = m1 + 1; m2 < M; ++m2) {
            for (int k1 = 0; k1 < K[m1]; ++k1) {
                for (int k2 = 0; k2 < K[m2]; ++k2) {
                    if (d >= dst.size()) {
                        stop("The length of D is not consistent with K");
                    }
                    dst[d].d = D[d];
                    dst[d].t1.first = m1;
                    dst[d].t1.second = k1;
                    dst[d].t2.first = m2;
                    dst[d].t2.second = k2;
                    d += 1;
                }
            }
        }
    }
    if (d != D.size()) {
        stop("The length of D is not consistent with K");
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

#ifdef __NAIVE_CLUSTER_LOG__
        Rcout << d->t1.first << " ";
        Rcout << d->t1.second << " | ";
        Rcout << d->t2.first << " ";
        Rcout << d->t2.second << " [";
        Rcout << d->d << "]";
#endif

        // get current cluster assignments
        r1 = result[d->t1.first][d->t1.second];
        r2 = result[d->t2.first][d->t2.second];
        if (r1 == r2) {
#ifdef __NAIVE_CLUSTER_LOG__
            Rcout << "...already merged." << std::endl;
#endif
            continue; // they're already merged
        }
        if (r1 > r2) {
            std::swap(r1, r2);  // guarantee r1 < r2
        }
        std::set<topic_index> &c1 = clusters[r1];
        std::set<topic_index> &c2 = clusters[r2];

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
        for (std::set<topic_index>::iterator t = c1.begin();
                t != c1.end(); ++t) {
#ifdef __NAIVE_CLUSTER_LOG__
            Rcout <<  " " << t->first;
#endif
            sect.insert(t->first);
        }
#ifdef __NAIVE_CLUSTER_LOG__
        Rcout << " c2:" << "(" << r2 << "):";
#endif
        for (std::set<topic_index>::iterator t = c2.begin();
                    t != c2.end() && allow; ++t) {
            // if same model: cluster disallowed
#ifdef __NAIVE_CLUSTER_LOG__
            Rcout << " " << t->first;
#endif
            allow = (sect.count(t->first) == 0);
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
        for (std::set<topic_index>::iterator t = c2.begin();
                t != c2.end(); ++t) {
            c1.insert(*t);
            result[t->first][t->second] = r1;
            result_distances[t->first][t->second] = d->d;
        }
        clusters.erase(r2); // also deletes the associated set object
    }

    return List::create(
        _["clusters"] = wrap(result),
        _["distances"] = wrap(result_distances)
    );
}

// Find maximum pairwise distance within clusters from a naive_cluster result
// cl list of vectors in which cl[[i]][j] is the ONE-BASED cluster number
// D element distances, specified as for naive_cluster
// [[Rcpp::export]]
std::vector<double> naive_cluster_width(
        std::vector<std::vector<int> > cl, NumericVector D) {
    int M = cl.size();
    std::vector<double> result;

    int d = 0; // runs along D
    int cl1, cl2; // ONE-BASED cluster numbers from cl
    for (int m1 = 0; m1 < M - 1; ++m1) {
        for (int m2 = m1 + 1; m2 < M; ++m2) {
            for (int k1 = 0; k1 < cl[m1].size(); ++k1) {
                cl1 = cl[m1][k1];
                if (cl1 > result.size()) {
                    result.resize(cl1);
                }
                for (int k2 = 0; k2 < cl[m2].size(); ++k2) {
                    if (d >= D.size()) {
                        stop("Something's wrong: counted past D");
                    }
                    cl2 = cl[m2][k2];
                    if (cl1 == cl2) {
                        result[cl1 - 1] = std::max(result[cl1 - 1], D[d]);
                    }

                    if (cl2 > result.size()) {
                        result.resize(cl2);
                    }
                    d += 1;
                }
            }
        }
    }
    if (d != D.size()) {
        stop("Something's wrong: didn't finish counting D.");
    }

    return result;
}
