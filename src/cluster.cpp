#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
IntegerVector naive_cluster(NumericVector D, int nm, int K,
        double threshold=0.0) {
    IntegerVector result(nm * K);
    IntegerVector d(D.size());
    IntegerVector ii(D.size()), jj(D.size());
    std::map<int, std::set<int> > clusters;
    
    // initialization
    std::iota(result.begin(), result.end(), 0);
    for (int i = 0; i < K * nm; ++i) { 
        clusters[i].insert(i);
    }
    
    int ij = 0; // runs along D
    for (int i = 0; i < (nm - 1) * K; ++i) {

        // brute force: we'll just write down which index in D
        // corresponds to which pair of topics. D is assumed to go rowwise
        // along
        // 1.1:2.1 1.1:2.2 ... 1.1:nm.K 
        // 1.2:2.1 1.1:2.1 ... 1.2:nm.K
        // ...
        // 2.1:3.1 ...
        // ...
        // (nm - 1).1:nm.1 ... (nm - 1):nm.K
        // where a.b:c.d is the distance from topic a in model b to topic c in 
        // model d.
        for (int j = i / K + K; j < nm * K; ++j) {
            ii[ij] = i;
            jj[ij] = j;
            d[ij] = ij;
            ij += 1;
        }
    }
    
    std::sort(d.begin(), d.end(), [&](int a, int b) {
        return D[a] < D[b];
    });

    int k1, k2, r1, r2;
    std::set<int> c1, c2, sect;
    bool allow;
    for (IntegerVector::iterator i = d.begin(); i != d.end(); ++i) {
        if (D[*i] < threshold) { // then we're done
            break;
        }
        // decode i into a position in the sequence of all topics 
        k1 = ii[*i];
        k2 = jj[*i];
        // guarantee k1 < k2
        if (k1 > k2) {
            std::swap(k1, k2);
        }
        Rcout << "Consider: " << k1 << " " << k2;
        r1 = result[k1];
        r2 = result[k2];
        c1 = clusters[r1];
        c2 = clusters[r2];

        // Check whether the two clusters share topics from the same model
        sect.clear();
        allow = true;
        for (std::set<int>::iterator t = c1.begin(); t != c1.end(); ++t) {
            sect.insert(*t / K);
        } 
        for (std::set<int>::iterator t = c2.begin();
                    t != c2.end() && allow; ++t) {
            allow = (sect.count(*t / K) == 0); // same model: cluster disallowed
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
        clusters.erase(r2);
    }

    return result;
}