# For testing JS_divergence against an "official" implementation of
# KLdiv. Not faster on a single pair of rows, and can't use KLdiv's
# vectorization to do lots of JS's at once, unfortunately.

test_that("JS_divergence works right", {
    if(require("flexmix")) {
        JS_flexmix <- function(P,Q) {
            PQ_mean = (P + Q) / 2
            # eps small, otherwise KLdiv replaces values less than 10^-4
            eps = min(P,Q) / 2    
            result <- (KLdiv(cbind(P,PQ_mean),eps=eps) +
                       KLdiv(cbind(Q,PQ_mean),eps=eps)) / 2
            result[1,2]
        }

        # I *think* this is equivalent to taking two single draws
        # from a Dirichlet distribution with K = 100, sum alpha = 5
        P <- rgamma(100,5)
        P <- P / sum(P)
        Q <- rgamma(100,5)
        Q <- Q / sum(Q)
        expect_that(JS_flexmix(P,Q),equals(JS_divergence(P,Q)))
    } else {
        message("flexmix library not available. Skipping test")
    }
})

# TODO test topic_divergences function
