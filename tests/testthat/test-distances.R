# For testing JS_divergence against an "official" implementation of
# KLdiv. Not faster on a single pair of rows, and can't use KLdiv's
# vectorization to do lots of JS's at once, unfortunately.

testthat("JS works right", {
    JS_flexmix <- function(P,Q) {
        library(flexmix)
        PQ_mean = (P + Q) / 2
        eps = min(P,Q) / 2    # otherwise KLdiv replaces values less than 10^-4
        result <- (KLdiv(cbind(P,PQ_mean),eps=eps) +
                   KLdiv(cbind(Q,PQ_mean),eps=eps)) / 2
        result[1,2]
    }

    # P <- 
    # Q <-
    expect_that(JS_flexmix(P,Q),equals(JS_divergence(P,Q)))
})
