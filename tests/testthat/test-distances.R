context("Distance functions")

# For testing JS_divergence against an "official" implementation of
# KLdiv. Not faster on a single pair of rows, and can't use KLdiv's
# vectorization to do lots of JS's at once, unfortunately.

# I *think* this is equivalent to taking two single draws
# from a Dirichlet distribution with K = 100, sum alpha = 5
P <- rgamma(100, 5)
P <- P / sum(P)
Q <- rgamma(100, 5)
Q <- Q / sum(Q)

test_that("JS_divergence reproduces flexmix::KLdiv", {
    skip_if_not_installed("flexmix")
    JS_flexmix <- function(P,Q) {
        PQ_mean = (P + Q) / 2
        # eps small, otherwise KLdiv replaces values less than 10^-4
        eps = min(P, Q) / 2
        result <- (KLdiv(cbind(P, PQ_mean), eps=eps) +
                   KLdiv(cbind(Q, PQ_mean), eps=eps)) / 2
        result[1,2]
    }

    expect_equal(JS_flexmix(P, Q), JS_divergence(P, Q))

})

test_that("JS_divergence copes with zeroes", {
    P <- c(0, P, 0)
    Q <- c(0, 0, Q)
    d <- JS_divergence(P, Q)
    # shouldn't be taking any logs of 0
    expect_true(is.finite(d))
    # mutual zeroes should be skipped
    expect_equal(JS_divergence(P[-1], Q[-1]), d)
})

test_that("row_dists works right", {
    x <- matrix(1:16, ncol=4)
    euc <- function (x, y) sqrt(sum((x - y)^2))
    expect_equal(row_dists(x, euc),
                 as.matrix(dist(x)),
                 check.attributes=FALSE)
})

# TODO test topic_divergences and topic_scaled_2d 
