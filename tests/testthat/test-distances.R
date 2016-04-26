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
    PQ_mean = (P + Q) / 2
    # eps small, otherwise KLdiv replaces values less than 10^-4
    eps = min(P, Q) / 2
    result <- (flexmix::KLdiv(cbind(P, PQ_mean), eps=eps) +
               flexmix::KLdiv(cbind(Q, PQ_mean), eps=eps)) / 2

    expect_equal(result[1, 2], JS_divergence(P, Q))

})

test_that("JS_divergence copes with zeroes", {
    P <- c(0, P, 0)
    Q <- c(0, 0, Q)
    d <- JS_divergence(P, Q)
    # shouldn't be taking any logs of 0
    expect_true(is.finite(d))
    # mutual zeroes should be skipped
    expect_equal(JS_divergence(P[-1], Q[-1]), d)

    expect_equal(JS_divergence(P, P), 0)
})

test_that("JS_divergence works on a 'known' example", {
    x <- c(0.3, 0.5, 0.2)
    y <- c(0.1, 0.4, 0.5)

    expect_equal(JS_divergence(x, y), 0.06215309, tolerance=1e-7)
})


test_that("row_dists works right", {
    x <- matrix(1:16, ncol=4)
    euc <- function (x, y) sqrt(sum((x - y)^2))
    expect_equal(row_dists(x, euc),
                 as.matrix(dist(x)),
                 check.attributes=FALSE)
})

test_that("JS_divergence's matrix version agrees with vector version", {
    x <- matrix(runif(200), nrow=20)
    x <- diag(1 / rowSums(x)) %*% x
    y <- matrix(runif(200), nrow=20)
    y <- diag(1 / rowSums(y)) %*% y

    expect_equal(JS_divergence(x, y),
        apply(y, 1, function (Y) apply(x, 1, JS_divergence, Y)))
})

test_that("cosine_distance works right", {
    x <- matrix(c(1, 0, 1, 1), byrow=T, nrow=2)
    y <- matrix(c(0, 1, 0.5, 0.5), byrow=T, nrow=2)
    expect_equal(cosine_distance(x, y),
        matrix(c(0, 1 / sqrt(2), 1 / sqrt(2), 1), byrow=T, nrow=2)
    )
})



# TODO test topic_divergences and topic_scaled_2d 
