context("miscellaneous matrix utilities")

test_that("row/col top-n functions work", {
    m <- matrix(1:9, ncol=3)    # fill by column

    expect_equal(m[top_n_row(m, 2)], c(7, 4, 8, 5, 9, 6))
    expect_equal(m[top_n_col(m, 2)], c(3, 2, 6, 5, 9, 8))

    m <- t(m)

    expect_equal(m[top_n_col(m, 2)], c(7, 4, 8, 5, 9, 6))
    expect_equal(m[top_n_row(m, 2)], c(3, 2, 6, 5, 9, 8))
})

