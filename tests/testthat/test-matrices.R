context("miscellaneous matrix utilities")

test_that("row/col top-n functions work", {
    m <- matrix(1:9, ncol=3)    # fill by column

    expect_equal(m[top_n_row(m, 2)], c(7, 4, 8, 5, 9, 6))
    expect_equal(m[top_n_col(m, 2)], c(3, 2, 6, 5, 9, 8))

    m <- t(m)

    expect_equal(m[top_n_col(m, 2)], c(7, 4, 8, 5, 9, 6))
    expect_equal(m[top_n_row(m, 2)], c(3, 2, 6, 5, 9, 8))
})

test_that("gather_matrix works", {
    m <- matrix(1:9, ncol=3)    # fill by column
    m_g <- gather_matrix(m)
    expect_equal(m_g,
                 data.frame(
        row_key=c(1, 1, 1, 2, 2, 2, 3, 3, 3),
        col_key=c(1, 2, 3, 1, 2, 3, 1, 2, 3),
        value=  c(1, 4, 7, 2, 5, 8, 3, 6, 9)
                 ))
    expect_equal(m[matrix(c(m_g$row_key, m_g$col_key), ncol=2)], m_g$value)

    expect_equal(gather_matrix(m, row_major=F),
                 data.frame(
        row_key=c(1, 2, 3, 1, 2, 3, 1, 2, 3),
        col_key=c(1, 1, 1, 2, 2, 2, 3, 3, 3),
        value=  1:9
                 ))

    expect_equal(gather_matrix(m,
                 col_names=c("rowz", "colz", "valz")),
                 data.frame(
        rowz=c(1, 1, 1, 2, 2, 2, 3, 3, 3),
        colz=c(1, 2, 3, 1, 2, 3, 1, 2, 3),
        valz=c(1, 4, 7, 2, 5, 8, 3, 6, 9)
                 ))

    expect_equal(gather_matrix(m,
                 row_values=letters[1:3],
                 col_values=1:3 * 10),
                 data.frame(
        row_key=c("a", "a", "a", "b", "b", "b", "c", "c", "c"),
        col_key=c(10, 20, 30, 10, 20, 30, 10, 20, 30),
        value=  c(1, 4, 7, 2, 5, 8, 3, 6, 9),
                 stringsAsFactors=F
                 ))
})

test_that("normalize_cols/rows works", {
    expect_equal(
        normalize_cols(
            matrix(c(1, 0,
                     3, 2), ncol=2, byrow=T)
        ),
        matrix(c(0.25, 0,
                 0.75, 1), ncol=2, byrow=T))

    expect_equal(
        normalize_cols(
            matrix(c(3, 0.3,
                     4, 0.4), ncol=2, byrow=T), norm="L2"
        ),
        matrix(c(0.6, 0.6,
                 0.8, 0.8), ncol=2, byrow=T))

    m <- matrix(c(1, 0, 0,
                  3, 2, 0), ncol=3, byrow=T)
    expect_equal(normalize_cols(m),
        matrix(c(0.25, 0, 0,
                 0.75, 1, 0), ncol=3, byrow=T))
    expect_error(normalize_cols(m, stopzero=T),
        "The matrix has columns of all zeroes, which cannot be normalized.")

    expect_equal(
        normalize_rows(
            matrix(c(1, 3,
                     0, 2), ncol=2, byrow=T)
        ),
        matrix(c(0.25, 0.75,
                 0,    1), ncol=2, byrow=T))

    expect_equal(
        normalize_rows(
            matrix(c(3,   4,
                     0.3, 0.4), ncol=2, byrow=T), norm="L2"
        ),
        matrix(c(0.6, 0.8,
                 0.6, 0.8), ncol=2, byrow=T))

    m <- matrix(c(1, 3,
                  0, 0,
                  2, 0), ncol=2, byrow=T)
    expect_equal(normalize_rows(m),
        matrix(c(0.25, 0.75,
                 0,    0,
                 1,    0), ncol=2, byrow=T))
    expect_error(normalize_rows(m, stopzero=T),
        "The matrix has rows of all zeroes, which cannot be normalized.")
})

test_that("rescaling row/col works", {
    expect_equal(
        rescale_rows(
            matrix(c(2, 4,
                     6, 8), ncol=2, byrow=T),
            c(3, 2)),
        matrix(c(6,  12,
                 12, 16), ncol=2, byrow=T))

    expect_equal(
        rescale_cols(
            matrix(c(2, 4,
                     6, 8), ncol=2, byrow=T),
            c(3, 2)),
        matrix(c(6,  8,
                 18, 16), ncol=2, byrow=T))
})

test_that("row/col group sums work", {
    m <- matrix(c(1, 2,
                  3, 4,
                  5, 6,
                  7, 8), ncol=2, byrow=T)
    f <- factor(c("a", "b", "a", "b"))
    f2 <- factor(c("b", "a", "b", "a"))
    expect_equal(
        sum_row_groups(m, f, row_names=NULL),
        matrix(c(6,  8,
                 10, 12), ncol=2, byrow=T)
    )
    expect_equal(
        sum_row_groups(m, f),
        matrix(c(6,  8,
                 10, 12), ncol=2, byrow=T, dimnames=list(c("a", "b")))
    )
    expect_equal(
        sum_row_groups(m, f2, row_names=NULL),
        matrix(c(10, 12,
                 6,  8), ncol=2, byrow=T)
    )

    rownames(m) <- c("smeagol", "deagol", "gollum", "precious")
    colnames(m) <- c("c", "d")
    expect_equal(
        sum_row_groups(m, f),
        matrix(c(6,  8,
                 10, 12), ncol=2, byrow=T,
               dimnames=list(c("a", "b"), c("c", "d")))
    )
    m2 <- matrix(c(1, 2, 3, 4,
                   5, 6, 7, 8), ncol=4, byrow=T)
    expect_equal(
        sum_col_groups(m2, f, col_names=NULL),
        matrix(c(4,  6,
                 12, 14), ncol=2, byrow=T)
    )
    expect_equal(
        sum_col_groups(m2, f2, col_names=NULL),
        matrix(c(6,  4,
                 14, 12), ncol=2, byrow=T)
    )
    expect_equal(
        sum_col_groups(m2, f),
        matrix(c(4,  6,
                 12, 14), ncol=2, byrow=T, dimnames=list(NULL, c("a", "b")))
    )
})

test_that("bounds checking for top_n_row/col works", {
    m <- matrix(1:12, ncol=3)
    expect_error(top_n_row(m, 4), "ncol")

    expect_error(top_n_col(t(m), 4), "nrow")
})
