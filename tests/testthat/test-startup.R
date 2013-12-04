test_that("Loading messaging works", {
    options(java.parameters="-Xmx512m")
    expect_that(devtools:::.onAttach(),shows_message(
'Your current Java heap setting is 512m.
I recommend giving Java at least 2GB of heap space.
To do this, put the following command in your scripts *before* loading
this package:

    options(java.parameters="-Xmx2g")

If you change this option in this session, you must then detach and
reload this package, mallet, and rJava. You can also simply restart R.
I apologize for this design flaw in R and rJava.'
    ))
})
