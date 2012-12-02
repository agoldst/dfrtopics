
test.routines <- function (topics.frame,keys.frame,n.topics) {

cat("Sanity tests on the data:\n")
test.year <- 1940
test.topic <- 25
test.row <- 3023
cat("Test year", test.year)
# TODO use topic.info to print information

cat("\nSum of all topics in test row (should be 1)","\n")
print(sum(topics.frame[test.row,1:n.topics]))

# TODO FINISH FROM HERE

cat("Sum of proportions over all docs and topics")
cat("\nShould be equal to the number of documents,",n.docs,"\n")

print(sum(topics.frame[,3:(n.topics + 2)]))

cat("Testing that year range is as expected:")
cat("\nrange(year.range)==range(topics.frame$year)\n")
print(all(range(year.range)==range(topics.frame$year)))

cat("Yearly aggregates:")
cat("\nSum of proportions of topics in the test year (should be 1)\n")
print(
      sum(
          sapply(0:(n.topics-1),
                 function(n) (topic.years.proportion(n,test.year)))
          )
      )

cat("Sum over all topics of proportions across all years")
cat("\nShould be equal to the number of years in the range",
    max(year.range)-min(year.range)+1,"\n")
print(
      sum(
          sapply(0:(n.topics-1), topic.proportions.by.year)
          )
      )

cat("Smoothing:")
cat("\nZero smoothing is same as proportions by year without smoothing\n")
print(
      all(
          topic.proportions.by.year(test.topic,tm,0)[,2]
          ==
          sapply(year.range, function (y) {
                 topic.years.proportion(test.topic,y,tm)
                 })
          )
      )


cat("Showing a plot superimposing smoothed (blue) and unsmoothed (orange) data")
plot.topic.smoothed.and.unsmoothed(test.topic)

cat("\nShowing exemplary documents for test topic\n")
metadata.frame <- metadata.from.db()
print(documents.by.topic(test.topic,metadata.frame))
cat("\nEnd of tests.")
}
