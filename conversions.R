
# Make sure the word counts are counts of the features mallet actually counts;
# i.e., exclude stopwords from the counts. that's what wc_stop is for

read.counts.dict <- function(filename=file.choose()) {
  counts <- read.csv(file=filename,as.is=TRUE,header=FALSE,
                     col.names=c("filename","words"))
  result <- counts$words
  names(result) <- as.id(counts$filename)
  result
}
 
denormalize.topics <- function(tm,counts.dict,alpha)  {
  counts <- counts.dict[as.character(tm$id)]
  m <- topic.model.matrix(tm)
  # So here's the deal. The proportions in doc-topics are calculated as
  # alpha[topic] + topicCounts[topic]) / (docLen + alphaSum)
  # in the printDocTopics method of cc.mallet.topics.ParallelTopicModel
  #
  # So we unwind it:
  # the number of words in document j assigned to topic k is:
  # result_jk = m_jk * (counts_j + sum_k(alpha_k)) - alpha_k
  # 
  # thanks to the vagaries of floating-point arithmetic, however,
  # all the zeroes will be...not zero.  I *think* this is purely rounding
  # error, but I'm a little worried about some of the inexactness I'm seeing
  # when I compare the "denormalized" counts produced this way
  # to the counts of tokens I can get from mallet's diagnostic output
  # 
  #
  # we can write all this w/o subscripts, thanks to R's recycling rule
  # but we need to make sure to treat alpha as a row vector
  alphaSum <- sum(alpha)

  round(m * (counts+alphaSum) - matrix(alpha,nrow=1),digits=0) 
}

