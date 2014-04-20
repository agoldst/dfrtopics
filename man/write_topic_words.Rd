\name{write_topic_words}
\alias{write_topic_words}
\title{Write the topic-word matrix to disk}
\usage{
write_topic_words(trainer, topic_words_file = "topics_words.csv",
  vocab_file = "vocab.txt", smoothed = T, normalized = T)
}
\arguments{
  \item{trainer}{the reference to the \code{RTopicModel}
  object.}

  \item{topic_words_file}{name of a file to write the
  topic-word matrix to as a CSV file (topics are rows). No
  row or column headers.}

  \item{vocab_file}{name of a file to write the vocabulary
  corresponding to columns of topic_wordfile. One word per
  line.}

  \item{smoothed}{passed on to
  \code{\link[mallet]{mallet.topic.words}}.}

  \item{normalized}{passed on to
  \code{\link[mallet]{mallet.topic.words}}. For raw counts,
  set \code{smoothed=F,normalized=F}.}
}
\description{
Saves the estimated weights of words in topics to a
headerless CSV file and the list of words in the
"vocabulary" of the model to a text file.
}
\details{
This is the same information, differently organized, output
in a single file by mallet's
\code{--topic-word-weights-file <outfile.tsv>} command-line
option. To create that from R, use: \code{
trainer$model$printTopicWordWeights(new(J("java.io.File"),
"outfile.tsv")) }
}
\seealso{
\code{\link{read_topic_words}},
\code{\link[mallet]{mallet.topic.words}},
\code{\link{topic_word_scores}},
\code{\link{weighted_keys_frame}} for just the "top" or key
words in each topic.
}

