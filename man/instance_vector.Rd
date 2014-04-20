\name{instance_vector}
\alias{instance_vector}
\title{Convert an instance to a vector}
\usage{
instance_vector(instance)
}
\arguments{
  \item{instance}{a reference to an Instance}
}
\value{
an integer vector, with \emph{one-based} indices into the
vocabulary
}
\description{
Extracts an R vector representation of the
\code{FeatureSequence}.
}
\details{
A \code{FeatureSequence} is a list of \emph{zero-based}
indices into the vocabulary. For convenience, this function
adds 1 so that the result can be used to index directly
into a vocabulary vector (e.g. from
\code{trainer$getVocabulary()} or
\code{\link{instances_vocabulary}}). Note that although
MALLET's topic-modeling works on feature \emph{sequences}
because it is designed to preserve the order of words in
the documents it models, the pre-aggregated data from JSTOR
means that the "sequences" will be meaningless:
\code{\link{docs_frame}} simply puts all the occurrences of
a word in a document next to one another.
}
\seealso{
\code{\link{instances_vocabulary}},
\code{\link{docs_frame}}, \code{\link{instance_text}}
}

