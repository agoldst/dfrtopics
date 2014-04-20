\name{instances_vocabulary}
\alias{instances_vocabulary}
\title{Retrieve the vocabulary from the instances}
\usage{
instances_vocabulary(instances, newlines_significant = F)
}
\arguments{
  \item{instances}{reference to the \code{InstanceList}}

  \item{newlines_significant}{if vocabulary terms include
  newlines, a slower method of extracting the vocabulary
  will be used (unusual for DfR)}
}
\value{
character vector mapping one-based word indices to terms as
strings
}
\description{
Retrieves the vocabulary from the instances.
}
\details{
If you have the topic model trainer object, the vocabulary
is retrievable more quickly with an \code{RTopicModel}
method: \code{trainer$getVocabulary()}. But every
\code{InstanceList} knows its vocabulary.
}

