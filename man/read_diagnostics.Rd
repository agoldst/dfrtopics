\name{read_diagnostics}
\alias{read_diagnostics}
\title{Read MALLET model-diagnostic results.}
\usage{
read_diagnostics(xml_file)
}
\arguments{
  \item{xml_file}{file holding XML to be parsed.}
}
\value{
a list of two dataframes of diagnostic information,
\code{topics} and \code{words}. The diagnostics are
sparsely documented by the MALLET source code
(\url{http://hg-iesl.cs.umass.edu/hg/mallet}: see
\code{src/cc/mallet/topics/TopicModelDiagnostics.java}).

In \code{topics}, columns include: \describe{
\item{\code{topic}}{The 1-indexed topic number.}
\item{\code{corpus_dist}}{The KL-divergence from the
corpus.} \item{\code{coherence}}{The topic coherence
measure defined by Mimno et al., eq. (1): the sum of
log-co-document-document frequency ratios for the top words
in the topic. The number of top words is set in the
\code{n_top_words} parameter to
\code{\link{write_diagnostics}}.} }

The function attempts to coerce numeric values, which
\pkg{XML} extracts as strings, into numbers.
}
\description{
Uses the \pkg{XML} package and \code{libxml} to parse the
MALLET diagnostic output.
}
\references{
David Mimno et al. Optimizing Semantic Coherence in Topic
Models. \emph{EMNLP} 2011.
\url{http://www.cs.princeton.edu/~mimno/papers/mimno-semantic-emnlp.pdf}.
}
\seealso{
\code{\link{write_diagnostics}}
}

