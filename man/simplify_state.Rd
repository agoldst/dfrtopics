\name{simplify_state}
\alias{simplify_state}
\title{Reduce a MALLET sampling state on disk to a simplified form}
\usage{
simplify_state(state_file, outfile)
}
\arguments{
  \item{state_file}{the MALLET state (assumed to be
  gzipped)}

  \item{outfile}{the name of the output file (will be
  clobbered)}
}
\value{
the return value from the python script
}
\description{
This function reads in the sampling state output by MALLET
and writes a CSV file giving the assignments of word types
to topics in each document.
}
\details{
The resulting file has a header
\code{document,word,topic,count} describing its columns.
Use \code{\link{read_simplified_state}} to access the
result in R. The MALLET state is typically too big to
handle in memory using R, so the "simplification" is done
using a very simple python script.
}
\seealso{
\code{\link{read_simplified_state}}
}

