\name{make_instances}
\alias{make_instances}
\title{Create MALLET instances from a document frame}
\usage{
make_instances(docs, stoplist_file, ...)
}
\arguments{
  \item{docs}{data frame with \code{id} and \code{text}
  columns}

  \item{stoplist_file}{passed on to MALLET}

  \item{...}{passed on to
  \code{\link[mallet]{mallet.import}}}
}
\value{
an rJava reference to a MALLET \code{InstanceList}
}
\description{
Given a frame like that returned by
\code{\link{docs_frame}}, create a MALLET
\code{InstanceList} object.
}
\details{
If java gives out-of-memory errors, try increasing the Java
heap size to a large value, like 4GB, by setting
\code{options(java.parameters="-Xmx4g")} \emph{before}
loading this package (or rJava).
}
\seealso{
\code{\link{train_model}} \code{\link{write_instances}}
}

