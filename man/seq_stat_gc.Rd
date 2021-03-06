% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seq_stat_operations.R
\name{seq_stat_gc}
\alias{seq_stat_gc}
\title{Compute G+C content}
\usage{
seq_stat_gc(x)
}
\arguments{
\item{x}{a DNA or RNA}
}
\value{
A numeric vector of G+C proportions.
}
\description{
Compute G+C content
}
\details{
Ambiguous characters (other than S and W) are ignored.
}
\examples{

x <- dna(c("ATGCAGA", "GGR-----","TTGCCTAGKTGAACC"))

seq_stat_gc(x)

}
\seealso{
Other op-misc: 
\code{\link{seq_disambiguate_IUPAC}()},
\code{\link{seq_nchar}()},
\code{\link{seq_nseq}()},
\code{\link{seq_spellout}()},
\code{\link{seq_stat_prop}()}
}
\concept{op-misc}
