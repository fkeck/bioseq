% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seq_string_operation.R
\name{seq_remove_position}
\alias{seq_remove_position}
\title{Remove a region between two positions in sequences.}
\usage{
seq_remove_position(x, position_in, position_out)
}
\arguments{
\item{x}{a DNA, RNA or AA vector.}

\item{position_in}{an integer giving the position where to start to remove.}

\item{position_out}{an integer giving the position where to stop to remove.}
}
\value{
A vector of same class as \code{x}.
}
\description{
Remove a region between two positions in sequences.
}
\examples{

x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
seq_remove_position(x, 2, 6)
seq_remove_position(x, 1:2, 3:4)

}
\seealso{
\code{\link[stringr]{str_remove}} from \pkg{stringr}
for the underlying implementation.

Other string operations: 
\code{\link{seq-replace}},
\code{\link{seq_combine}()},
\code{\link{seq_count_pattern}()},
\code{\link{seq_crop_pattern}()},
\code{\link{seq_crop_position}()},
\code{\link{seq_detect_pattern}()},
\code{\link{seq_extract_pattern}()},
\code{\link{seq_extract_position}()},
\code{\link{seq_remove_pattern}()},
\code{\link{seq_replace_position}()},
\code{\link{seq_split_kmer}()},
\code{\link{seq_split_pattern}()}
}
\concept{string operations}
