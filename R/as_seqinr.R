
#' Coerce to seqinr alignment
#'
#' @param x An object.
#' @param ... Other parameters.
#'
#' @return An alignment object.
#' @export
#'
#'
as_seqinr_alignment <- function (x, ...) {
  UseMethod("as_seqinr_alignment", x)
}


#' @export
#'
as_seqinr_alignment.bioseq_dna <- function(x, ...) {
  check_dna(x)
  s <- x
  attributes(s) <- NULL
  res <- list(nb = length(x),
              nam = names(x),
              seq = s)
  class(res) <- "alignment"
  return(res)
}

#' @export
#'
as_seqinr_alignment.bioseq_rna <- function(x, ...) {
  check_rna(x)
  s <- x
  attributes(s) <- NULL
  res <- list(nb = length(x),
              nam = names(x),
              seq = s)
  class(res) <- "alignment"
  return(res)
}

#' @export
#'
as_seqinr_alignment.bioseq_aa <- function(x, ...) {
  check_aa(x)
  s <- x
  attributes(s) <- NULL
  res <- list(nb = length(x),
              nam = names(x),
              seq = s)
  class(res) <- "alignment"
  return(res)
}
