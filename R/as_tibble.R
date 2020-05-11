
#' Convert DNAbin/AAbin to tibble
#'
#' These methods convert sequences from \pkg{ape}
#' formats DNAbin and AAbin to tibbles.
#'
#' @param x a DNAbin or AAbin object.
#' @param label Name of the column that stores the sequence labels
#' in the returned tibble.
#' @param sequence Name of the column that stores the sequences in the
#' returned tibble.
#' @param ... Not used.
#'
#' @return A tibble with two columns (if name is not NULL, the default) or one
#' column (otherwise).
#'
#' @export
#'
#' @examples
#' require(ape)
#' require(tibble)
#' x <- rDNAbin(nrow = 10, ncol = 25)
#' as_tibble(x)
#'
#'
#' @name as-tibble-ape
as_tibble.DNAbin <- function(x, label = "label", sequence = "sequence", ...) {
  x <- as_dna(x)
  res <- tibble::enframe(x, name = label, value = sequence)
  return(res)
}

#' @rdname as-tibble-ape
#' @export
as_tibble.AAbin <- function(x, label = "label", sequence = "sequence", ...) {
  x <- as_aa(x)
  res <- tibble::enframe(x, name = label, value = sequence)
  return(res)
}




#' Convert bioseq DNA, RNA and AA to tibble
#'
#' @param x a DNA, RNA or AA vector.
#' @param label Name of the column that stores the sequence
#' labels in the returned tibble.
#' @param sequence Name of the column that stores the sequences
#' in the returned tibble.
#' @param ... Not used.
#'
#' @return A tibble with two columns (if name is not NULL, the default)
#' or one column (otherwise).
#'
#' @export
#'
#' @examples
#'
#' require(tibble)
#' x <- dna(A = "ACGTTAGTGTAGCCGT", B = "CTCGAAATGA", C = NA)
#' as_tibble(x)
#'
#'
#' @name as-tibble-bioseq
#'
as_tibble.bioseq_dna <- function(x, label = "label",
                                 sequence = "sequence", ...) {
  check_dna(x)
  res <- tibble::enframe(x, name = label, value = sequence)
  return(res)
}

#' @rdname as-tibble-bioseq
#' @export
as_tibble.bioseq_rna <- function(x, label = "label",
                                 sequence = "sequence", ...) {
  check_rna(x)
  res <- tibble::enframe(x, name = label, value = sequence)
  return(res)
}

#' @rdname as-tibble-bioseq
#' @export
as_tibble.bioseq_aa <- function(x, label = "label",
                                sequence = "sequence", ...) {
  check_aa(x)
  res <- tibble::enframe(x, name = label, value = sequence)
  return(res)
}
