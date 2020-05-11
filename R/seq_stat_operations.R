
#' Count the number of character in sequences
#'
#' @param x  a DNA, RNA or AA vector.
#' @param gaps if \code{FALSE} gaps are ignored.
#'
#' @return An integer vector giving the size of each sequence of \code{x}.
#' @export
#'
#' @examples
#'
#' x <- dna(c("ATGCAGA", "GGR-----","TTGCCTAGKTGAACC"))
#' seq_nchar(x)
#' seq_nchar(x, gaps = FALSE)
#'
seq_nchar <- function(x, gaps = TRUE) {
  check_dna_rna_aa(x)
  x_names <- names(x)

  if(!gaps) {
    x <- stringr::str_remove_all(x, "-")
  }
  res <- stringr::str_length(x)
  names(res) <- x_names
  return(res)
}


#' Number of sequences in a vector
#'
#' This is an alias for \code{length}.
#'
#' @param x a DNA, RNA or AA vector.
#'
#' @return an integer.
#' @export
#'
seq_nseq <- function(x) {
  check_dna_rna_aa(x)
  length(x)
}


#' Compute proportions for characters
#'
#' @param x a DNA, RNA or AA vector.
#' @param gaps if \code{FALSE} gaps are ignored.
#'
#' @return A list of vectors indicating the
#' proportion of characters in each sequence.
#' @export
#'
#' @examples
#'
#' x <- dna(c("ATGCAGA", "GGR-----","TTGCCTAGKTGAACC"))
#' seq_stat_prop(x)
#' seq_stat_prop(x, gaps = TRUE)
#'
seq_stat_prop <- function(x, gaps = FALSE) {
  check_dna_rna_aa(x)
  x_names <- names(x)
  x_na <- is.na(x)

  res_names <- dic_dna()$alphabet

  if(!gaps) {
    x <- stringr::str_remove_all(x, "-")
    res_names <- res_names[res_names != "-"]
  }
  res <- numeric(length(res_names))
  names(res) <- res_names

  x_split <- stringr::str_split(x, "")
  res <- lapply(x_split, function(x) {
    tab <- table(x)
    res[names(tab)] <- tab/sum(tab)
    return(res)
    })

  res[x_na] <- NA
  names(res) <- x_names
  return(res)
}



#' Compute G+C content
#'
#' @param x a DNA or RNA
#'
#' @details Ambiguous characters (other than S and W) are ignored.
#'
#' @return A numeric vector of G+C proportions.
#' @export
#'
#' @examples
#'
#' x <- dna(c("ATGCAGA", "GGR-----","TTGCCTAGKTGAACC"))
#'
#' seq_stat_gc(x)
#'
seq_stat_gc <- function(x) {
  check_dna_rna(x)
  x_names <- names(x)

  gc_nuc <- stringr::str_count(x, "G|C|S")
  all_nuc <- stringr::str_count(x, "A|C|G|T|U|S|W")
  res <- gc_nuc/all_nuc
  res[is.nan(res)] <- NA
  names(res) <- x_names
  return(res)
}


