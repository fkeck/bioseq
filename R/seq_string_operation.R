
############################# By Pattern #############################

#' Detect the presence of patterns in sequences
#'
#' @param x a DNA, RNA or AA vector.
#' @param pattern a DNA, RNA or AA vectors (but same as \code{x})
#' or a character vector of regular expressions, or a list.
#' See section Patterns.
#'
#'
#' @section Patterns:
#' It is important to understand how patterns are treated in \pkg{bioseq}.
#'
#' Patterns are recycled along the sequences (usually the \code{x} argument).
#' This means that if a pattern (vector or list) is of length > 1, it will be
#' replicated until it is the same length as \code{x}.
#' The reverse is not true and a vector of patterns longer than
#' a vector of sequences will raise a warning.
#'
#' Patterns can be DNA, RNA or AA vectors
#' (but they must be from the same class as
#' the sequences they are matched against).
#' If patterns are DNA, RNA or AA vectors,
#' they are disambiguated prior to matching.
#' For example pattern dna("ARG") will match AAG or AGG.
#'
#' Alternatively, patterns can be a simple character vector
#' containing regular expressions.
#'
#' Vectors of patterns (DNA, RNA, AA or regex) can also be provided in a list.
#' In that case, each vector of the list will be collapsed prior matching,
#' which means that each vector element will be used as an alternative pattern.
#' For example pattern list(c("AAA", "CCC"), "GG")
#' will match AAA or CCC in the first sequence,
#' GG in the second sequence, AAA or CCC in the third,
#' and so on following the recycling rule.
#'
#' @seealso
#' \code{\link[stringi]{stri_detect}} from \pkg{stringi} and
#' \code{\link[stringr]{str_detect}} from \pkg{stringr}
#' for the underlying implementation.
#'
#' @return A logical vector.
#' @family string operations
#' @export
#'
#' @examples
#'
#' x <- dna(c("ACGTTAGTGTAGCCGT", "CTCGAAATGA"))
#' seq_detect_pattern(x, dna(c("CCG", "AAA")))
#' seq_detect_pattern(x, "^A.{2}T")
#'
seq_detect_pattern <- function(x, pattern) {
  check_dna_rna_aa(x)
  pattern <- check_and_prepare_pattern(x, pattern)
  res <- stringr::str_detect(string = x, pattern = pattern)
  return(res)
}



#' Crop sequences using delimiting patterns
#'
#' @param x a DNA, RNA or AA vector to be cropped.
#' @param pattern_in patterns defining the beginning (left-side).
#' @param pattern_out patterns defining the end (right-side).
#'
#' @inheritSection seq_detect_pattern Patterns
#' @return  A cropped DNA, RNA or AA vector.
#'
#' @family string operations
#' @seealso
#' \code{\link[stringi]{stri_extract}} from \pkg{stringi} and
#' \code{\link[stringr]{str_extract}} from \pkg{stringr}
#' for the underlying implementation.
#'
#' @export
#'
#' @examples
#'
#' x <- dna("ACGTTAAAAAGTGTAGCCCCCGT", "CTCGAAATGA")
#' seq_crop_pattern(x, pattern_in = "AAAA", pattern_out = "CCCC")
seq_crop_pattern <- function(x, pattern_in, pattern_out) {
  check_dna_rna_aa(x)
  pattern_in <- check_and_prepare_pattern(x, pattern_in)
  pattern_out <- check_and_prepare_pattern(x, pattern_out)

  rgx <- paste0("(?<=", pattern_in, ").*(?=", pattern_out, ")")
  res <- stringr::str_extract(string = x, pattern = rgx)
  res <- coerce_seq_as_input(res, x)
  return(res)
}



#' Extract matching patterns from sequences
#'
#' @inheritParams seq_detect_pattern
#' @inheritSection seq_detect_pattern Patterns
#' @return A list of vectors of same class as \code{x}.
#' @family string operations
#' @seealso
#' \code{\link[stringi]{stri_extract}} from \pkg{stringi} and
#' \code{\link[stringr]{str_extract}} from \pkg{stringr}
#' for the underlying implementation.
#'
#' @export
#'
#' @examples
#'
#' x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
#' seq_extract_pattern(x, dna("AAA"))
#' seq_extract_pattern(x, "T.G")
#'
seq_extract_pattern <- function(x, pattern) {
  check_dna_rna_aa(x)
  pattern <- check_and_prepare_pattern(x, pattern)
  res <- stringr::str_extract_all(string = x, pattern = pattern)
  res <- lapply(res, coerce_seq_as_input, input = x)
  return(res)
}



#' Remove matched patterns in sequences
#'
#' @inheritParams seq_detect_pattern
#' @inheritSection seq_detect_pattern Patterns
#' @return A vector of same class as \code{x}.
#' @export
#' @family string operations
#' @seealso
#' \code{\link[stringr]{str_remove}} from \pkg{stringr}
#' for the underlying implementation.
#'
#' @examples
#'
#' x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
#' seq_remove_pattern(x, dna("AAA"))
#' seq_remove_pattern(x, "^A.{2}T")
#'
seq_remove_pattern <- function(x, pattern) {
  check_dna_rna_aa(x)
  pattern <- check_and_prepare_pattern(x, pattern)
  res <- stringr::str_remove_all(string = x, pattern = pattern)
  res <- coerce_seq_as_input(res, input = x)
  return(res)
}



#' Replace matched patterns in sequences
#'
#' @inheritParams seq_detect_pattern
#' @param replacement a vector of replacements.
#' @inheritSection seq_detect_pattern Patterns
#' @return A vector of same class as \code{x}.
#' @family string operations
#' @seealso
#' \code{\link[stringi]{stri_replace}} from \pkg{stringi} and
#' \code{\link[stringr]{str_replace}} from \pkg{stringr}
#' for the underlying implementation.
#'
#' @export
#'
#' @name seq-replace
#'
#' @examples
#' x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
#' seq_replace_pattern(x, dna("AAA"), dna("GGGGGG"))
#' seq_replace_pattern(x, "^A.{2}T", "TTTTTT")
#'
seq_replace_pattern <- function(x, pattern, replacement) {
  check_dna_rna_aa(x)
  pattern <- check_and_prepare_pattern(x, pattern)
  res <- stringr::str_replace_all(string = x, pattern = pattern,
                                  replacement = replacement)
  res <- coerce_seq_as_input(res, input = x)
  return(res)
}



#' Split sequences
#'
#' @inheritParams seq_detect_pattern
#' @inheritSection seq_detect_pattern Patterns
#' @return A list of vectors of same class as \code{x}.
#' @family string operations
#' @seealso
#' \code{\link[stringi]{stri_split}} from \pkg{stringi} and
#' \code{\link[stringr]{str_split}} from \pkg{stringr}
#' for the underlying implementation.
#'
#' @export
#'
#' @examples
#'
#' x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
#' seq_split_pattern(x, dna("AAA"))
#' seq_split_pattern(x, "T.G")
#'
seq_split_pattern <- function(x, pattern) {
  check_dna_rna_aa(x)
  pattern <- check_and_prepare_pattern(x, pattern)
  res <- stringr::str_split(string = x, pattern = pattern,
                            n = Inf, simplify = FALSE)
  res <- lapply(res, coerce_seq_as_input, input = x)
  return(res)
}



#' Count the number of matches in sequences
#'
#' @inheritParams seq_detect_pattern
#' @inheritSection seq_detect_pattern Patterns
#' @return An integer vector.
#' @family string operations
#' @seealso
#' \code{\link[stringi]{stri_count}} from \pkg{stringi} and
#' \code{\link[stringr]{str_count}} from \pkg{stringr}
#' for the underlying implementation.
#'
#' @export
#'
#' @examples
#'
#' x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
#' seq_count_pattern(x, dna("AAA"))
#' seq_count_pattern(x, "T.G")
#'
seq_count_pattern <- function(x, pattern) {
  check_dna_rna_aa(x)
  pattern <- check_and_prepare_pattern(x, pattern)
  res <- stringr::str_count(string = x, pattern = pattern)
  return(res)
}




############################# By Position #############################

#' Crop sequences between two positions
#'
#'
#' @param x a DNA, RNA or AA vector.
#' @param position_in an integer giving the position where to start cropping.
#' @param position_out an integer giving the position where to stop cropping.
#'
#' @return A cropped DNA, RNA or AA vector.
#' @family string operations
#' @seealso
#' \code{\link[stringi]{stri_sub}} from \pkg{stringi} and
#' \code{\link[stringr]{str_sub}} from \pkg{stringr}
#' for the underlying implementation.
#'
#' @export
#'
#' @examples
#'
#' x <- dna("ACGTTAGTGTAGCCGT")
#'
#' # Drop the first 3 nucleotides (ACG)
#' seq_crop_position(x, position_in = 4)
#'
#' # Crop codon between position 4 and 6
#' seq_crop_position(x, position_in = 4, position_out = 6)
#'
seq_crop_position <- function(x, position_in = 1, position_out = -1) {
  check_dna_rna_aa(x)
  out <- stringr::str_sub(x, start = position_in, end = position_out)
  out <- coerce_seq_as_input(out, x)
  return(out)
}



#' Remove a region between two positions in sequences.
#'
#' @param x a DNA, RNA or AA vector.
#' @param position_in an integer giving the position where to start to remove.
#' @param position_out an integer giving the position where to stop to remove.
#'
#' @return A vector of same class as \code{x}.
#' @family string operations
#' @seealso
#' \code{\link[stringr]{str_remove}} from \pkg{stringr}
#' for the underlying implementation.
#'
#' @export
#'
#' @examples
#'
#' x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
#' seq_remove_position(x, 2, 6)
#' seq_remove_position(x, 1:2, 3:4)
#'
seq_remove_position <- function(x, position_in, position_out) {
  check_dna_rna_aa(x)
  x_nchar <- nchar(x)
  position_out <- ifelse(position_out > x_nchar, x_nchar, position_out)
  d_pos <- position_out - position_in
  rgx <- paste0("(?<=.{", position_in - 1, "}).{", d_pos + 1, "}")
  res <- stringr::str_remove(x, pattern = rgx)
  res <- coerce_seq_as_input(res, input = x)
  return(res)
}



#' Replace a region between two positions in sequences
#'
#' @param x a DNA, RNA or AA vector.
#' @param position_in an integer giving the position where to start to replace.
#' @param position_out an integer giving the position where to stop to replace.
#' @param replacement a vector of replacements.
#'
#' @return A vector of same class as \code{x}.
#' @family string operations
#' @seealso
#' \code{\link[stringi]{stri_replace}} from \pkg{stringi} and
#' \code{\link[stringr]{str_replace}} from \pkg{stringr}
#' for the underlying implementation.
#'
#' @export
#' @examples
#'
#' x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
#' seq_replace_position(x, c(5, 2), 6, "-------")
seq_replace_position <- function(x, position_in, position_out, replacement){
  check_dna_rna_aa(x)
  x_nchar <- nchar(x)
  position_out <- ifelse(position_out > x_nchar, x_nchar, position_out)
  d_pos <- position_out - position_in
  rgx <- paste0("(?<=.{", position_in - 1, "}).{", d_pos + 1, "}")
  res <- stringr::str_replace(x, pattern = rgx, replacement = replacement)
  res <- coerce_seq_as_input(res, input = x)
  return(res)
}


#' Extract a region between two positions in sequences
#'
#' @param x a DNA, RNA or AA vector.
#' @param position_in an integer giving the position where to start to extract.
#' @param position_out an integer giving the position where to stop to extract.
#'
#' @return A vector of same class as \code{x}.
#' @family string operations
#' @seealso
#' \code{\link[stringi]{stri_extract}} from \pkg{stringi} and
#' \code{\link[stringr]{str_extract}} from \pkg{stringr}
#' for the underlying implementation.
#'
#' @export
#' @examples
#'
#' x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
#' seq_extract_position(x, 3, 8)
#'
seq_extract_position <- function(x, position_in, position_out){
  check_dna_rna_aa(x)
  x_nchar <- nchar(x)
  position_out <- ifelse(position_out > x_nchar, x_nchar, position_out)
  d_pos <- position_out - position_in
  rgx <- paste0("(?<=.{", position_in - 1, "}).{", d_pos + 1, "}")
  res <- stringr::str_extract(x, pattern = rgx)
  res <- coerce_seq_as_input(res, input = x)
  return(res)
}


############################# Assemble #############################

#' Combine multiple sequences
#'
#' @param ... One or more vectors of sequences (DNA, RNA, AA).
#' They must all be of the same type. Short vectors are recycled.
#' @param sep String to insert between input vectors.
#' @param collapse If not \code{NULL}, combine everything with this
#' string as separator.
#'
#' @details
#' The strings \code{sep} and \code{collapse}w ill be coercced to
#' the type of input vectors with a warning if some character have to replaced.
#'
#' @return A vector of sequences (if collapse is \code{NULL}).
#' A vector with a single sequence, otherwise.
#' @family string operations
#' @seealso
#' \code{\link[stringi]{stri_join}} from \pkg{stringi} and
#' \code{\link[stringr]{str_c}} from \pkg{stringr}
#' for the underlying implementation.
#'
#' @export
#'
#' @examples
#'
#' x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
#' y <- dna("TTTTTTTT", "AAAAAAAAA")
#' seq_combine(x, y)
#' seq_combine(y, x, sep = "CCCCC")
#' seq_combine(y, x, sep = "CCCCC", collapse = "GGGGG")
#'
seq_combine <- function(..., sep = "", collapse = NULL) {
  x <- list(...)
  if(!(all(vapply(x, is_dna, vector("logical", 1))) |
       all(vapply(x, is_rna, vector("logical", 1))) |
       all(vapply(x, is_aa, vector("logical", 1)))
       )) {
    stop("Vectors must be of same class to be combined.")
  }
  res <- stringr::str_c(..., sep = sep, collapse = collapse)
  res <- coerce_seq_as_input(res, input = x[[1]])
  return(res)
}




############################# Extra #############################

seq_remove_gap <- function(x) {
  seq_remove_pattern(x, "-")
}


#' Split sequences into k-mers
#'
#' @param x A DNA, RNA or AA vector.
#' @param k an integer giving the size of the k-mer.
#'
#' @return a list of k-mer vectors of same class as \code{x}.
#' @family string operations
#' @seealso
#' \code{\link{seq_split_pattern}}.
#'
#' @export
#'
#' @examples
#'
#' x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
#' seq_split_kmer(x, k = 5)
seq_split_kmer <- function(x, k) {
  check_dna_rna_aa(x)
  res <- lapply(x, function(x) {
    x_len <- nchar(x)
    if(k > x_len) {
      warning("k was larger than the sequence: return NA.")
      return(NA)
    }
    km_start <- seq(1, x_len - k + 1)
    km_stop <- km_start + k - 1
    out <- mapply(stringr::str_sub, x, km_start, km_stop, USE.NAMES = FALSE)
    coerce_seq_as_input(out, input = x)
  })
  return(res)
}



# ### Assemble
# - seq_duplicate
# - seq_insert
