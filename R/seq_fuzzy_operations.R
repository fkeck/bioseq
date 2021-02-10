


# Approximate string matching to detect patterns
# with a maximum error rate.
# Pattern and max_dist are vectorized along x.
seq_detect_fuzzypattern <- function(x, pattern, max_dist = 0.0) {

  check_dna_rna_aa(x)
  pattern <- check_and_prepare_pattern(x, pattern)
  pattern <- stringr::str_split(pattern, pattern = "\\|")
  pattern <- rep_len(pattern, length(x))
  max_dist <- rep_len(max_dist, length(x))

  if(any(max_dist < 0) | any(max_dist > 1)) {
    stop("max_dist must be a numeric value ranging from 0 to 1.")
  }

  mapply(function(x, pattern, max_dist) {
    max_dist <- floor(max_dist * stringr::str_length(pattern))
    pat_dist <- stringdist::afind(x, pattern, value = FALSE)$distance
    apply(pat_dist, 1, function(x) any(x <= max_dist))
    },
    x = x, pattern = pattern, max_dist = max_dist,
    SIMPLIFY = TRUE, USE.NAMES = FALSE)

}





#' Crop sequences using fuzzy delimiting patterns
#'
#' This function extracts sub-sequences given two
#' patterns delimiting the cropping region.
#' This is useful to isolate a genetic region
#' from two sets of primers (like a PCR would do).
#' The function uses approximate string matching,
#' which allows to specify an error rate for pattern matching.
#'
#' @param x a DNA, RNA or AA vector to be cropped.
#' @param pattern_in a vector of patterns to be matched
#' against the sequences in \code{x}
#' and defining the beginning (left-side) of the returned
#' sequences. Pass \code{NULL} to avoid cropping.
#' @param pattern_out a vector of patterns to be matched
#' against the sequences in \code{x}
#' and defining the end (right-side) of the returned sequences.
#' Pass \code{NULL} to avoid cropping.
#' @param max_dist_in the maximum edit distance between the
#' sequence and the best matching \code{pattern_in}
#' i.e. maximum error allowed between the sequence and the patterns.
#' If the error rate is higher,
#' this will produce \code{NA}.
#' @param max_dist_out same for \code{pattern_out}.
#' @param include_patterns A logical. Should the patterns included
#' in the returned cropped sequences (default is \code{TRUE}).
#'
#' @details
#'
#' If \code{pattern_in}/\code{pattern_out} are DNA, RNA or AA vectors,
#' patterns sequences will be processed with \code{seq_disambiguate_IUPAC}.
#'
#' Currently, ambiguities in the input sequences are ignored.
#'
#' @inheritSection seq_detect_pattern Patterns
#'
#' @return A character vector of cropped sequences.
#'
#' @examples
#' x <-as_dna(c("ACTGGTCAGCATTAGTGCTACTGTTACAGGT", "GCCCCTGCAATT", "AAAAGGGTTTTTTT"))
#'
#'
#' # If no match result will be NA
#' seq_crop_pattern(x, pattern_in = dna("TCAGCATT", "CCCCTG"),
#'                   pattern_out = dna("TGTTACA"))
#'
#' # Pattern can be a regex.
#' #seq_crop_pattern(x, pattern_in = "A..A..A")
#'
#' # Ambiguous patterns
#' x <- dna("ACGATTTTTTT")
#' seq_crop_pattern(x, pattern_in = dna("ACRA"),
#'                 include_patterns = FALSE)
#'
#' x <- dna("ACGAGTTTTTTCCCCCCC", "VACGAGTTTTTTCCCCCCC")
#' seq_crop_fuzzypattern(x,
#'                  pattern_in = dna("ACRAN"),
#'                  pattern_out = dna("CCCC", "GGGG"),
#'                  max_dist_in = 0,
#'                  max_dist_out = 0,
#'                  include_patterns = FALSE)
#'
#'
seq_crop_fuzzypattern <- function(x,
                                  pattern_in = NULL,
                                  pattern_out = NULL,
                                  max_dist_in = 0,
                                  max_dist_out = 0,
                                  include_patterns = TRUE) {

  check_dna_rna_aa(x)

  # Pattern in
  if(is.null(pattern_in)) {
    pattern_in_idx <- rep(1L, length(x))
  } else {
    pattern_in <- check_and_prepare_pattern(x, pattern_in)
    pattern_in <- stringr::str_split(pattern_in, pattern = "\\|")
    pattern_in <- rep_len(pattern_in, length(x))
    max_dist_in <- rep_len(max_dist_in, length(x))

    if(any(max_dist_in < 0) | any(max_dist_in > 1)) {
      stop("max_dist_in must be a numeric value ranging from 0 to 1.")
    }

    pattern_in_idx <-
      mapply(function(x, pattern_in, max_dist_in) {
        max_dist_in <- floor(max_dist_in * stringr::str_length(pattern_in))
        pattern_find <- stringdist::afind(x, pattern_in, value = TRUE, method = "hamming")
        sel <- pattern_find$distance <= max_dist_in
        pattern_find$location <- pattern_find$location[sel]
        #pattern_find$distance <- pattern_find$distance[sel]
        pattern_find$match <- pattern_find$match[sel]
        res <- ifelse(all(sel == FALSE),
                      NA,
                      min(pattern_find$location))
        if(!include_patterns) {
          res <- ifelse(is.na(res),
                        NA,
                        res + stringr::str_length(pattern_find$match[which.min(pattern_find$location)]))
        }
        res
      },
      x = x, pattern = pattern_in, max_dist = max_dist_in,
      SIMPLIFY = TRUE, USE.NAMES = FALSE)

  }

  out <- stringr::str_sub(x, start = pattern_in_idx)

  # Pattern out
  if(is.null(pattern_out)) {
    pattern_out_idx <- rep(-1L, length(x))
  } else {
    pattern_out <- check_and_prepare_pattern(x, pattern_out)
    pattern_out <- stringr::str_split(pattern_out, pattern = "\\|")
    pattern_out <- rep_len(pattern_out, length(x))
    max_dist_out <- rep_len(max_dist_out, length(x))

    if(any(max_dist_out < 0) | any(max_dist_out > 1)) {
      stop("max_dist_out must be a numeric value ranging from 0 to 1.")
    }

    pattern_out_idx <-
      mapply(function(x, pattern_out, max_dist_out) {
        max_dist_out <- floor(max_dist_out * stringr::str_length(pattern_out))
        pattern_find <- stringdist::afind(stringi::stri_reverse(x),
                                          stringi::stri_reverse(pattern_out),
                                          value = TRUE, method = "hamming")
        sel <- pattern_find$distance <= max_dist_out
        pattern_find$location <- stringr::str_length(x) - pattern_find$location[sel]
        # pattern_find$distance <- pattern_find$distance[sel]
        pattern_find$match <- pattern_find$match[sel]

        res <- ifelse(all(sel == FALSE) ,
                      NA,
                      max(pattern_find$location) + 1)

        if(!include_patterns) {
          res <- ifelse(is.na(res),
                        NA,
                        res - stringr::str_length(pattern_find$match[which.max(pattern_find$distance)]))
        }
        res
      },
      x = out, pattern = pattern_out, max_dist = max_dist_out,
      SIMPLIFY = TRUE, USE.NAMES = FALSE)

  }

  out <- stringr::str_sub(out, end = pattern_out_idx)

  out <- coerce_seq_as_input(out, x)
  return(out)
}
