
# Experimental stuff. Non-exported yet.


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
#' #x <-as_dna(c("ACTGGTCAGCATTAGTGCTACTGTTACAGGT", "GCCCCTGCAATT"))
#'
#'
#' # If no match result will be NA
#' #seq_crop_pattern(x, pattern_in = dna("TCAGCATT"),
#' #                  pattern_out = dna("TGTTACA"))
#'
#' # Pattern can be a regex.
#' #seq_crop_pattern(x, pattern_in = "A..A..A")
#'
#' # Ambiguous patterns
#' #x <- dna("ACGATTTTTTT")
#' #seq_crop_pattern(x, pattern_in = dna("ACRA"),
#' #                include_patterns = FALSE)
#'
# seq_crop_fuzzypattern <- function(x, pattern_in = NULL, pattern_out = NULL,
#                              max_dist_in = 0, max_dist_out = 0,
#                              include_patterns = TRUE) {
#
#   check_dna_rna_aa(x)
#
#   # Pattern in
#   if(is.null(pattern_in)) {
#     pat_in_idx <- rep(0, length(x))
#   } else {
#     pattern_in <- check_and_prepare_pattern(pattern_in)
#     pattern_in <- paste0(pattern_in, collapse = "|")
#     pat_in_detect <- utils::aregexec(pattern_in, x,
#                                      max.distance = max_dist_in)
#     pat_in_len <- vapply(pat_in_detect, attr,
#                          vector("numeric", 1), which = "match.length")
#     pat_in_idx <- unlist(pat_in_detect)
#     if(!include_patterns){
#       pat_in_idx <- pat_in_idx + pat_in_len
#     }
#     pat_in_idx <- ifelse(pat_in_len == -1, NA, pat_in_idx)
#   }
#
#   # Pattern out
#   if(is.null(pattern_out)) {
#     pat_out_idx <- nchar(x)
#   } else {
#     pattern_out <- check_and_prepare_pattern(pattern_out)
#     pat_out_detect <- utils::aregexec(pattern_out, x,
#                                       max.distance = max_dist_out)
#     pat_out_len <- vapply(pat_out_detect, attr,
#                           vector("numeric", 1), which = "match.length")
#     pat_out_idx <- unlist(pat_out_detect)
#     if(include_patterns){
#       pat_out_idx <- pat_out_idx + pat_out_len - 1
#     } else {
#       pat_out_idx <- pat_out_idx - 1
#     }
#     pat_out_idx <- ifelse(pat_out_len == -1, NA, pat_out_idx)
#   }
#
#   # Crop and return
#   res <- stringr::str_sub(x, start = pat_in_idx, end = pat_out_idx)
#   res <- coerce_seq_as_input(res, x)
#   return(res)
# }
