
#' Disambiguate a sequence of nucleotides
#'
#' This function finds all the combinations of sequences corresponding
#' to a sequence with ambiguities (IUPAC codes for multiple nucleotides).
#'
#' @param x a character string corresponding to the sequence to be disambiguated
#'
#' @return A character vector with all the possible combinations.
#' @export
#'
#' @examples
#'
#' seq_disambiguate_IUPAC("AYCTGW")
#'
#' # Characters that do not correspond to a valid IUPAC code are kept unchanged.
#' seq_disambiguate_IUPAC("QACTY")
seq_disambiguate_IUPAC <- function(x) {
  x <- unlist(strsplit(x, ""))
  x <- lapply(x, function(x) {
    res <- switch(EXPR = x,
                  "R" = c("A", "G"),
                  "Y" = c("C", "T"),
                  "S" = c("G", "C"),
                  "W" = c("A", "T"),
                  "K" = c("G", "T"),
                  "M" = c("A", "C"),
                  "B" = c("C", "G", "T"),
                  "D" = c("A", "G", "T"),
                  "H" = c("A", "C", "T"),
                  "V" = c("A", "C", "G"),
                  "N" = c("A", "C", "G", "T"),
                  x)
    return(res)
  })
  x <- expand.grid(x, stringsAsFactors = FALSE)
  x <- apply(x, 1, paste, collapse = "")
  return(x)
}



#' Crop sequences using delimiting patterns
#'
#' @param x a character vector of sequences to be cropped
#' @param pattern_in a character vector of patterns to be matched against the sequences
#' and defining the beginning (left-side) of the sequences
#' @param pattern_out a character vector of patterns to be matched against the sequences
#' and defining the end (right-side) of the sequences
#' @param max_dist_in the maximum edit distance between the sequence and the best matching pattern_in
#' i.e. maximum error allowed between the sequence and the patterns. If the error rate is higher,
#' this will produce \code{NA}.
#' @param max_dist_out same for \code{pattern_out}.
#' @param include_patterns A logical. Should the patterns included in the cropped sequences (default is \code{TRUE}).
#' @param disambiguate_patterns A logical. If \code{TRUE} (default), pattern sequences will be processed with \code{seq_disambiguate_IUPAC}.
#'
#' @return A character vector of cropped sequences.
#' @export
#'
#' @examples
#' x <- paste(LETTERS, collapse = "")
#'
#' seq_crop_pattern(x, pattern_in = "CDE", pattern_out = "MNO")
#' seq_crop_pattern(x, pattern_in = "CDE", pattern_out = "MNO", include_patterns = FALSE)
#' seq_crop_pattern(x, pattern_in = "CDE", pattern_out = NA)
#' seq_crop_pattern(x, pattern_in = NA, pattern_out = "MNO")
#' seq_crop_pattern(x, pattern_in = "MNO", pattern_out = "CDE")
#'
#' seq_crop_pattern(x, pattern_in = c("CDE", "XXXXBCDE"), pattern_out = c("MNO", "QRST"))
seq_crop_pattern <- function(x, pattern_in, pattern_out,
                             max_dist_in = 2, max_dist_out = 2,
                             include_patterns = TRUE, disambiguate_patterns = TRUE) {

  if(length(pattern_in) == 1 && is.na(pattern_in[1])) {
    pat_in_idx <- rep(0, length(x))
  } else {
    if(disambiguate_patterns) {
      pattern_in <- do.call("c", lapply(pattern_in, seq_disambiguate_IUPAC))
    }
    pattern_in <- paste0(pattern_in, collapse = "|")
    pat_in_detect <- aregexec(pattern_in, x, max.distance = max_dist_in)
    pat_in_len <- sapply(pat_in_detect, attr, which = "match.length")
    pat_in_idx <- unlist(pat_in_detect)
    if(!include_patterns){
      pat_in_idx <- pat_in_idx + pat_in_len
    }
    pat_in_idx <- ifelse(pat_in_len == -1, NA, pat_in_idx)
  }

  if(length(pattern_out) == 1 && is.na(pattern_out[1])) {
    pat_out_idx <- nchar(x)
  } else {
    if(disambiguate_patterns) {
      pattern_out <- do.call("c", lapply(pattern_out, seq_disambiguate_IUPAC))
    }
    pattern_out <- paste0(pattern_out, collapse = "|")
    pat_out_detect <- aregexec(pattern_out, x, max.distance = max_dist_out)
    pat_out_len <- sapply(pat_out_detect, attr, which = "match.length")
    pat_out_idx <- unlist(pat_out_detect)
    if(include_patterns){
      pat_out_idx <- pat_out_idx + pat_out_len -1
    } else {
      pat_out_idx <- pat_out_idx - 1
    }
    pat_out_idx <- ifelse(pat_out_len == -1, NA, pat_out_idx)
  }

  x <- substr(x, start = pat_in_idx, stop = pat_out_idx)
  return(x)
}

seq_crop_position <- function(x, position_in, position_out) {}

seq_insert <- function(x, ins, position) {}

