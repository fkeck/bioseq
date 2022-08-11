

#' Sequence validator
#'
#' Validate character strings before sequence construction.
#'
#' @param x a character vector.
#' @param alphabet a character vector defining the sequence alphabet;
#' @param invalid_replacement a character to replace non valid characters
#' @param type type of sequence ("DNA", "RNA", "AA"). It is only
#' used to provide more informative warning messages.
#'
#' @details
#' Validation steps:
#' \enumerate{
#'   \item Check that \code{x} is a character vector, fails if not.
#'   \item Force alpha characters to uppercase
#'   \item Delete blank characters (spaces and tabs)
#'   \item Delete line breaks
#'   \item Converts . (dots) to - (as both can represent a gap)
#'   \item Replace invalid characters with N/X (with a warning).
#' }
#' @return A character vector.
#' @export
#' @keywords internal
#'
validate_seq <- function(x, alphabet, invalid_replacement, type = "DNA") {
  stopifnot(is.character(x))
  x_names <- names(x)
  x <- gsub("([abcdefghijklmnopqrstuvwxyz])","\\U\\1", x, perl = TRUE)
  x <- stringr::str_replace_all(x, pattern = "\\.", replacement = "-")
  x <- stringr::str_replace_all(x, pattern = "[:blank:]|\n", replacement = "")
  rgx_nonvalid <- paste0("[^", paste(alphabet, collapse = ""), "]")
  n_nonvalid <- sum(stringr::str_count(x, pattern = rgx_nonvalid),
                    na.rm = TRUE)
  if(n_nonvalid > 0) {
    x <- stringr::str_replace_all(x, pattern = rgx_nonvalid,
                                  replacement = invalid_replacement)
    if(type == "DNA" || type == "RNA") {
      warning("Non-standard IUPAC symbols detected for ",
              type,": ", n_nonvalid,
              " characters were converted to N.", call. = FALSE)
    }

    if(type == "AA") {
      warning("Non-standard IUPAC symbols detected for ",
              type,": ", n_nonvalid,
              " characters were converted to X.", call. = FALSE)
    }

  }
  if(!is.null(x_names)){
    names(x) <- x_names
  }
  return(x)
}


check_dna <- function(x, obj = "x") {
  if(!is_dna(x)) {
    stop(obj, " must be DNA (bioseq_dna).",
         " Use function as_dna() to turn objects into DNA.",
         call. = FALSE)
  }
}

check_rna <- function(x, obj = "x") {
  if(!is_rna(x)) {
    stop(obj, " must be RNA (bioseq_rna).",
         " Use function as_rna() to turn objects into RNA.",
         call. = FALSE)
  }
}

check_aa <- function(x, obj = "x") {
  if(!is_aa(x)) {
    stop(obj, " must be an amino acid sequence (bioseq_aa).",
    " Use function as_aa() to turn objects into bioseq_aa.",
         call. = FALSE)
  }
}

check_dna_rna <- function(x, obj = "x") {
  if(!is_dna(x) & !is_rna(x)) {
    stop(obj, " must be DNA (bioseq_dna) or RNA (bioseq_rna).",
         " See functions dna() and rna()",
         call. = FALSE)
  }
}

check_dna_rna_aa <- function(x, obj = "x") {
  if(!is_dna(x) & !is_rna(x) & !is_aa(x)) {
    stop(obj, " must be DNA (bioseq_dna), RNA (bioseq_rna) or",
         " amino acids (bioseq_aa). See functions dna(), rna() and aa()",
         call. = FALSE)
  }
}


coerce_seq_as_input <- function(x, input, keep_names = TRUE) {
  if(is_dna(input)) {
    out <- as_dna(x)
  } else if (is_rna(input)) {
    out <- as_rna(x)
  } else if (is_aa(input)) {
    out <- as_aa(x)
  } else {
    check_dna_rna_aa(x)
  }
  if(keep_names) {
    names(out) <- names(input)
  }
  return(out)
}


# Check class compatibily between x and pattern
# Return a disambiguated pattern
check_and_prepare_pattern <- function(x, pattern){

  if(length(x) < length(pattern)) {
    pattern <- pattern[seq_along(x)]
    warning("Pattern was longer than the vector of sequences.",
            " Last elements were ignored.")
  }
  pattern <- vapply(pattern, function(p) {
    if(is_dna(p) | is_rna(p) | is_aa(p)) {
      if((is_dna(x) & !is_dna(p)) |
         (is_rna(x) & !is_rna(p)) |
         (is_aa(x) & !is_aa(p))) {
        stop("If a pattern is a DNA/RNA/AA sequence,",
             " it must of same type as x.")
      }
      p <- seq_disambiguate_IUPAC(p)
      p <- do.call("c", p)
    }
    paste0(p, collapse = "|")
  }, vector("character", 1))
  names(pattern) <- NULL
  return(pattern)
}




