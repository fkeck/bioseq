
#' Transcribe DNA, reverse-transcribe RNA
#'
#' @param x A vector of DNA for \code{seq_transcribe},
#' a vector of RNA for \code{seq_rev_transcribe}
#'
#' @return A vector of RNA for \code{seq_transcribe},
#' a vector of DNA for \code{seq_rev_transcribe}
#' @family biological operations
#' @export
#'
#' @name transcription
seq_transcribe <- function(x) {
  check_dna(x)
  out <- stringr::str_replace_all(x, "T", "U")
  out <- as_rna(out)
  return(out)
}

#' @rdname transcription
#' @export
seq_rev_transcribe <- function(x) {
  check_rna(x)
  out <- stringr::str_replace_all(x, "U", "T")
  out <- as_dna(out)
  return(out)
}



#' Translate DNA/RNA sequences into amino acids
#'
#' @param x a vector of DNA (bioseq_dna) or RNA (bioseq_rna).
#' @param code an integer indicating the genetic code to use for translation
#' (default 1 uses the Standard genetic code). See Details.
#' @param codon_frame an integer giving the nucleotide position
#' where to start translation.
#' @param codon_init a logical indicating whether the first codon is evaluated
#' as a possible codon start and translated to methionine.
#'
#' @details
#'
#' Several genetic codes can be used for translation. See \link{genetic-codes}
#' to get the list of available genetic codes and their ID number.
#'
#' Gaps (-) are interpreted as unknown nucleotides (N) but can be
#' removed prior to the translation with the function \code{seq_remove_gap}.
#'
#' The function deals with ambiguities on both sides.
#' This means that if ambiguous codons cannot
#' be translated to amino acid, they are translated to
#' the most specific ambiguous amino acids
#' (X in the most extreme case).
#'
#'
#' @return An amino acid vector (\code{bioseq_aa}).
#' @family biological operations
#' @export
#'
#' @examples
#' x <- dna(c("ATGCAGA", "GGR","TTGCCTAGKTGAACC", "AGGNGC", "NNN"))
#' seq_translate(x)
#'
seq_translate <- function(x, code = 1, codon_frame = 1, codon_init = FALSE) {

  check_dna_rna(x)
  x_names <- names(x)
  x_na <- is.na(x)

  if(is_rna(x)) {
    x <- seq_rev_transcribe(x)
  }

  # Get genetic code
  code <- as.character(code)
  gencode <- dic_genetic_codes()[[code]]

  # Crop sequences to respect codon_frame
  x <- stringr::str_sub(x, start = codon_frame)

  last_frame <- nchar(x) - nchar(x) %% 3
  x <- stringr::str_sub(x, end = last_frame)

  # Interpret gaps as N
  x <- stringr::str_replace_all(x, "-", "N")

  codons <- strsplit(x, "")
  codons <- lapply(codons, function(x) paste0(x[c(TRUE, FALSE, FALSE)],
                                              x[c(FALSE, TRUE, FALSE)],
                                              x[c(FALSE, FALSE, TRUE)]))

  dic_dna_ambig <- dic_dna()$ambiguity
  dic_aa_ambig <- dic_aa()$ambiguity
  n_dna_ambig <- vapply(dic_dna_ambig, length, vector("integer", 1))
  n_aa_ambig <- vapply(dic_aa_ambig, length, vector("integer", 1))

  n_codons <- vapply(codons, length, vector("integer", 1))

  ff <- factor(rep(seq_along(codons), times = n_codons))
  codons <- unlist(codons)

  rgx <- names(dic_dna_ambig)[n_dna_ambig > 1]
  rgx <- paste0("[", paste(rgx, collapse = ""), "]")
  ambig_codons <- stringr::str_detect(codons, rgx)

  nnn_codons <- stringr::str_count(codons, "N") >= 2

  dc <- seq_disambiguate_IUPAC(dna(codons[ambig_codons & !nnn_codons]))
  dc <- lapply(dc, function(x) unique(gencode[x]))
  dc <- vapply(dc, function(x) {
    if(length(x) == 1L) {
      x <- as.character(x)
      return(x)
    } else {
      sub_dic_aa_ambig <- dic_aa_ambig[n_aa_ambig >= length(x)]
      inclusion <- vapply(sub_dic_aa_ambig,
                          function(y) all(x %in% y),
                          vector("logical", 1))
      out <- n_aa_ambig[names(inclusion)][inclusion]
      out <- names(out)[which.min(out)]
      out <- out
      return(out)
    }
  }, vector("character", 1))

  res <- vector(mode = "character", length = length(codons))
  res[ambig_codons & !nnn_codons] <- dc
  res[!ambig_codons] <- gencode[codons[!ambig_codons]]
  res[nnn_codons] <- "X"

  if(codon_init) {
    gencode_init_codon <- gencode
    gencode_init_codon[attr(gencode, "Codon_start")] <- "M"
    first_codons <- vector("logical", length(codons))
    first_codons[cumsum(c(1, n_codons))[seq_along(n_codons)]] <- TRUE
    res[first_codons & !ambig_codons] <-
      gencode_init_codon[codons[first_codons & !ambig_codons]]
  }

  res <- split(res, ff)
  res <- vapply(res, paste, vector("character", 1),
                collapse = "", USE.NAMES = FALSE)
  res[x_na] <- NA
  names(res) <- x_names
  res <- aa(res)
  return(res)
}



#' Reverse translate amino acid sequences
#'
#' The function perform reverse translation of amino acid sequences.
#' Such operation does not exist in nature but is provided for completeness.
#' Because of codon degeneracy it is expected
#' to produce many ambiguous nucleotides.
#'
#' @param x an amino acid sequence (\code{bioseq_aa})
#' @param code an integer indicating the genetic code to use for
#' reverse translation (default 1 uses the Standard genetic code). See Details.
#'
#' @details
#' Gaps (-) are interpreted as unknown amino acids (X) but can be
#' removed prior to the translation with the function \code{seq_remove_gap}.
#'
#' @return a vector of DNA sequences.
#' @family biological operations
#' @export
#'
#' @examples
#'
#' x <- dna("ACTTTGGCTAAG")
#' y <- seq_translate(x)
#' z <- seq_rev_translate(y)
#' z
#' # There is a loss of information during the reverse translation
#' all.equal(x, z)
#'
seq_rev_translate <- function(x, code = 1) {
  check_aa(x)
  x_names <- names(x)
  x_na <- is.na(x)

  # Get genetic code
  code <- as.character(code)
  gencode <- dic_genetic_codes()[[code]]
  gencode <-split(names(gencode), gencode)

  dic_dna_ambig <- dic_dna()$ambiguity
  dic_aa_ambig <- dic_aa()$ambiguity
  n_dna_ambig <- vapply(dic_dna_ambig, length, vector("integer", 1))

  # Interpret gaps as X
  x <- stringr::str_replace_all(x, "-", "X")

  x_split <- strsplit(x, "")
  n_aa <- vapply(x_split, length, vector("integer", 1))
  ff <- factor(rep(seq_along(x_split), times = n_aa))
  x_split <- unlist(x_split)

  res <- lapply(x_split, function(x) {
    if(is.na(x)){
      res <- list(NA, NA, NA)
    } else {
      tc <- unlist(strsplit(unlist(gencode[dic_aa_ambig[[x]]]), ""))
      tc_len <- length(tc)
      codon_1 <- unique(tc[seq(1, tc_len - 2, by = 3)])
      codon_2 <- unique(tc[seq(2, tc_len - 1, by = 3)])
      codon_3 <- unique(tc[seq(3, tc_len, by = 3)])
      res <- list(codon_1, codon_2, codon_3)
    }
    return(res)
  })

  res <- vapply(unlist(res, recursive = FALSE), function(x) {
    if(length(x) == 1L) {
      x <- as.character(x)
      return(x)
    } else {
      sub_dic_dna_ambig <- dic_dna_ambig[n_dna_ambig >= length(x)]
      inclusion <- vapply(sub_dic_dna_ambig,
                          function(y) all(x %in% y),
                          vector("logical", 1))
      out <- n_dna_ambig[names(inclusion)][inclusion]
      out <- names(out)[which.min(out)]
      return(out)
    }
  }, vector("character", 1))

  res <- split(res, rep(ff, each = 3))
  res <- vapply(res, paste, vector("character", 1),
                collapse = "", USE.NAMES = FALSE)
  res[x_na] <- NA
  names(res) <- x_names
  res <- dna(res)
  return(res)

}



#' Reverse and complement sequences
#'
#' @param x  a DNA or RNA vector.
#' Function \code{seq_reverse} also accepts AA vectors.
#'
#' @return A reverse or complement sequence (same class as the input).
#' @export
#' @family biological operations
#' @name rev_complement
#'
#' @examples
#' x <- dna("ACTTTGGCTAAG")
#' seq_reverse(x)
#' seq_complement(x)
#'
seq_complement <- function(x) {
  check_dna_rna(x)
  if(is_dna(x)) dic <- dic_dna()$complement
  if(is_rna(x)) dic <- dic_rna()$complement
  out <- vapply(stringr::str_split(x, ""), function(x) {
    stringr::str_flatten(dic[x], collapse = "")
    }, vector("character", 1))
  out <- coerce_seq_as_input(out, x)
  return(out)
}


#' @rdname rev_complement
#' @export
#'
seq_reverse <- function(x) {
  check_dna_rna_aa(x)
  out <- vapply(stringr::str_split(x, ""), function(x) {
    stringr::str_flatten(rev(x), collapse = "")
    }, vector("character", 1))
  out <- coerce_seq_as_input(out, x)
  return(out)
}



