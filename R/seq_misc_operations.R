
#' Disambiguate biological sequences
#'
#' This function finds all the combinations of sequences corresponding
#' to a given vector of sequences with ambiguities (IUPAC codes).
#'
#' @param x a DNA, RNA or AA vector
#'
#' @return A list of DNA, RNA or AA vectors (depending on the input)
#' giving all possible combinations.
#' @family op-misc
#' @export
#'
#' @examples
#'
#' x <- dna(c("AYCTGW", "CTTN"))
#' seq_disambiguate_IUPAC(x)
#'
#' y <- seq_transcribe(x)
#' seq_disambiguate_IUPAC(y)
#'
#' z <- aa("YJSNAALNX")
#' z <- seq_translate(y)
#' seq_disambiguate_IUPAC(z)
#'
seq_disambiguate_IUPAC <- function(x) {

  check_dna_rna_aa(x)

  if(is_dna(x)) dic_ambig <- dic_dna()$ambiguity
  if(is_rna(x)) dic_ambig <- dic_rna()$ambiguity
  if(is_aa(x)) dic_ambig <- dic_aa()$ambiguity

  res <- stringr::str_split(x, "")
  res <- lapply(res, function(x){
    if(is.na(x[1])) {
      return(NA)
    }
    out <- lapply(x, function(y){
      dic_ambig[[y]]
    })
    out <- base::expand.grid(out, stringsAsFactors = FALSE)
    out <- apply(out, 1, paste, collapse = "")
    return(out)
  })

  res <- lapply(res, coerce_seq_as_input,
                input = x, keep_names = FALSE)

  names(res) <- names(x)
  return(res)
}


#' Spell out sequences
#'
#' This function spells out nucleotides and amino acids in sequences.
#'
#' @param x  a DNA, RNA or AA vector
#' @param short logical. If TRUE, the function will return
#' 3-letters short names for amino acids (ignored for DNA and RNA).
#' @param collapse a character vector to separate the results.
#' Set to \code{NULL} to avoid collapsing the results.
#'
#' @return A character vector if collapse is not \code{NULL}.
#' A list of character vectors otherwise.
#' @family op-misc
#' @export
#'
#' @examples
#' x <- dna("ACGT")
#' seq_spellout(x)
#'
#' x <- rna("ACGU")
#' seq_spellout(x)
#'
#' x <- aa("ACGBTX")
#' seq_spellout(x)
#'
seq_spellout <- function(x, short = FALSE, collapse = " - "){
  check_dna_rna_aa(x)

  if(is_dna(x)) dic <- dic_dna()$description
  if(is_rna(x)) dic <- dic_rna()$description
  if(is_aa(x) & !short) dic <- dic_aa()$description
  if(is_aa(x) & short) dic <- dic_aa()$short_description

  out <- stringr::str_split(x, "")
  out <- lapply(out, function(x) dic[x])

  if(is.character(collapse)){
    vapply(out, stringr::str_c, vector("character", 1), collapse = collapse)
  }

}




