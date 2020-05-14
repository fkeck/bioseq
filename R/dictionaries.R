
# DNA

dic_dna <- function(){
  list(
    alphabet = c("A", "C", "G", "T", "W", "S", "M", "K",
                 "R", "Y", "B", "D", "H", "V", "N", "-"),
    ambiguity = list(
      "A" = "A",
      "C" = "C",
      "G" = "G",
      "T" = "T",
      "W" = c("A", "T"),
      "S" = c("C", "G"),
      "M" = c("A", "C"),
      "K" = c("G", "T"),
      "R" = c("A", "G"),
      "Y" = c("C", "T"),
      "B" = c("C", "G",	"T"),
      "D" = c("A", "G", "T"),
      "H" = c("A", "C", "T"),
      "V" = c("A", "C", "G"),
      "N" = c("A", "C",	"G", "T"),
      "-" = "-"
    ),
    complement = c(
      "A" = "T",
      "C" = "G",
      "G" = "C",
      "T" = "A",
      "W" = "W",
      "S" = "S",
      "M" = "K",
      "K" = "M",
      "R" = "Y",
      "Y" = "R",
      "B" = "V",
      "D" = "H",
      "H" = "D",
      "V" = "B",
      "N" = "N",
      "-" = "-"
    ),
    description = c(
      "A" = "adenine",
      "C" = "cytosine",
      "G" = "guanine",
      "T" = "thymine",
      "W" = "weak",
      "S" = "strong",
      "M" = "amino",
      "K" = "keto",
      "R" = "purine",
      "Y" = "pyrimidine",
      "B" = "not adenine",
      "D" = "not cytosine",
      "H" = "not guanine",
      "V" = "not thymine",
      "N" = "any",
      "-" = "gap"
    )
  )
}



# RNA

dic_rna <- function(){
  list(
    alphabet = c("A", "C", "G", "U", "W", "S", "M", "K",
                 "R", "Y", "B", "D", "H", "V", "N", "-"),
    ambiguity = list(
      "A" = "A",
      "C" = "C",
      "G" = "G",
      "U" = "U",
      "W" = c("A", "U"),
      "S" = c("C", "G"),
      "M" = c("A", "C"),
      "K" = c("G", "U"),
      "R" = c("A", "G"),
      "Y" = c("C", "U"),
      "B" = c("C", "G",	"U"),
      "D" = c("A", "G", "U"),
      "H" = c("A", "C", "U"),
      "V" = c("A", "C", "G"),
      "N" = c("A", "C",	"G", "U"),
      "-" = "-"
    ),
    complement = c(
      "A" = "U",
      "C" = "G",
      "G" = "C",
      "U" = "A",
      "W" = "W",
      "S" = "S",
      "M" = "K",
      "K" = "M",
      "R" = "Y",
      "Y" = "R",
      "B" = "V",
      "D" = "H",
      "H" = "D",
      "V" = "B",
      "N" = "N",
      "-" = "-"
    ),
    description = c(
      "A" = "adenine",
      "C" = "cytosine",
      "G" = "guanine",
      "U" = "uracil",
      "W" = "weak",
      "S" = "strong",
      "M" = "amino",
      "K" = "keto",
      "R" = "purine",
      "Y" = "pyrimidine",
      "B" = "not adenine",
      "D" = "not cytosine",
      "H" = "not guanine",
      "V" = "not uracil",
      "N" = "any",
      "-" = "gap"
    )
  )
}


# Amino acids

dic_aa <- function(){
  list(
    alphabet = c("A", "C", "D", "E", "F", "G", "H", "I",
                 "K", "L", "M", "N", "P", "Q", "R", "S", "T",
                 "V", "W", "Y", "B", "X", "Z", "J", "U", "O", "*", "-"),
    ambiguity = list(
      "A" = "A",
      "C" = "C",
      "D" = "D",
      "E" = "E",
      "F" = "F",
      "G" = "G",
      "H" = "H",
      "I" = "I",
      "K" = "K",
      "L" = "L",
      "M" = "M",
      "N" = "N",
      "P" = "P",
      "Q" = "Q",
      "R" = "R",
      "S" = "S",
      "T" = "T",
      "V" = "V",
      "W" = "W",
      "Y" = "Y",
      "B" = c("D", "N"),
      "X" = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L",
              "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y", "*"),
      "Z" = c("E", "Q"),
      "J" = c("I", "L"),
      "*" = "*",
      "-" = "-"
    ),
    description = c(
      "A" = "alanine",
      "C" = "cysteine",
      "D" = "aspartic acid",
      "E" = "glutamic acid",
      "F" = "phenylalanine",
      "G" = "glycine",
      "H" = "histidine",
      "I" = "isoleucine",
      "K" = "lysine",
      "L" = "leucine",
      "M" = "methionine",
      "N" = "asparagine",
      "P" = "proline",
      "Q" = "glutamine",
      "R" = "arginine",
      "S" = "serine",
      "T" = "threonine",
      "V" = "valine",
      "W" = "tryptophan",
      "Y" = "tyrosine",
      "B" = "asparagine or aspartic acid",
      "X" = "any",
      "Z" = "glutamine or glutamic acid",
      "J" = "leucine or isoleucine",
      "U" = "selenocysteine",
      "O" = "pyrrolysine",
      "*" = "stop",
      "-" = "gap"
    ),
    short_description = c(
      "A" = "ala",
      "C" = "cys",
      "D" = "asp",
      "E" = "glu",
      "F" = "phe",
      "G" = "gly",
      "H" = "his",
      "I" = "ile",
      "K" = "lys",
      "L" = "leu",
      "M" = "met",
      "N" = "asn",
      "P" = "pro",
      "Q" = "gln",
      "R" = "arg",
      "S" = "ser",
      "T" = "thr",
      "V" = "val",
      "W" = "trp",
      "Y" = "tyr",
      "B" = "asx",
      "X" = "xaa",
      "Z" = "glx",
      "J" = "xle",
      "U" = "sec",
      "O" = "pyl",
      "*" = "stp",
      "-" = "gap"
    )
  )
}

#' Biological alphabets
#'
#' List of the allowed characters for each type of sequences.
#'
#' @section DNA:
#' \code{A C G T W S M K R Y B D H V N -}
#'
#' @section RNA:
#' \code{A C G U W S M K R Y B D H V N -}
#'
#' @section AA:
#' \code{A C D E F G H I K L M N P Q R S T V W Y B X Z J U O * -}
#'
#' @references
#'  Nomenclature Committee of the
#'  International Union of Biochemistry (NC-IUB) (1986).
#'  Proc. Natl. Acad. Sci. USA. 83 (1): 4–8.
#'
#'  Nomenclature and Symbolism for Amino Acids and Peptides.
#'  IUPAC-IUB Joint Commission on Biochemical Nomenclature. 1983.
#'
#' @name alphabets
NULL
