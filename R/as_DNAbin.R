

#' Coerce to DNAbin
#'
#' @param x An object.
#' @param ... Other parameters.
#'
#' @return A DNAbin object.
#' @export
#'
#'
as_DNAbin <- function (x, ...) {
  UseMethod("as_DNAbin", x)
}


#' @export
#'
as_DNAbin.bioseq_dna <- function(x, ...) {
  check_dna(x)
  x <- stringr::str_split(x, "")
  res <- ape::as.DNAbin(x)
  return(res)
}


#' Coerce tibble to DNAbin
#'
#' @param x a tibble.
#' @param labels Name of the tibble column that stores the sequence labels.
#' @param sequences Name of the tibble column that stores the sequences.
#' @param ... Other params.
#'
#' @return A DNAbin object.
#' @export
#'
#'
as_DNAbin.tbl_df <- function(x, sequences, labels = NULL, ...){

  quo_labels <- rlang::enquo(labels)
  quo_sequences <- rlang::enquo(sequences)

  if(!rlang::quo_is_null(quo_labels)) {
    labels <- tibble::deframe(dplyr::select(x, !!quo_labels))
  }

  sequences <- tibble::deframe(dplyr::select(x, !!quo_sequences))
  check_dna(sequences)

  res <- stringr::str_split(sequences, pattern = "")
  res <- ape::as.DNAbin.list(res)
  names(res) <- labels

  return(res)
}




#' Coerce to AAbin
#'
#' @param x An object.
#' @param ... Other parameters.
#'
#' @return An AAbin object.
#' @export
#'
#'
as_AAbin <- function (x, ...) {
  UseMethod("as_AAbin", x)
}


#' @export
#'
as_AAbin.bioseq_aa <- function(x, ...) {
  check_aa(x)
  x_na <- is.na(x)
  x <- stringr::str_split(x, "")
  res <- ape::as.AAbin(x)
  res[x_na] <- NA
  return(res)
}


#' Coerce tibble to AAbin
#'
#' @param x a tibble.
#' @param labels Name of the tibble column that stores the sequence labels.
#' @param sequences Name of the tibble column that stores the sequences.
#' @param ... Other params.
#'
#' @return An AAbin object.
#' @export
#'
#'
as_AAbin.tbl_df <- function(x, sequences, labels = NULL, ...){

  quo_labels <- rlang::enquo(labels)
  quo_sequences <- rlang::enquo(sequences)

  if(!rlang::quo_is_null(quo_labels)) {
    labels <- tibble::deframe(dplyr::select(x, !!quo_labels))
  }

  sequences <- tibble::deframe(dplyr::select(x, !!quo_sequences))
  check_aa(sequences)

  res <- stringr::str_split(sequences, pattern = "")
  res_na <- is.na(res)
  res <- ape::as.AAbin(res)
  res[res_na] <- NA
  names(res) <- labels

  return(res)
}
