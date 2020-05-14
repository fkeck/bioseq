
#' Read sequences in FASTA format
#'
#' @param file A path to a file, a connection or a character string.
#' @param type Type of data. Can be "DNA" (the default), "RNA" or "AA".
#'
#' @return A DNA, RNA or AA vector (depending on \code{type} argument).
#' @family input/output operations
#' @export
#'
#'
read_fasta <- function(file, type = "DNA") {
  fas <- readr::read_file(file)
  fas <- stringr::str_split(fas, ">")
  fas <- stringr::str_split_fixed(fas[[1]][-1], "\n", n = 2)
  res <- stringr::str_replace_all(fas[, 2], "[:space:]", "")
  names(res) <- fas[, 1]
  if(type == "DNA") {
    return(dna(res))
  } else if (type == "RNA") {
    return(rna(res))
  } else if (type == "AA"){
    return(aa(res))
  }
}


#' Write sequences in FASTA format
#'
#' @param x a DNA, RNA or AA vector.
#' @param file a path to a file or a connection.
#' @param append a logical. If \code{TRUE} append the data to the file.
#' If \code{FALSE} (default), overwrite the file.
#' @family input/output operations
#' @export
#'
write_fasta <- function(x, file, append = FALSE) {
  x <- as.character(x)
  x <- vapply(x, function(x) {
    x_len <- stringr::str_length(x)
    blocks <- seq(1, x_len, by = 10)
    res <- stringr::str_sub(x, blocks, blocks -1 + 10)
    separator <- rep(" ", length(res))
    if(x_len > 80) {
      separator[seq(8, 8 * (x_len %/% 80), 8)] <- "\n"
    }
    separator[length(separator)] <- ""
    res <- stringr::str_c(res, separator, collapse = "")
    res
  }, vector("character", 1))
  fas <- stringr::str_c(">", names(x), sep = "")
  fas <- stringr::str_c(fas, x, sep = "\n", collapse = "\n\n")
  readr::write_file(fas, file, append = append)
}

