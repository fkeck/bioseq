
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
  fas <- stringr::str_split(fas, "^>|(?<=\\n)>")
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
#' @param line_length length (in number of character) of one line
#' (excluding spaces separating blocks). Use \code{Inf} to avoid line breaks.
#' @param block_length length (in number of character) of one block.
#' Use the same value as \code{line_length} or \code{Inf} to avoid
#' block separation.
#'
#' @family input/output operations
#' @export
#'
write_fasta <- function(x, file, append = FALSE, line_length = 80, block_length = 10) {

  x_nchar <- stringr::str_length(x)
  x_is_na <- is.na(x)

  if(line_length == Inf) {
    line_length <- max(x_nchar, na.rm = TRUE)
  }

  if(block_length == Inf) {
    block_length <- line_length
  }

  blocks_by_line <- line_length/block_length

  if(block_length > line_length) {
    stop("The length of blocks cannot be higher than the length of lines")
  }

  if(line_length %% block_length > 0L) {
    stop("The length of lines must be a multiple of the length of blocks")
  }


  if(any(x_is_na | x_nchar == 0L)) {
    input_len <- length(x)
    x <- x[!is.na(x)]
    output_len <- length(x)
    warning("Found ", input_len - output_len,
            " NA and/or empty sequences. They were not exported since ",
            "the FASTA format does not support missing values.")
  }

  x <- as.character(x)
  x <- vapply(x, function(x) {
    x_len <- stringr::str_length(x)
    blocks <- seq(1, x_len, by = block_length)
    res <- stringr::str_sub(x, blocks, blocks -1 + block_length)
    separator <- rep(" ", length(res))
    if(x_len > line_length) {
      separator[seq(blocks_by_line, blocks_by_line * (x_len %/% line_length), blocks_by_line)] <- "\n"
    }
    separator[length(separator)] <- ""
    res <- stringr::str_c(res, separator, collapse = "")
    res
  }, vector("character", 1))
  fas <- stringr::str_c(">", names(x), sep = "")
  fas <- stringr::str_c(fas, x, sep = "\n", collapse = "\n\n")
  readr::write_file(fas, file, append = append)
}

