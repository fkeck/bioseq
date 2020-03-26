

DNAbin_to_character <- function(x) {
  if(!is(x, "DNAbin")) {
    stop("x must be of class DNAbin")
  }
  x <- as.list(x)
  x <- as.character(x)
  res <- sapply(x, function(x) toupper(paste(x, collapse = "")))
  return(res)
}

character_to_DNAbin <- function(x) {
  x <- strsplit(x, "")
  res <- ape::as.DNAbin(x)
  return(res)
}


DNAbin_to_tibble <- function(x, label = "label", sequence = "sequence") {
  x <- DNAbin_to_character(x)
  res <- tibble::enframe(x, name = label, value = sequence)
  return(res)
}

tibble_to_DNAbin <- function(x, label, sequence){
  quo_label <- dplyr::enquo(label)
  quo_sequence <- dplyr::enquo(sequence)
  labels <- tibble::deframe(dplyr::select(x, !!quo_label))
  sequences <- tibble::deframe(dplyr::select(x, !!quo_sequence))
  res <- str_split(sequences, pattern = "")
  res <- as.DNAbin(res)
  names(res) <- labels
  return(res)
}



