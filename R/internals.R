

as_character_matrix <- function(x){
  res <- stringr::str_split(x, "")
  do.call(rbind, res)
}


# Delete columns with at least one gap
# Return character vector
delete_gap_cols <- function(x) {
  x_mat <- as_character_matrix(x)
  sel <- apply(x_mat, 2, function(x) any(x == "-"))
  res <- x_mat[, !sel]
  res <- apply(res, 1, paste, collapse = "")
  return(res)
}
