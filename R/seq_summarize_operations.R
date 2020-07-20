
#' Cluster sequences by similarity
#'
#' @param x a DNA, RNA or AA vector of sequences to clustered.
#' @param threshold Threshold value (range in [0, 1]).
#' @param method the clustering method (see details).
#'
#' @details
#' The function uses \pkg{ape} \code{\link[ape]{dist.dna}} and
#' \code{\link[ape]{dist.aa}}
#' functions to compute pairwise distances among sequences and
#' \code{\link[stats]{hclust}} for clustering.
#'
#' Computing a full pairwise diastance matrix can be computationally expensive.
#' It is recommended to use this function for moderate size dataset.
#'
#' Supported methods are:
#'  \itemize{
#'   \item \code{"single"} (= Nearest Neighbour Clustering)
#'   \item \code{"complete"} (= Farthest Neighbour Clustering)
#'   \item \code{"average"} (= UPGMA)
#'   \item \code{"mcquitty"} (= WPGMA)
#'}
#'
#' @return An integer vector with group memberships.
#' @family aggregation operations
#' @seealso Function \code{\link{seq_consensus}} to compute consensus
#' and representative sequences for clusters.
#'
#' @export
#'
#' @examples
#'
#' x <- c("-----TACGCAGTAAAAGCTACTGATG",
#'        "CGTCATACGCAGTAAAAACTACTGATG",
#'        "CTTCATACGCAGTAAAAACTACTGATG",
#'        "CTTCATATGCAGTAAAAACTACTGATG",
#'        "CTTCATACGCAGTAAAAACTACTGATG",
#'        "CGTCATACGCAGTAAAAGCTACTGATG",
#'        "CTTCATATGCAGTAAAAGCTACTGACG")
#' x <- dna(x)
#' seq_cluster(x)
#'
seq_cluster <- function(x, threshold = 0.05, method = "complete") {
  check_dna_rna_aa(x)
  x_names <- names(x)

  if(is_rna(x)) {
    x <- seq_rev_transcribe(x)
  }

  x_nchar <- nchar(x)
  if(max(x_nchar, na.rm = TRUE) - min(x_nchar, na.rm = TRUE) != 0L) {
    stop("All elements of x must be of same length.",
         " You probably want x to be aligned.")
  }

  if(is_dna(x)) {
    x_bin <- as_DNAbin(x)
    x_bin <- as.matrix(x_bin)
    x_dist <- ape::dist.dna(x_bin, model = "raw")
  } else {
    x_bin <- as_AAbin(x)
    x_bin <- as.matrix(x_bin)
    x_dist <- ape::dist.aa(x_bin)/x_nchar[1]
    }

  res <- stats::hclust(x_dist, method = method)
  res <- stats::cutree(res, h = threshold)
  names(res) <- x_names
  return(res)
}




#' Find a consensus sequence for a set of sequences.
#'
#' @param x a DNA, RNA or AA vector.
#' @param method the consensus method (see Details).
#' @param weights an optional numeric vector of same length
#' as \code{x} giving a weight for each input sequence.
#' @param gaps logical. Should the gaps ("-") taken into account.
#'
#' @details
#' "chr_majority", "chr_ambiguity", "seq_centrality", "seq_majority"
#'
#' For chr_ambiguity gap character always override other characters.
#' Use gaps = FALSE to ignore gaps.
#'
#' @return A consensus sequence
#' @family aggregation operations
#' @export
#'
#' @examples
#' x <- c("-----TACGCAGTAAAAGCTACTGATG",
#'        "CGTCATACGCAGTAAAAACTACTGATG",
#'        "CTTCATACGCAGTAAAAACTACTGATG",
#'        "CTTCATATGCAGTAAAAACTACTGATG",
#'        "CTTCATACGCAGTAAAAACTACTGATG",
#'        "CGTCATACGCAGTAAAAGCTACTGATG",
#'        "CTTCATATGCAGTAAAAGCTACTGACG")
#' x <- dna(x)
#' seq_consensus(x)
seq_consensus <- function(x, method = "chr_majority",
                          weights = NULL, gaps = TRUE){
  check_dna_rna_aa(x)

  if(is.null(weights)) {
    weights <- rep(1, length(x))
  }

  if(is_dna(x)) dic_ambig <- dic_dna()$ambiguity
  if(is_rna(x)) dic_ambig <- dic_rna()$ambiguity
  if(is_aa(x)) dic_ambig <- dic_aa()$ambiguity
  n_ambig <- vapply(dic_ambig, length, vector("integer", 1))


  if(method == "chr_majority") {
    x_mat <- stringr::str_split(x, pattern = "", simplify = TRUE)
    res <- apply(x_mat, 2, function(x_v){
      x_w <- weights
      if(!gaps) {
        x_w <- x_w[x_v != "-"]
        x_v <- x_v[x_v != "-"]
      }
      x_v <- dic_ambig[x_v]
      x_w <- mapply(function(x, y) rep(y, length(x)), x = x_v, y = x_w)
      x_v <- unlist(x_v, use.names = FALSE)
      x_w <- unlist(x_w, use.names = FALSE)

      res <- tapply(x_w, x_v, sum)
      res <- which(res == max(res))
      res <- names(res)
      if(length(res) > 1) {
        sub_dic_ambig <- dic_ambig[n_ambig >= length(res)]
        inclusion <- vapply(sub_dic_ambig,
                            function(y) all(res %in% y),
                            vector("logical", 1))
        res <- n_ambig[names(inclusion)][inclusion]
        res <- names(res)[which.min(res)]
      }
      return(res)
    })
  }

  if(method == "chr_ambiguity") {
    x_mat <- stringr::str_split(x, pattern = "", simplify = TRUE)
    res <- apply(x_mat, 2, function(x_v){
      x_w <- weights
      if(!gaps) {
        x_w <- x_w[x_v != "-"]
        x_v <- x_v[x_v != "-"]
      }
      x_v <- dic_ambig[x_v]
      x_w <- mapply(function(x, y) rep(y, length(x)), x = x_v, y = x_w)
      x_v <- unlist(x_v, use.names = FALSE)
      x_w <- unlist(x_w, use.names = FALSE)

      if(any(x_v == "-")) {
        return("-")
      }

      res <- tapply(x_w, x_v, sum)
      res <- names(res)
      if(length(res) > 1) {
        sub_dic_ambig <- dic_ambig[n_ambig >= length(res)]
        inclusion <- vapply(sub_dic_ambig,
                            function(y) all(res %in% y),
                            vector("logical", 1))
        res <- n_ambig[names(inclusion)][inclusion]
        res <- names(res)[which.min(res)]
      }
      return(res)
    })
  }

  res <- stringr::str_flatten(res)
  res <- coerce_seq_as_input(x = res, input = x, keep_names = FALSE)
  return(res)
}

