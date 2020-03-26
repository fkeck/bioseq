#' SeaView: DNA sequences and phylogenetic tree viewer
#' 
#' This function uses SeaView (Gouy, Guindon & Gascuel, 2010) to visualize DNA sequences and phylogenetic trees.
#'
#' @param x DNA Sequences in a \code{DNAbin} object or a phylogenetic tree (class \code{phylo}).
#' @param seaview.exec a character string giving the path of the program.
#'
#' @references Gouy M., Guindon S. & Gascuel O. (2010) SeaView version 4 :
#' a multiplatform graphical user interface for sequence alignment and phylogenetic tree building.
#' Molecular Biology and Evolution 27(2):221-224.
#'
#' @export
#'
seaview <- function(x, seaview.exec = "seaview"){
  tmp <- tempfile()
  if(is(x, "phylo")){
    write.tree(x, file = tmp)
  }
  if(is(x, "DNAbin")){
    write.dna(x, file = tmp, format = "fasta")
  }
  comm <- paste(seaview.exec, tmp)
  system(comm, wait = FALSE)
}


#' AliView: DNA sequences viewer
#' 
#' This function uses AliView (Larsson, 2014) to visualize DNA sequences.
#'
#' @param x DNA Sequences in a \code{DNAbin} object.
#' @param aliview.exec a character string giving the path of the program.
#'
#' @references Larsson, A. (2014). AliView: a fast and lightweight alignment viewer and editor for large data sets. Bioinformatics30(22): 3276-3278.
#'
#' @export
#'
aliview <- function(x, aliview.exec = "aliview"){
  tmp <- tempfile()
  if(is(x, "DNAbin")){
    write.dna(x, file = tmp, format = "fasta")
  }
  comm <- paste(aliview.exec, tmp)
  system(comm, wait = FALSE)
}

