#' SeaView: DNA sequences and phylogenetic tree viewer
#'
#' This function opens SeaView (Gouy, Guindon & Gascuel, 2010)
#' to visualize biological sequences and phylogenetic trees.
#' The software must be installed on the computer.
#'
#' @param x a DNA, RNA or AA vector.
#' Alternatively a \code{DNAbin} or \code{AAbin} object or
#' a phylogenetic tree (class \code{phylo}).
#' @param seaview_exec a character string giving the path of the program.
#'
#' @details
#' By default, the function assumes that the executable is installed
#' in a directory located on the PATH. Alternatively the user can provide
#' an absolute path to the executable (i.e. the location where the software
#' was installed/uncompressed). This can be stored in the global
#' options settings using
#' \code{options(bioseq.seaview.exec = "my_path_to_seaview")}.
#'
#'
#' @references Gouy M., Guindon S. & Gascuel O. (2010) SeaView version 4 :
#' a multiplatform graphical user interface for sequence
#' alignment and phylogenetic tree building.
#' Molecular Biology and Evolution 27(2):221-224.
#'
#' @family GUI wrappers
#' @export
#'
seaview <- function(x, seaview_exec = getOption("bioseq.seaview.exec",
                                                default = "seawiew")){
  tmp <- tempfile()
  if(!any(class(x) %in% c("phylo", "DNAbin", "AAbin",
                          "bioseq_dna", "bioseq_rna",
                          "bioseq_aa"))) {
    stop("x cannot be open by SeaView")
  }
  if(is(x, "phylo")){
    ape::write.tree(x, file = tmp)
  }
  if(is(x, "DNAbin")){
    x <- as_dna(x)
  }
  if(is(x, "AAbin")){
    x <- as_aa(x)
  }
  if(is(x, "bioseq_dna") | is(x, "bioseq_rna") | is(x, "bioseq_aa")){
    write_fasta(x, file = tmp)
  }
  comm <- paste(seaview_exec, tmp)
  system(comm, wait = FALSE)
}


#' AliView: DNA sequences viewer
#'
#' This function uses AliView (Larsson, 2014) to visualize DNA sequences.
#' The software must be installed on the computer.
#'
#' @param x a DNA, RNA or AA vector.
#' Alternatively a \code{DNAbin} or \code{AAbin} object.
#' @param aliview_exec a character string giving the path of the program.
#'
#' @details
#' By default, the function assumes that the executable is installed
#' in a directory located on the PATH. Alternatively the user can provide
#' an absolute path to the executable (i.e. the location where the software
#' was installed/uncompressed). This information can be stored in the global
#' options settings using
#' \code{options(bioseq.aliview.exec = "my_path_to_aliview")}.
#'
#' @references Larsson, A. (2014). AliView: a fast and lightweight alignment
#' viewer and editor for large data sets. Bioinformatics 30(22): 3276-3278.
#'
#' @family GUI wrappers
#' @export
#'
aliview <- function(x, aliview_exec = getOption("bioseq.aliview.exec",
                                                default = "aliview")){
  tmp <- tempfile()
  if(!any(class(x) %in% c("DNAbin", "AAbin",
                          "bioseq_dna", "bioseq_rna",
                          "bioseq_aa"))) {
    stop("x cannot be open by AliView")
  }
  if(is(x, "DNAbin")){
    x <- as_dna(x)
  }
  if(is(x, "AAbin")){
    x <- as_aa(x)
  }
  if(is(x, "bioseq_dna") | is(x, "bioseq_rna") | is(x, "bioseq_aa")){
    write_fasta(x, file = tmp)
  }
  comm <- paste(aliview_exec, tmp)
  system(comm, wait = FALSE, ignore.stdout = TRUE)
}

