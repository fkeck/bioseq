
read_fasta <- function(file){
  res <- ape::read.FASTA(file = file)
  res <- DNAbin_to_tibble(res)
  return(res)
}


write_fasta <- function(x, file) {

}
