
.onLoad <- function(...) {
  vctrs::s3_register("pillar::pillar_shaft", "bioseq_dna")
  vctrs::s3_register("pillar::pillar_shaft", "bioseq_rna")
  vctrs::s3_register("pillar::pillar_shaft", "bioseq_aa")
  invisible()
}
