context("Clustering and consensus")

test_that("Clustering works with DNA", {
  x <- dna("-----TACGCAGTAAAAGCTACTGATG",
           "CGTCATACGCAGTAAAAACTACTGATG",
           "CTTCATACGCAGTAAAAACTACTGATG",
           "CTTCATATGCAGTAAAAACTACTGATG",
           "CTTCATACGCAGTAAAAACTACTGATG",
           "CGTCATACGCAGTAAAAGCTACTGATG",
           "CTTCATATGCAGTAAAAGCTACTGACG")
  clus <- seq_cluster(x)

  expect_type(clus, "integer")
  expect_length(clus, length(x))
})

test_that("Clustering works with RNA", {
  x <- rna("-----UACGCAGUAAAAGCUACUGAUG",
           "CGUCAUACGCAGUAAAAACUACUGAUG",
           "CUUCAUACGCAGUAAAAACUACUGAUG",
           "CUUCAUAUGCAGUAAAAACUACUGAUG",
           "CUUCAUACGCAGUAAAAACUACUGAUG",
           "CGUCAUACGCAGUAAAAGCUACUGAUG",
           "CUUCAUAUGCAGUAAAAGCUACUGACG")
  clus <- seq_cluster(x)

  expect_type(clus, "integer")
  expect_length(clus, length(x))
})

test_that("Clustering works with AA", {
  x <- aa("CKLSI", "CLLSN")
  clus <- seq_cluster(x)
  expect_type(clus, "integer")
  expect_length(clus, length(x))
})

test_that("Clustering fails if different length sequences", {
  x <- aa("CKLSIXXX", "CLLSN")
  expect_error(seq_cluster(x), "length")
})

