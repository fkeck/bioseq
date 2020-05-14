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




test_that("Consensus majority works with DNA", {
  x <- dna("-----TACGCAGTAAAAGCTACTGATG",
           "CGTCATACGCAGTAAAAACTACTGATG",
           "CTTCATACGCAGTAAAAACTACTGATG",
           "CTTCATATGCAGTAAAATCTACTGATG",
           "CTTCATACGCAGTAAAAACTACTGATG",
           "CGTCATACGCAGTAAAAGCTACTGATG",
           "CTTCATATGCAGTAAAAGCTACTGACG")
  cons <- seq_consensus(x, method = "chr_majority")
  expect_equal(cons, dna("CTTCATACGCAGTAAAARCTACTGATG"))
  cons <- seq_consensus(x, method = "chr_majority",
                        weights = c(10, 1, 1, 1, 1, 1, 1))
  expect_equal(cons, dna("-----TACGCAGTAAAAGCTACTGATG"))
})

test_that("Consensus majority works with DNA", {
  x <- rna("-----UACGCAGUAAAAGCUACUGAUG",
           "CGUCAUACGCAGUAAAAACUACUGAUG",
           "CUUCAUACGCAGUAAAAACUACUGAUG",
           "CUUCAUAUGCAGUAAAAUCUACUGAUG",
           "CUUCAUACGCAGUAAAAACUACUGAUG",
           "CGUCAUACGCAGUAAAAGCUACUGAUG",
           "CUUCAUAUGCAGUAAAAGCUACUGACG")
  cons <- seq_consensus(x, method = "chr_majority")
  expect_equal(cons, rna("CUUCAUACGCAGUAAAARCUACUGAUG"))
})

test_that("Consensus majority works with AA", {
  x <-  aa("CKLSI", "CLLSL")
  cons <- seq_consensus(x, method = "chr_majority")
  expect_equal(cons, aa("CXLSJ"))
})


test_that("Consensus ambiguity works with DNA", {
  x <- dna("-----TACGCAGTAAAAGCTACTGATG",
           "CGTCATACGCAGTAAAAACTACTGATG",
           "CTTCATACGCAGTAAAAACTACTGATG",
           "CTTCATATGCAGTAAAATCTACTGATG",
           "CTTCATACGCAGTAAAAACTACTGATG",
           "CGTCATACGCAGTAAAAGCTACTGATG",
           "CTTCATATGCAGTAAAAGCTACTGACG")
  cons <- seq_consensus(x, method = "chr_ambiguity")
  expect_equal(cons, dna("-----TAYGCAGTAAAADCTACTGAYG"))
  cons <- seq_consensus(x, method = "chr_ambiguity",
                        weights = c(10, 1, 1, 1, 1, 1, 1))
  expect_equal(cons, dna("-----TAYGCAGTAAAADCTACTGAYG"))
})

test_that("Consensus majority works with DNA", {
  x <- rna("-----UACGCAGUAAAAGCUACUGAUG",
           "CGUCAUACGCAGUAAAAACUACUGAUG",
           "CUUCAUACGCAGUAAAAACUACUGAUG",
           "CUUCAUAUGCAGUAAAAUCUACUGAUG",
           "CUUCAUACGCAGUAAAAACUACUGAUG",
           "CGUCAUACGCAGUAAAAGCUACUGAUG",
           "CUUCAUAUGCAGUAAAAGCUACUGACG")
  cons <- seq_consensus(x, method = "chr_ambiguity")
  expect_equal(cons, rna("-----UAYGCAGUAAAADCUACUGAYG"))
})

test_that("Consensus majority works with AA", {
  x <-  aa("CKLSI", "CKLSI", "CLLSL")
  cons <- seq_consensus(x, method = "chr_ambiguity")
  expect_equal(cons, aa("CXLSJ"))
})
