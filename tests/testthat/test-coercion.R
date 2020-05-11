context("Coercion")

library(tibble)

test_that("Character to DNA and reverse", {
  test <- c("ACGT", "GTCCTATCATCTTACCA", NA)
  test_dna <- dna(test)
  test_as_dna <- as_dna(test)
  expect_true(is_dna(test_dna))
  expect_equal(test_dna, test_as_dna)
  expect_equal(test, as.character(test_dna))
})


test_that("Character to RNA and reverse", {
  test <- c("ACGU", "GUCCUAUCAUCUUACCA", NA)
  test_rna <- rna(test)
  test_as_rna <- as_rna(test)
  expect_true(is_rna(test_rna))
  expect_equal(test_rna, test_as_rna)
  expect_equal(test, as.character(test_rna))
})

test_that("Character to AA and reverse", {
  test <- c("MKLGU", "GUXCURUCSUMMUKCCV", NA)
  test_aa <- aa(test)
  test_as_aa <- as_aa(test)
  expect_true(is_aa(test_aa))
  expect_equal(test_aa, test_as_aa)
  expect_equal(test, as.character(test_aa))
})


test_that("DNA to tibble", {
  test <- dna("ACGT", "GTCCTATCATCTTACCA", NA)
  test_tibble <- as_tibble(test)
  expect_true(tibble::is_tibble(test_tibble))
  expect_equal(ncol(test_tibble), 2)
  expect_equal(nrow(test_tibble), 3)
  expect_equal(test_tibble$sequence, test)
  expect_equal(test_tibble$label, 1:3)
})

test_that("RNA to tibble", {
  test <- rna("ACGU", "GUCCUAUCAUCUUACCA", NA)
  test_tibble <- as_tibble(test)
  expect_true(tibble::is_tibble(test_tibble))
  expect_equal(ncol(test_tibble), 2)
  expect_equal(nrow(test_tibble), 3)
  expect_equal(test_tibble$sequence, test)
  expect_equal(test_tibble$label, 1:3)
})

test_that("AA to tibble", {
  test <- aa("MKLGU", "GUXCURUCSUMMUKCCV", NA)
  test_tibble <- as_tibble(test)
  expect_true(tibble::is_tibble(test_tibble))
  expect_equal(ncol(test_tibble), 2)
  expect_equal(nrow(test_tibble), 3)
  expect_equal(test_tibble$sequence, test)
  expect_equal(test_tibble$label, 1:3)
})

test_that("DNA to DNAbin and reverse", {
  test_dna <- dna("-ACGT", "GTCCTATCATCTTACCA", NA)
  test_DNAbin <- as_DNAbin(test_dna)
  expect_true(is(test_DNAbin, "DNAbin"))
  expect_equal(as_dna(test_DNAbin), test_dna)
})

test_that("AA to AAbin and reverse", {
  test_aa <- aa("MKLGU", "GUXCURUCSUMMUKCCV", NA,  "")
  test_AAbin <- as_AAbin(test_aa)
  expect_true(is(test_AAbin, "AAbin"))
  expect_equal(as_aa(test_AAbin), test_aa)
})


test_that("DNA to seqinr alignment and reverse", {
  test_dna <- dna("-ACGT", "GTCCTATCATCTTACCA", NA)
  test_ali <- as_seqinr_alignment(test_dna)
  expect_true(is(test_ali, "alignment"))
  expect_equal(as_dna(test_ali), test_dna)
})

test_that("RNA to seqinr alignment and reverse", {
  test_rna <- rna("ACGU", "GUCCUAUCAUCUUACCA", NA)
  test_ali <- as_seqinr_alignment(test_rna)
  expect_true(is(test_ali, "alignment"))
  expect_equal(as_rna(test_ali), test_rna)
})

test_that("AA to seqinr alignment and reverse", {
  test_aa <- aa("MKLGU", "GUXCURUCSUMMUKCCV", NA)
  test_ali <- as_seqinr_alignment(test_aa)
  expect_true(is(test_ali, "alignment"))
  expect_equal(as_aa(test_ali), test_aa)
})


test_that("DNAbin to tibble and reverse", {
  test_dna <- dna("ACGT", "GTCCTATCATCTTACCA", NA)
  test <- as_DNAbin(test_dna)
  test_tibble <- tibble::as_tibble(test)
  expect_true(tibble::is_tibble(test_tibble))
  expect_equal(ncol(test_tibble), 2)
  expect_equal(nrow(test_tibble), 3)
  expect_equal(test_tibble$sequence, test_dna)
  expect_equal(test_tibble$label, 1:3)
  expect_equal(as_DNAbin(test_tibble, sequences = sequence),
               test)
})

test_that("AAbin to tibble and reverse", {
  test_aa <- aa("MKLGU", "GUXCURUCSUMMUKCCV", NA)
  test <- as_AAbin(test_aa)
  test_tibble <- tibble::as_tibble(test)
  expect_true(tibble::is_tibble(test_tibble))
  expect_equal(ncol(test_tibble), 2)
  expect_equal(nrow(test_tibble), 3)
  expect_equal(test_tibble$sequence, test_aa)
  expect_equal(test_tibble$label, 1:3)
  expect_equal(as_AAbin(test_tibble, sequences = sequence),
               test)
})


