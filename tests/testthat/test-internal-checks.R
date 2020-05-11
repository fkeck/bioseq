context("Internal check functions")

test_that("Check this is DNA", {
  dna_test <- dna("ACGT")
  rna_test <- rna("ACGU")
  aa_test <- aa("QWKT")
  expect_silent(check_dna(dna_test))
  expect_error(check_dna(rna_test))
  expect_error(check_dna(aa_test))
})

test_that("Check this is RNA", {
  dna_test <- dna("ACGT")
  rna_test <- rna("ACGU")
  aa_test <- aa("QWKT")
  expect_silent(check_rna(rna_test))
  expect_error(check_rna(dna_test))
  expect_error(check_rna(aa_test))
})

test_that("Check this is AA", {
  dna_test <- dna("ACGT")
  rna_test <- rna("ACGU")
  aa_test <- aa("QWKT")
  expect_silent(check_aa(aa_test))
  expect_error(check_aa(dna_test))
  expect_error(check_aa(rna_test))
})


test_that("coerce_seq_as_input fails if input is not AA, RNA or DNA", {
  expect_error(coerce_seq_as_input(x = "AAAA", input = "AAAA"))
})


test_that("check_and_prepare_pattern fails
  if x and pattern of different classes", {
    dna_test <- dna("ACGT")
    rna_test <- rna("ACGU")
    expect_error(check_and_prepare_pattern(dna_test, rna_test))
})


test_that("check_and_prepare_pattern warns if length x < pattern", {
  dna_test1 <- dna("ACGT")
  dna_test2 <- dna("ACGA", "AAC")
  expect_warning(check_and_prepare_pattern(dna_test1, dna_test2))
})
