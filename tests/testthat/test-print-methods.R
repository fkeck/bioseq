context("Print methods")

test_that("DNA prints correctly", {
  test <- dna(A = "ACGT", B = "RWSAG", C = NA)
  expect_output(print(test), "DNA vector of 3 sequences")
})


test_that("RNA prints correctly", {
  test <- rna("ACGU", "RWSAG", NA)
  expect_output(print(test), "RNA vector of 3 sequences")
  test <- rna(paste(rep("ACGU", 100), collapse = ""))
  expect_output(print(test), "+ 340 bases")
  test <- rna(rep("ACGU", 100))
  expect_output(print(test), "with 88 more sequences")
})

test_that("AA prints correctly", {
  test <- aa("ACGU", "RWSAG", NA)
  expect_output(print(test), "AA vector of 3 sequences")
})


test_that("tibble with DNA prints correctly", {
  test <- tibble::tibble(A = dna("ACGT", "RWSAG", NA))
  expect_output(print(test), "<DNA>")
})

test_that("tibble with RNA prints correctly", {
  test <- tibble::tibble(A = rna("ACGU", "RWSAG", NA))
  expect_output(print(test), "<RNA>")
})

test_that("tibble with AA prints correctly", {
  test <- tibble::tibble(A = aa("ACGU", "RWSAG", NA))
  expect_output(print(test), "<AA>")
})
