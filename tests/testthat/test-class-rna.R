context("RNA class behaviour")

test_that("Construction of rna vector works", {
  test <- rna("ACGU", "RWSAG", "NNN")
  expect_s3_class(test, "bioseq_rna")
  expect_type(test, "character")
  expect_length(test, 3)
})


test_that("Non IUPAC character are changed to N", {
  suppressWarnings(test <- rna("AcgU", "RWSAG", "QQQ"))
  expect_equal(as.character(test[1]), "ACGU")
  expect_equal(as.character(test[2]), "RWSAG")
  expect_equal(as.character(test[3]), "NNN")
})


test_that("Lower case changed to upper case", {
  test <- rna("AcgU", "RWSAG", "NnN")
  expect_equal(as.character(test[1]), "ACGU")
  expect_equal(as.character(test[2]), "RWSAG")
  expect_equal(as.character(test[3]), "NNN")
})


test_that("Names are correctly returned", {
  test <- rna("AcgU", "RWSAG", "NnN")
  expect_null(names(test))
  test <- rna(A = "AcgU", B = "RWSAG", C = "NnN")
  expect_equal(names(test), c("A", "B", "C"))
})

