context("DNA class behaviour")

test_that("Construction of dna vector works", {
  test <- dna("ACGT", "RWSAG", NA)
  expect_s3_class(test, "bioseq_dna")
  expect_type(test, "character")
  expect_length(test, 3)
})


test_that("Non IUPAC character are changed to N", {
  suppressWarnings(test <- dna("AcgT", "RWSAG", "QQQ"))
  expect_equal(as.character(test[1]), "ACGT")
  expect_equal(as.character(test[2]), "RWSAG")
  expect_equal(as.character(test[3]), "NNN")
})


test_that("Lower case changed to upper case", {
  test <- dna("AcgT", "RWSAG", "NnN")
  expect_equal(as.character(test[1]), "ACGT")
  expect_equal(as.character(test[2]), "RWSAG")
  expect_equal(as.character(test[3]), "NNN")
})


test_that("Names are correctly returned", {
  test <- dna("AcgT", "RWSAG", NA)
  expect_null(names(test))
  test <- dna(A = "AcgT", B = "RWSAG", C = NA)
  expect_equal(names(test), c("A", "B", "C"))
})

