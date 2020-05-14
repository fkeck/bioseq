context("AA class behaviour")

test_that("Construction of aa vector works", {
  test <- aa("ACGT", "RWSAG", "QQQ-")
  expect_s3_class(test, "bioseq_aa")
  expect_type(test, "character")
  expect_length(test, 3)
})


test_that("Non IUPAC character are changed to X", {
  test <- suppressWarnings(aa("AcgT", "RWSAG", "QQQ", "K@"))
  expect_equal(as.character(test[1]), "ACGT")
  expect_equal(as.character(test[2]), "RWSAG")
  expect_equal(as.character(test[3]), "QQQ")
  expect_equal(as.character(test[4]), "KX")
})


test_that("Lower case changed to upper case", {
  test <- aa("AcgT", "RWSAG", "QqQ")
  expect_equal(as.character(test[1]), "ACGT")
  expect_equal(as.character(test[2]), "RWSAG")
  expect_equal(as.character(test[3]), "QQQ")
})


test_that("Names are correctly returned", {
  test <- aa("AcgT", "RWSAG", "QqQ")
  expect_null(names(test))
  test <- aa(A = "AcgT", B = "RWSAG", C = "QqQ")
  expect_equal(names(test), c("A", "B", "C"))
})

