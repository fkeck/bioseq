context("Misc operations")


test_that("Spellout works with DNA", {
  test <- dna(A = "ACGT", B = "RWSAG", C = NA)
  expect_equal(seq_spellout(test),
               c("adenine - cytosine - guanine - thymine",
                 "purine - weak - strong - adenine - guanine",
                 NA)
  )
})

test_that("Spellout works with RNA", {
  test <- rna(A = "ACGU", B = "RWSAG", C = NA)
  expect_equal(seq_spellout(test),
               c("adenine - cytosine - guanine - uracil",
                 "purine - weak - strong - adenine - guanine",
                 NA)
  )
})


test_that("Spellout works with AA", {
  test <- aa(A = "ACGU", B = "RWSAG", C = NA)
  expect_equal(seq_spellout(test),
               c("alanine - cysteine - glycine - selenocysteine",
                 "arginine - tryptophan - serine - alanine - glycine",
                 NA)
  )
  expect_equal(seq_spellout(test, short = TRUE),
               c("ala - cys - gly - sec",
                 "arg - trp - ser - ala - gly",
                 NA)
  )
})


test_that("seq_disambiguate_IUPAC with RNA", {
  test <- rna(A = "ACGU", B = "RWSAG", C = NA)
  test_disamb <- seq_disambiguate_IUPAC(test)
  expect_length(test_disamb, 3)
  expect_length(test_disamb[[1]], 1)
  expect_length(test_disamb[[2]], 8)
  expect_equal(test_disamb[[3]], rna(NA))
})
