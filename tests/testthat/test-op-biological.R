context("Biological operations on sequences")

test_that("Transcription DNA -> RNA works", {
  test <- dna("ACGT", "RWSAG", "NNN", NA)
  expect_equal(seq_transcribe(test), rna("ACGU", "RWSAG", "NNN", NA))
})

test_that("Reverse transcription RNA -> DNA works", {
  test <- rna("ACGU", "RWSAG", "NNN", NA)
  target <- dna("ACGT", "RWSAG", "NNN", NA)
  expect_equal(seq_rev_transcribe(test), target)
})

test_that("Translation DNA -> AA works", {
  test <- dna("ACGTTA", "RWSTAA", "NNN", NA)
  target<- aa("TL", "X*", "X", NA)
  expect_equal(seq_translate(test), target)
})

test_that("Translation DNA -> AA works single nucleotide", {
  test <- dna("A")
  target<- aa("X")
  expect_equal(seq_translate(test), target)
})


test_that("Translation RNA -> AA works", {
  test <- rna("ACGUUA", "RWSUAA", "NNN", NA)
  target<- aa("TL", "X*", "X", NA)
  expect_equal(seq_translate(test), target)
})

test_that("Reverse translation AA -> DNA works", {
  test <- aa("TL", "X*", "X", NA)
  target <- dna("ACNYTN", "NNNTRR", "NNN", NA)
  expect_equal(seq_rev_translate(test), target)
})


test_that("Translation DNA -> AA with different codes works", {
  test <- dna("AGA")
  expect_equal(seq_translate(test), aa("R"))
  expect_equal(seq_translate(test, code = 2), aa("*"))
  expect_equal(seq_translate(test, code = 13), aa("G"))
  expect_equal(seq_translate(test, code = 5), aa("S"))
})


test_that("Codon init in translation DNA -> AA works", {
  test <- dna("AGATTG", "TTGAGA")
  expect_equal(seq_translate(test), aa("RL", "LR"))
  expect_equal(seq_translate(test, codon_init = TRUE), aa("RL", "MR"))
})


test_that("Reverse sequence works", {
  test <- dna("AGATTG", "TTGAGA", NA)
  expect_equal(seq_reverse(test), dna("GTTAGA", "AGAGTT", NA))
  test <- rna("AGAUUG", "UUGAGA", NA)
  expect_equal(seq_reverse(test), rna("GUUAGA", "AGAGUU", NA))
  test <- aa("AXRKMQ", NA)
  expect_equal(seq_reverse(test), aa("QMKRXA", NA))
})


test_that("Complement sequence works", {
  test <- dna("AGATTG", "NARWS", NA)
  expect_equal(seq_complement(test), dna("TCTAAC", "NTYWS", NA))
  test <- rna("AGAUUG", "NARWS", NA)
  expect_equal(seq_complement(test), rna("UCUAAC", "NUYWS", NA))
})

