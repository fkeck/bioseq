context("String operations on sequences")

test_that("Pattern detection works", {
  x <- dna(c("ACGTTAGTGTAGCCGT", "CTCGAAATGA", NA))
  expect_equal(seq_detect_pattern(x, dna("CCG", "AAA", "GGG")),
               c(TRUE, TRUE, NA))
  expect_equal(seq_detect_pattern(x, "^A.{2}T"), c(TRUE, FALSE, NA))
})


test_that("Crop patterns works", {
  x <- dna("ACGTTAAAAAGTGTAGCCCCCGT", "CTCGAAATGA", NA)
  expect_equal(seq_crop_pattern(x, pattern_in = "AAAA", pattern_out = "CCCC"),
               dna("AGTGTAGC", NA, NA))
})

test_that("Pattern extraction works", {
  x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA", NA)
  expect_equal(seq_extract_pattern(x, dna("AAA")),
               list(dna(), dna("AAA"), dna(NA)))
  expect_equal(seq_extract_pattern(x, "T.G"),
               list(dna("TAG", "TAG"), dna("TCG"), dna(NA)))
})


test_that("Pattern deletion works", {
  x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA", NA)
  expect_equal(seq_remove_pattern(x, dna("AAA")),
               dna("ACGTTAGTGTAGCCGT", "CTCGTGA", NA))
  expect_equal(seq_remove_pattern(x, "^A.{2}T"),
               dna("TAGTGTAGCCGT", "CTCGAAATGA", NA))
})

test_that("Pattern replacement works", {
  x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA", NA)
  expect_equal(seq_replace_pattern(x, dna("AAA"), dna("GGGGGG")),
               dna("ACGTTAGTGTAGCCGT", "CTCGGGGGGGTGA", NA))
  expect_equal(seq_replace_pattern(x, "^A.{2}T", "TTTTTT"),
               dna("TTTTTTTAGTGTAGCCGT", "CTCGAAATGA", NA))
})


test_that("Pattern split works", {
  x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA", NA)
  expect_equal(seq_split_pattern(x, dna("AAA")),
               list(dna("ACGTTAGTGTAGCCGT"), dna("CTCG", "TGA"), dna(NA)))
  expect_equal(seq_split_pattern(x, "T.G"),
               list(dna("ACGT", "TG", "CCGT"), dna("C", "AAATGA"), dna(NA)))
})

test_that("Pattern count works", {
  x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA", NA)
  expect_equal(seq_count_pattern(x, dna("AAA")), c(0, 1, NA))
  expect_equal(seq_count_pattern(x, "T.G"), c(2, 1, NA))
})


test_that("Crop by position works", {
  x <- dna("ACGTTAGTGTAGCCGT")
  expect_equal(seq_crop_position(x, position_in = 4),
               dna("TTAGTGTAGCCGT"))
  expect_equal(seq_crop_position(x, position_in = 4, position_out = 6),
               dna("TTA"))
})

test_that("Remove by position works", {
  x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
  expect_equal(seq_remove_position(x, 2, 6),
               dna("AGTGTAGCCGT", "CATGA"))
  expect_equal(seq_remove_position(x, 1:2, 1:2),
               dna("CGTTAGTGTAGCCGT", "CCGAAATGA"))
})

test_that("Replace by position works", {
  x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
  expect_equal(seq_replace_position(x, c(5, 2), 6, "-------"),
               dna("ACGT-------GTGTAGCCGT", "C-------ATGA"))
})

test_that("Extract by position works", {
  x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
  expect_equal(seq_extract_position(x, 3, 8),
               dna("GTTAGT", "CGAAAT"))
})

test_that("Combine sequences works", {
  x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
  y <- dna("TTTTTTT", "AAAAAAAAA")
  expect_equal(seq_combine(x, y),
               dna("ACGTTAGTGTAGCCGTTTTTTTT", "CTCGAAATGAAAAAAAAAA"))
  expect_equal(seq_combine(y, x, sep = "CCCCC"),
               dna("TTTTTTTCCCCCACGTTAGTGTAGCCGT", "AAAAAAAAACCCCCCTCGAAATGA"))
  expect_equal(seq_combine(y, x, sep = "CCCCC", collapse = "GGGGG"),
               dna("TTTTTTTCCCCCACGTTAGTGTAGCCGTGGGGGAAAAAAAAACCCCCCTCGAAATGA"))
})


test_that("Combine sequences from two different classes fails", {
  x <- dna("ACGTTAGTGTAGCCGT", "CTCGAAATGA")
  y <- rna("UUUUUG", "AAAAAAAAA")
  expect_error(seq_combine(x, y),
               "Vectors must be of same class to be combined.")
})

test_that("Split kmers works", {
  x <- dna("ACGTT")
  expect_equal(seq_split_kmer(x, k = 3), list(dna("ACG", "CGT", "GTT")))
  expect_warning(seq_split_kmer(x, k = 10), "k was larger than the sequence")
})


test_that("Remove gaps works", {
  x <- dna("ACGTTAGT-------GTAGCCGT", "----CTCGAAA", NA)
  expect_equal(seq_remove_gap(x), dna("ACGTTAGTGTAGCCGT", "CTCGAAA", NA))
})
