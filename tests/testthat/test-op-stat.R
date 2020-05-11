
context("Stat operations on sequences")


test_that("Length of sequences works", {
  x <- dna(c("ATGCAGA", "GGR-----","TTGCCTAGKTGAACC", NA))
  expect_equal(seq_nchar(x), c(7, 8, 15, NA))
  expect_equal(seq_nchar(x, gaps = FALSE), c(7, 3, 15, NA))
})

test_that("Length of vector works", {
  x <- dna(c("ATGCAGA", "GGR-----","TTGCCTAGKTGAACC", NA))
  expect_equal(seq_nseq(x), 4)
})

test_that("Proportion letter frequencies in sequences works", {
  x <- dna(c("ATGCAGA", "GGR-----", "TTGCCTAGKTGAACC", NA))
  expect_equal(seq_stat_prop(x)[[1]][c("A", "C", "G", "T")],
               c(A = 3/7, C = 1/7, G = 2/7, T = 1/7))
  expect_equal(seq_stat_prop(x)[[4]], NA)
  expect_equal(seq_stat_prop(x)[[2]][c("R")], c(R = 1/3))
  expect_equal(seq_stat_prop(x, gaps = TRUE)[[2]][c("R", "-")],
               c(R = 1/8, "-" = 5/8))
})


test_that("GC content works", {
  x <- dna(c("ATGCAGA", "GGR-----","TTGCCTAGKTGAACC", NA))
  expect_equal(seq_stat_gc(x), c(3/7, 3/3, 7/14, NA))
})
