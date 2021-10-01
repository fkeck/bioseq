context("Read and write FASTA format")

test_that("Read/write fasta works with single sequence", {
  test <- read_fasta("example_fasta_single.fasta")
  expect_s3_class(test, "bioseq_dna")
  expect_length(test, 1L)
  expect_equal(as.numeric(nchar(test)), 1231L)
  #expect_named(test, "HSBGPG Human gene for bone gla protein (BGP)")

  test_file <- tempfile(fileext = ".fasta")
  write_fasta(test, file = test_file)
  test_reread <- read_fasta(test_file)
  expect_equal(test, test_reread)
})

test_that("Read/write fasta works with multiple sequences", {
  test <- read_fasta("example_fasta_multi.fasta")
  expect_s3_class(test, "bioseq_dna")
  expect_length(test, 3L)
  expect_equal(as.numeric(nchar(test)), c(1231L, 1020L, 44L))
  #expect_named(test, c("HSBGPG Human gene for bone gla protein (BGP)",
  #                     "HSGLTH1 Human theta 1-globin gene",
  #                     "Fragilaria vaucheriae rbcL extract"))

  test_file <- tempfile(fileext = ".fasta")
  write_fasta(test, file = test_file)
  test_reread <- read_fasta(test_file)
  expect_equal(test, test_reread)
})


test_that("Read/write fasta works with > character in sequence names", {
  test <- read_fasta("example_fasta_multi_chevron.fasta")
  expect_s3_class(test, "bioseq_dna")
  expect_length(test, 3L)
  expect_equal(as.numeric(nchar(test)), c(1231L, 1020L, 44L))
  test_file <- tempfile(fileext = ".fasta")
  write_fasta(test, file = test_file)
  test_reread <- read_fasta(test_file)
  expect_equal(test, test_reread)
})


test_that("Read/write fasta works with header", {
  test <- read_fasta("example_fasta_header.fasta")
  expect_s3_class(test, "bioseq_dna")
  expect_length(test, 2L)
  expect_equal(as.numeric(nchar(test)), c(1231L, 1020L))
  #expect_named(test, c("HSBGPG Human gene for bone gla protein (BGP)",
  #                     "HSGLTH1 Human theta 1-globin gene"))

  test_file <- tempfile(fileext = ".fasta")
  write_fasta(test, file = test_file)
  test_reread <- read_fasta(test_file)
  expect_equal(test, test_reread)
})


test_that("Read/write fasta works with protein sequence", {
  test <- read_fasta("example_fasta_protein.fasta", type = "AA")
  expect_s3_class(test, "bioseq_aa")
  expect_length(test, 1L)
  expect_equal(as.numeric(nchar(test)), 223L)
  #expect_named(test, "sp|P33049|CASA2_CAPHI Alpha-S2-casein")

  test_file <- tempfile(fileext = ".fasta")
  write_fasta(test, file = test_file)
  test_reread <- read_fasta(test_file, type = "AA")
  expect_equal(test, test_reread)
})


test_that("Read/write fasta works with RNA", {
  test <- read_fasta("example_fasta_RNA.fasta", type = "RNA")
  expect_s3_class(test, "bioseq_rna")
  expect_length(test, 1L)
  expect_equal(as.numeric(nchar(test)), 12L)
  #expect_named(test, "RNA sequence")

  test_file <- tempfile(fileext = ".fasta")
  write_fasta(test, file = test_file)
  test_reread <- read_fasta(test_file, type = "RNA")
  expect_equal(test, test_reread)
})


test_that("Write fasta raises warning with NA ", {
  test <- read_fasta("example_fasta_multi.fasta")
  test[2] <- NA
  test_file <- tempfile(fileext = ".fasta")
  expect_warning(write_fasta(test, file = test_file))
  test_reread <- read_fasta(test_file)
  expect_length(test_reread, 2)
})


test_that("Read/write fasta works with block and line length arguments", {
  test <- read_fasta("example_fasta_multi.fasta")

  # Defaults
  test_file <- tempfile(fileext = ".fasta")
  write_fasta(test, file = test_file)
  test_reread <- read_fasta(test_file)
  expect_equal(test, test_reread)

  # Other values
  test_file <- tempfile(fileext = ".fasta")
  write_fasta(test, file = test_file, line_length = 100, block_length = 10)
  test_reread <- read_fasta(test_file)
  expect_equal(test, test_reread)

  # Same values
  test_file <- tempfile(fileext = ".fasta")
  write_fasta(test, file = test_file, line_length = 12, block_length = 12)
  test_reread <- read_fasta(test_file)
  expect_equal(test, test_reread)

  # Infinite values
  test_file <- tempfile(fileext = ".fasta")
  write_fasta(test, file = test_file, line_length = Inf, block_length = Inf)
  test_reread <- read_fasta(test_file)
  expect_equal(test, test_reread)

  # Error non-multiple values
  test_file <- tempfile(fileext = ".fasta")
  expect_error(
    write_fasta(test, file = test_file, line_length = 99, block_length = 10),
    "multiple"
  )

  # Error blocks too large
  test_file <- tempfile(fileext = ".fasta")
  expect_error(
    write_fasta(test, file = test_file, line_length = 100, block_length = 10000),
    "higher")
})
