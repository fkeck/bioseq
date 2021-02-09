


# Approximate string matching to detect patterns
# with a maximum error rate.
# Pattern and max_dist are vectorized along x.
seq_detect_fuzzypattern <- function(x, pattern, max_dist = 0.0) {

  check_dna_rna_aa(x)
  pattern <- check_and_prepare_pattern(x, pattern)
  pattern <- stringr::str_split(pattern, pattern = "\\|")
  pattern <- rep_len(pattern, length(x))
  max_dist <- rep_len(max_dist, length(x))

  if(any(max_dist < 0) | any(max_dist > 1)) {
    stop("max_dist must be a numeric value ranging from 0 to 1.")
  }

  mapply(function(x, pattern, max_dist) {
    max_dist <- floor(max_dist * stringr::str_length(pattern))
    pat_dist <- stringdist::afind(x, pattern, value = FALSE)$distance
    apply(pat_dist, 1, function(x) any(x <= max_dist))
    },
    x = x, pattern = pattern, max_dist = max_dist,
    SIMPLIFY = TRUE, USE.NAMES = FALSE)

}
