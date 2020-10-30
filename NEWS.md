# devel

## Documentation improvements

* New logo

# bioseq 0.1.1

## New features

* New system to disable colors in the console when printing sequences using
global options: `options(bioseq.color = "none")`.

* New system to set GUI paths for `seaview()` and `aliview()` using global
options (`bioseq.seaview.exec` and `bioseq.aliview.exec`).

## Bug fixes

* Fix bug with names when named biological sequences are provided to
functions that return a list (`seq_extract_pattern`, `seq_split_pattern`,
`seq_split_kmer`).

## Documentation improvements

* Documentation of `seaview()` and `aliview()` was improved to explain
the exec arguments.



# bioseq 0.1.0

* First version
