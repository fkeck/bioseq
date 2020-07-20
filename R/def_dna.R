

#' DNA vector constructor
#'
#' @param x a character vector.
#'
#' @export
#'
new_dna <- function(x = character()) {
  vec_assert(x, character())
  x <- validate_seq(x,
                    alphabet = dic_dna()$alphabet,
                    invalid_replacement = "N",
                    type = "DNA")
  new_vctr(x, class = "bioseq_dna")
}


#' Build a DNA vector
#'
#' \code{dna()} build a DNA vector from a character vector.
#'
#' @param ... characters to turn into DNA. Can be a set of name-value pairs.
#'
#' @return a vector of class \code{bioseq_dna}
#' @family classes
#' @export
#'
#' @examples
#'
#' dna("AGGTGC", "TTCGA")
#'
#' dna(Seq_1 = "AGGTGC", Seq_2 = "TTCGA")
#'
#' x <- c("AGGTGC", "TTCGA")
#' dna(x)
#'
dna <- function(...) {
  x <- unlist(list(...))
  if(is.null(x)) x <- character()
  x <- vec_cast(x, character())
  new_dna(x)
}

#' Coercion to DNA vector
#'
#'
#' @param x An object to coerce.
#'
#' @return A DNA vector of class bioseq_dna
#' @family conversions
#' @export
#'
as_dna <- function(x) {
  vec_cast(x, new_dna())
}

#' Test if the object is a DNA vector
#'
#' This function returns TRUE for objects of class bioseq_dna
#'
#' @param x An object.
#'
#' @return Logical.
#' @export
#'
#' @examples
#' x <- c("AGGTGC", "TTCGA")
#' is_dna(x)
#' y <- dna(x)
#' is_dna(y)
#'
is_dna <- function(x) {
  inherits(x, "bioseq_dna")
}


# Formatting

#' @export
print.bioseq_dna <- function(x, n_bases = 60, n_seq = 12,
                             color = options("bioseq.color"), ...){
  print_sequences(x, n_bases = n_bases, n_seq = n_seq,
                  seq_type = "DNA", color = color)
}

#' Internal formatting
#'
#' @param x an object.
#' @param ... other params.
#'
#' @export
#' @keywords internal
pillar_shaft.bioseq_dna <- function(x, ...) {
  pillar_sequences(x, seq_type = "DNA")
}

#' @export
#' @keywords internal
vec_ptype_abbr.bioseq_dna <- function(x, ...) {
  "DNA"
}

# Implicit coercion

#' Internal
#'
#' @param x an object.
#' @param y an object.
#' @param ... other arguments.
#'
#' @method vec_ptype2 bioseq_dna
#' @export
#' @export vec_ptype2.bioseq_dna
#' @rdname internal-methods
#' @keywords internal
vec_ptype2.bioseq_dna <- function(x, y, ...) {
  UseMethod("vec_ptype2.bioseq_dna", y)
}

#' @method vec_ptype2.bioseq_dna default
#' @export
#' @keywords internal
vec_ptype2.bioseq_dna.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.bioseq_dna bioseq_dna
#' @export
#' @keywords internal
vec_ptype2.bioseq_dna.bioseq_dna <- function(x, y, ...) new_dna()

#' @method vec_ptype2.bioseq_dna character
#' @export
#' @keywords internal
vec_ptype2.bioseq_dna.character <- function(x, y, ...) new_dna()

#' @method vec_ptype2.character bioseq_dna
#' @export
#' @keywords internal
vec_ptype2.character.bioseq_dna <- function(x, y, ...) new_dna()



# Explicit casting

#' Internal
#'
#' @param to a class
#'
#' @method vec_cast bioseq_dna
#' @export
#' @export vec_cast.bioseq_dna
#' @rdname internal-methods
#' @keywords internal
vec_cast.bioseq_dna <- function(x, to, ...) UseMethod("vec_cast.bioseq_dna")

#' @method vec_cast.bioseq_dna default
#' @export
#' @keywords internal
vec_cast.bioseq_dna.default <- function(x, to, ...) vec_default_cast(x, to)

#' @method vec_cast.bioseq_dna bioseq_dna
#' @export
#' @keywords internal
vec_cast.bioseq_dna.bioseq_dna <- function(x, to, ...) x

#' @method vec_cast.bioseq_dna character
#' @export
#' @keywords internal
vec_cast.bioseq_dna.character <- function(x, to, ...) dna(x)

#' @method vec_cast.bioseq_dna DNAbin
#' @export
#' @keywords internal
vec_cast.bioseq_dna.DNAbin <- function(x, to, ...) {
  if(!is(x, "DNAbin")) {
    stop("x must be of class DNAbin")
  }
  x <- as.list(x)
  x <- as.character(x)
  res <- vapply(x, function(x) {
    stringr::str_to_upper(stringr::str_flatten(x, collapse = ""))
    }, vector("character", 1))
  res <- dna(res)
  return(res)
}

#' @method vec_cast.bioseq_dna alignment
#' @export
#' @keywords internal
vec_cast.bioseq_dna.alignment <- function(x, to, ...) {
  if(!is(x, "alignment")) {
    stop("x must be of class alignment (seqinr)")
  }
  res <- x$seq
  names(res) <- x$nam
  res <- dna(res)
  return(res)
}


#' @method vec_cast.character bioseq_dna
#' @export
#' @keywords internal
vec_cast.character.bioseq_dna <- function(x, to, ...) vec_data(x)


