

#' RNA vector constructor
#'
#' @param x a character vector.
#'
#' @export
#'
new_rna <- function(x = character()) {
  vec_assert(x, character())
  x <- validate_seq(x,
                    alphabet = dic_rna()$alphabet,
                    invalid_replacement = "N",
                    type = "RNA")
  new_vctr(x, class = "bioseq_rna")
}


#' Build a RNA vector
#'
#' \code{rna()} build a RNA vector from a character vector.
#'
#' @param ... characters to turn into RNA. Can be a set of name-value pairs.
#'
#' @return a vector of class bioseq_rna
#' @family classes
#' @export
#'
#' @examples
#'
#' rna("AGGUGC", "UUCGA")
#'
#' rna(Seq_1 = "AGGUGC", Seq_2 = "UUCGA")
#'
#' x <- c("AGGTGC", "TTCGA")
#' rna(x)
#'
rna <- function(...) {
  x <- unlist(list(...))
  if(is.null(x)) x <- character()
  x <- vec_cast(x, character())
  new_rna(x)
}

#' Coercion to RNA vector
#'
#'
#' @param x An object to coerce.
#'
#' @return A RNA vector of class bioseq_rna
#' @family conversions
#' @export
#'
as_rna <- function(x) {
  vec_cast(x, new_rna())
}

#' Test if the object is a RNA vector
#'
#' This function returns TRUE for objects of class bioseq_rna
#'
#' @param x An object.
#'
#' @return Logical.
#' @export
#'
#' @examples
#' x <- c("AGGTGC", "TTCGA")
#' is_rna(x)
#' y <- rna(x)
#' is_rna(x)
#'
is_rna <- function(x) {
  inherits(x, "bioseq_rna")
}


# Formatting

#' @export
print.bioseq_rna <- function(x, n_bases = 60, n_seq = 12,
                             color = options("bioseq.color"), ...){
  print_sequences(x, n_bases = n_bases, n_seq = n_seq,
                  seq_type = "RNA", color = color)
}


#' Internal formating
#'
#' @param x a object.
#' @param ... other params.
#'
#' @export
#'@keywords internal
pillar_shaft.bioseq_rna <- function(x, ...) {
  pillar_sequences(x, seq_type = "RNA")
}

#' @export
#' @keywords internal
vec_ptype_abbr.bioseq_rna <- function(x, ...) {
  "RNA"
}

# Implicit coercion

#' Internal
#'
#'
#' @method vec_ptype2 bioseq_rna
#' @export
#' @export vec_ptype2.bioseq_rna
#' @rdname internal-methods
#' @keywords internal
vec_ptype2.bioseq_rna <- function(x, y, ...) {
  UseMethod("vec_ptype2.bioseq_rna", y)
}

#' @method vec_ptype2.bioseq_rna default
#' @export
#' @keywords internal
vec_ptype2.bioseq_rna.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.bioseq_rna bioseq_rna
#' @export
#' @keywords internal
vec_ptype2.bioseq_rna.bioseq_rna <- function(x, y, ...) new_rna()

#' @method vec_ptype2.bioseq_rna character
#' @export
#' @keywords internal
vec_ptype2.bioseq_rna.character <- function(x, y, ...) new_rna()

#' @method vec_ptype2.character bioseq_rna
#' @export
#' @keywords internal
vec_ptype2.character.bioseq_rna <- function(x, y, ...) new_rna()



# Explicit casting

#' Internal
#'
#'
#' @method vec_cast bioseq_rna
#' @export
#' @export vec_cast.bioseq_rna
#' @rdname internal-methods
#' @keywords internal
vec_cast.bioseq_rna <- function(x, to, ...) UseMethod("vec_cast.bioseq_rna")

#' @method vec_cast.bioseq_rna default
#' @export
#' @keywords internal
vec_cast.bioseq_rna.default <- function(x, to, ...) vec_default_cast(x, to)

#' @method vec_cast.bioseq_rna bioseq_rna
#' @export
#' @keywords internal
vec_cast.bioseq_rna.bioseq_rna <- function(x, to, ...) x

#' @method vec_cast.bioseq_rna character
#' @export
#' @keywords internal
vec_cast.bioseq_rna.character <- function(x, to, ...) rna(x)

#' @method vec_cast.bioseq_rna alignment
#' @export
#' @keywords internal
vec_cast.bioseq_rna.alignment <- function(x, to, ...) {
  if(!is(x, "alignment")) {
    stop("x must be of class alignment (seqinr)")
  }
  res <- x$seq
  names(res) <- x$nam
  res <- rna(res)
  return(res)
}

#' @method vec_cast.character bioseq_rna
#' @export
#' @keywords internal
vec_cast.character.bioseq_rna <- function(x, to, ...) vec_data(x)


