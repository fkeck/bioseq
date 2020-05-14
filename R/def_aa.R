

#' Amino acid (AA) vector constructor
#'
#' @param x a character vector.
#'
#' @export
#'
new_aa <- function(x = character()) {
  vec_assert(x, character())
  x <- validate_seq(x,
                    alphabet = dic_aa()$alphabet,
                    invalid_replacement = "X",
                    type = "AA")
  new_vctr(x, class = "bioseq_aa")
}


#' Build an amino acid (AA) vector
#'
#' \code{aa()} build a AA vector from a character vector.
#'
#' @param ... character to turn into AA. Can be a set of name-value pairs.
#'
#' @return  vector of class bioseq_aa
#' @family classes
#' @export
#'
#' @examples
#'
#' aa("AGGTGC", "TTCGA")
#'
#' aa(Seq_1 = "AGGTGC", Seq_2 = "TTCGA")
#'
#' x <- c("AGGTGC", "TTCGA")
#' aa(x)
#'
aa <- function(...) {
  x <- unlist(list(...))
  if(is.null(x)) x <- character()
  x <- vec_cast(x, character())
  new_aa(x)
}

#' Coercion to an amino acid (AA) vector
#'
#'
#' @param x An object to coerce.
#'
#' @return An amino acid vector of class bioseq_aa
#' @family conversions
#' @export
#'
as_aa <- function(x) {
  vec_cast(x, new_aa())
}

#' Test if the object is an amino acid vector
#'
#' This function returns TRUE for objects of class bioseq_aa
#'
#' @param x An object.
#'
#' @return Logical.
#' @export
#'
#' @examples
#' x <- c("AGGTGC", "TTCGA")
#' is_aa(x)
#' y <- aa(x)
#' is_aa(x)
#'
is_aa <- function(x) {
  inherits(x, "bioseq_aa")
}


# Formatting

#' @export
#'
print.bioseq_aa <- function(x, n_bases = 60, n_seq = 12, ...){
  print_sequences(x, n_bases = n_bases, n_seq = n_seq, seq_type = "AA")
}

#' Internal formating
#'
#' @param x a object.
#' @param ... other params.
#'
#' @export
#' @keywords internal
pillar_shaft.bioseq_aa <- function(x, ...) {
  pillar_sequences(x, seq_type = "AA")
}


#' @export
#' @keywords internal
vec_ptype_abbr.bioseq_aa <- function(x, ...) {
  "AA"
}

# Implicit coercion

#' Internal
#'
#'
#' @method vec_ptype2 bioseq_aa
#' @export
#' @export vec_ptype2.bioseq_aa
#' @rdname internal-methods
#' @keywords internal
vec_ptype2.bioseq_aa <- function(x, y, ...) UseMethod("vec_ptype2.bioseq_aa", y)

#' @method vec_ptype2.bioseq_aa default
#' @export
#' @keywords internal
vec_ptype2.bioseq_aa.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.bioseq_aa bioseq_aa
#' @export
#' @keywords internal
vec_ptype2.bioseq_aa.bioseq_aa <- function(x, y, ...) new_aa()

#' @method vec_ptype2.bioseq_aa character
#' @export
#' @keywords internal
vec_ptype2.bioseq_aa.character <- function(x, y, ...) new_aa()

#' @method vec_ptype2.character bioseq_aa
#' @export
#' @keywords internal
vec_ptype2.character.bioseq_aa <- function(x, y, ...) new_aa()



# Explicit casting

#' Internal
#'
#'
#' @method vec_cast bioseq_aa
#' @export
#' @export vec_cast.bioseq_aa
#' @rdname internal-methods
#' @keywords internal
vec_cast.bioseq_aa <- function(x, to, ...) UseMethod("vec_cast.bioseq_aa")

#' @method vec_cast.bioseq_aa default
#' @export
#' @keywords internal
vec_cast.bioseq_aa.default <- function(x, to, ...) vec_default_cast(x, to)

#' @method vec_cast.bioseq_aa bioseq_aa
#' @export
#' @keywords internal
vec_cast.bioseq_aa.bioseq_aa <- function(x, to, ...) x

#' @method vec_cast.bioseq_aa character
#' @export
#' @keywords internal
vec_cast.bioseq_aa.character <- function(x, to, ...) aa(x)

#' @method vec_cast.bioseq_aa AAbin
#' @export
#' @keywords internal
vec_cast.bioseq_aa.AAbin <- function(x, to, ...) {
  if(!is(x, "AAbin")) {
    stop("x must be of class AAbin")
  }
  x <- as.list(x)
  x <- lapply(x, function(x){
    if(length(x) == 0) {
      return("")
    } else if(all(is.na(x))) {
      return(NA)
    } else {
      as.character(x)
    }
    })
  res <- vapply(x, function(x) {
    stringr::str_to_upper(stringr::str_flatten(x, collapse = ""))
    }, vector("character", 1))
  res <- aa(res)
  return(res)
}

#' @method vec_cast.bioseq_aa alignment
#' @export
#' @keywords internal
vec_cast.bioseq_aa.alignment <- function(x, to, ...) {
  if(!is(x, "alignment")) {
    stop("x must be of class alignment (seqinr)")
  }
  res <- x$seq
  names(res) <- x$nam
  res <- aa(res)
  return(res)
}

#' @method vec_cast.character bioseq_aa
#' @export
#' @keywords internal
vec_cast.character.bioseq_aa <- function(x, to, ...) vec_data(x)


