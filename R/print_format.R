bioseq_color <- function(seq_type) {
  if(seq_type == "DNA") {
    res <- c(A = crayon::bold(crayon::red("A")),
             C = crayon::bold(crayon::green("C")),
             G = crayon::bold(crayon::yellow("G")),
             T = crayon::bold(crayon::blue("T"))
             )
  }

  if(seq_type == "RNA") {
    res <- c(A = crayon::bold(crayon::red("A")),
             C = crayon::bold(crayon::green("C")),
             G = crayon::bold(crayon::yellow("G")),
             U = crayon::bold(crayon::blue("U"))
             )
  }

  if(seq_type == "AA") {
    res <- c(F = crayon::bold(crayon::make_style("#1980e6")("F")),
             L = crayon::bold(crayon::make_style("#78a6d5")("L")),
             S = crayon::bold(crayon::make_style("#029602")("S")),
             Y = crayon::bold(crayon::make_style("#14c6c8")("Y")),
             C = crayon::bold(crayon::make_style("#e68080")("C")),
             W = crayon::bold(crayon::make_style("#0355a9")("W")),
             P = crayon::bold(crayon::make_style("#cccc00")("P")),
             H = crayon::bold(crayon::make_style("#19b3b3")("H")),
             Q = crayon::bold(crayon::make_style("#5ced5c")("Q")),
             R = crayon::bold(crayon::make_style("#f6442c")("R")),
             I = crayon::bold(crayon::make_style("#4ea0f3")("I")),
             M = crayon::bold(crayon::make_style("#0f549b")("M")),
             T = crayon::bold(crayon::make_style("#45c945")("T")),
             N = crayon::bold(crayon::make_style("#19cc19")("N")),
             K = crayon::bold(crayon::make_style("#e63319")("K")),
             V = crayon::bold(crayon::make_style("#047df9")("V")),
             A = crayon::bold(crayon::make_style("#276eb7")("A")),
             D = crayon::bold(crayon::make_style("#cc4dcc")("D")),
             E = crayon::bold(crayon::make_style("#984097")("E")),
             G = crayon::bold(crayon::make_style("#e6994d")("G"))
    )
  }

  return(res)
}


print_sequences <- function(x, n_bases = 60, n_seq = 12, seq_type = "DNA"){

  # FIX PRINT WITH NA VALUES
  if(length(x) > n_seq){
    x_sub <- x[1:n_seq]
  } else {
    x_sub <- x
  }
  x_len <- length(x)
  x_sub_len <- length(x_sub)
  x_sub_names <- names(x_sub)
  x_sub_nchar <- stringr::str_length(x_sub)

  top_line <- paste(seq_type, "vector of", x_len, "sequences")
  top_line <- pillar::style_subtle(top_line)

  bottom_line <- NULL
  if(x_len > x_sub_len){
    bottom_line <- paste0("... with ", x_len - x_sub_len, " more sequences.")
    bottom_line <- pillar::style_subtle(bottom_line)
  }

  body_seq <- stringr::str_trunc(x_sub, width = n_bases, ellipsis = "")
  sequence_col <- bioseq_color(seq_type)

  body_seq <- stringr::str_split(body_seq, "")
  body_seq <- lapply(body_seq, function(x) {
    if(is.na(x)[1] & length(x) == 1){
      return(pillar::style_na(NA))
    }
    col_char <- sequence_col[x]
    x[!is.na(col_char)] <- col_char[!is.na(col_char)]
    return(x)
  })
  body_seq <- vapply(body_seq, stringr::str_c,
                     vector("character", 1), collapse = "")

  if(is.null(x_sub_names)){
    margin_left <- rep("> ", x_sub_len)
  } else {
    lab_max_width <- max(nchar(x_sub_names))
    lab_max_width <- ifelse(lab_max_width < 20, lab_max_width, 20)
    ellipsis_char <- ifelse(lab_max_width < 3, "", "...")
    margin_left <- stringr::str_trunc(x_sub_names, width = lab_max_width,
                                      ellipsis = ellipsis_char, side = "right")
    margin_left <- stringr::str_pad(margin_left, width = lab_max_width,
                                    side = "right")
  }
  margin_left <- pillar::style_subtle(margin_left)
  margin_left <- crayon::italic(margin_left)

  d_nchar <- x_sub_nchar - n_bases
  d_nchar[is.na(d_nchar)] <- 0
  margin_right_unit <- ifelse(seq_type == "AA", " amino acids", " bases")
  margin_right <- ifelse(d_nchar <= 0, "", paste0("... + ", d_nchar,
                                                  margin_right_unit))
  margin_right <- pillar::style_subtle(margin_right)

  central_col <- paste0(margin_left, "  ",
                        body_seq, margin_right,
                        collapse = "\n")
  out <- paste(top_line, central_col, bottom_line, sep = "\n")
  cat(out)
}


pillar_sequences <- function(x, seq_type = "DNA") {
  sequence_col <- bioseq_color(seq_type)
  out <- strsplit(x, "")
  out <- lapply(out, function(x) {
    col_char <- sequence_col[x]
    x[!is.na(col_char)] <- col_char[!is.na(col_char)]
    return(x)
  })
  out <- vapply(out, paste, vector("character", 1), collapse = "")
  pillar::new_pillar_shaft_simple(out, align = "left", min_width = 5)
}
