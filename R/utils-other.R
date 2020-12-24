###unexported object derived from usethis###
todo_bullet <- function() {
  if (!requireNamespace("clisymbols", quietly = TRUE)) {
    crayon::red("TODO:")
  } else {
    crayon::red(clisymbols::symbol$bullet)
  }
}
done_bullet <- function() {
  if (!requireNamespace("clisymbols", quietly = TRUE)) {
    crayon::green("DONE:")
  } else {
    crayon::green(clisymbols::symbol$tick)
  }
}
fail_bullet <- function() {
  if (!requireNamespace("clisymbols", quietly = TRUE)) {
    crayon::bgRed("FAIL:")
  } else {
    crayon::bgRed(clisymbols::symbol$cross)
  }
}

bulletize <- function(line, bullet) paste0(bullet, " ", line)
todo <- function(..., .envir = parent.frame()) {
  out <- stringr::str_glue(..., .envir = .envir)
  cli::cat_line(bulletize(out, bullet = todo_bullet()))
}

done <- function(..., .envir = parent.frame()) {
  out <- stringr::str_glue(..., .envir = .envir)
  cli::cat_line(bulletize(out, bullet = done_bullet()))
}

fail <- function(..., .envir = parent.frame()) {
  out <- stringr::str_glue(..., .envir = .envir)
  cli::cat_line(bulletize(out, bullet = fail_bullet()))
}

###adapted from rlist::list.flatten###
list.flatten <- function (x, use.names = TRUE, classes = "ANY") {
  len <- sum(rapply(x, function(x) 1L, classes = classes))
  y <- vector("list", len)
  i <- 0L
  items <- rapply(x, function(x) {
    i <<- i + 1L
    y[[i]] <<- x
    TRUE
  }, classes = classes)
  if (use.names && !is.null(nm <- names(items))) 
    names(y) <- nm
  y
}
