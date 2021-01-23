# utils for qsub command
dots_parser <- function(..., sep_collapse = "\n") {
  rlang::list2(...) %>% 
    purrr::map(as.character) %>% 
    purrr::map_chr(stringr::str_c, collapse = sep_collapse) %>% 
    stringr::str_c(collapse = sep_collapse)
}

try_system <- function(x, trial_times = 5L) {
  if (trial_times <= 0L) rlang::abort(paste0("Error occurred in ", x), "command_error")
  res <- try(system(x, intern = TRUE))
  if (class(res) == "try-error") {
    try_system(x, trial_times - 1L)
  } else {
    return(res)
  }
}

seq_int_chr <- function(from_to_by){
  from = to = by = integer()
  c(from, to, by) %<-% vctrs::vec_cast(from_to_by, integer())
  if (is.na(from) || is.na(to) || is.na(by)) {
    "undefined"
  }else{
    as.character(seq.int(from, to, by))
  }
}

qsub_verbose <- function(ID_body, task, time){
  stringr::str_glue("ID: ", crayon::cyan(ID_body), 
                    "\ntaskid: ", crayon::cyan(stringr::str_c(task, collapse = ", ")),
                    "\ntime: ", crayon::cyan(time)) %>% cli::cat_line()
}

parse_id <- function(ID) {
  ID_vec <- as.integer(stringr::str_split(ID, "\\.|-|:")[[1]])
  list(
    ID_body = ID_vec[1],
    task = seq_int_chr(ID_vec[2:4])
  )
}

read_shebang <- function(path) {
  con = file(path, "r")
  if (readChar(con, 2L) == "#!") shebang <- readLines(con, n = 1L)
  else shebang <- NA_character_
  close(con)
  shebang
}
