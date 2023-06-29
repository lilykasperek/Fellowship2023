meta_function <- function(eval) {
  tab <- pdf_text(eval)
  first_page <- strsplit(tab[1], "\n")[[1]]
  first_page_zeroes <- str_replace_all(first_page,
                                       pattern = "-",
                                       replacement = "0")
  only_numbers <- str_extract_all(first_page_zeroes, "\\d+")
  only_numbers <- compact(only_numbers)
  semester_string <- str_replace_all(first_page_zeroes[[2]], " ", "")
  semester <- semester_string |> substring(5, 6)
  
  info <- noquote(first_page[[2]])
  info_split <- strsplit(info, "-")
  info_split[[1]][3] <- str_replace_all(info_split[[1]][3], " ", "")
  info_split <- noquote(info_split)
  meta_test <- tibble::tibble(course_num = only_numbers[[2]][3],
                              subject = info_split[[1]][3],
                              semester = semester, 
                              year = only_numbers[[2]][1],
                              control_num = only_numbers[[1]][2],
                              n_evals = only_numbers[[4]][1],
                              n_evals_univ = only_numbers[[6]][1]) 
  return(meta_test)
}
