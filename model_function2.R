model_function2 <- function(pdf) {
  # page 1
  tab <- pdf_text(pdf)
  first_page <- strsplit(tab[1], "\n")[[1]]
  first_page_zeroes <- str_replace_all(first_page,
                                       pattern = "-",
                                       replacement = "0")
  only_numbers <- str_extract_all(first_page_zeroes, "\\d+")
  only_numbers <- compact(only_numbers)
  year <- c(only_numbers[[2]][1],
            only_numbers[[2]][1],
            only_numbers[[2]][1],
            only_numbers[[2]][1],
            only_numbers[[2]][1],
            only_numbers[[2]][1],
            only_numbers[[2]][1])
  course <- c(only_numbers[[2]][3],
              only_numbers[[2]][3],
              only_numbers[[2]][3],
              only_numbers[[2]][3],
              only_numbers[[2]][3],
              only_numbers[[2]][3],
              only_numbers[[2]][3])
  semester_string <- str_replace_all(first_page_zeroes[[2]], " ", "")
  semester <- semester_string |> substring(5, 6)
  semester_1 <- c(semester,
                  semester,
                  semester,
                  semester,
                  semester,
                  semester,
                  semester)
  # question 4a
  q4a_indeces <- c(29, 30, 31, 32, 33)
  q4a <- only_numbers[q4a_indeces]
  q4a[[1]] <- q4a[[1]][4:6]
  q4a[[2]] <- q4a[[2]][4:6]
  q4a[[3]] <- q4a[[3]][4:6]
  q4a_df <- do.call(rbind.data.frame, q4a)
  names(q4a_df) <- c("n_students", "perc", "univ_perc")
  responses_2 <- c("Too High",
                   "Somewhat High", 
                   "Appropriate",
                   "Somewhat Low",
                   "Too Low") 
  year_2 <- c(only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1])
  semester_2 <- c(semester,
                  semester,
                  semester,
                  semester,
                  semester)
  course_2 <- c(only_numbers[[2]][3],
                only_numbers[[2]][3],
                only_numbers[[2]][3],
                only_numbers[[2]][3],
                only_numbers[[2]][3])
  core_df_4a <- tibble::tibble(responses = responses_2,
                               q4a_df) |>
    mutate(question = "Q4a")
  core_df_4a <- tibble::tibble(year_2 = year_2,
                               core_df_4a) 
  core_df_4a <- core_df_4a |> dplyr::rename(year = 'year_2')
  core_df_4a <- core_df_4a |> mutate(year = as.numeric(year))
  core_df_4a <- tibble::tibble(semester = semester_2,
                               core_df_4a)
  core_df_4a <- tibble::tibble(course = course_2,
                               core_df_4a)
  # question 4b
  q4b_indeces <- c(38, 40, 42, 44, 45)
  q4b <- only_numbers[q4b_indeces]
  q4b_df <- do.call(rbind.data.frame, q4b)
  names(q4b_df) <- c("n_students", "perc", "univ_perc")
  core_df_4b <- tibble::tibble(responses = responses_2,
                               q4b_df) |>
    mutate(question = "Q4b")
  core_df_4b <- tibble::tibble(year_2 = year_2,
                               core_df_4b)
  core_df_4b <- core_df_4b |> dplyr::rename(year = 'year_2')
  core_df_4b <- core_df_4b |> mutate(year = as.numeric(year))
  core_df_4b <- tibble::tibble(semester = semester_2,
                               core_df_4b)
  core_df_4b <- tibble::tibble(course = course_2,
                               core_df_4b)
  # question 4c
  q4c_indeces <- c(49, 50, 51, 52, 53)
  q4c <- only_numbers[q4c_indeces]
  q4c[[1]] <- q4c[[1]][4:6]
  q4c[[2]] <- q4c[[2]][4:6]
  q4c[[3]] <- q4c[[3]][4:6]
  q4c[[4]] <- q4c[[4]][4:6]
  q4c[[5]] <- q4c[[5]][4:6]
  q4c_df <- do.call(rbind.data.frame, q4c)
  names(q4c_df) <- c("n_students", "perc", "univ_perc")
  core_df_4c <- tibble::tibble(responses = responses_2,
                               q4c_df) |>
    mutate(question = "Q4c")
  core_df_4c <- tibble::tibble(year_2 = year_2,
                               core_df_4c)
  core_df_4c <- core_df_4c |> dplyr::rename(year = 'year_2')
  core_df_4c <- core_df_4c |> mutate(year = as.numeric(year))
  core_df_4c <- tibble::tibble(semester = semester_2,
                               core_df_4c)
  core_df_4c <- tibble::tibble(course = course_2,
                               core_df_4c)

  binded_df <- bind_rows(core_df_4a,
                         core_df_4b,
                         core_df_4c)
  binded_df <- binded_df |>
    mutate(full_question = 
             case_when(question == "Q4a" ~ "Q4a. Workload",
                       question == "Q4b" ~ "Q4b. Grading",
                       question == "Q4c" ~ "Q4c. Sophistication")) 
  binded_df <- binded_df |> unite("Semester", c(year, semester),
                                  sep = "_")
  # step 1
  #binded_df <- binded_df |> separate(col = Semester, into = c("year", "semester"),
  #sep = "_")
  # step 2
  #binded_df <- binded_df |> arrange(year, desc(semester))
  # step 3
  #binded_df <- binded_df |> unite("new_semester", c(year, semester),
  #sep = "_")
  #binded_df <- binded_df |> mutate(new_semester = fct_inorder(new_semester))
  return(binded_df)
}

