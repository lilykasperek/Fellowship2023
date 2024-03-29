model_function <- function(pdf) {
  # page 1
  tab <- pdf_text(pdf)
  first_page <- strsplit(tab[1], "\n")[[1]]
  first_page_zeroes <- str_replace_all(first_page,
                                       pattern = "-",
                                       replacement = "0")
  only_numbers <- str_extract_all(first_page_zeroes, "\\d+")
  only_numbers <- compact(only_numbers)
  # page 2
  second_page <- strsplit(tab[2], "\n")[[1]]
  second_page_zeroes <- str_replace_all(second_page,
                                        pattern = "-",
                                        replacement = "0")
  only_numbers_2 <- str_extract_all(second_page_zeroes, "\\d+")
  only_numbers_2 <- compact(only_numbers_2)
  # question 1
  q1_indeces <- c(49, 50, 51, 52, 53, 54, 55)
  q1 <- only_numbers[q1_indeces]
  q1[[1]] <- q1[[1]][1:3]
  q1[[2]] <- q1[[2]][1:3]
  q1[[3]] <- q1[[3]][1:3]
  q1[[4]] <- q1[[4]][1:3]
  q1[[5]] <- q1[[5]][1:3]
  q1[[6]] <- q1[[6]][1:3]
  q1_df <- do.call(rbind.data.frame, q1)
  names(q1_df) <- c("n_students", "perc", "univ_perc")
  responses <- c("Strongly Agree",
                 "Agree", 
                 "Agree Somewhat",
                 "Neutral",
                 "Disagree Somewhat",
                 "Disagree",
                 "Disagree Strongly")
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
  core_df_1 <- tibble::tibble(responses = responses,
                              q1_df) |>
    mutate(question = "Q1")
  core_df_1 <- tibble::tibble(year = year, 
                              core_df_1)
  core_df_1 <- core_df_1 |> mutate(year = as.numeric(year))
  core_df_1 <- tibble::tibble(semester = semester_1,
                              core_df_1)
  core_df_1 <- tibble::tibble(course = course,
                              core_df_1)
  # question 2
  q2_indeces <- c(5, 7, 8, 9, 10, 11, 12) ## back half of 16
  q2 <- only_numbers[q2_indeces]
  q2[[7]] <- q2[[7]][4:6]
  q2_df <- do.call(rbind.data.frame, q2)
  names(q2_df) <- c("n_students", "perc", "univ_perc")
  core_df_2 <- tibble::tibble(responses = responses,
                              q2_df) |>
    mutate(question = "Q2")
  core_df_2 <- tibble::tibble(year = year,
                              core_df_2)
  core_df_2 <- core_df_2 |> mutate(year = as.numeric(year))
  core_df_2 <- tibble::tibble(semester = semester_1,
                              core_df_2)
  core_df_2 <- tibble::tibble(course = course,
                              core_df_2)
  # question 3
  q3_indeces <- c(16, 17, 18, 19, 20, 22, 24)
  q3 <- only_numbers[q3_indeces]
  q3[[1]] <- q3[[1]][4:6]
  q3[[2]] <- q3[[2]][4:6]
  q3[[4]] <- q3[[4]][4:6]
  q3_df <- do.call(rbind.data.frame, q3)
  names(q3_df) <- c("n_students", "perc", "univ_perc")
  core_df_3 <- tibble::tibble(responses = responses,
                              q3_df) |>
    mutate(question = "Q3")
  core_df_3 <- tibble::tibble(year = year,
                              core_df_3)
  core_df_3 <- core_df_3 |> mutate(year = as.numeric(year))
  core_df_3 <- tibble::tibble(semester = semester_1,
                              core_df_3)
  core_df_3 <- tibble::tibble(course = course,
                              core_df_3)
  # question 5a
  q5a_indeces <- c(2, 3, 4, 5, 6, 7, 8)
  q5a <- only_numbers_2[q5a_indeces]
  q5a[[1]] <- q5a[[1]][1:3]
  q5a[[2]] <- q5a[[2]][1:3]
  q5a[[3]] <- q5a[[3]][1:3]
  q5a[[4]] <- q5a[[4]][1:3]
  q5a[[5]] <- q5a[[5]][1:3]
  q5a[[6]] <- q5a[[6]][1:3]
  q5a[[7]] <- q5a[[7]][1:3]
  q5a_df <- do.call(rbind.data.frame, q5a)
  names(q5a_df) <- c("n_students", "perc", "univ_perc")
  core_df_5a <- tibble::tibble(responses = responses,
                               q5a_df) |>
    mutate(question = "Q5a")
  core_df_5a <- tibble::tibble(year = year,
                               core_df_5a) 
  core_df_5a <- core_df_5a |> mutate(year = as.numeric(year))
  core_df_5a <- tibble::tibble(semester = semester_1,
                               core_df_5a)
  core_df_5a <- tibble::tibble(course = course,
                               core_df_5a)
  # question 5b
  q5b_indeces <- c(11, 12, 13, 14, 15, 16, 17)
  q5b <- only_numbers_2[q5b_indeces]
  q5b[[1]] <- q5b[[1]][1:3]
  q5b[[2]] <- q5b[[2]][1:3]
  q5b[[3]] <- q5b[[3]][1:3]
  q5b[[4]] <- q5b[[4]][1:3]
  q5b[[5]] <- q5b[[5]][1:3]
  q5b[[6]] <- q5b[[6]][1:3]
  q5b[[7]] <- q5b[[7]][1:3]
  q5b_df <- do.call(rbind.data.frame, q5b)
  names(q5b_df) <- c("n_students", "perc", "univ_perc")
  core_df_5b <- tibble::tibble(responses = responses,
                               q5b_df) |>
    mutate(question = "Q5b")
  core_df_5b <- tibble::tibble(year = year,
                               core_df_5b)
  core_df_5b <- core_df_5b |> mutate(year = as.numeric(year))
  core_df_5b <- tibble::tibble(semester = semester_1,
                               core_df_5b)
  core_df_5b <- tibble::tibble(course = course,
                               core_df_5b)
  # question 5c
  q5c_indeces <- c(22, 23, 24, 25, 26, 28, 29)
  q5c <- only_numbers_2[q5c_indeces]
  q5c[[2]] <- q5c[[2]][1:3]
  q5c[[4]] <- q5c[[4]][1:3]
  q5c[[7]] <- q5c[[7]][1:3]
  q5c_df <- do.call(rbind.data.frame, q5c)
  names(q5c_df) <- c("n_students", "perc", "univ_perc")
  core_df_5c <- tibble::tibble(responses = responses,
                               q5c_df) |>
    mutate(question = "Q5c")
  core_df_5c <- tibble::tibble(year = year,
                               core_df_5c)
  core_df_5c <- core_df_5c |> mutate(year = as.numeric(year))
  core_df_5c <- tibble::tibble(semester = semester_1,
                               core_df_5c)
  core_df_5c <- tibble::tibble(course = course,
                               core_df_5c)
  # question 6a
  q6a_indeces <- c(34, 35, 36, 38, 39, 40, 41)
  q6a <- only_numbers_2[q6a_indeces]
  q6a[[2]] <- q6a[[2]][1:3]
  q6a[[7]] <- q6a[[7]][1:3]
  q6a_df <- do.call(rbind.data.frame, q6a)
  names(q6a_df) <- c("n_students", "perc", "univ_perc")
  core_df_6a <- tibble::tibble(responses = responses,
                               q6a_df) |>
    mutate(question = "Q6a")
  core_df_6a <- tibble::tibble(year = year,
                               core_df_6a)
  core_df_6a <- core_df_6a |> mutate(year = as.numeric(year))
  core_df_6a <- tibble::tibble(semester = semester_1,
                               core_df_6a)
  core_df_6a <- tibble::tibble(course = course,
                               core_df_6a)
  # question 6b
  q6b_indeces <- c(2, 3, 4, 5, 6, 7, 8)
  q6b <- only_numbers_2[q6b_indeces]
  q6b[[1]] <- q6b[[1]][4:6]
  q6b[[2]] <- q6b[[2]][4:6]
  q6b[[3]] <- q6b[[3]][4:6]
  q6b[[4]] <- q6b[[4]][4:6]
  q6b[[5]] <- q6b[[5]][4:6]
  q6b[[6]] <- q6b[[6]][4:6]
  q6b[[7]] <- q6b[[7]][4:6]
  q6b_df <- do.call(rbind.data.frame, q6b)
  names(q6b_df) <- c("n_students", "perc", "univ_perc")
  core_df_6b <- tibble::tibble(responses = responses,
                               q6b_df) |>
    mutate(question = "Q6b")
  core_df_6b <- tibble::tibble(year = year, 
                               core_df_6b)
  core_df_6b <- core_df_6b |> mutate(year = as.numeric(year))
  core_df_6b <- tibble::tibble(semester = semester_1,
                               core_df_6b)
  core_df_6b <- tibble::tibble(course = course,
                               core_df_6b)
  # question 6c
  q6c_indeces <- c(11, 12, 13, 14, 15, 16, 17)
  q6c <- only_numbers_2[q6c_indeces]
  q6c[[1]] <- q6c[[1]][4:6]
  q6c[[2]] <- q6c[[2]][4:6]
  q6c[[3]] <- q6c[[3]][4:6]
  q6c[[4]] <- q6c[[4]][4:6]
  q6c[[5]] <- q6c[[5]][4:6]
  q6c[[6]] <- q6c[[6]][4:6]
  q6c[[7]] <- q6c[[7]][4:6]
  q6c_df <- do.call(rbind.data.frame, q6c)
  names(q6c_df) <- c("n_students", "perc", "univ_perc")
  core_df_6c <- tibble::tibble(responses = responses, 
                               q6c_df) |>
    mutate(question = "Q6c")
  core_df_6c <- tibble::tibble(year = year, 
                               core_df_6c) 
  core_df_6c <- core_df_6c |> mutate(year = as.numeric(year))
  core_df_6c <- tibble::tibble(semester = semester_1,
                               core_df_6c)
  core_df_6c <- tibble::tibble(course = course,
                               core_df_6c)
  binded_df <- bind_rows(core_df_1, 
                         core_df_2,
                         core_df_3,
                         core_df_5a,
                         core_df_5b,
                         core_df_5c,
                         core_df_6a,
                         core_df_6b,
                         core_df_6c)
  binded_df <- binded_df |> mutate(responses_hm = 
                                     fct_recode(responses, AgreeStrongly = 'Strongly Agree',
                                                AgreeSomewhat = 'Agree Somewhat',
                                                DisagreeSomewhat = 'Disagree Somewhat',
                                                DisagreeStrongly = 'Disagree Strongly')
  ) |>
    mutate(question_hm = as.factor(question)) |>
    mutate(count_hm = as.double(n_students)) |> mutate(responses_hm = 
                                                         fct_relevel(responses_hm, c("DisagreeStrongly",
                                                                                     "Disagree",
                                                                                     "DisagreeSomewhat",
                                                                                     "Neutral",
                                                                                     "AgreeSomewhat",
                                                                                     "Agree",
                                                                                     "AgreeStrongly"))) |>
    mutate(full_question = 
             case_when(question_hm == "Q1" ~ "Q1. Valuable",
                       question_hm == "Q2" ~ "Q2. Organized",
                       question_hm == "Q3" ~ "Q3. Learning",
                       question_hm == "Q5a" ~ "Q5a. Fair",
                       question_hm == "Q5b" ~ "Q5b. Timely",
                       question_hm == "Q5c" ~ "Q5c. Constructive",
                       question_hm == "Q6a" ~ "Q6a. Rec_Prof",
                       question_hm == "Q6b" ~ "Q6b. Rec_Course",
                       question_hm == "Q6c" ~ "Q6c. Effective")) 
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

