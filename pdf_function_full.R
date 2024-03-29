pdf_function_full <- function(eval) {
  # page 1
  tab <- pdf_text(eval)
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
  
  responses <- c("Strongly Agree",
                 "Agree", 
                 "Agree Somewhat",
                 "Neutral",
                 "Disagree Somewhat",
                 "Disagree",
                 "Disagree Strongly")
  
  responses_2 <- c("Too High",
                   "Somewhat High", 
                   "Appropriate",
                   "Somewhat Low",
                   "Too Low") 
  
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
  core_df_1 <- tibble::tibble(responses = responses,
                              q1_df) |>
    mutate(question = "Q1")
  # question 2
  q2_indeces <- c(5, 7, 8, 9, 10, 11, 12) ## back half of 16
  q2 <- only_numbers[q2_indeces]
  q2[[7]] <- q2[[7]][4:6]
  q2_df <- do.call(rbind.data.frame, q2)
  names(q2_df) <- c("n_students", "perc", "univ_perc")
  core_df_2 <- tibble::tibble(responses = responses,
                              q2_df) |>
    mutate(question = "Q2")
  
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
  
  # question 4a
  q4a_indeces <- c(29, 30, 31, 32, 33)
  q4a <- only_numbers[q4a_indeces]
  q4a[[1]] <- q4a[[1]][4:6]
  q4a[[2]] <- q4a[[2]][4:6]
  q4a[[3]] <- q4a[[3]][4:6]
  q4a_df <- do.call(rbind.data.frame, q4a)
  names(q4a_df) <- c("n_students", "perc", "univ_perc")
  core_df_4a <- tibble::tibble(responses = responses_2,
                               q4a_df) |>
    mutate(question = "Q4a")
  
  # question 4b
  q4b_indeces <- c(38, 40, 42, 44, 45)
  q4b <- only_numbers[q4b_indeces]
  q4b_df <- do.call(rbind.data.frame, q4b)
  names(q4b_df) <- c("n_students", "perc", "univ_perc")
  core_df_4b <- tibble::tibble(responses = responses_2,
                               q4b_df) |>
    mutate(question = "Q4b")
  
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
  
  # all questions, missing summary of means 
  full_df <- bind_rows(core_df_1,
                       core_df_2,
                       core_df_3,
                       core_df_4a,
                       core_df_4b,
                       core_df_4c,
                       core_df_5a,
                       core_df_5b,
                       core_df_5c,
                       core_df_6a,
                       core_df_6b,
                       core_df_6c)
  
  full_df <- full_df |> mutate(responses_hm = 
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
                       question_hm == "Q6c" ~ "Q6c. Effective",
                       question_hm == "Q4a" ~ "Q4a. Workload",
                       question_hm == "Q4b" ~ "Q4b. Grading",
                       question_hm == "Q4c" ~ "Q4c. Sophistication")) 
  return(full_df)
}
