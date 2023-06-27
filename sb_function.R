sb_function <- function(eval) {
  tab <- pdf_text(eval)
  first_page <- strsplit(tab[1], "\n")[[1]]
  first_page_zeroes <- str_replace_all(first_page,
                                       pattern = "-",
                                       replacement = "0")
  only_numbers <- str_extract_all(first_page_zeroes, "\\d+")
  gender_responses <- c("Male",
                        "Female",
                        "Other",
                        "Prefer not to disclose")
  reasons_responses <- c("Major",
                         "Minor",
                         "Distribution/Diversity",
                         "Interest in Subject",
                         "Instructor",
                         "Off Campus",
                         "Work on own schedule",
                         "Other")
  class_responses <- c("First Year",
                       "Sophomore",
                       "Junior",
                       "Senior",
                       "Grad Student",
                       "Other")
  year <- c(only_numbers[[2]][1],
            only_numbers[[2]][1],
            only_numbers[[2]][1],
            only_numbers[[2]][1])
  
  year_2 <- c(only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1])
  
  year_3 <- c(only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1],
              only_numbers[[2]][1])
  
  reasons_indeces <- c(16, 18, 19, 20, 21, 23, 26, 30)
  reasons <- only_numbers[reasons_indeces] 
  reasons[[1]] <- reasons[[1]][1:3]
  reasons[[2]] <- reasons[[2]][1:3]
  reasons[[3]] <- reasons[[3]][1:3]
  reasons[[5]] <- reasons[[5]][1:3]
  reasons[[6]] <- reasons[[6]][1:3]
  reasons[[7]] <- reasons[[7]][1:3]
  reasons_df <- do.call(rbind.data.frame, reasons)
  names(reasons_df) <- c("n_students", "perc", "univ_perc")
  core_reasons_df <- tibble::tibble(responses = reasons_responses,
                                    reasons_df) |>
    mutate(question = "primary reason for enrollment")
  core_reasons_df <- tibble::tibble(year = year_2, 
                                    core_reasons_df) 
  core_reasons_df <- core_reasons_df |> mutate(year = as.numeric(year))
  # gender
  gender_indeces <- c(35, 37, 38, 39)
  gender <- only_numbers[gender_indeces]
  gender[[3]] <- gender[[3]][1:3]
  gender[[4]] <- gender[[4]][1:3]
  gender_df <- do.call(rbind.data.frame, gender)
  names(gender_df) <- c("n_students", "perc", "univ_perc")
  core_gender_df <- tibble::tibble(responses = gender_responses,
                                   gender_df) |>
    mutate(question = "gender")
  core_gender_df <- tibble::tibble(year = year, 
                                   core_gender_df) 
  core_gender_df <- core_gender_df |> mutate(year = as.numeric(year))
  # class year 
  class_indeces <- c(44, 45, 47, 48, 51, 53)
  class_year <- only_numbers[class_indeces]
  class_year[[1]] <- class_year[[1]][1:3]
  class_year[[2]] <- class_year[[2]][1:3]
  class_year[[3]] <- class_year[[3]][1:3]
  classyear_df <- do.call(rbind.data.frame, class_year)
  names(classyear_df) <- c("n_students", "perc", "univ_perc")
  core_class_df <- tibble::tibble(responses = class_responses,
                                  classyear_df) |>
    mutate(question = "class year")
  core_class_df <- tibble::tibble(year = year_3, 
                                  core_class_df) 
  core_class_df <- core_class_df |> mutate(year = as.numeric(year))
  sb_df <- bind_rows(core_reasons_df,
                     core_gender_df,
                     core_class_df)
  return(sb_df)
}
