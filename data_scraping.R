##install.packages("tcltk2")
##install.packages("PDE", dependencies = TRUE)

##install.packages("pdftools")
library(tidyverse)
library(pdftools)

## scan for hurtful and insensitive wording

evals_pdf <- pdf_text("STAT113_Fall_2020_1.pdf") 
evals_pdf 

## https://crimebythenumbers.com/scrape-table.html
tab <- pdf_text("STAT234_Fall_2022.pdf")
first_page <- strsplit(tab[1], "\n")[[1]]


## replace all dashes with 0's: this should help
## with consistency from one PDF to another
first_page_zeroes <- str_replace_all(first_page,
                                     pattern = "-",
                                     replacement = "0")

## d stands for "digits": this line says to grab all of the
## digits from the table
only_numbers <- str_extract_all(first_page_zeroes, "\\d+")
only_numbers

## obtaining some info about the course itself
metadata_df <- tibble::tibble(control_num = only_numbers[[1]][2],
                              year = only_numbers[[2]][1],
                              course = only_numbers[[2]][3],
                              n_evals = only_numbers[[4]][1],
                              n_evals_univ = only_numbers[[8]][1])
metadata_df

## match which parts only_numbers go with Question 2
q2_indeces <- c(7, 9, 10, 11, 12, 14, 16) ## back half of 16
q2 <- only_numbers[q2_indeces]
q2

q2[[7]] <- q2[[7]][4:6] ## only getting the last three numbers of 16

## do.call says to says to call the rbind.data.frame() function
## on the list q2 to bind together all of the elements in that list
q2_df <- do.call(rbind.data.frame, q2)
names(q2_df) <- c("n_students", "perc", "univ_perc")
q2_df

responses <- c("strongly agree",
               "agree", 
               "agree somewhat",
               "neutral",
               "disagree somewhat",
               "disagree",
               "disagree strongly")

core_df <- tibble::tibble(responses = responses,
                          q2_df) |>
  mutate(question = "Q2")
core_df ## back to a regular tibble!!

## you might actually want to include the row with the total numbers 
## somewhere as a quick check, making sure that the sum of n_students
## actually does equal that total and that the sum of perc actual does
## add up to 100.
## 
## the end goal would be to convert this code to some functions to pull
## this information for each question. That is something for after
## getting it to work for all of the questions in just 1 PDF.


