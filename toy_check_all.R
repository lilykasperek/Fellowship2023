## Reflect on your learning in this course as a whole and identify the top 3-5 elements that contributed the most to your learning.

toy_df_full <- tibble::tibble(student_id = c(LETTERS[1:15]),
               "a. Course materials" = c(0, rep(1, 14)),
               "b. Course assignments" = rep(1, 15),
               "c. Collaborative learning experiences (peer review, teamwork, partnered learning, etc.)" = c(rep(0, 8), 1, 1, rep(0, 5)),
               "d. Hands-on experiences (e.g., labs, field trips, field work, community-engaged learning)" = rep(0, 15),
               "e. Clarity in guidelines or requirements for assignments" = c(1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1),
               "f. Explanation of challenging concepts / methods" = c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1),
               "g. Inclusion of diverse voices and perspectives in the course materials" = c(rep(0, 10), 1, rep(0, 4)),
               "h. Use of a variety of teaching methods (lecture, group work, problem solving, discussion, etc.)" = c(0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1),
               "i. Instructor’s receptiveness to diverse student viewpoints" = c(rep(0, 14), 1),
               "j. Instructor’s sensitivity to students of different backgrounds and life experiences" = rep(0, 15),
               "k. Instructor’s availability to students during office hours and/or via email" = c(1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0),
               "l. Feedback on graded work" = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1))

toy_df_full



## OTHER QUESTIONS (but should have same visualization structure so can ignore):

## In this course, I had opportunities to participate in the following in-class activities (check all that apply):
toy_df <- tibble::tibble(student_id = c(LETTERS),
                         `Performances / demonstrations` = c(1, 1, 0, 1, 1, 0),
                         Discussion = c(1, 0, 0, 1, 1, 1),
                         `Asking questions` = c(0, 0, 0, 0, 0, 0),
                         `Collaborative learning` = c(1, 1, 0, 1, 1, 1),
                         `Peer review of other students’ work` = c(1, 1, 1, 1, 1, 1),
                         `Analyzing problems or case studies` = ,
                         `Analyzing articles and texts` = ,
                         `Drafting and revising work` = ,
                         `Reflective writing / journaling` = ,
                         `Note-taking during lecture` = ,
                         `Research` = ,
                         `Use of clickers or electronic quizzes with immediate feedback`)
## Reflect on your learning in this course as a whole: Could any of the following have been employedmoreeffectivelytopromoteyourlearninginthecourse? (Selectanythatapply.)


toy_df <- tibble::tibble(student_id = c("A", "B", "C", "D", "E", "F"),
                         lecture = c(1, 1, 0, 1, 1, 0),
                         group_work = c(1, 0, 0, 1, 1, 1),
                         reading = c(0, 0, 0, 0, 0, 0),
                         presentation = c(1, 1, 0, 1, 1, 1),
                         code = c(1, 1, 1, 1, 1, 1))
toy_df