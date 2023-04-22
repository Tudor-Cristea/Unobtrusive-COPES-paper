library(dplyr)
library(psych)
#library(VIM)
options(scipen = 100, digits = 4) #necessary to see the whole ID of students/course, but also for some indicators

#1. Take all 4 courses and remove variables with 0 hits all across (these are basically variables that do not work for that particular course, eg. course with no discussion forum or no video lecture)
Course_D<- read.csv("Course_D_all_raw_indicators.csv", 
                     colClasses=c("user_id"="character", "course_id"= "character")) %>% 
  select(-X, -TD_n_disc, -TD_n_disc_bef, -TD_m_disc, -TD_sd_disc, -GS_n_schedule, 
         -GS_m_schedule, -GS_sd_schedule, -LS_n_Q_opt, -LS_m_Q_opt, -LS_sd_Q_opt, 
         -A_n_sub_comm, -TD_assig_per_ses)

Course_A<- read.csv("Course_A_all_raw_indicators.csv", 
                      colClasses=c("user_id"="character", "course_id"= "character")) %>% 
  select(-X, -TD_n_disc_bef, -A_n_sub_comm, -TD_assig_per_ses)

Course_B<- read.csv("Course_B_all_raw_indicators.csv", 
                    colClasses=c("user_id"="character", "course_id"= "character")) %>% 
  select(-X, -TD_n_disc_bef, -A_n_sub_comm, -TD_assig_per_ses)

Course_S<- read.csv("Course_S_all_raw_indicators.csv", 
                    colClasses=c("user_id"="character", "course_id"= "character")) %>% 
  select(-X, -TD_n_disc_bef, -A_n_sub_comm, -TD_assig_per_ses)

#Use these to see the 0 variables
#summary(Course_A)
#summary(Course_B)
#summary(Course_D)
#summary(Course_S)


#2. Replace NAs with 0, but only for variables for which it makes sense (for which NA really means 0 clicks). For some variables NA means it was impossible to calculate that variable due to other circumstances 

copes_datasets<- list(Course_A, Course_B, Course_D, Course_S)
new_names<- c("Course_A_clean", "Course_B_clean", "Course_D_clean", "Course_S_clean")
replace_with_NAs<- c("_n_|dl_|TD_time_bef|TD_time_on_assig|A_time_grades|A_time_assig_after_deadl|A_sessions|A_after_course|A_time_quiz|perc_|")


apply_mutate_across_matches_replace_with_NAs_and_rename <- function(datasets, replace_NAs, new_names) {
  modified_datasets = lapply(datasets, function(x) {
    x %>%
      mutate(across(matches(replace_NAs),
                    ~replace(., is.na(.), 0)))
      
  })
  names(modified_datasets) <- new_names
  return(modified_datasets)
}

apply_mutate_across_matches_replace_with_NAs_and_rename(copes_datasets, replace_with_NAs, new_names) %>% 
  list2env(.GlobalEnv)

#3. Eliminate variables that have low variance
#Use the nearZeroVar function from the caret package, in order to find variables that have very low variance. 
#report freqCut (cutoff for the ratio of the most common value to the second most common value) and uniqueCut (cutoff for percentage of distinct values out of the the number of total samples)
#default is 95/5 for freqCut and 10 for uniqueCut

#caret::nearZeroVar(Course_A_clean, saveMetrics = T)
#caret::nearZeroVar(Course_B_clean, saveMetrics = T)
#caret::nearZeroVar(Course_D_clean, saveMetrics = T)
#caret::nearZeroVar(Course_S_clean, saveMetrics = T)

#4. Run the transformations. We will run some simple standardization

Course_D_transformed<- Course_D_clean
Course_D_transformed[ ,3:85] <- data.frame(lapply(Course_D_clean[ ,3:85], scale))

Course_A_transformed<- Course_A_clean
Course_A_transformed[,3:106] <- data.frame(lapply(Course_A_clean[,3:106], scale))

Course_B_transformed<- Course_B_clean
Course_B_transformed[,3:106] <- data.frame(lapply(Course_B_clean[,3:106], scale))

Course_S_transformed<- Course_S_clean
Course_S_transformed[,3:105] <- data.frame(lapply(Course_S_clean[,3:105], scale))


#5. One final glance, and it is obvious that many variables have very bad distribution, even after all these transformations. We remove those
#we look across distributions and try to remove the same variables for all courses. Thus we remove 2 variables,both that relate to student online behavior from before the course start (which makes sense)

Course_D_final<- Course_D_transformed %>% select(-TD_n_ann_bef, -GS_n_mod_bef)
Course_A_final<- Course_A_transformed %>% select(-TD_n_ann_bef, -GS_n_mod_bef)
Course_B_final<- Course_B_transformed %>% select(-TD_n_ann_bef, -GS_n_mod_bef)
Course_S_final<- Course_S_transformed %>% select(-TD_n_ann_bef, -GS_n_mod_bef)

write.csv(Course_D_final, "Course_D_clean_trans.csv")
write.csv(Course_A_final, "Course_A_clean_trans.csv")
write.csv(Course_B_final, "Course_B_clean_trans.csv")
write.csv(Course_S_final, "Course_S_clean_trans.csv")