#These can be run for all courses, using the same script

library(dplyr)
library(lubridate)

#A. TASK DEFINITION

#Data ONLY FROM BEFORE the course start
total_time_in_course<- stus %>% 
  filter(timestamp <= as.POSIXct(start_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  group_by(course_id, user_id, session_no)  %>%
  arrange(timestamp, .by_group = TRUE) %>%
  summarize(
    click_diff= as.numeric(timestamp-lag(timestamp))/3600) %>%  
  ungroup(session_no) %>%
  summarize(
    TD_time_bef= sum(click_diff, na.rm=TRUE), #time spent in canvas before the start of the course (in HOURS)
    TD_m_time_bef= mean(click_diff, na.rm=TRUE), #average time spent in canvas before the start of the course PER SESSION (in HOURS)
    TD_sd_time_bef= sd(click_diff, na.rm=TRUE)) %>% #STANDARD DEVIATION of the time spent in canvas before the start of the course PER SESSION (in HOURS)
 ungroup()

#Data ONLY FROM BEFORE the course start
task_def_before<- stu_dtopic %>% 
  filter(timestamp <= as.POSIXct(start_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  group_by(course_id, user_id) %>%
  summarize(
    TD_n_disc_bef= sum(disc_topic_type=="\\N", na.rm= TRUE), #no. discussion clicks before start of course (all students have 0), no discussion post before the official start of the course
    TD_n_ann_bef= sum(disc_topic_type=="Announcement", na.rm= TRUE))  %>%
  ungroup()

#Data from BEFORE AND DURING the course
time_on_assignments_withlag<- stus %>% 
  filter(timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>%  
  group_by(course_id, user_id, session_no, assignment_id)  %>%
  arrange(timestamp, .by_group = TRUE) %>%
  summarize(
    click_diff= as.numeric(timestamp-lag(timestamp))/3600) %>%
  ungroup(session_no, assignment_id) %>%  #can only ungroup session_no if you want to see how much time per assignment was spent also
  filter(lag(assignment_id) != "\\N") %>%
  summarize(
    TD_time_on_assig= sum(click_diff, na.rm=TRUE), #time spent on assignment pages (before and during course) in hours (this is the more "correct" one), version with lag
    TD_m_time_on_assig= mean(click_diff, na.rm=TRUE), #average of time spent on assignment pages in hours
    TD_sd_time_on_assig= sd(click_diff, na.rm=TRUE)) %>% #standard deviation of time spent on assignment pages in hours
  ungroup()

#Data from BEFORE AND DURING the course
first_announcement_clicks<- stu_dtopic %>% filter(timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>%   
  filter(disc_topic_type=="Announcement") %>%
  group_by(course_id, user_id, discussion_id) %>%
  summarize(
    hours_after_first_announcement_posted= min(posted_diff)) %>%  
  ungroup(discussion_id) %>% 
  summarize(
    TD_m_1st_ann_after_post= mean(hours_after_first_announcement_posted), #average time it took students (in hours) to click on announcements after they were posted
    TD_sd_1st_ann_after_post= sd(hours_after_first_announcement_posted)) %>%
  ungroup()

#Data from DURING the course
task_definition_main<- stu_dtopic %>% filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>%  #only clicks during the course (I do this in order to not include again the clicks before since some indicators are similar to the ones already calculated with those)
  group_by(course_id, user_id) %>%
  summarize(
    TD_n_ann= sum(disc_topic_type=="Announcement", na.rm= TRUE),  #announcement clicks during the course (there were none before)
    TD_n_disc= sum(disc_topic_type=="\\N", na.rm= TRUE), #discussion clicks during the course (there were 0 clicks before the start)
    TD_assig_per_ses= sum(assignment_id != "\\N")/sum(session_start, na.rm = TRUE)) %>% #assignment clicks per study session
 ungroup()

task_definition_main_2<- stu_dtopic %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>%  #only clicks during the course (I do this in order to not include again the clicks before since some indicators are similar to the ones already calculated with those)
  group_by(course_id, user_id, session_no) %>%
  summarize(
    total_ann= sum(disc_topic_type=="Announcement", na.rm= TRUE),
    total_disc= sum(disc_topic_type=="\\N", na.rm= TRUE),
    total_ass= sum(assignment_id != "\\N")) %>%
  ungroup(session_no) %>%
  summarize(
    TD_m_ann= mean(total_ann),  #announcement clicks during the course (there were none before)
    TD_sd_ann= sd(total_ann),
    TD_m_disc= mean(total_disc),
    TD_sd_disc= sd(total_disc), #discussion clicks during the course (there were 0 clicks before the start)
    TD_m_assig= mean(total_ass),
    TD_sd_assig= sd(total_ass)) %>%
  ungroup()



#B. GOAL SETTING

#Data ONLY FROM BEFORE the course start
goal_setting_before<- stu_dtopic %>% 
  filter(timestamp <= as.POSIXct(start_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  group_by(course_id, user_id) %>%
  summarize(
    GS_n_mod_bef= sum(grepl('module', url))) %>% #looking at the module page before the start of the course
  ungroup()

#Data from BEFORE and DURING the course
goal_setting<- stus %>% 
  filter(timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>%  
  group_by(course_id, user_id, session_no) %>%
  summarize(
    total_schedule= sum(grepl('schedule', url)),  #schedule clicks during the course
    total_rubric= sum(grepl('rubric', url)),  #clicks on the rubric
    total_studyg= sum(grepl('course-information|guide', url)))  %>% #clicks on the study guide
  ungroup(session_no) %>%
  summarise(
    GS_n_schedule= sum(total_schedule),
    GS_m_schedule= mean(total_schedule),
    GS_sd_schedule= sd(total_schedule),
    GS_n_rubric= sum(total_rubric),
    GS_m_rubric= mean(total_rubric),
    GS_sd_rubric= sd(total_rubric),
    GS_n_studyg= sum(total_studyg),
    GS_m_studyg= mean(total_studyg),
    GS_sd_studyg= sd(total_studyg)) %>%
  ungroup()

#GS indicator, instead of difference between start of course (use the first click of the first student)
GS_first_click<- stus %>% #difference between the start of the course and the first click on the course (in hours)
  group_by(course_id, user_id) %>%
  arrange(timestamp, .by_group = TRUE) %>%
  summarize(
    first_click= min(timestamp), 
    GS_first_click= as.numeric(difftime(as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam"), first_click, units="hours"))) %>%
  select(- first_click) %>%
  ungroup()


#D. ADAPTATION (mainly data from during the course and after its end, so students have time to reflect and adapt future tactics and goals)

stus<- stus %>% mutate(grade_id= (grepl('grades', url)))
#Data from DURING and AFTER the course 
A_time_on_grades_page<- stus %>% filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% #time spent on the Grades page in hours
  group_by(course_id, user_id, session_no, grade_id)  %>%
  arrange(timestamp, .by_group = TRUE) %>%
  summarize(
    click_diff= as.numeric(timestamp-lag(timestamp))/3600) %>%
  ungroup(session_no, grade_id) %>%  
  filter(lag(grade_id==TRUE)) %>%
  summarize(
    A_time_grades= sum(click_diff, na.rm=TRUE), #total time on gradebook
    A_m_time_grades= mean(click_diff, na.rm=TRUE), #mean time per session spent on gradebook
    A_sd_time_grades= sd(click_diff, na.rm=TRUE))  %>% #sd per session spent on gradebook
 ungroup()

#Data from DURING and AFTER the course 
A_average_assignment_clicks_after_deadline<- stu_assig %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  filter(assignment_id!="\\N" & timestamp>assig_deadline) %>%
  group_by(course_id, user_id, assignment_id) %>%
  summarize(
    assig_clicks_after_deadline= sum(assignment_id!=0, na.rm=TRUE)) %>%
  ungroup(assignment_id) %>%
  summarize(
    A_n_assig_after_deadl= sum(assig_clicks_after_deadline, na.rm=TRUE),  #total clicks on assignments after their deadline 
    A_m_assig_after_deadl= mean(assig_clicks_after_deadline, na.rm=TRUE),  #average clicks per assignment after their deadline
    A_sd_assig_after_deadl= sd(assig_clicks_after_deadline, na.rm=TRUE)) %>% #sd of the clicks per assignment after their deadline
 ungroup()

#Data from DURING and AFTER the course 
stu_assig<- stu_assig %>% mutate(assig_deadline_diff= as.numeric((timestamp - assig_deadline))/3600)

A_average_assignment_time_after_deadline<- stu_assig %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% #average time spent on the assignments  after their deadline 
  filter(assignment_id!="\\N" & timestamp>assig_deadline & quiz_id=="\\N") %>% #I added these extra conditions to make it more accurate
  group_by(course_id, user_id, assignment_id) %>%
  summarize(
    assig_deadline_diff= as.numeric((timestamp - assig_deadline))/3600) %>%
  ungroup(assignment_id) %>%
  summarize(
    A_time_assig_after_deadl = sum(assig_deadline_diff, na.rm=TRUE), #time spent on the assignments  after their deadline 
    A_m_time_assig_after_deadl = mean(assig_deadline_diff, na.rm=TRUE),  # average time spent per assignment after deadline
    A_sd_time_assig_after_deadl = sd(assig_deadline_diff, na.rm=TRUE))  %>% #sd of the average time spent per assignment after deadlines
 ungroup()
#I added both indicators that look at the same thing, the access of the students on the assignments after their deadline (clicks vs. time). The corr is non-sign (although quite close), but some of the time values are extremely high, thus I am not sure how to interpret it. Better to also look at other course and see which one to go with

#Data from DURING and AFTER the course 
A_time_on_quizzes<- stus %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  group_by(course_id, user_id, session_no, quiz_id)  %>%
  arrange(timestamp, .by_group = TRUE) %>%
  summarize(
    click_diff= as.numeric(timestamp-lag(timestamp))/3600) %>%
  ungroup(session_no) %>%  
  filter(lag(quiz_id!="\\N")) %>%
  summarize(
    total_quiz= sum(click_diff, na.rm=TRUE)) %>%
  ungroup(quiz_id) %>%
  summarize(
    A_time_quiz= sum(total_quiz, na.rm=TRUE),   #time spent on the quizzes pages in hours
    A_m_time_quiz= mean(total_quiz, na.rm=TRUE),  #time spent on average per quiz in hours
    A_sd_time_quiz= sd(total_quiz, na.rm=TRUE)  #sd of the time spent on average per quiz in hours
  ) %>%
  ungroup()


#Data from DURING and AFTER the course 
stu_assig_subs$sub_submission_at<- as.POSIXlt(stu_assig_subs$sub_submission_at, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")
stu_assig_subs$assig_deadline<- as.POSIXlt(stu_assig_subs$assig_deadline, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")

#the problem here is that most assignments don't have clear deadline saved in the assignment table. 
avg_diff_between_sub_and_assig_deadline<- stu_assig_subs %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>%   #average time difference between the assignment deadline and the submission 
  filter(assignment_id!="\\N" & sub_submission_at < assig_deadline) %>%
  group_by(user_id, course_id, assignment_id) %>%
  summarize(
    diff_sub_assig_posted= min(assig_deadline- sub_submission_at)/3600) %>%  
  ungroup(assignment_id) %>% 
  summarize(
    A_diff_subs_post_assig= sum(as.numeric(diff_sub_assig_posted)),
    A_m_diff_subs_post_assig= mean(as.numeric(diff_sub_assig_posted)),
    A_sd_diff_subs_post_assig= sd(as.numeric(diff_sub_assig_posted))) %>%
  ungroup()

#Data from DURING and AFTER the course 
adaptation_main<- stu_subs %>% filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>%  
  group_by(user_id, course_id) %>%
  summarize(
    A_n_upd_subs= sum(sub_updated!="\\N", na.rm= TRUE), #number of clicks on submission updates
    A_n_sub_comm= sum(sub_comm_id!=0, na.rm=TRUE), #number of comments on submissions -3 people left comments (check below how to filter for them)
    A_n_grade_access=sum(grepl('grades', url))) %>%
  ungroup()

#Data from AFTER the course's end
clicks_after_course_end<- stus %>% filter(timestamp >= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  group_by(user_id, course_id, session_no) %>%
  summarize(
    total_clicks= n(),
    sessions= sum(session_start)) %>%
  ungroup(session_no) %>%
  summarise(
    A_sessions= sum(sessions),  #total number of session after the course ended
    A_after_course= sum(total_clicks),  #total number of clicks after the course ended
    A_m_after_course= mean(total_clicks),  #average number of clicks per session after the course ended
    A_sd_after_course= sd(total_clicks)  #sd of the clicks per session after the course ended
  ) %>%
  ungroup()
  
#Run the enactment phase. This needs to be done by running different scripts based on the course
#This is just an example of how we did it. You have to change and include actual numbers and paths
file_indicator_function_only_scripts <- function(id) {
  if (id == "Course_A_id") {
    source("~/Course A enactment script.R")
  } else if (id == "Course_B_id") {
    source("~/Course B enactment script.R")
  }   else if (id == "Course_S_id") {
    source("~/Course S enactment script.R")
  } else if (id == "Course_D_id") {
    source("~/Course D enactment script.R")
  } else {
    # Handle unknown IDs
    print("Invalid ID")
  }
}


