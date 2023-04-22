
#Load the necessary main table from the (1) script. 

library(dplyr)
library(lubridate)

#1. Form the sessions (stus= students + sessions)

stus <- stu %>%
  group_by(user_id, course_id) %>% 
  mutate(
    timestamp = as.POSIXct(timestamp,format="%Y-%m-%d %H:%M:%S ",tz="Europe/Amsterdam"),
    week= week(timestamp)) %>%
  arrange(timestamp, .by_group = TRUE) %>% #.by_group is very important, otherwise it only arranges by the timestamp, the students are presented randomly
  mutate(
    diff_time = timestamp - lag(timestamp))


stus <- stus %>%
  group_by(user_id) %>% #add the course_id when we have the data for it
  mutate(
    # 30 minutes of idle time starts a new session, change here if necessary
    session_start = (diff_time >= 1800)) 

stus <- stus %>%
  group_by(user_id, course_id) %>%  
  mutate(session_start = ifelse(row_number() == 1, 1,
                                ifelse(is.na(session_start), FALSE,
                                       session_start)),
         session_no = cumsum(session_start))


#filtering for students with more than 2 sessions can be done here
length(unique(as.factor(stus$user_id)))

stus<- stus %>%
  group_by(user_id) %>%
  filter(max(session_no) >2) %>%
  ungroup()

length(unique(as.factor(stus$user_id)))

#calculate average number of sessions per student
no_sessions<- stus %>%
  group_by(user_id) %>%
  summarize(
    max= max(session_no))

mean(no_sessions$max)
# Course D= 94
# Course A= 95
# Course C= 121
# Course S= 81

#2. Some quick checks
hist(stus$timestamp, breaks="days") #distribution of clicks
range(stus$timestamp)


#3. We need to merge with certain tables for certain indicators. 
#I thought it might be more efficient to start by looking at the indicator and use the minimum amount of tables necessary for it (instead of merging all tables and doing all the indicators from the final one, as initially). This will prove useful later, when we have more courses and the datasets will be huge

#A. STUDENTS ONLY + Discussion TOPICS_DIM table (needed from Task Definition (Phase 1) on)

dtopic<- read.csv("disctopic.csv", colClasses=c('character')) #Makes distinction between announcements and discussions! (title, /N is discussion topics)
#REMOVE CLUTTER + RENAME (I do this for all tables in order to make the dataset more intuitive and easier to work with)
dtopic<- select(dtopic, -X, -created_at, -delayed_post_at, -pinned, 
                -locked, -course_id, -deleted_at, -discussion_type) 
dtopic<- dtopic %>% rename(disc_id= id, disc_topic_type= type, disc_title= title, 
                           disc_workflow= workflow_state, disc_last_reply= last_reply_at, 
                           disc_update= updated_at, disc_posted= posted_at, disc_group_id= group_id)
#MERGE STUDENTS with DISCUSSION TOPIC table
stu_dtopic<- left_join(stus, dtopic, by= c("discussion_id" = "canvas_id")) 
#For one of the indicators, we need to change the class of one variables (disc_posted=when the discussion was posted), and form a new variable= the difference between the click and when the discussion was posted
stu_dtopic$disc_posted<- as.POSIXlt(stu_dtopic$disc_posted, "%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")
stu_dtopic<- stu_dtopic %>% mutate(posted_diff= as.numeric((timestamp - disc_posted))/3600) #represented by hours


#B. STUDENTS ONLY + ASSIGNMENTS_DIM TABLE
assigs<- read.csv("assignments.csv", colClasses=c('character'))
#REMOVE CLUTTER + RENAME
assigs<- select(assigs, -X, -lock_at, -peer_reviews_assigned, -peer_reviews_due_at, 
                -automatic_peer_reviews, points_possible, -created_at, -updated_at, 
                -all_day, -all_day_date, -could_be_locked, -anonymous_peer_reviews, 
                -muted, -position, -visibility, -grade_group_students_individually) 
assigs<- assigs %>% rename(assig_id= id, assig_peer_reviews= peer_reviews, 
                           assig_grading_type= grading_type, assig_peer_review_count= peer_review_count, 
                           assig_canvas_id= canvas_id, assig_title= title, assig_description= description, 
                           assig_deadline= due_at, assig_post= unlock_at, assig_submission_type= submission_types, 
                           assig_workflow= workflow_state, assig_external_tool= external_tool_id)
stus$assignment_id<- str_remove(stus$assignment_id, "754200000000") #remove first part of the assignment id so it matches with canvas id from the other tables

#MERGE STUDENTS ONLY with ASSIGNMENTS table (stu= only student clicks + assig= assignment tables)
stu_assig<- left_join(stus, assigs, by= c("assignment_id" = "assig_canvas_id", "course_id"))
#Necessary for one of the Adaptation indicators 
stu_assig$assig_deadline<- as.POSIXlt(stu_assig$assig_deadline, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")


#C. SUBMISSION_DIM table
subs<- read.csv("subd.csv", colClasses=c('character'))
#REMOVE CLUTTER + RENAME
subs<- subs %>% rename(sub_id=id, sub_group_id= group_id, sub_workflow= workflow_state, 
                       sub_type= submission_type, sub_created= created_at, sub_updated= updated_at, 
                       sub_submission_at= submitted_at, sub_attempts= attempt)
subs$assignment_id<- str_remove(subs$assignment_id, "754200000000") #remove first part of the assignment id so it matches with canvas id from the request
stus$assignment_id<- str_remove(stus$assignment_id, "754200000000") #remove first part of the submission id so it matches with canvas id from the other tables

#MERGE STUDENTS with SUBMISSION_DIM table (stu= only student clicks + subs= submissions tables)
stu_subs<- left_join(stus, subs, by= c("user_id", "assignment_id"))

#D. Submission Comments_DIM table (needed for Adaptation (Phase 4))
subd_comm<- read.csv("../subd_comm.csv", colClasses=c('character')) %>% rename(sub_comm_id= id)
stu_subs<- left_join(stu_subs, subd_comm, by=c("sub_id"= "submission_id", "user_id" = "author_id"))


#E. QUIZ TABLES 
quiz_main_fact<- read.csv("quiz_main_fact.csv", colClasses=c('character')) %>% 
  select(quiz_id, course_id, time_limit, allowed_attempts, question_count, assignment_id, 
         points_possible) 
quiz_main_fact<- quiz_main_fact %>% rename(quiz_time_limit= time_limit, quiz_max_attempts= allowed_attempts, 
                                           quiz_question_count= question_count, quiz_max_points= points_possible)
quiz_main<- read.csv("quiz_main.csv", colClasses=c('character')) %>% 
  select(id, course_id, canvas_id, points_possible, quiz_type, assignment_id, workflow_state, 
         display_questions, unlock_at, lock_at) 
quiz_main<- quiz_main %>% rename(quiz_id= id, quiz_canvas_id= canvas_id, quiz_max_points= points_possible, 
                                 quiz_workflow= workflow_state, quiz_display_questions= display_questions, 
                                 quiz_unlock= unlock_at, quiz_lock= lock_at)

# Merge quiz_fact and quiz_dim
quiz_fact_dim <- quiz_main %>%
  left_join(quiz_main_fact,by = c("quiz_id", "course_id", "assignment_id", "quiz_max_points")) 

# get canvas_id from URL in requests
stus <- stus %>%
  mutate(quiz_canvas_id_url = gsub(".*/quizzes/","", url),
         quiz_canvas_id_url = as.numeric(gsub("/.*", "", quiz_canvas_id_url)))

# check canvas id's across tables
table(quiz_fact_dim$quiz_canvas_id)
table(stus$quiz_canvas_id_url) #! works

# merge via canvas_id
stus$quiz_canvas_id_url<- as.character(stus$quiz_canvas_id_url)

stus_quiz <- stus %>%
  left_join(quiz_fact_dim, by = c("quiz_canvas_id_url" = "quiz_canvas_id", "course_id" ))


#F. STUDENTS WITH ASSIGNMENTS MERGED WITH SUBMISSIONS (_DIM)
stu_assig_subs<- left_join(stu_assig, subs, by= c("user_id", "assignment_id"))
#use previous code for each table


#G. STUDENTS with FILES_DIM TABLE
stu_dtopic<- stu_dtopic %>% mutate(url2= gsub(".*files/*", "", url),
                                   url3= gsub("/.*", "", url2),
                                   url4= gsub("[?].*", "", url3),
                                   file_canvas_id= gsub('folder|search', "", url4))   #USE [] around special characters in regex

files <- read.csv("files.csv", colClasses=c('character')) %>% 
  select(-X, -user_id, -quiz_submission_id)
files<- files %>% rename(file_id= id, file_canvas_id= canvas_id, file_name= display_name, 
                         file_content= content_type, file_unlock_at= unlock_at)
files$file_id<- str_remove(files$file_id, "7542000000")


stus_discs_files<- left_join(stu_dtopic, files, by=c("file_canvas_id", "quiz_id", "assignment_id", "course_id")) %>% 
  select(-url2, -url3, -url4)


#Remove all unnecessary tables from memory
#rm(assigs, dtopic, files, quiz_main, quiz_main_fact, stu, subs, subd_comm)


