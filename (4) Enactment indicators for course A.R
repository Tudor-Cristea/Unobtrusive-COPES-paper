#Use stus_disc_files datasets

library(dplyr)
lubrary(lubridate)

#rename file_content categories to make them easier to follow
stus_discs_files$file_content<- as.factor(stus_discs_files$file_content)
levels(stus_discs_files$file_content)
stus_discs_files$file_content <- recode_factor(stus_discs_files$file_content, 'application/msword' = "Word Document", 
                                               'application/pdf' = "PDF", 'application/vnd.openxmlformats-officedocument.presentationml.slideshow'= "Slides", 
                                               'application/vnd.openxmlformats-officedocument.wordprocessingml.document'= "Word Document", 
                                               'image/png'= "Image", 'image/jpeg'= "Image", 
                                               'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'= "Excel Sheet", 
                                               'application/vnd.ms-excel'= "CSV file")
table(stus_discs_files$file_content)
length(unique(as.factor(stus_discs_files$file_name)))  #92 unique files

#Some preparation for the canvas materials!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

table(stus_discs_files$file_name) #it seems that for some lectures two versions were uploaded and both have clicks. We need to collapse the factors before moving on

#So we need to collapse the factor levels. We transform to factors, then use factor count and factor collapse. I think this is the most straightforward way
#I have tried to convert to factor first, but it is difficult since the level has numbers in it. It is easier to keep it in string and convert after
stus_discs_files$file_name <- as.character(stus_discs_files$file_name)
stus_discs_files$file_name[stus_discs_files$file_name == "Study guide Business Modeling.doc"] <- "Study guide Business Modeling.doc" #replace 1st by 2nd
stus_discs_files$file_name[stus_discs_files$file_name == "Study guide Business Modeling.doc"] <- "Study guide Business Modeling2.doc"          
stus_discs_files$file_name[stus_discs_files$file_name == "SolutionsExercisesWeek8-Recap-old.pdf"] <- "SolutionsExercisesWeek8-Recap.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Reader Information Modelling.pdf"] <- "Reader Information Modelling 2020.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "ExerciseWeek8-Recap.pdf"] <- "ExercisesWeek8-Recap1.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "signavio-getting-started-v2.pdf"] <- "signavio-getting-started.pdf"          

stus_discs_files$file_name[stus_discs_files$file_name == "Lecture1a.pdf"] <- "Lecture 1a.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture1b.pdf"] <- "Lecture 1b.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture2a.pdf"] <- "Lecture 2a.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture2b.pdf"] <- "Lecture 2b.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture3a.pdf"] <- "Lecture 3a.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture3b.pdf"] <- "Lecture 3b.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture4a.pdf"] <- "Lecture 4a.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture4b.pdf"] <- "Lecture 4b.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture4b-nosolutions.pdf"] <- "Lecture 4b.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == " Lecture 5a.pdf"] <- "Lecture 5A.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture 5b.pdf"] <- "Lecture 5B.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "BM 2020 - Lecture 5b.pdf"] <- "Lecture 5b.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture 6a.pdf"] <- "Lecture 6A.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == " Lecture 6b.pdf"] <- "Lecture 6B.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture 7a.pdf"] <- "Lecture 7A.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture 7b.pdf"] <- "Lecture 7B.pdf"
stus_discs_files$file_name[stus_discs_files$file_name == "Lecture 8.pdf"] <- "Lecture 8A.pdf"

stus_discs_files$file_name<- as.factor(stus_discs_files$file_name)
table(stus_discs_files$file_name)

stus_discs_files<- stus_discs_files %>% mutate(Lectures= str_match(file_name, "Lect.*|lect.*"), #can do a case_when after, for now it does not work directly
                                               Other_mandatory= str_match(file_name, "Reader.*|Weblecture.*|Petri.*|Assignment.*|Essential.*|Introduction.*|guide.*"),
                                               Optional_materials= str_match(file_name, "practice.*|Quiz.*|Exercises.*|Solution.*|exam.*"),
                                               canvas_material_type = case_when(
                                                 !is.na(Lectures) ~ "Lectures",
                                                 !is.na(Other_mandatory) ~ "Other_mandatory",
                                                 !is.na(Optional_materials) ~ "Optional_materials"
                                               ))

#ADD QUIZZES (has already been done in the preparation script, just join them)

# get canvas_id from URL in requests
stus_discs_files <- stus_discs_files %>%
  mutate(quiz_canvas_id_url = gsub(".*/quizzes/","", url),
         quiz_canvas_id_url = as.numeric(gsub("/.*", "", quiz_canvas_id_url)))

# merge via canvas_id
stus_discs_files$quiz_canvas_id_url<- as.character(stus_discs_files$quiz_canvas_id_url)
stus_discs_files_quiz <- stus_discs_files %>%
  left_join(quiz_fact_dim, by = c("quiz_canvas_id_url" = "quiz_canvas_id", "course_id" )) %>% unique()


#Make a new variables for video lecture title (here they use weblectures from wiki_pages, we will use the url)

stus_discs_files_quiz <- stus_discs_files_quiz %>% 
  mutate(video_lecture_title= case_when(grepl("association&", url) ~ "Association_1",  
                                        grepl("object-class", url) ~ "Object, Class, Attribute_1",
                                        grepl("video-exercise-1-dot-6", url) ~ "Exercise 1.6",
                                        grepl("video-exercise-1-dot-7", url) ~ "Exercise 1.7",
                                        grepl("association-class", url) ~ "Association Class_2",
                                        grepl("inheritance", url) ~ "Inheritance_2",
                                        grepl("video-exercise-2-dot-4", url) ~ "Exercise 2.4",
                                        grepl("video-exercise-2-dot-5", url) ~ "Exercise 2.5",
                                        grepl("modeling-levels-of-information-systems", url) ~ "Modeling levels",
                                        grepl("transforming-domain-models-into-design-models", url) ~ "Transforming domain models_3",
                                        grepl("video-exercise-3-dot-1", url) ~ "Exercise 3.1",
                                        grepl("video-exercise-41", url) ~ "Exercise 4.1",
                                        grepl("video-exercise-46", url) ~ "Exercise 4.6",
                                        grepl("video-exercise-51", url) ~ "Exercise 5.1",
                                        grepl("video-exercise-55", url) ~ "Exercise 5.5",
                                        grepl("video-exercise-61", url) ~ "Exercise 6.1",
                                        grepl("video-exercise-71", url) ~ "Exercise 7.1",
                                        grepl("video-exercise-75", url) ~ "Exercise 7.5",
                                        grepl("into-implementation-models-part-1", url) ~ "Transforming into Implementation Models (part 1)_4",
                                        grepl("into-implementation-models-part-2", url) ~ "Transforming into Implementation Models (part 2)_4"))
                                        
                                        
stus_discs_files_quiz$video_lecture_title<- as.factor(stus_discs_files_quiz$video_lecture_title)
                                  

#ENACTMENT

#These indicators look at total number of clicks on the materials (the 4 types), and average clicks per session on materials, uses clicks from DURING the course duration
enactment_test<- stus_discs_files_quiz %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  group_by(course_id, user_id, session_no) %>%
  summarize(n_lect= sum(is.na(video_lecture_title)),  #number of clicks on conferences per student 
            n_slides= sum(grepl('Lectures', canvas_material_type)), #clicks on lecture slides per student
            n_other= sum(grepl('Other_mandatory', canvas_material_type)), #clicks on other mandatory material per student
            n_opt= sum(grepl('Optional_materials', canvas_material_type)), #clicks on optional material per student
            lect_ses= sum(video_lecture_title)/sum(session_start, na.rm = TRUE),  #clicks on video lectures per session
            slides_ses= sum(grepl('Lectures', canvas_material_type)/sum(session_start, na.rm = TRUE)),  #clicks on lecture slides per session
            other_ses= sum(grepl('Other_mandatory', canvas_material_type)/sum(session_start, na.rm = TRUE)),  #clicks on other mandatory material per session
            opt_ses= sum(grepl('Optional_materials', canvas_material_type)/sum(session_start, na.rm = TRUE)), #clicks on optional material per session
            n_opt_Q= sum(quiz_max_attempts=="-1", na.rm=T)) %>% #clicks on optional quizzes
  ungroup(session_no) %>%
  summarise(
    E_n_lect= sum(n_lect, na.rm= TRUE),
    E_m_lect= mean(n_lect, na.rm= TRUE),
    E_sd_lect= sd(n_lect, na.rm= TRUE),
    E_n_slides= sum(n_slides, na.rm= TRUE),
    E_m_slides= mean(n_slides, na.rm= TRUE),
    E_sd_slides= sd(n_slides, na.rm= TRUE),
    E_n_other= sum(n_other, na.rm= TRUE),
    E_m_other= mean(n_other, na.rm= TRUE),
    E_sd_other= sd(n_other, na.rm= TRUE),
    E_n_opt= sum(n_opt, na.rm= TRUE),
    E_m_opt= mean(n_opt, na.rm= TRUE),
    E_sd_opt= sd(n_opt, na.rm= TRUE),
    E_n_Q_opt= sum(n_opt_Q, na.rm= TRUE),
    E_m_Q_opt= mean(n_opt_Q, na.rm= TRUE),
    E_sd_Q_opt= sd(n_opt_Q, na.rm= TRUE),
  ) %>%
  ungroup()

stus_discs_files_quiz$timestamp<- as.POSIXlt(stus_discs_files_quiz$timestamp, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")

#INDICATORS FOR THE AVERAGE TIME OF STUDENTS OF ACCESING THE SAME MATERIAL (in hours) (I tried with at least one hour for the same material but it would remove too much data)

access_time_same_video_lectures <- stus_discs_files_quiz %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  filter(!is.na(video_lecture_title)) %>% 
  group_by(course_id, user_id, video_lecture_title) %>%
  mutate(access_same_video_lecture= if_else(video_lecture_title==lag(video_lecture_title), as.numeric(timestamp-lag(timestamp)), 0),
         access_same_video_lect_h= case_when(!is.na(access_same_video_lecture) ~ as.numeric(access_same_video_lecture)/3600)) %>%
  ungroup(video_lecture_title) %>% 
  summarize(m_same_lect= mean(access_same_video_lect_h, na.rm=TRUE),
            n_same_lect= sum(access_same_video_lect_h, na.rm=TRUE),
            sd_same_lect= sd(access_same_video_lect_h, na.rm=TRUE),
            no_same_lect=sum(!is.na(access_same_video_lect_h))) %>%
  ungroup()


access_time_same_lecture_slides<- stus_discs_files_quiz %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  filter(!is.na(Lectures)) %>% 
  group_by(course_id, user_id, Lectures) %>% 
  mutate(access_same_slides= if_else(Lectures==lag(Lectures), as.numeric(timestamp-lag(timestamp)), 0),
         access_same_lecture_slides_hour= case_when(!is.na(access_same_slides) ~ access_same_slides/3600)) %>% 
  ungroup(Lectures) %>% 
  summarize(m_same_slides= mean(access_same_lecture_slides_hour, na.rm=TRUE),
            n_same_slides= sum(access_same_lecture_slides_hour, na.rm=TRUE),
            sd_same_slides= sd(access_same_lecture_slides_hour, na.rm=TRUE),
            no_same_slides=sum(!is.na(access_same_lecture_slides_hour))) %>%
  ungroup()


access_time_same_other_mandatory_material<- stus_discs_files_quiz %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  filter(!is.na(Other_mandatory)) %>% 
  group_by(course_id, user_id, Other_mandatory) %>% 
  mutate(access_same_other_mandatory= if_else(Other_mandatory==lag(Other_mandatory), as.numeric(timestamp-lag(timestamp)), 0),
         access_same_other_mandatory_hour= case_when(!is.na(access_same_other_mandatory) ~ access_same_other_mandatory/3600)) %>% 
  ungroup(Other_mandatory) %>% 
  summarize(m_same_other= mean(access_same_other_mandatory_hour, na.rm=TRUE),
            n_same_other= sum(access_same_other_mandatory_hour, na.rm=TRUE),
            sd_same_other= sd(access_same_other_mandatory_hour, na.rm=TRUE),
            no_same_other=sum(!is.na(access_same_other_mandatory_hour))) %>%
  ungroup()


access_time_same_optional_material<- stus_discs_files_quiz %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  filter(!is.na(Optional_materials)) %>% 
  group_by(course_id, user_id, Optional_materials) %>% 
  mutate(access_same_optional= if_else(Optional_materials==lag(Optional_materials), as.numeric(timestamp-lag(timestamp)), 0),
         access_same_optional_hour= case_when(!is.na(access_same_optional) ~ access_same_optional/3600)) %>% 
  ungroup(Optional_materials) %>% 
  summarize(m_same_opt= mean(access_same_optional_hour, na.rm=TRUE),
            n_same_opt= sum(access_same_optional_hour, na.rm=TRUE),
            sd_same_opt= sd(access_same_optional_hour, na.rm=TRUE),
            no_same_opt= sum(!is.na(access_same_optional_hour))) %>%
  ungroup()


enactment2<- stus_discs_files_quiz %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  group_by(course_id, user_id) %>% 
  summarize(perc_lect= length(unique(video_lecture_title))/(max(length(unique(stus_discs_files_quiz$video_lecture_title)))-1), #percentage of accessed video lectures
            perc_slides= length(unique(Lectures))/(max(length(unique(stus_discs_files_quiz$Lectures)))- 1),   #percentage of accessed lecture slides
            perc_other= length(unique(Other_mandatory))/(max(length(unique(stus_discs_files_quiz$Other_mandatory)))- 1),  #percentage of accessed other mandatory material
            perc_opt= length(unique(Optional_materials))/(max(length(unique(stus_discs_files_quiz$Optional_materials)))- 1),   #percentage of accessed optional material
            max_inactivity= as.numeric(max(timestamp -lag(timestamp),na.rm= TRUE))) %>%
  ungroup()

enactment_downloaded_files<- stus_discs_files_quiz %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  filter(grepl('download', url)) %>% 
  group_by(course_id, user_id) %>% 
  summarise(dl_slides= length(unique(Lectures))/(max(length(unique(stus_discs_files_quiz$Lectures)))- 1),   #percentage of downloaded lecture slides
            dl_other= length(unique(Other_mandatory))/(max(length(unique(stus_discs_files_quiz$Other_mandatory)))- 1),  #percentage of downloaded other mandatory materials
            dl_opt= length(unique(Optional_materials))/(max(length(unique(stus_discs_files_quiz$Optional_materials)))- 1)) %>% #percentage of downloaded optional materials
  ungroup()


#INDICATORS FOR THE AVERAGE TIME IT TAKES STUDENTS TO ACCESS DIFFERENT MATERIALS (in hours) . Here we put a threshold of at least 1 day. This can be of two types: 

#From the same category, giving 4 more indicators

access_time_diff_video_lectures<- stus_discs_files_quiz %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  filter(!is.na(video_lecture_title)) %>% 
  group_by(course_id, user_id) %>% 
  mutate(access_diff_video_lecture= if_else(video_lecture_title!=lag(video_lecture_title), 
                                            as.numeric(timestamp-lag(timestamp)), 0),
         access_diff_video_lecture_hour= case_when(!is.na(access_diff_video_lecture) ~ access_diff_video_lecture/3600)) %>% 
  summarize(m_diff_lect= mean(access_diff_video_lecture_hour, na.rm=TRUE),
            n_diff_lect= sum(access_diff_video_lecture_hour, na.rm=TRUE),
            sd_diff_lect= sd(access_diff_video_lecture_hour, na.rm=TRUE),
            no_diff_lect= sum(!is.na(access_diff_video_lecture_hour))) %>%
  ungroup()


access_time_diff_lecture_slides<- stus_discs_files_quiz %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  filter(!is.na(Lectures)) %>% 
  group_by(course_id, user_id) %>% 
  mutate(access_diff_slides= if_else(Lectures!=lag(Lectures), 
                                     as.numeric(timestamp-lag(timestamp)), 0),
         access_diff_slides_hour= case_when(!is.na(access_diff_slides) ~ access_diff_slides/3600)) %>% 
  summarize(m_diff_slides= mean(access_diff_slides_hour, na.rm=TRUE),
            n_diff_slides= sum(access_diff_slides_hour, na.rm=TRUE),
            sd_diff_slides= sd(access_diff_slides_hour, na.rm=TRUE),
            no_diff_slides= sum(!is.na(access_diff_slides_hour))) %>%
  ungroup()


access_time_diff_other_mandatory_material<- stus_discs_files_quiz %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  filter(!is.na(Other_mandatory)) %>% 
  group_by(course_id, user_id) %>% 
  mutate(access_diff_other_mandatory= if_else(Other_mandatory!=lag(Other_mandatory), as.numeric(timestamp-lag(timestamp)), 0),
         access_diff_other_mandatory_hour= case_when(!is.na(access_diff_other_mandatory) ~ access_diff_other_mandatory/3600)) %>% 
  summarize(m_diff_other= mean(access_diff_other_mandatory_hour, na.rm=TRUE),
            n_diff_other= sum(access_diff_other_mandatory_hour, na.rm=TRUE),
            sd_diff_other= sd(access_diff_other_mandatory_hour, na.rm=TRUE),
            no_diff_other= sum(!is.na(access_diff_other_mandatory_hour))) %>%
  ungroup()


access_time_diff_optional_material<- stus_discs_files_quiz %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  filter(!is.na(Optional_materials)) %>% 
  group_by(course_id, user_id) %>% 
  mutate(access_diff_optional= if_else(Optional_materials!=lag(Optional_materials), as.numeric(timestamp-lag(timestamp)), 0),
         access_diff_optional_hour= case_when(!is.na(access_diff_optional) ~ access_diff_optional/3600)) %>% 
  summarize(m_diff_opt= mean(access_diff_optional_hour, na.rm=TRUE),
            n_diff_opt= sum(access_diff_optional_hour, na.rm=TRUE),
            sd_diff_opt= sd(access_diff_optional_hour, na.rm=TRUE),
            no_diff_opt= sum(!is.na(access_diff_optional_hour))) %>%
  ungroup()

#One OVERALL indicator regarding all types of files

access_time_diff_files<- stus_discs_files_quiz %>% 
  filter(timestamp >= as.POSIXlt(start_of_course, format="%Y-%m-%d %H:%M:%S", tz="Europe/Amsterdam")  & 
           timestamp <= as.POSIXlt(end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")) %>% 
  filter(!is.na(canvas_material_type) | !is.na(video_lecture_title)) %>% 
  group_by(course_id, user_id) %>%
  mutate(access_diff_files= if_else(Optional_materials!=lag(Optional_materials) | Other_mandatory!=lag(Other_mandatory) | Lectures!=lag(Lectures) | video_lecture_title!=lag(video_lecture_title), as.numeric(timestamp-lag(timestamp)), 0),
         access_diff_files_hour= case_when(!is.na(access_diff_files) ~ access_diff_files/3600)) %>%  
  summarize(m_diff_all= mean(access_diff_files_hour, na.rm=TRUE), 
            n_diff_all= sum(access_diff_files_hour, na.rm=TRUE), 
            sd_diff_all= sd(access_diff_files_hour, na.rm=TRUE), 
            no_diff_all= sum(!is.na(access_diff_files_hour))) %>%
  ungroup()


#Connect all indicator tables per phase and altogether.
#Join all task definition indicators into one table
task_definition<- task_definition_main %>% 
  left_join(total_time_in_course) %>% 
  left_join(task_def_before) %>% 
  left_join(first_announcement_clicks)  %>% 
  left_join(time_on_assignments_withlag) %>% 
  left_join(task_definition_main_2) %>% 
  ungroup()


#Join all goal setting indicators into one table
goal_setting_all<- goal_setting %>% 
  left_join(goal_setting_before, by=c("course_id", "user_id")) %>% 
  left_join(GS_first_click, by=c("course_id", "user_id")) %>% ungroup()

#Join all adaptation indicators into one table
adaptation<- adaptation_main %>% 
  left_join(A_time_on_grades_page) %>% 
  left_join(A_average_assignment_clicks_after_deadline) %>% 
  left_join(A_average_assignment_time_after_deadline) %>% 
  left_join(avg_diff_between_sub_and_assig_deadline) %>% 
  left_join(clicks_after_course_end) %>% 
  left_join(A_time_on_quizzes) %>% 
  ungroup() 


enactment_same_materials<-  access_time_same_other_mandatory_material %>% 
  left_join(access_time_same_lecture_slides) %>% 
  left_join(access_time_same_video_lectures) %>% 
  left_join(access_time_same_optional_material) %>% 
  ungroup() 


enactment_diff_materials<- access_time_diff_lecture_slides %>% 
  left_join(access_time_diff_optional_material) %>% 
  left_join(access_time_diff_other_mandatory_material) %>% 
  left_join(access_time_diff_video_lectures) %>% 
  ungroup() 


enactment_same_diff_materials<- enactment_same_materials %>% 
  left_join(enactment_diff_materials) 

#Join all enactment indicators into one table
enactment_all<- enactment %>% 
  left_join(enactment_2) %>% 
  left_join(enactment_diff_materials) %>% 
  left_join(enactment_same_materials) %>% 
  left_join(enactment_downloaded_files)

#Join ALL indicators into one table
all_indicators<- task_definition %>% 
  left_join(goal_setting_all) %>% 
  left_join(enactment_all) %>%
  left_join(adaptation) %>%
  ungroup()

write.csv(all_indicators, "Course_A_all_raw_indicators.csv")