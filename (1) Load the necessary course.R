library(dplyr)
library(stringr)
library(psych)
library(lubridate) #might cause problems at some point
options(scipen = 999)

#1. Save all the dates and write the quartile/year of which the course is part of

#save all dates in a list
start_end_Q_dates <- list(
  Q3_20 = tibble(start_of_course = "2020-02-03 00:00:00", end_of_course = "2020-04-19 00:00:00"),
  Q4_20 = tibble(start_of_course = "2020-04-20 00:00:00", end_of_course = "2020-07-05 00:00:00"),
  Q4_21 = tibble(start_of_course = "2021-04-19 00:00:00", end_of_course = "2021-07-04 00:00:00"),
  Q2_22 = tibble(start_of_course = "2021-11-15 00:00:00", end_of_course = "2022-02-05 00:00:00")
)

#2.change all classes of dates to POSIXct
start_end_Q_dates <- lapply(start_end_Q_dates, function(x) {
  x$start_of_course <- as.POSIXct(x$start_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")
  x$end_of_course <- as.POSIXct(x$end_of_course, format="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")
  return(x)
})


##############3. CHANGE THIS ACCORDING TO THE COURSE QUARTILE
#Write the quartile of which the course is part of so you can use it from now on
quartile_year<- start_end_Q_dates$Q4_21 #(change here the quartile period)

start_of_course<- quartile_year$start_of_course
end_of_course<- quartile_year$end_of_course
 
#4. The course paths, depending on which course you are interested in (load based on the course you want)

#Q3 2019-2020 

#Course D
stu<- read.csv('Course_D.csv', colClasses=c("user_id"="character", "course_id"= "character")) %>% 
  select(-X, -type)
stu$discussion_id<- str_remove(stu$discussion_id, "754200000000") #remove first part of the discussion id so it matches with canvas id from the other disc tables
id_course<- #the ID of your course

#Course A
setwd("C:/Users/tudor/ownCloud/inter_data (2)/Tudor Data/Q3 2019-2020/1BV00/")
stu<- read.csv('FBIS20_stu.csv', colClasses=c("user_id"="character", "course_id"= "character")) %>% 
  select(-X, -type)
stu$discussion_id<- str_remove(stu$discussion_id, "754200000000") #remove first part of the discussion id so it matches with canvas id from the other disc tables
id_course<- #the ID of your course


#Q4 2019-2020 

#Course C
setwd("C:/Users/tudor/ownCloud/inter_data (2)/Tudor Data/Q4 2019-2020/0HV60/")
stu<- read.csv('TD20_stu.csv', colClasses=c("user_id"="character", "course_id"= "character")) %>% 
  select(-X, -type) 
stu$discussion_id<- str_remove(stu$discussion_id, "754200000000") #remove first part of the discussion id so it matches with canvas id from the other disc tables
id_course<- #the ID of your course


#Q4 2020-2021

#Course S
setwd("C:/Users/tudor/ownCloud/inter_data (2)/Tudor Data/Q4 2020-2021/0HV60/")
stu<- read.csv('TD21_stu.csv', colClasses=c("user_id"="character", "course_id"= "character")) %>% 
  select(-X, -type) 
stu$discussion_id<- str_remove(stu$discussion_id, "75420000000") #remove first part of the discussion id so it matches with canvas id from the other disc tables
id_course<- #the ID of your course