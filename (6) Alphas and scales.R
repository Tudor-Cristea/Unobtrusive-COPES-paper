library(dplyr)
library(psych)
library(REdaS)
#library(corrplot)

#library(VIM)
options(scipen = 100, digits = 4) #necessary to see the whole ID of students/course, but also for some indicators

#Thus, from now on, we will continue by removing all types of the same variable one by one (removing all frequencies, means, or SDs at the same time)
#For certain indicators, throughout the analysis, I included both the freq and the time spent by students on that page. For example for TD, I have both clicks on assignments, and time spent on the assignments page. At the same time, I also have these per sessions, and the SDs (consistency)

#Task definition

#Course_A: .8 with all (16 indicators)
psych::alpha(select(Course_A_TDs, -user_id, -course_id), check.keys=TRUE) #smoothing and sign (1)
#Only frequency indicators with means when there is no freq alternative (time for assignment)
psych::alpha(select(Course_A_TDs, -user_id, -course_id, -starts_with("TD_m"), "TD_m_1st_ann_after_post", -starts_with("TD_sd")), check.keys=TRUE) #5 indicators, .62
#Frequency indicators with means when there is no freq alternative (freq for assignment)
psych::alpha(select(Course_A_TDs, -user_id, -course_id, -starts_with("TD_m"), "TD_m_1st_ann_after_post", -starts_with("TD_sd"), "TD_m_assig", -TD_time_on_assig), check.keys=TRUE) #5 indicators, .59, sign (1)
#keep only means (time for assign)
psych::alpha(select(Course_A_TDs, -user_id, -course_id, -starts_with("TD_n"), -starts_with("TD_sd"),-"TD_time_bef", -"TD_time_on_assig", -TD_m_assig, -TD_sd_assig), check.keys=TRUE) #5 indicators, .39 smoothing/sign (1)
#keep only means (freq for assign)
psych::alpha(select(Course_A_TDs, -user_id, -course_id, -starts_with("TD_n"), -starts_with("TD_sd"),-"TD_time_bef", -"TD_time_on_assig", -TD_m_time_on_assig, -TD_sd_time_on_assig), check.keys=TRUE) #5 indicators, .48 smoothing/sign (2)
#Keep only SDs (time for assign)
psych::alpha(select(Course_A_TDs, -user_id, -course_id, -starts_with("TD_m"),-starts_with("TD_n"), -TD_sd_assig, -TD_time_bef, -TD_time_on_assig), check.keys=TRUE) #5 indicators, .47
#Keep only SDs (freq for assign)
psych::alpha(select(Course_A_TDs, -user_id, -course_id, -starts_with("TD_m"),-starts_with("TD_n"), -TD_sd_time_on_assig, -TD_time_bef, -TD_time_on_assig), check.keys=TRUE) #5 indicators, .49

#Course_B: .8 with all (16 indicators)
psych::alpha(select(Course_B_TDs, -user_id, -course_id), check.keys=TRUE) #smoothing and sign (1)
#Only frequency indicators with means when there is no freq alternative (time for assignment)
psych::alpha(select(Course_B_TDs, -user_id, -course_id, -starts_with("TD_m"), "TD_m_1st_ann_after_post", -starts_with("TD_sd")), check.keys=TRUE) #5 indicators, .44
#Frequency indicators with means when there is no freq alternative (freq for assignment)
psych::alpha(select(Course_B_TDs, -user_id, -course_id, -starts_with("TD_m"), "TD_m_1st_ann_after_post", -starts_with("TD_sd"), "TD_m_assig", -TD_time_on_assig), check.keys=TRUE) #5 indicators, .39, sign (1)
#keep only means (time for assign)
psych::alpha(select(Course_B_TDs, -user_id, -course_id, -starts_with("TD_n"), -starts_with("TD_sd"),-"TD_time_bef", -"TD_time_on_assig", -TD_m_assig, -TD_sd_assig), check.keys=TRUE) #5 indicators, .18 smoothing/sign (1)
#keep only means (freq for assign)
psych::alpha(select(Course_B_TDs, -user_id, -course_id, -starts_with("TD_n"), -starts_with("TD_sd"),-"TD_time_bef", -"TD_time_on_assig", -TD_m_time_on_assig, -TD_sd_time_on_assig), check.keys=TRUE) #5 indicators, .39 smoothing/sign (1)
#Keep only SDs (time for assign)
psych::alpha(select(Course_B_TDs, -user_id, -course_id, -starts_with("TD_m"),-starts_with("TD_n"), -TD_sd_assig, -TD_time_bef, -TD_time_on_assig), check.keys=TRUE) #5 indicators, .19
#Keep only SDs (freq for assign)
psych::alpha(select(Course_B_TDs, -user_id, -course_id, -starts_with("TD_m"),-starts_with("TD_n"), -TD_sd_time_on_assig, -TD_time_bef, -TD_time_on_assig), check.keys=TRUE) #5 indicators, .41


#looking at correlations, it seems that they do  correlate, thus we cannot take the alpha with both (time and clicks). last one is recommended again 
cor.test(Course_A_TDs$TD_time_on_assig, Course_A_TDs$TD_m_assig)
cor.test(Course_B_TDs$TD_time_on_assig, Course_B_TDs$TD_m_assig)


#1. remove all Ms (freq for assign)
psych::alpha(select(Course_A_TDs, -user_id, -course_id, -starts_with("TD_m"), -TD_sd_time_on_assig), check.keys=TRUE) #9 indicators, .74
psych::alpha(select(Course_B_TDs, -user_id, -course_id, -starts_with("TD_m"), -TD_sd_time_on_assig), check.keys=TRUE) #9 indicators, .61

#2. remove all Ms (consider time as M, thus, removing all times, freq for assign)
psych::alpha(select(Course_A_TDs, -user_id, -course_id, -starts_with("TD_m"), -starts_with("TD_time"), -TD_sd_time_on_assig), check.keys=TRUE) #7 indicators, .69
psych::alpha(select(Course_B_TDs, -user_id, -course_id, -starts_with("TD_m"), -starts_with("TD_time"), -TD_sd_time_on_assig), check.keys=TRUE) #7 indicators, .6

#3. remove all Ms(keep both)
psych::alpha(select(Course_A_TDs, -user_id, -course_id, -starts_with("TD_m")), check.keys=TRUE) #10 indicators, .61
psych::alpha(select(Course_B_TDs, -user_id, -course_id, -starts_with("TD_m")), check.keys=TRUE) #10 indicators, .63

#Final: removed Ms and others
psych::alpha(select(Course_A_TDs, -user_id, -course_id, -starts_with("TD_m"), -TD_sd_1st_ann_after_post), check.keys=TRUE) #9 indicators, .63
psych::alpha(select(Course_B_TDs, -user_id, -course_id, -starts_with("TD_m"), -TD_sd_1st_ann_after_post), check.keys=TRUE) #9 indicators, .66
psych::alpha(select(Course_D_TDs, -user_id, -course_id, -starts_with("TD_m"), -TD_sd_1st_ann_after_post), check.keys=TRUE) #9 indicators, .57
psych::alpha(select(Course_S_TDs, -user_id, -course_id, -starts_with("TD_m"), -TD_sd_1st_ann_after_post), check.keys=TRUE) #9 indicators, .67



#Goal setting
#Course_A: .79, 10 variables
psych::alpha(select(Course_A_GSs, -user_id, -course_id), check.keys=TRUE) 
#Keep only freq
psych::alpha(select(Course_A_GSs, -user_id, -course_id, -starts_with("GS_m"), -starts_with("GS_sd")), check.keys=TRUE) #4 indicators, .41
#Keep only aggregates (mean + SDs)
psych::alpha(select(Course_A_GSs, -user_id, -course_id, -starts_with("GS_n"), -GS_first_click), check.keys=TRUE) #6 indicators, .75
#Remove only Ms
psych::alpha(select(Course_A_GSs, -user_id, -course_id, -starts_with("GS_m")), check.keys=TRUE) #7 indicators, .67
#Remove only SDs
psych::alpha(select(Course_A_GSs, -user_id, -course_id, -starts_with("GS_sd")), check.keys=TRUE) #7 indicators, .65

#Course_B: .75, 10 variables, sign(1)
psych::alpha(select(Course_B_GSs, -user_id, -course_id), check.keys=TRUE) 
#Keep only freq
psych::alpha(select(Course_B_GSs, -user_id, -course_id, -starts_with("GS_m"), -starts_with("GS_sd")), check.keys=TRUE) #4 indicators, .32
#Keep only aggregates (mean + SDs)
psych::alpha(select(Course_B_GSs, -user_id, -course_id, -starts_with("GS_n"), -GS_first_click), check.keys=TRUE) #6 indicators, .73
#Remove only Ms
psych::alpha(select(Course_B_GSs, -user_id, -course_id, -starts_with("GS_m")), check.keys=TRUE) #7 indicators, .63
#Remove only SDs
psych::alpha(select(Course_B_GSs, -user_id, -course_id, -starts_with("GS_sd")), check.keys=TRUE) #7 indicators, .58, sign (1)

#Final: Keep only aggregates (mean + SDs)
#here an important assumption is that they represent different student behaviour (relates back to what we started with). Could also be part of the questions/discussion
psych::alpha(select(Course_A_GSs, -user_id, -course_id, -starts_with("GS_n"), -GS_first_click), check.keys=TRUE) #6 indicators, .75
psych::alpha(select(Course_B_GSs, -user_id, -course_id, -starts_with("GS_n"), -GS_first_click), check.keys=TRUE) #6 indicators, .73


#Adaptation
#clicks and time spent on the assignments after deadline
cor.test(Course_A_As$A_time_assig_after_deadl, Course_A_As$A_n_assig_after_deadl)

#Course_A: .84, 18 variables, sign (1)
psych::alpha(select(Course_A_As, -user_id, -course_id), check.keys=TRUE) 
#remove means and SDs (freq)
psych::alpha(select(Course_A_As, -user_id, -course_id, -starts_with("A_m"), -starts_with("A_sd"), -"A_time_grades", -"A_time_assig_after_deadl"), check.keys=TRUE) #7 indicators, .72
#remove means and SDs (time)
psych::alpha(select(Course_A_As, -user_id, -course_id, -starts_with("A_m"), -starts_with("A_sd"), -"A_n_grade_access", -"A_n_assig_after_deadl"), check.keys=TRUE) #7 indicators, .56

#Keep only aggregates (mean + SDs), (freq)
psych::alpha(select(Course_A_As, -user_id, -course_id, -starts_with("A_n"), -starts_with("A_time_"), -"A_diff_subs_post_assig", -"A_after_course", -"A_time_quiz", -A_m_time_assig_after_deadl, -A_sd_time_assig_after_deadl), check.keys=TRUE) #11 indicators, .67, sign (1)
#Keep only aggregates (mean + SDs), (time)
psych::alpha(select(Course_A_As, -user_id, -course_id, -starts_with("A_n"), -"A_time_grades", -"A_time_assig_after_deadl", -"A_diff_subs_post_assig", -"A_after_course", -"A_time_quiz", -A_m_assig_after_deadl, -A_sd_assig_after_deadl), check.keys=TRUE) #11 indicators, .65

#Remove only Ms (freq)
psych::alpha(select(Course_A_As, -user_id, -course_id, -starts_with("A_m"), -"A_time_grades", -"A_time_assig_after_deadl", -A_sd_time_assig_after_deadl), check.keys=TRUE) #12 indicators, .8
#Remove only Ms (time)
psych::alpha(select(Course_A_As, -user_id, -course_id, -starts_with("A_m"), -"A_n_grade_access", -"A_n_assig_after_deadl", -A_sd_assig_after_deadl), check.keys=TRUE) #12 indicators, .72



#Course_B: .84, 21 variables
psych::alpha(select(Course_B_As, -user_id, -course_id), check.keys=TRUE) 
#remove means and SDs (freq)
psych::alpha(select(Course_B_As, -user_id, -course_id, -starts_with("A_m"), -starts_with("A_sd"), -"A_time_grades", -"A_time_assig_after_deadl"), check.keys=TRUE) #7 indicators, .77
#remove means and SDs (time)
psych::alpha(select(Course_B_As, -user_id, -course_id, -starts_with("A_m"), -starts_with("A_sd"), -"A_n_grade_access", -"A_n_assig_after_deadl"), check.keys=TRUE) #7 indicators, .64

#Keep only aggregates (mean + SDs), (freq)
psych::alpha(select(Course_B_As, -user_id, -course_id, -starts_with("A_n"), -starts_with("A_time_"), -"A_diff_subs_post_assig", -"A_after_course", -"A_time_quiz", -A_m_time_assig_after_deadl, -A_sd_time_assig_after_deadl), check.keys=TRUE) #11 indicators, .71
#Keep only aggregates (mean + SDs), (time)
psych::alpha(select(Course_B_As, -user_id, -course_id, -starts_with("A_n"), -"A_time_grades", -"A_time_assig_after_deadl", -"A_diff_subs_post_assig", -"A_after_course", -"A_time_quiz", -A_m_assig_after_deadl, -A_sd_assig_after_deadl), check.keys=TRUE) #11 indicators, .65, sign (2)

#Remove only Ms (freq)
psych::alpha(select(Course_B_As, -user_id, -course_id, -starts_with("A_m"), -"A_time_grades", -"A_time_assig_after_deadl", -A_sd_time_assig_after_deadl), check.keys=TRUE) #12 indicators, .8
#Remove only Ms (time)
psych::alpha(select(Course_B_As, -user_id, -course_id, -starts_with("A_m"), -"A_n_grade_access", -"A_n_assig_after_deadl", -A_sd_assig_after_deadl), check.keys=TRUE) #12 indicators, .73


#Final: Remove means and SDs (freq)
psych::alpha(select(Course_A_As, -user_id, -course_id, -starts_with("A_m"), -starts_with("A_sd"), -"A_time_grades", -"A_time_assig_after_deadl"), check.keys=TRUE) #7 indicators, .72
psych::alpha(select(Course_B_As, -user_id, -course_id, -starts_with("A_m"), -starts_with("A_sd"), -"A_time_grades", -"A_time_assig_after_deadl"), check.keys=TRUE) #7 indicators, .77


#Enactment
#This phase has the most indicators, thus there are a lot of ways to go about it

#Course_A
#Keep only n with diff
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("n_same"), -starts_with("perc"), -starts_with("LS_m"), -starts_with("LS_sd"), -starts_with("m_"), -starts_with("sd_"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .9
#Keep only n with same
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("n_diff"), -starts_with("perc"), -starts_with("LS_m"), -starts_with("LS_sd"), -starts_with("m_"), -starts_with("sd_"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .9
#Keep only n with the 2 extra (n_Q and max_inactivity)
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("n_same"), -starts_with("n_diff"), -starts_with("perc"), -starts_with("LS_m"), -starts_with("LS_sd"), -starts_with("m_"), -starts_with("sd_"), -starts_with("no_")), check.keys=TRUE) #9 indicators, .86
#Keep only n without any (basically just downloaded materials and clicks on files)
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("n_same"), -starts_with("n_diff"), -starts_with("perc"), -starts_with("LS_m"), -starts_with("LS_sd"), -starts_with("m_"), -starts_with("sd_"), -starts_with("no_"), -max_inactivity, -LS_n_Q_opt), check.keys=TRUE) #7 indicators, .88


#Keep only m with same
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("m_diff"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_sd"), -starts_with("n_"), -starts_with("sd_"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .75
#Keep only m with diff
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("m_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_sd"), -starts_with("n_"), -starts_with("sd_"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .64, sign (2)
#Keep only m without any (basically just downloaded materials and m), the 2 extra (m_Q and max_inactivity)
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("m_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_sd"), -starts_with("n_"), -starts_with("sd_"), -starts_with("no_"), -starts_with("m_diff")), check.keys=TRUE) #9 indicators, .6
#Keep only m without any (basically just downloaded materials and m)
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("m_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_sd"), -starts_with("n_"), -starts_with("sd_"), -starts_with("no_"), -starts_with("m_diff"), -max_inactivity, -LS_m_Q_opt), check.keys=TRUE) #7 indicators, .69

#Keep only m and sd without any, the 2 extra (m_Q and max_inactivity)
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("sd_same"), -starts_with("sd_diff"), -starts_with("m_same"), -starts_with("m_diff"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("n_"), -starts_with("no_"), -starts_with("dl_")), check.keys=TRUE) #11 indicators, .8, sign (1)
#Keep only m and sd without any, 
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("sd_same"), -starts_with("sd_diff"), -starts_with("m_same"), -starts_with("m_diff"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("n_"), -starts_with("no_"), -starts_with("dl_"), -max_inactivity, -LS_m_Q_opt, -LS_sd_Q_opt), check.keys=TRUE) #8 indicators, .83


#Keep only sd with same
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("sd_diff"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .78
#Keep only sd with diff
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("sd_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .71
#Keep only sd without any (just with dl)
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("sd_diff"), -starts_with("sd_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("no_")), check.keys=TRUE) #9 indicators, .69
#Keep only sds, with percentage and downloaded
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("sd_same"), -starts_with("sd_diff"), -starts_with("m_same"), -starts_with("m_diff"), -starts_with("LS_n"), -starts_with("n_"), -starts_with("LS_m"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .84
#Keep only sds, with downloaded
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("sd_same"), -starts_with("sd_diff"), -starts_with("m_same"), -starts_with("m_diff"), -starts_with("LS_n"), -starts_with("n_"), -starts_with("LS_m"), -starts_with("no_"), -starts_with("dl_"), -max_inactivity, -LS_sd_Q_opt), check.keys=TRUE) #8 indicators, .78

#Keep only no with same
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("sd_diff"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("sd_"), -starts_with("LS_sd_"), -starts_with("no_diff")), check.keys=TRUE) #8 indicators, .83
#Keep only no with diff
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("sd_diff"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("sd_"), -starts_with("LS_sd_"), -starts_with("no_same")), check.keys=TRUE) #8 indicators, .85
#Keep only no without any, the 2 extra (n_Q and max_inactivity)
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("sd_diff"), -starts_with("sd_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("LS_sd_"), -starts_with("dl_"), LS_n_Q_opt), check.keys=TRUE) #10 indicators, .88
#Keep only no without any
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("sd_diff"), -starts_with("sd_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("LS_sd_"), -starts_with("dl_"), -max_inactivity), check.keys=TRUE) #8 indicators, .9


#Course_B
#Keep only n with diff
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("n_same"), -starts_with("perc"), -starts_with("LS_m"), -starts_with("LS_sd"), -starts_with("m_"), -starts_with("sd_"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .85
#Keep only n with same
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("n_diff"), -starts_with("perc"), -starts_with("LS_m"), -starts_with("LS_sd"), -starts_with("m_"), -starts_with("sd_"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .79
#Keep only n with the 2 extra (n_Q and max_inactivity)
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("n_same"), -starts_with("n_diff"), -starts_with("perc"), -starts_with("LS_m"), -starts_with("LS_sd"), -starts_with("m_"), -starts_with("sd_"), -starts_with("no_")), check.keys=TRUE) #9 indicators, .82
#Keep only n without any (basically just downloaded materials and clicks on files)
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("n_same"), -starts_with("n_diff"), -starts_with("perc"), -starts_with("LS_m"), -starts_with("LS_sd"), -starts_with("m_"), -starts_with("sd_"), -starts_with("no_"), -max_inactivity, -LS_n_Q_opt), check.keys=TRUE) #7 indicators, .79


#Keep only m with same
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("m_diff"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_sd"), -starts_with("n_"), -starts_with("sd_"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .61
#Keep only m with diff
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("m_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_sd"), -starts_with("n_"), -starts_with("sd_"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .63
#Keep only m without any (basically just downloaded materials and m), the 2 extra (m_Q and max_inactivity)
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("m_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_sd"), -starts_with("n_"), -starts_with("sd_"), -starts_with("no_"), -starts_with("m_diff")), check.keys=TRUE) #9 indicators, .65
#Keep only m without any (basically just downloaded materials and m)
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("m_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_sd"), -starts_with("n_"), -starts_with("sd_"), -starts_with("no_"), -starts_with("m_diff"), -max_inactivity, -LS_m_Q_opt), check.keys=TRUE) #7 indicators, .62

#Keep only m and sd without any, the 2 extra (m_Q and max_inactivity)
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("sd_same"), -starts_with("sd_diff"), -starts_with("m_same"), -starts_with("m_diff"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("n_"), -starts_with("no_"), -starts_with("dl_")), check.keys=TRUE) #11 indicators, .74
#Keep only m and sd without any, 
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("sd_same"), -starts_with("sd_diff"), -starts_with("m_same"), -starts_with("m_diff"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("n_"), -starts_with("no_"), -starts_with("dl_"), -max_inactivity, -LS_m_Q_opt, -LS_sd_Q_opt), check.keys=TRUE) #8 indicators, .76


#Keep only sd with same
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("sd_diff"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .64
#Keep only sd with diff
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("sd_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .66
#Keep only sd without any (just with dl)
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("sd_diff"), -starts_with("sd_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("no_")), check.keys=TRUE) #9 indicators, .66
#Keep only sds, with percentage and downloaded
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("sd_same"), -starts_with("sd_diff"), -starts_with("m_same"), -starts_with("m_diff"), -starts_with("LS_n"), -starts_with("n_"), -starts_with("LS_m"), -starts_with("no_")), check.keys=TRUE) #13 indicators, .82
#Keep only sds, with downloaded
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("sd_same"), -starts_with("sd_diff"), -starts_with("m_same"), -starts_with("m_diff"), -starts_with("LS_n"), -starts_with("n_"), -starts_with("LS_m"), -starts_with("no_"), -starts_with("dl_"), -max_inactivity, -LS_sd_Q_opt), check.keys=TRUE) #8 indicators, .75

#Keep only no with same
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("sd_diff"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("sd_"), -starts_with("LS_sd_"), -starts_with("no_diff")), check.keys=TRUE) #8 indicators, .74
#Keep only no with diff
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("sd_diff"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("sd_"), -starts_with("LS_sd_"), -starts_with("no_same")), check.keys=TRUE) #8 indicators, .81
#Keep only no without any, the 2 extra (n_Q and max_inactivity)
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("sd_diff"), -starts_with("sd_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("LS_sd_"), -starts_with("dl_"), LS_n_Q_opt), check.keys=TRUE) #10 indicators, .85
#Keep only no without any
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("sd_diff"), -starts_with("sd_same"), -starts_with("perc"), -starts_with("LS_n"), -starts_with("LS_m"), -starts_with("n_"), -starts_with("m_"), -starts_with("LS_sd_"), -starts_with("dl_"), -max_inactivity), check.keys=TRUE) #8 indicators, .85


#NOW WE BACK THROUGH ALL OF THEM AND APPLY THEM TO THE NEW COURSES

#TASK DEFINITION

#Remove all Ms(keep both)
psych::alpha(select(Course_A_TDs, -user_id, -course_id, -starts_with("TD_m"), -TD_sd_assig, -TD_sd_1st_ann_after_post, -TD_sd_ann), check.keys=TRUE) #7 indicators, .66
psych::alpha(select(Course_B_TDs, -user_id, -course_id, -starts_with("TD_m"), -TD_sd_assig, -TD_sd_1st_ann_after_post, -TD_sd_ann), check.keys=TRUE) #7 indicators, .65
psych::alpha(select(Course_D_TDs, -user_id, -course_id, -starts_with("TD_m"), -TD_sd_assig, -TD_sd_1st_ann_after_post, -TD_sd_ann), check.keys=TRUE) #5 indicators, .53
psych::alpha(select(Course_S_TDs, -user_id, -course_id, -starts_with("TD_m"), -TD_sd_assig, -TD_sd_1st_ann_after_post, -TD_sd_ann), check.keys=TRUE) #7 indicators, .63

TD_test<- Course_A_TDs %>% select(-user_id, -course_id, -starts_with("TD_m"), -TD_sd_assig, -TD_sd_1st_ann_after_post, -TD_sd_ann)

list_4_courses<- list(Course_A= Course_A, Course_B= Course_B, Course_D= Course_D, Course_S= Course_S)
name <- names(list_4_courses)

reliability_analysis_TD<- function(x) {
  TD<- x %>% 
    select(starts_with("TD_"), -starts_with("TD_m"), -TD_sd_assig, 
            -TD_sd_1st_ann_after_post, -TD_sd_ann)
  #Run some PCA to see if we can actually find only one dimension (for the whole phase)
  print(alpha(TD, check.keys=TRUE))
  print(bart_spher(TD)) #sphericity, this should be sign (it is)
  print(KMO(TD)) #Kaiser-Meyer-Olkin, should be above .7 (.5, it is not). However, I also read that some people use .5 as a threshold so I will continue
  TD_PCA<- princomp(TD, cor=TRUE) #principal components, this just used for details
  print(summary(TD_PCA))
  print(TD_PCA$loadings)
  
  plot(TD_PCA, type="l") #remove type for histogram
  abline(1,0, col='red', lty=2)
  biplot(TD_PCA)
  fa.parallel(TD, fm= "ml", fa="both", n.iter= 500) #fapara from stata, parallel analysis, compares both in this case (fa and pc) with 500 random simulated analyses
  
  TD_cors<- TD %>% 
    mutate(
      scale_score= select(.,everything()) %>% rowMeans(na.rm=T),
      PCA_score= TD_PCA$scores[,1]
      
    ) 
  
  print(cor.test(TD_cors$PCA_score, TD_cors$scale_score))
  plot(TD_cors$PCA_score, TD_cors$scale_score)
  
}

TD_output_list <- lapply(seq_along(list_4_courses), function(i) {
  print(paste("Processing dataset:", names(list_4_courses)[i]))
  reliability_analysis_TD(list_4_courses[[i]])
})


#GOAL SETTING
#Keep only aggregates (mean + SDs)
psych::alpha(select(Course_A_GSs, -user_id, -course_id, -starts_with("GS_n"), -GS_first_click), check.keys=TRUE) #6 indicators, .75
psych::alpha(select(Course_B_GSs, -user_id, -course_id, -starts_with("GS_n"), -GS_first_click), check.keys=TRUE) #6 indicators, .73
psych::alpha(select(Course_D_GSs, -user_id, -course_id, -starts_with("GS_n"), -GS_first_click), check.keys=TRUE) #4 indicators, .68 (2)
psych::alpha(select(Course_S_GSs, -user_id, -course_id, -starts_with("GS_n"), -GS_first_click), check.keys=TRUE) #6 indicators, .65, sign (2)

reliability_analysis_GS<- function(x) {
  GS<- x %>% 
    select(starts_with("GS_"), -starts_with("GS_n"), -GS_first_click) 
  #Run some PCA to see if we can actually find only one dimension (for the whole phase)
  print(alpha(GS, check.keys=TRUE))
  print(bart_spher(GS)) #sphericity, this should be sign (it is)
  print(KMO(GS)) #Kaiser-Meyer-Olkin, should be above .7 (.5, it is not). However, I also read that some people use .5 as a threshold so I will continue
  GS_PCA<- princomp(GS, cor=TRUE) #principal components, this just used for details
  print(summary(GS_PCA)) #proportion of variance, between 1 and 5
  print(GS_PCA$loadings)
  
  plot(GS_PCA, type="l") #remove type for hist
  abline(1,0, col='red', lty=2)
  biplot(GS_PCA)
  fa.parallel(GS, fm= "ml", fa="both", n.iter= 500) #fapara from stata, parallel analysis, compares both in this case (fa and pc) with 500 random simulated analyses
  
  GS_cors<- GS %>% 
    mutate(
      scale_score= select(.,everything()) %>% rowMeans(na.rm=T),
      PCA_score= GS_PCA$scores[,1]
      
    ) 
  
  print(cor.test(GS_cors$PCA_score, GS_cors$scale_score))
  plot(GS_cors$PCA_score, GS_cors$scale_score)
  
}

GS_output_list <- lapply(seq_along(list_4_courses), function(i) {
  print(paste("Processing dataset:", names(list_4_courses)[i]))
  reliability_analysis_GS(list_4_courses[[i]])
})


#ADAPTATION
#Remove means and SDs (freq)
psych::alpha(select(Course_A_As, -user_id, -course_id, -starts_with("A_m"), -starts_with("A_sd"), -"A_time_grades", -"A_time_assig_after_deadl", -A_diff_subs_post_assig, -A_sessions), check.keys=TRUE) #6 indicators, .68
psych::alpha(select(Course_B_As, -user_id, -course_id, -starts_with("A_m"), -starts_with("A_sd"), -"A_time_grades", -"A_time_assig_after_deadl", -A_diff_subs_post_assig, -A_sessions), check.keys=TRUE) #6 indicators, .78
psych::alpha(select(Course_D_As, -user_id, -course_id, -starts_with("A_m"), -starts_with("A_sd"), -"A_time_grades", -"A_time_assig_after_deadl", -A_diff_subs_post_assig, -A_sessions), check.keys=TRUE) #6 indicators, .69
psych::alpha(select(Course_S_As, -user_id, -course_id, -starts_with("A_m"), -starts_with("A_sd"), -"A_time_grades", -"A_time_assig_after_deadl", -A_diff_subs_post_assig, -A_sessions), check.keys=TRUE) #6 indicators, .87


reliability_analysis_A<- function(x) {
  A<- x %>% 
    select(starts_with("A_"), -starts_with("A_m"), -starts_with("A_sd"), 
           -A_time_grades, -A_time_assig_after_deadl, -A_diff_subs_post_assig, -A_sessions)
  #Run some PCA to see if we can actually find only one dimension (for the whole phase)
  print(alpha(A, check.keys=TRUE))
  print(bart_spher(A)) #sphericity, this should be sign (it is)
  print(KMO(A)) #Kaiser-Meyer-Olkin, should be above .7 (.5, it is not). However, I also read that some people use .5 as a threshold so I will continue
  A_PCA<- princomp(A, cor=TRUE) #principal components, this just used for details
  print(summary(A_PCA)) #proportion of variance, between 1 and 5
  print(A_PCA$loadings)
  
  plot(A_PCA, type="l") #remove type for hist
  abline(1,0, col='red', lty=2)
  biplot(A_PCA)
  fa.parallel(A, fm= "ml", fa="both", n.iter= 500) #fapara from stata, parallel analysis, compares both in this case (fa and pc) with 500 random simulated analyses
  
  A_cors<- A %>% 
    mutate(
      scale_score= select(.,everything()) %>% rowMeans(na.rm=T),
      PCA_score= A_PCA$scores[,1]
      
    ) 
  
  print(cor.test(A_cors$PCA_score, A_cors$scale_score))
  plot(A_cors$PCA_score, A_cors$scale_score)
}

A_output_list <- lapply(seq_along(list_4_courses), function(i) {
  print(paste("Processing dataset:", names(list_4_courses)[i]))
  reliability_analysis_A(list_4_courses[[i]])
})


#LEARNING TACTICS
#Keep only n without any (basically just downloaded materials and clicks on files)
psych::alpha(select(Course_A_E, -user_id, -course_id, -starts_with("n_same"), -starts_with("n_diff"), -starts_with("perc"), -starts_with("LS_m"), -starts_with("LS_sd"), -starts_with("m_"), -starts_with("sd_"), -starts_with("no_"), -max_inactivity, -LS_n_Q_opt), check.keys=TRUE) #7 indicators, .88
psych::alpha(select(Course_B_E, -user_id, -course_id, -starts_with("n_same"), -starts_with("n_diff"), -starts_with("perc"), -starts_with("LS_m"), -starts_with("LS_sd"), -starts_with("m_"), -starts_with("sd_"), -starts_with("no_"), -max_inactivity, -LS_n_Q_opt), check.keys=TRUE) #7 indicators, .79
psych::alpha(select(Course_D_E, -user_id, -course_id, -starts_with("n_same"), -starts_with("n_diff"), -starts_with("perc"), -starts_with("LS_m"), -starts_with("LS_sd"), -starts_with("m_"), -starts_with("sd_"), -starts_with("no_"), -max_inactivity), check.keys=TRUE) #6 indicators, .86
psych::alpha(select(Course_S_E, -user_id, -course_id, -starts_with("n_same"), -starts_with("n_diff"), -starts_with("perc"), -starts_with("LS_m"), -starts_with("LS_sd"), -starts_with("m_"), -starts_with("sd_"), -starts_with("no_"), -max_inactivity, -LS_n_Q_opt), check.keys=TRUE) #7 indicators, .81


reliability_analysis_LS<- function(x) {
  LS<- x %>% 
    select(starts_with("LS_n"), starts_with("dl_"), -starts_with("LS_n_Q"))
  #Run some PCA to see if we can actually find only one dimension (for the whole phase)
  print(alpha(LS, check.keys=TRUE))
  print(bart_spher(LS)) #sphericity, this should be sign (it is)
  print(KMO(LS)) #Kaiser-Meyer-Olkin, should be above .7 (.5, it is not). However, I also read that some people use .5 as a threshold so I will continue
  LS_PCA<- princomp(LS, cor=TRUE) #principal components, this just used for details
  print(summary(LS_PCA)) #proportion of variance, between 1 and 5
  print(LS_PCA$loadings)
  
  plot(LS_PCA, type="l") #remove type for hist
  abline(1,0, col='red', lty=2)
  biplot(LS_PCA)
  fa.parallel(LS, fm= "ml", fa="both", n.iter= 500) #fapara from stata, parallel analysis, compares both in this case (fa and pc) with 500 random simulated analyses
  
  LS_cors<- LS %>% 
    mutate(
      scale_score= select(.,everything()) %>% rowMeans(na.rm=T),
      PCA_score= LS_PCA$scores[,1]
      
    ) 
  
  print(cor.test(LS_cors$PCA_score, LS_cors$scale_score))
  plot(LS_cors$PCA_score, LS_cors$scale_score)
}

LS_output_list <- lapply(seq_along(list_4_courses), function(i) {
  print(paste("Processing dataset:", names(list_4_courses)[i]))
  reliability_analysis_LS(list_4_courses[[i]])
})


pcaResult<-princomp(data)
pc=LS_PCA$scores[,1]
print(pc)

#save all the final indicators resulted. Form one dataset with all the indicators for all courses