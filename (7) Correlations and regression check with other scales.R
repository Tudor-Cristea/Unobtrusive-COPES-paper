#Continuing to run analyses with the final datasets
library(car)
library(lavaan)
library(psych)
library(dplyr)
library(corrplot)

options(scipen = 999)

#1. Correlate the 4 dimensions with the rest of the scales, especially with final grades
#this include per course and the whole sample at the same time (4 courses)


COPES_survey_corrs<- function(x) {
  
  COPES_scales<- c("TD_online_scale", "GS_online_scale", "LS_online_scale", "A_online_scale")
  SRL_scales<- c("SRL", "timem", "resourcem", "SE", "GRADES")
  wellbeing_scales<- c("burnout", "amotivation", "studyengage")
  stateGO_scales<- c("stateprove", "stateavoidance", "statemastery")
  traitGO_scales<- c("traitprove", "traitmastery", "traitavoidance")
  
  print(corr.test(select(course, COPES_scales), use= "complete")$stars)
  print(corr.test(select(x, COPES_scales), use= "complete")$p)
  print(corr.test(select(x, COPES_scales), use= "complete")$n)
  
  
  print(psych::corr.test(select(x, COPES_scales, all_of(SRL_scales)), use= "complete")$stars)
  print(psych::corr.test(select(x, COPES_scales, all_of(SRL_scales)), use= "complete")$p)
  print(psych::corr.test(select(x, COPES_scales, all_of(SRL_scales)), use= "complete")$n)
  
  #corrplot.mixed(cor(select(x, COPES_scales, all_of(SRL_scales)), use= "complete"), order= "AOE")
  
  print(psych::corr.test(select(x, COPES_scales, all_of(wellbeing_scales)), use= "complete")$stars)
  print(psych::corr.test(select(x, COPES_scales, all_of(wellbeing_scales)), use= "complete")$n)
  #corrplot.mixed(cor(select(x, COPES_scales, all_of(wellbeing_scales)), use= "complete"), order= "AOE")
  
  
  print(psych::corr.test(select(x, COPES_scales, all_of(stateGO_scales)), use= "complete")$stars)
  #corrplot.mixed(cor(select(x, COPES_scales, all_of(stateGO_scales)), use= "complete"), order= "AOE")
  print(psych::corr.test(select(x, COPES_scales, all_of(stateGO_scales)), use= "complete")$n)
  
  
  print(psych::corr.test(select(x, COPES_scales, all_of(traitGO_scales)), use= "complete")$stars)
  #corrplot.mixed(cor(select(x, COPES_scales, all_of(traitGO_scales)), use= "complete"), order= "AOE")
  print(psych::corr.test(select(x, COPES_scales, all_of(traitGO_scales)), use= "complete")$n)
  
  
}

course<- all_4_courses_all_final
COPES_survey_corrs(course)

#2. Create an online scale (from all 4 phases), and correlate that with survey SRL
all_4_courses_all_final2<- all_4_courses_all_final %>%
  mutate(online_scale_score= (TD_online_scale+ GS_online_scale+ LS_online_scale+ A_online_scale)/4) 

cor.test(all_4_courses_all_final2$SRL, all_4_courses_all_final2$online_scale_score, na.rm=T)


#3. do the cors between GS (only 3 courses that work) and 
only_3_courses_all_final<- all_4_courses_all_final %>%
  filter(course_id!= "75420000000011861")

print(corr.test(select(only_3_courses_all_final, COPES_scales), use= "complete")$stars)
print(corr.test(select(only_3_courses_all_final, COPES_scales), use= "complete")$p)
print(corr.test(select(only_3_courses_all_final, COPES_scales), use= "complete")$n)

print(corr.test(select(only_3_courses_all_final, GS_online_scale, GRADES, SRL), use= "complete")$stars)
print(corr.test(select(only_3_courses_all_final, GS_online_scale, GRADES, SRL), use= "complete")$p)
print(corr.test(select(only_3_courses_all_final, GS_online_scale, GRADES, SRL), use= "complete")$n)

print(corr.test(select(only_3_courses_all_final, GS_online_scale, traitGO_scales), use= "complete")$stars)
print(corr.test(select(only_3_courses_all_final, GS_online_scale, wellbeing_scales), use= "complete")$p)
print(corr.test(select(only_3_courses_all_final, GS_online_scale, wellbeing_scales), use= "complete")$n)




#4. Regressions
#you have to choose the dependent variable by hand it seems
rm= lm(GRADES ~TD_online_scale + GS_online_scale + LS_online_scale + A_online_scale, course) #add factor(course_id) in order to create dummy varaibles per course
summary(rm)
vif(rm)




#checking VIF for multicoliniearity (the threshold is 5 (above is bad))
vif_values <- vif(rm)
barplot(vif_values, main = "VIF Values", horiz = TRUE)
abline(v = 5, lwd = 3, lty = 2)

#Run ANOVAS
#run anova on grades between courses. Can also do that for the other response variables, if necessary
one.way <- aov(final_grade ~ course_id, data = all_4_courses_all_final)
summary(one.way)
tukey<-TukeyHSD(one.way) #post-hoc in order to run pairwise comparisons, to see exactly where the difference lies
tukey
rm=lm(final_grade~course_id,all_4_courses_all_final)
summary(rm)


one.way_srl <- aov(SRL ~ course_id, data = all_4_courses_all_final) #I also checked the copes online scales, but there is no sign differences on none of the 4
summary(one.way_srl)
tukey<-TukeyHSD(one.way_srl) #post-hoc in order to run pairwise comparisons, to see exactly where the difference lies
tukey
rm=lm(SRL~course_id,all_3_courses)
summary(rm)


  
#SEM in the "Building COPES scales 3"
BBI_20_COPES_model <- ' TD  =~ TD_n_ann + TD_time_bef + TD_sd_time_bef + TD_time_on_assig + TD_sd_time_on_assig  
              GS =~ GS_m_rubric + GS_sd_rubric + GS_m_studyg + GS_sd_studyg 
              LS   =~ LS_n_slides + LS_n_other + LS_n_opt + dl_slides + dl_other + dl_opt 
              A =~ A_n_upd_subs + A_n_grade_access + A_n_assig_after_deadl + A_after_course + A_time_quiz'

COPES_model <- ' TD  =~ TD_n_ann + TD_n_disc + TD_time_bef + TD_sd_time_bef + TD_time_on_assig + TD_sd_time_on_assig + TD_sd_disc  
              GS =~ GS_m_schedule + GS_sd_schedule + GS_m_rubric + GS_sd_rubric + GS_m_studyg + GS_sd_studyg
              LS   =~ LS_n_slides + LS_n_other + LS_n_opt + dl_slides + dl_other + dl_opt + LS_n_lect
              A =~ A_n_upd_subs + A_n_grade_access + A_n_assig_after_deadl + A_after_course + A_time_quiz'



BBI_20_fit <- cfa(BBI_20_COPES_model, data = BBI20_indicators_grades_survey)
summary(BBI_20_fit, fit.measures = TRUE)

FBIS_20_fit <- cfa(COPES_model, data = FBIS20_indicators_grades_survey)
summary(FBIS_20_fit, fit.measures = TRUE)

TD_20_fit <- cfa(COPES_model, data = TD20_indicators_grades_survey)
summary(TD_20_fit, fit.measures = TRUE)

TD_21_fit <- cfa(COPES_model, data = TD21_indicators_grades_survey)
summary(TD_21_fit, fit.measures = TRUE)

#For a good model fit, CFI and TLI should be greater than 0.9, while RMSEA should be less than 0.08 and SRMR less than 0.08.

#look at the difference between correlations 

print(corr.test(select(all_4_courses_all_final, final_grade, SRL), use= "complete")$stars)
print(corr.test(select(all_4_courses_all_final, final_grade, SRL), use= "complete")$p)
print(corr.test(select(all_4_courses_all_final, final_grade, SRL), use= "complete")$n)

print(corr.test(select(all_4_courses_all_final, final_grade, LS_online_scale), use= "complete")$stars)
print(corr.test(select(all_4_courses_all_final, final_grade, LS_online_scale), use= "complete")$p)
print(corr.test(select(all_4_courses_all_final, final_grade, LS_online_scale), use= "complete")$n)

r1<- 0.07
r2<- 0.38

# Apply Fisher's r-to-z transformation
z1 <- 0.5 * log((1 + r1) / (1 - r1))
z2 <- 0.5 * log((1 + r2) / (1 - r2))

# Calculate the standard error of the difference between z-scores
n <- 174
se_diff <- sqrt(1 / (n - 3))

# Calculate the z-score of the difference between z1 and z2
z_diff <- (z1 - z2) / se_diff

# Calculate the p-value using the normal distribution
p_value <- 2 * pnorm(-abs(z_diff))

# Print the results
cat("r1 =", r1, "\n")
cat("r2 =", r2, "\n")
cat("z1 =", z1, "\n")
cat("z2 =", z2, "\n")
cat("z_diff =", z_diff, "\n")
cat("p_value =", p_value, "\n")
