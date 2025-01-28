library(correlation)
library(WRS2)
library(boot)
library(tidyverse)
library(broom)
library(ggfortify)
library(robust)

#TASK 1

timeData<-read.csv("shopping.csv", header = TRUE)

timeData <- mutate (timeData, sex = as_factor(sex))

correlation::correlation(timeData,
                         method = "pearson",
                         p_adjust = "holm",
                         ci = 0.95)
#P-value <0.05 indicates significant association if assumptions are met
#the distance has a very strong correlation with time of r=0.77
#the longer the time spent in store, the more distance is covered

#TASK 2

winall(timeData[, c("time", "distance")])

#P-value <0.05 means null hypothesis can be rejected and indicates significance

#TASK 3

personalityData <- read.csv("chamorro_premuzic.csv",  header = TRUE)

correlation(personalityData[, c("stu_open", "stu_agree", "lec_open", "lec_agree")], 
            method = "percentage")

#the relationships between stu_open and lec_open is highly significant with p=0.002
#so is stu_agree and lec_agree with p=0.006
#this means that, when comparing the same traits between students and lecturers, results are significant
#students with certain characteristics would like to see those in their lecturers

#TASK 4
#4A
aggress <- read.csv("child_aggression.csv",  header = TRUE)

aggress_lm_1 <- lm(aggression ~ sibling_aggression + parenting_style, 
                   data = aggress, 
                   na.action = na.exclude)
#4B
aggress_lm_2 <- lm(aggression ~ sibling_aggression + parenting_style + computer_games, 
                   data = aggress, 
                   na.action = na.exclude)
#4C
aggress_lm_3 <- lm(aggression ~ sibling_aggression + parenting_style + computer_games + diet, 
                   data = aggress, 
                   na.action = na.exclude)
#4D
aggress_factors_comp <- anova(aggress_lm_1, aggress_lm_2, aggress_lm_3)

#4E
tidy(aggress_lm_3, conf.int = FALSE, conf.level = 0.95)

#4F

#Parenting style (b = 0.062 , t = 4.93, p < 0.001) significantly predicted aggression. 
#The beta value indicates that as parenting increases (i.e. as bad practices increase), aggression increases also.

#Sibling aggression (b = 0.086, t = 2.26, p = 0.024) significantly predicted aggression. 
#The beta value indicates that as sibling aggression increases, so does aggression

#Computer games (b= 0.143, t= 3.89, p < 0.001) significantly predicted aggression.
#The beta value indicates that as the time spent playing computer games increases, aggression increases also.

#Good diet (b = –0.112, = –0.118, t = –2.95, p = 0.003) significantly predicted aggression. 
#The beta value indicates that the worse the diet, the higher the aggression score.

#TASK 5

#5A
autoplot(aggress_lm_3,
         which = 4:6,
         colour = "#5c97bf",
         smooth.colour = "#ef4836",
         alpha = 0.5,
         size = 1) + 
  theme_minimal()

plot(aggress_lm_3, which = c(1,3))

#the assumptions of linearity and homoscedasticity are met 
#the graphs look like a random array of dots and the red trend lines are relatively flat.

#5B
plot(aggress_lm_3, which = 2)
#points mostly fall along the line, so are normally distributed
#the ends deviate slightly which can be contributed to the outliers
#however, as determined, these are not influential or need to be removed

#TASK 6
aggress_lm_rob <- lmRob(aggression ~ sibling_aggression + parenting_style + computer_games + diet,  
                           data = aggress, 
                           na.action = na.exclude)

summary(aggress_lm_rob)

tidy(aggress_lm_rob, conf.int = FALSE, conf.level = 0.95)
#sibling aggression now has a P-value > 0.05
#all others P-values kept their previous significance determination
#

