#activating libraries
library(tidyverse)
library(pastecs)

#task 1
assignment_data <- read.csv("data/shopping.csv", header = TRUE)
head(assignment_data, 10)
sex <- c(rep("Male", 5), rep("Female", 5))
distance <- c(0.25, 0.64, 2.19, 3.20, 5.81, 2.25, 2.91, 3.15, 4.86, 7.76)
time <- c(15, 30, 37, 65, 103, 22, 140, 160, 183, 245)
timeData <- data.frame(sex = sex, distance = distance, time = time)
print(timeData)

#task 2
library(dplyr)
timeData <- mutate (timeData, sex = as_factor(sex))
levels(timeData$sex)

#task 3
femaleFilter <- filter(timeData, sex == "Female")
print(femaleFilter)

#task 4
library(ggplot2)
ggplot(timeData, aes(x = sex, y = distance, fill = sex)) +
  geom_boxplot() +
  labs(title = "Distances by Sex",
       x = "Sex",
       y = "Distance") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "purple")) +
  theme_minimal()

#task 5
lecturerData<-read.csv("data/Lecturer Data.csv", header = TRUE)
head(lecturerData, 10)
ggplot(lecturerData, aes(alcohol, neurotic)) +
  geom_point()+
  theme_minimal()
colnames(lecturerData)
ggplot(lecturerData, aes(x = alcohol, y = neurotic, colour = job)) +
  geom_point() +
  labs(title = "Alcohol vs. Neuroticism",
       x = "alcohol consumption",
       y = "neuroticism") +
  theme_minimal()
ggplot(lecturerData, aes(x = alcohol, y = neurotic, colour = job)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.1) +
  labs(title = "Alcohol vs. Neuroticism",
       x = "Alcohol consumption per week (units)",
       y = "Neuroticism",
       colour = "Job") +
  theme_minimal()










