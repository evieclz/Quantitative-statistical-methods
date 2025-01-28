library(tidyverse)
library(broom)
library(parameters)
library(interactions)
library(ggfortify)


marriageNew<-read.csv("marriageSupport_New.csv", header = TRUE)
tumour<-read.csv("tumourNew.csv", header = TRUE)


tumour <- mutate (tumour, 
                  usage = as_factor(usage))
tumour <- mutate (tumour,
                  usage = fct_relevel(usage,
                                      "0 hours", "1 hour", "2 hours", "3 hours", "4 hours", "5 hours"))
levels(tumour$usage)

##Task 1

marriageNew <- mutate (marriageNew, 
                  remarried = as_factor(remarried))
marriageNew <- mutate (marriageNew,
                  remarried = fct_relevel(remarried,
                                      "Yes", "No"))
levels(marriageNew$remarried)

#Task 2

marriageNew <- mutate(marriageNew,
                      attractiveness_cent = attractiveness - mean(attractiveness, na.rm = TRUE))

#Task 3

marriage_lm <- lm(support ~ attractiveness_cent*remarried, data = marriageNew)

#task 4

tidy(marriage_lm, conf.int = TRUE)

#task 5

sim_slopes(
  marriage_lm,
  pred = support,
  modx = remarried,
  jnplot = FALSE, 
  robust = TRUE,
  confint = TRUE
)

#task 6
interact_plot(
  marriage_lm,
  pred = support,
  modx = remarried,
  interval = TRUE,
  x.label = "Level of attractiveness",
  y.label = "Predicted support",
  legend.main = "Remarried"
)





