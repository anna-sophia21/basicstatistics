library("usethis")
library("gitcreds")


use_git()
usethis::use_github()

library(tidyverse)
library(lubridate)
library(unibeCols)

install.packages("medicaldata")
library(medicaldata)
str(opt)
head(opt)

opt <- opt
sup <- supraclavicular

# data: opt
data <- opt |>
  select(PID, Clinic, Group, Age, Birthweight)

ggplot(data, aes(x = Group, y = Birthweight))+
  geom_boxplot()

#data: sup
supra <- supraclavicular |>
  select(subject, group, gender, bmi, age, med_duration, opioid_total)

ggplot(supra, aes(x = opioid_total, y = bmi, fill = group, colour = group))+
  geom_point()
