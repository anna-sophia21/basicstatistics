# 1. choose Dataset (this one, medicaldata package)
# formulate research question
#1 quantitative variable as variable of interest, descriptive part, plot, inference of mean (confidential interval, test)

#1. outline the dataset, your questions
#2. descriptive part and graph (boxplot)
#3. inferential part: analyse something by group 


#### Versicherungsdaten aus Vorlesung Statistik II?
#### no Label on boxplot
#### Aussehen Boxplot definieren 


library(tidyverse)
library(dplyr)
library(here)
library(gtsummary)
library(unibeCols)

insurance <- read_csv(here("data/raw/insurance_with_date.csv")) #here tells the R-Studio that here is the topdomain of your project 


#Statistik nach Geschlecht 
summarysex <- insurance |> 
  tbl_summary(
    by = sex, 
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})", "{median}", "{p25}, {p75}", "{min}", "{max}"),
    include = c("sex", "smoker", "charges")
  )

#Statistik nach Raucherstatus
summarysmoker <- insurance |> 
  tbl_summary(
    by = smoker, 
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})", "{median}", "{p25}, {p75}", "{min}", "{max}"),
    include = c("sex", "smoker", "charges")
  )


#Plot1: #https://appsilon.com/ggplot2-boxplots/
ggplot(data = insurance, aes (x = sex, y = charges, fill = sex))+
  geom_boxplot(alpha = 1) +
    scale_fill_manual(name = "sex",
                  breaks = c("male", "female"),
                  values = c(unibeApricotS()[1], unibeMintS()[1]),
                  labels = c("male", "female")) +
   scale_colour_manual(name = "sex",
                  breaks = c("male", "female"),
                  values = c(unibeApricotS()[1], unibeMintS()[1]),
                  labels = c("male", "female"))+
  ylab(label = "Charges ($)") +
  xlab("Sex") +
  ggtitle(label = "Insurance-Charges by Sex")+
  theme_minimal() + 
    theme(legend.position = "none")

unibePalettes()

#Plot2:
ggplot(data = insurance, aes (x = smoker, y = charges))+
  geom_boxplot()+
  ylab(label = "Charges ($)")

#Statistics
#1. Test for Normality

ggplot(data = insurance, aes(x = charges, colour = sex)) +
  geom_histogram()

insurance |>
  ggplot(aes(sample = charges, ill = sex, colour = sex)) +
  geom_qq_line(distribution = stats::qnorm)+
  geom_qq()+
  facet_wrap("sex")

insurance |>
  group_by(sex) |>
  shapiro_test(charges)

#Logtransformation der Daten

insurance$chargeslog <- log(insurance$charges)

insurance |>
  ggplot(aes(sample = chargeslog, ill = sex, colour = sex)) +
  geom_qq_line(distribution = stats::qnorm)+
  geom_qq()+
  facet_wrap("sex")

ggplot(data = insurance, aes(x = chargeslog, colour = sex)) +
  geom_histogram()

insurance |>
  group_by(sex) |>
  shapiro_test(chargeslog)

#T-Test von logtransformierten Daten, weil samplesize nicht so gross ist
#T-Test nicht transformiert mit ...

pairwise_wilcox_test

##smokerzeug

insurance |>
  ggplot(aes(sample = charges, fill = smoker, colour = smoker)) +
  geom_qq_line(distribution = stats::qnorm)+
  geom_qq()+
  facet_wrap("smoker")

insurance |>
  group_by(smoker) |>
  shapiro_test(charges)
