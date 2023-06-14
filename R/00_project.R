# 1. choose Dataset (this one, medicaldata package)
# formulate research question
#1 quantitative variable as variable of interest, descriptive part, plot, inference of mean (confidential interval, test)

#1. outline the dataset, your questions
#2. descriptive part and graph (boxplot)
#3. inferential part: analyse something by group 


library(tidyverse)
library(dplyr)
library(here)
library(gtsummary)
library(unibeCols)
library(cowplot)


insurance <- read_csv(here("data/raw/insurance_with_date.csv")) #here tells the R-Studio that here is the topdomain of your project 


#Statistik nach Geschlecht 
summarysex <- insurance |> 
  tbl_summary(
    by = sex, 
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})", "{median}", "{p25}, {p75}", "{min}", "{max}"),
    include = c("sex", "smoker", "charges")
  )

#Boxplot 
ggplot(data = insurance, aes (x = sex, y = charges, fill = sex))+
  geom_boxplot(alpha = 0.7) +
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

#Statistics
#1. Test for Normality

histogram1 <- ggplot(data = insurance, aes(x = charges, fill = sex)) +
  geom_histogram()+
  facet_wrap("sex")+
  scale_fill_manual(name = "sex",
                    breaks = c("male", "female"),
                    values = c(unibeApricotS()[1], unibeMintS()[1]),
                    labels = c("male", "female")) +
  scale_colour_manual(name = "sex",
                      breaks = c("male", "female"),
                      values = c(unibeApricotS()[1], unibeMintS()[1]),
                      labels = c("male", "female"))+
  theme_minimal() + 
  theme(legend.position = "none")
  

qq_plot1 <- insurance |>
  ggplot(aes(sample = charges, ill = sex, colour = sex)) +
  geom_qq_line(distribution = stats::qnorm)+
  geom_qq()+
  facet_wrap("sex")+
  scale_fill_manual(name = "sex",
                  breaks = c("male", "female"),
                  values = c(unibeApricotS()[1], unibeMintS()[1]),
                  labels = c("male", "female")) +
  scale_colour_manual(name = "sex",
                      breaks = c("male", "female"),
                      values = c(unibeApricotS()[1], unibeMintS()[1]),
                      labels = c("male", "female"))+
  theme_minimal() + 
  theme(legend.position = "none")

insurance |>
  group_by(sex) |>
  shapiro_test(charges)



#klappt noch nicht
plot1 <- plot_grid(plotlist = list(histogram1, qq_plot1),
                                   labels = c("Histogram", "QQ-Plot"), nrow = 1)




#Logtransformation der Daten

insurance$chargeslog <- log(insurance$charges)

insurance |>
  ggplot(aes(sample = chargeslog, ill = sex, colour = sex)) +
  geom_qq_line(distribution = stats::qnorm)+
  geom_qq()+
  facet_wrap("sex")

ggplot(data = insurance, aes(x = chargeslog, colour = sex)) +
  geom_histogram()+
  facet_wrap("sex")

insurance |>
  group_by(sex) |> 
  shapiro_test(chargeslog)

#T-Test von logtransformierten Daten, weil samplesize nicht so gross ist sollte das reichen

t.test(chargeslog ~ sex, data = insurance)

#T-Test nicht transformiert mit ... pairwise_wilcox_test?

wilcox.test(charges ~ sex, data = insurance)





##smokerzeug

insurance |>
  ggplot(aes(sample = charges, fill = smoker, colour = smoker)) +
  geom_qq_line(distribution = stats::qnorm)+
  geom_qq()+
  facet_wrap("smoker")

insurance |>
  group_by(smoker) |>
  shapiro_test(charges)


#Statistik nach Raucherstatus
summarysmoker <- insurance |> 
  tbl_summary(
    by = smoker, 
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})", "{median}", "{p25}, {p75}", "{min}", "{max}"),
    include = c("sex", "smoker", "charges")
  )

#Plot2:
ggplot(data = insurance, aes (x = smoker, y = charges))+
  geom_boxplot()+
  ylab(label = "Charges ($)")