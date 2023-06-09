library(tidyverse)
data <- read_csv("data/raw/perulung_ems.csv")
str(data)

data<- data |> mutate(id=as.integer(id), 
                      sex = factor(sex, levels=c(0,1), labels=c("f","m")), 
                      respsymptoms=factor(respsymptoms, levels=c(0,1), labels=c("no","yes")), 
                      asthma_hist=factor(asthma_hist))
data

ggplot(data, aes(x=fev1)) + 
  geom_histogram()

ggplot(data, aes(x=fev1)) + 
  geom_histogram(bins=40)

toy_dat<-data.frame(x1=1:11)
breaks<-seq(from=0.5,to=12.5,by=2)
ggplot(toy_dat, aes(x=x1)) + 
  geom_histogram(breaks=breaks)

toy_dat<-data.frame(x1=1:11)
breaks<-seq(from=0,to=12.5,by=2.5)
ggplot(toy_dat, aes(x=x1)) + 
  geom_histogram(breaks=breaks)

ggplot(data, aes(x=fev1)) + 
  geom_histogram(aes(y=after_stat(density)))

#Descriptive statistics
summary(data)

mean(c(6,8,11,3,5,6))
median(c(6,8,17,3,5,8))

toy_dat<-tibble(x1=1:11, 
                x2=c(1:10, 100))
toy_dat
summary(toy_dat)

ggplot(data, aes(fev1)) +  stat_ecdf() #empirical cumulative distribution function

quantile(data$fev1, c(0.25, 0.4, 0.75, 0.99))
?quantile


ggplot(data, aes(y=fev1, color=sex)) + 
  geom_boxplot() +
  scale_x_discrete()

library(gtsummary)
tabl_summary <- data |>   
  select(where(is.double)) |>
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})", 
                                     "{median} ({p25}, {p75})", 
                                     "{min}, {max}")
  )

#Standardizing variables

z<-(180-171.5)/(6.5) # Using z transformation and standard normal
1-pnorm(z) #pnorm gives the probability in a normal distribution

pnorm(z, lower.tail = FALSE) # or

1-pnorm(180, mean=171.5, sd=6.5) # or directly using the distribution of X

pnorm(180, mean = 171.5, sd = 6.5, lower.tail = FALSE)

qnorm(c(0.95, 0.975, 0.995)) #Werte bei diesen Wahrscheinlichkeiten

#Inference about the mean (Inferential statistics)

data$age_cat<-cut(data$age,seq(6,12,2), right=FALSE)
tbl_fev1 <- data  %>% 
  group_by(sex, age_cat) %>%
  summarise(n=n(), mean=mean(fev1), sd=sd(fev1))
tbl_fev1

ggplot(data, aes(x=age_cat, y=fev1, fill=sex)) + 
  geom_boxplot() + 
  xlab("Age group") + 
  ylab("FEV1") +
  facet_wrap(~sex)

#95% Confidence intervals
tbl_fev1$SE <-tbl_fev1$sd/sqrt(tbl_fev1$n)
tbl_fev1$CI_norm_lb<-tbl_fev1$mean - qnorm(0.975)*tbl_fev1$SE
tbl_fev1$CI_norm_ub<-tbl_fev1$mean + qnorm(0.975)*tbl_fev1$SE
print(tbl_fev1, digits=3)

##Exercises=========================================

###
# Framingham heart study data
install.packages("riskCommunicator")
library(riskCommunicator)

framData <- framingham
?riskCommunicator
?framingham

framingham

# We keep only the baseline examination data
framData_base <- subset(framData, TIME == 0)

# Numbers of rows and columns / show first rows and variables
head(framData_base)

?mean

#======================================

# t-Distribution
tbl_fev1$SE <-tbl_fev1$sd/sqrt(tbl_fev1$n)
tbl_fev1$CI_t_lb<-tbl_fev1$mean - qt(0.975,tbl_fev1$n-1)*tbl_fev1$SE
tbl_fev1$CI_t_ub<-tbl_fev1$mean + qt(0.975,tbl_fev1$n-1)*tbl_fev1$SE
print(tbl_fev1, digits=3)

ind<-data$age_cat=="[6,8)"
t.test(fev1~sex, data=data, subset=ind)

ggplot(data, aes(x=asthma_hist, y=fev1)) + 
  geom_boxplot() + 
  ylab("FEV1") + xlab("Asthma history") +
  facet_wrap(~sex)

dat<-data %>% filter(sex=="f")
tbl2_fev1 <- dat %>% group_by(asthma_hist) %>% summarise(n=n(), mean=mean(fev1), sd=sd(fev1))
tbl2_fev1 

total <- dat %>% summarise(n=n(), mean=mean(fev1), sd=sd(fev1))
total
