prop.test(x = 20, n = 150) #Cholestisin
binom.test(x = 20, n = 150)
prop.test(x = 31, n = 150) #Placebo
binom.test(31, 150)

#Exercises
install.packages("TH.data")
breastCancer <- TH.data::GBSG2
str(breastCancer)
head(breastCancer)
#1.1
Tab_horTh_recur <-  table(breastCancer$horTh, breastCancer$cens)
Tab_horTh_recur

n_byhorTh <- rowSums(Tab_horTh_recur)

n_byhorTh <- rowSums(Tab_horTh_recur)

p_recur_byhorTh <- Tab_horTh_recur[, "1"] / n_byhorTh
as.matrix(p_recur_byhorTh)

as.matrix(p_recur_byhorTh)

#1.2
SE_recur_byhorTh <- sqrt(p_recur_byhorTh * (1 - p_recur_byhorTh) / n_byhorTh)
SE_recur_byhorTh

lowerCI_p_recur_byhorTh <- p_recur_byhorTh - qnorm(0.975) * SE_recur_byhorTh

upperCI_p_recur_byhorTh <- p_recur_byhorTh + qnorm(0.975) * SE_recur_byhorTh

tab1 <- cbind(p_recur_byhorTh, lowerCI_p_recur_byhorTh, upperCI_p_recur_byhorTh)
tab1

p_recur_byhorTh

#1.3
diffp_recur_byhorTh <- p_recur_byhorTh[1] - p_recur_byhorTh[2]

SE_diffp_recur_byhorTh <- sqrt(sum(SE_recur_byhorTh ^ 2))

lowerCI_diffp_recur_byhorTh <- diffp_recur_byhorTh - 
  qnorm(0.975) * SE_diffp_recur_byhorTh

upperCI_diffp_recur_byhorTh <- diffp_recur_byhorTh + 
  qnorm(0.975) * SE_diffp_recur_byhorTh

tab2 <- cbind(diffp_recur_byhorTh, lowerCI_diffp_recur_byhorTh,
      upperCI_diffp_recur_byhorTh)

prop.test(cbind(Tab_horTh_recur[, 2], Tab_horTh_recur[, 1]))

cbind(Tab_horTh_recur[, 1], Tab_horTh_recur[, 2])

#2
install.packages("epitools")
library(epitools)

#2.1
Tab_horTh_recur
p_recur_byhorTh
n_byhorTh
rr <- p_recur_byhorTh[2]/p_recur_byhorTh[1]
or <- (Tab_horTh_recur["yes", "1"]/Tab_horTh_recur["yes", "0"])/(Tab_horTh_recur["no", "1"]/Tab_horTh_recur["no", "0"])

#Riskratio
rrserr <- sqrt((1/Tab_horTh_recur[2,2]-1/n_byhorTh[2])+(1/Tab_horTh_recur[1,2]-1/n_byhorTh[1]))
rrlogcilower <- log(rr) - qnorm(0.975)*serr 
rrlogciupper <- log(rr) + qnorm(0.975)*serr
rrcilower <- exp(logcilower)
rrciupper <- exp(logciupper)
cbind(rr, cilower, ciupper)

orserr <- 




riskratio(Tab_horTh_recur)
oddsratio(Tab_horTh_recur)

#3
library(survival)

## Manually ....................................................................

breastCancer$time_years <- breastCancer$time / 365.25

recur_byhorTh <- aggregate(cens ~ horTh, data = breastCancer, FUN = sum)

pYr_byhorTh <- aggregate(time_years ~ horTh, data = breastCancer, FUN = sum)

recur_pYr_byhorTh <- merge(recur_byhorTh, pYr_byhorTh, by = "horTh")

recur_pYr_byhorTh$rate <- recur_pYr_byhorTh$cens / recur_pYr_byhorTh$time_years

SE_logrecurRate <- 1 / sqrt(recur_pYr_byhorTh$cens)

recur_pYr_byhorTh$rate_lowerCI <- exp(
  log(recur_pYr_byhorTh$rate) - 1.96 * SE_logrecurRate)

recur_pYr_byhorTh$rate_upperCI <- exp(
  log(recur_pYr_byhorTh$rate) + 1.96 * SE_logrecurRate)

recur_pYr_byhorTh

