library(dplyr)
library(tidyr)
library(stringr)



#load the library
library(SocEpi)


##Standardized rate
?st_rate2

d <- dep_data_long
head(d)

st_data <- st_rate2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="all"))


st_data %>% filter(age == "all")


#comparison with popEpi
library(popEpi)

esp2013 <- c(0.050, 0.055, 0.055, 0.055, 0.060, 0.060, 0.065, 0.070, 0.070, 0.070, 0.070, 0.065, 0.060, 0.055, 0.050, 0.040, 0.025, 0.025)
d2 <- d %>% filter(ethnicity == "all")

rate(data = d2, obs = bad, pyrs = pop, print = quintile, adjust = age, weights = esp2013)*1000


#Flexibility of rate function
#rate for different ethnicities
rate(data = d, obs = bad, pyrs = pop, print = list(quintile, ethnicity), adjust = age, weights = esp2013)




#Standardized rates for Scottish
st_rate2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="Scot")) %>% filter(age=="all")

#Standardized rates for Scottish aged 15-44
st_rate2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="Scot"), CI=99, age_group=c("15-44"))

#Standardized rates for Scottish aged 15-34 and 35-44
st_rate2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="Scot"), CI=99, age_group=c("15-34", "35-44"))



####Health inequalities
?rii2

#RII with 95% CI
rii2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="all"))

#SII with 99% CI
rii2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="all"), RII=FALSE, CI=99, W=T)

#SII for new age groups with 95% CI
rii2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="all"),
     age_group=c("0-19", "20-34", "35-49"), RII=FALSE)

#supply own population weights
new_w <- c(0.075, 0.075, 0.075, 0.06, 0.060, 0.060, 0.06, 0.070, 0.050,
           0.050, 0.050, 0.06, 0.060, 0.055, 0.050, 0.040, 0.025, 0.025)

rii2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="all"), RII=FALSE, CI=99, st_pop=new_w)


