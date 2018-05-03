library(dplyr)
library(tidyr)
library(stringr)

d <- read.csv(file.choose())

d2 <- d %>%
  select(-c(18:36)) %>%
  select(-starts_with("T_")) %>%
  gather("vars", "n", 18:188) %>%
  separate(vars, c("health", "ethnicity", "age"), sep="_") %>%
  filter(age != "Total") %>%
  mutate(age = factor(age, labels = 1:18)) %>%
  mutate(age = as.numeric(age)) %>%
  spread(health, n) %>%
  mutate(pop = good + fg + bad)

write.csv(d2, "Dep_ethn_longform.csv", row.names = F)
  head(d2)


  health = d$bad
  population = d$pop
  ses = d$quintile
  age = d$age
  groups = list(d$ethnicity=="all")
  st_pop="esp2013_18ag"
  age_group=NA
  CI=95
  thousand=1000
  N=3


  rii2(health = d$bad, population = d$pop, grouping = d$quintile, age = d$age, sex=list(d$ethnicity=="all"),
       st_pop="esp2013_18ag", age_group=NA, CI=95, N=1000, thousand=1000)

x1 <-  st_rate2(health=d$bad, population=d$pop, ses=d$quintile, age=d$age,
           groups=list(d$ethnicity=="all"), st_pop="esp2013_18ag", age_group=NA, CI=95, thousand=1000)

data <- read.csv(file.choose())
x2 <- st_rate(data[, 76:93], data[, 19:36], data$quintile)
x2 <- x2$data

x3 <- merge(x1, x2, by.x=c("ses", "age"), by.y=c("dep", "age"), all.x=T)
cor(x3[,3:8])

rii(data[, 76:93], data[, 19:36], data$quintile, st_pop="esp2013_18ag", age_group=NA, CI=95, N=1000, thousand=1000)



names(d)


rmultinom(1, 10, c(0.1, 0.15, 0.2, 0.25, 0.3))


d <- dep_data_long

#' #RII with 95% CI
rii2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="all"))
#'
#' #SII with 99% CI
rii2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="all"), RII=FALSE, CI=99)
#'
#' #SII for new age groups with 95% CI
rii2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="all"), age_group=c("0-19", "20-34", "35-49"), RII=FALSE)

nw <- c(0.075, 0.075, 0.075, 0.06, 0.060, 0.060, 0.06, 0.070, 0.050, 0.050, 0.050, 0.06, 0.060, 0.055, 0.050, 0.040, 0.025, 0.025)
#' #Standardized rates for all people
x <- st_rate2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="all"), st_pop="esp2013_18ag")
x2 <- st_rate2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="all"), st_pop=nw)

rii2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="all"), RII=FALSE, CI=99)
rii2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="all"), RII=FALSE, CI=99, st_pop=nw)


#'
#' #Standardized rates for Scottish
st_rate2(d$bad, d$pop, d$quintile, d$age, list(d$ethnicity=="Scot"), CI=99)
