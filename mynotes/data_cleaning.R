

library(dplyr)
library(tidyr)
library(stringr)

d <- read.csv("N:/MRC/Inequalities in Health/Esthers young men article/data/deaths 2011-2013 DZ added.csv")
names(d)
dim(d)

table(d$sex, useNA = "a")
summary(d$ageyrs)
table(d$ageg)

d$Datazone.y[1:20]
d$Datazone.x[1:20]


ds <- d[, c(3, 6:27, 31)]
head(ds)
names(ds)[24] <- "Datazone"
names(ds)

length(unique(ds$Datazone))
table(is.na(ds$Datazone))
ds <- ds[!is.na(ds$Datazone), ]

ds$age <- str_replace(ds$ageg, "_", "-")
ds$sex <- as.character(ds$sex)

ds2 <- ds %>%
  select(-yod, -ageg) %>%
  group_by(Datazone, sex, age) %>%
  summarise_all(sum)

# population and deprivation
d <- read.csv("N:/MRC/Inequalities in Health/Esthers young men article/data/Mortality, population and SIMD by DZ 2011-2013.csv")
dim(d)
names(d)
d <- d[, c(2, 763:830)]
names(d)

dep <- d %>% gather("name", "value", 2:61) %>%
  separate(name, c("sex", "age"), sep = 1) %>%
  mutate(age = str_remove(age, "_")) %>%
  mutate(age = str_replace(age, "_", "-")) %>%
  filter(sex != "T") %>%
  filter(age != "all")

head(dep)
dim(dep)[1]/2/19

unique(ds$age)[order(unique(ds$age))] == unique(dep$age)[order(unique(dep$age))]
dep$age[dep$age == "85-90"] <- "85-89"

unique(ds$sex)[order(unique(ds$sex))] == unique(dep$sex)[order(unique(dep$sex))]


d <- merge(dep, ds2, by = c("Datazone", "sex", "age"), all = T)
head(d)
names(d)
sum(d$allc, na.rm = T)
sum(ds$allc)

table(is.na(d$allc))

d[is.na(d) ] <- 0

sum(d$allc)
table(is.na(d))

d <- d[, -c(5:11)]
names(d)[5] <- "population"

rm(dep, ds, ds2)


dep <- read.csv("N:/MRC/Inequalities in Health/Esthers young men article/data/Mortality, population and SIMD by DZ 2011-2013.csv")
dep <- dep[, c(2, 763, 823:830)]
head(dep)
dim(dep)

library(SocEpi)

summary(dep$income.rank)
dep$D.inc.rank <- w_pcntile(dep, T_all, income.rank, low = T)
table(dep$D.inc.rank)
tapply(dep$T_all, dep$D.inc.rank, sum)/sum(dep$T_all)*100
tapply(dep$income.rank, dep$D.inc.rank, mean)

dep$Q.inc.rank <- cut(dep$D.inc.rank, breaks=5, labels=1:5)

table(dep$Q.inc.rank, dep$D.inc.rank)

dep <- dep[, c(1, 8, 9)]

head(dep)


d <- merge(dep, d, by = "Datazone")

head(d)
dim(d)
d$age2 <- factor(d$age, labels = c(1, 3:10, 2, 11:19))
table(d$age2, d$age)
d$age2 <- as.numeric(as.character(d$age2))
table(d$age, d$age2)

write.csv(d, "deaths_2011_2013_by_cause.csv", row.names = F)


rii(d, allc, population, D.inc.rank, age2, groups = sex == "F" , st_pop = "esp2013_19ag", thousand = 100000)
rii_all <- rii(d, allc, population, D.inc.rank, age2, groups = sex == "M" , st_pop = "esp2013_19ag", thousand = 100000)

d$other2 <- d$allc - d$drugs - d$suicide - d$alcohol - d$cbvd

rii_other <- rii(d, other2, population, D.inc.rank, age2, groups = sex == "M",
                age_group = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80"),
                RII = F, st_pop = "esp2013_19ag", thousand = 100000)
rii_drug <- rii(d, drugs, population, D.inc.rank, age2, groups = sex == "M",
                age_group = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80"),
                RII = F, st_pop = "esp2013_19ag", thousand = 100000)
rii_suicide <- rii(d, suicide, population, D.inc.rank, age2, groups = sex == "M",
                   age_group = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80"),
                   RII = F, st_pop = "esp2013_19ag", thousand = 100000)
rii_alcohol <- rii(d, alcohol, population, D.inc.rank, age2, groups = sex == "M",
                   age_group = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80"),
                   RII = F, st_pop = "esp2013_19ag", thousand = 100000)
rii_cbvd <- rii(d, cbvd, population, D.inc.rank, age2, groups = sex == "M",
                age_group = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80"),
                RII = F, st_pop = "esp2013_19ag", thousand = 100000)

sii <- rbind(rii_drug, rii_alcohol, rii_cbvd, rii_suicide, rii_other)
sii$cause <- rep(c("Drugs", "Alcohol", "CBVD", "Suicide", "Other"), each = 11)

rate <- st_rate(d, allc, population, D.inc.rank, age2, groups = sex == "M",
                age_group = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80"),
                st_pop = "esp2013_19ag", total = 100000)

rate <- rate %>% filter(ses == "overall") %>% select(-ses)

sii <- merge(sii, rate, by = "age")
sii$rii_dc <- sii$sii/sii$rate


###

data <- sii
age <- sii$age
RII <- sii$rii_dc
cause <- sii$cause

data <- data.frame(age, RII, cause)
data <- data %>% filter(!( age %in% c("all", "0-64")))
data$age <- droplevels(data$age)



sii2 <-  sii %>% filter(!( age %in% c("0-9", "80")))
sii2$age <- droplevels(sii2$age)
