#libraries and tools ncessary for creating a package
library(devtools)
library(roxygen2)
library(usethis)
library(testthat)
library(knitr)

find_rtools()
has_devel()


#create package
create("K:/SocEpi")

#add dependencies to description
devtools::use_package("tidyr")
devtools::use_package("dplyr")
#usethis::use_package("Rcpp")
#Rcpp::fun()
#devtools::use_package("stringr")
devtools::install()
devtools::install_deps(dependencies = TRUE)

#to update documentation
devtools::document()

roxygen2::roxygenise()

usethis::use_vignette("SocEpi-vignette")

usethis::use_build_ignore("mynotes") #ingnore directory "mynotes"
usethis::use_build_ignore("cran-comments.md") #ingnore cran comments
usethis::use_build_ignore("NEWS.md") #ingnore cran comments

devtools::check()
devtools::install()
library(SocEpi)

use_revdep() # Reverse dependency check
revdepcheck::revdep_check(num_workers = 4)

devtools::load_all()

devtools::test()

# usethis::use_testthat()
# usethis::use_test()

# may help sometimes to update packages
# https://github.com/r-lib/devtools/issues/1607
# update.packages(ask = FALSE, checkBuilt = TRUE)
# to build package in parent directory
devtools::build()
devtools::build(binary = TRUE)

#to build package from terminal
#cd /k
#R CMD build SocEpi

#to install package with devtools
library(devtools)
install("K:/SocEpi")
library(SocEpi)


#DATA
##prepare data for examples


# names(d)
# d <- d[, 1:77]
# d <- d[, c(1, 78:153, 458:533)]
#
# index <- read.csv("K:/MRC/Ethnicity/RawDataSets/OA_TO_HIGHER_AREAS 2011.csv")
# index <- index[, c(1, 3)]
#
# d <- merge(d, index, by.y="OutputArea2011", by.x="oa")
#
# d <- d %>%
#   select(-oa) %>%
#   group_by(LC2011) %>%
#   summarise_all(sum)
#
#
# dm <- read.csv("Deprivation_Scotland_2011.csv")
#
# dep_data <- merge(dm, d, by.x="PS_code", by.y="LC2011")
# dep_data <- merge(dep_data, d,  by.x="PS_code", by.y="LC2011")



#save example data to package
devtools::use_data(dep_data)
devtools::use_data(dep_data,  overwrite = TRUE)
devtools::use_data(health_data,  overwrite = TRUE)

#long data with ethnic groups
d <- read.csv("N:/MRC/Ethnicity/RawDataSets/General_health_2011_oa.csv")
d <- d[d$age3 != 19, -1]
names(d)[2] <- "age"


index <- read.csv("N:/MRC/Ethnicity/RawDataSets/OA_TO_HIGHER_AREAS 2011.csv")
index <- index[, c(1, 3)]

d <- merge(d, index, by.x = "oa", by.y = "OutputArea2011")

d_pcs <- d %>%
 select(-oa) %>%
 group_by(LC2011, age, ethnicity) %>%
 summarise_all(sum)

d_pcs <- merge(dep, d_pcs, by.y = "LC2011", by.x = "PS_code")

#dep_data_long <- read.csv("K:\\Dep_ethn_longform.csv")
dep_data_long$quintile_2 <- factor(dep_data_long$quintile, labels = c("First", "Second", "Third", "Fourth", "Fifth"))

table(dep_data_long$quintile_2, dep_data_long$quintile)
tapply(dep_data_long$carstairs, dep_data_long$quintile_2, mean)

dep_data_long <- dep_data_long[, c(1:17, 24, 18:23)]

devtools::use_data(dep_data_long)
devtools::use_data(health_data)

# #save csv copy of example data
# write.csv(dep_data, "Deprivation_Scotland_2011.csv", row.names = F)
#
names(stn_data)[1:5] <- c("esp2013_18ag", "esp2013_20ag", "esp2013_21ag", "esp1976_18ag", "esp1976_19ag")
# devtools::use_data(stn_data, internal=T, overwrite = TRUE)



# WHO standard up to 100+
stn_data$who2025_21ag <- c(0.0886, 0.0869, 0.086, 0.0847, 0.0822, 0.0793, 0.0761, 0.0715, 0.0659, 0.0604,
                  0.0537, 0.0455, 0.0372, 0.0296, 0.0221, 0.0152, 0.0091, 0.0044, 0.0015, 0.0004, 0.00005)
length(stn_data$who2025_21ag)
sum(stn_data$who2025_21ag)


# WHO standard up to 85+
stn_data$who2025_18ag <- c(0.0886, 0.0869, 0.086, 0.0847, 0.0822, 0.0793, 0.0761, 0.0715,
                    0.0659, 0.0604, 0.0537, 0.0455, 0.0372, 0.0296, 0.0221, 0.0152, 0.0091, 0.0063)
length(stn_data$who2025_18ag)
sum(stn_data$who2025_18ag)

# Segi standard up to 85+
stn_data$segi_18ag <- c(0.12, 0.1, 0.09, 0.09, 0.08, 0.08, 0.06, 0.06, 0.06, 0.06, 0.05, 0.04, 0.04,
                        0.03, 0.02, 0.01, 0.005, 0.005)
length(stn_data$segi_18ag)
sum(stn_data$segi_18ag)


usethis::use_data(stn_data, internal=T, overwrite = TRUE)



devtools::check_rhub()

# submit
devtools::release()

