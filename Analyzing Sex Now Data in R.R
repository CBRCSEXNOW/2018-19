##Install packages that you will be using
install.packages(pkgs = c("plyr", "tidyverse"))
library(plyr) 
library(tidyverse)

##Read in your data file
sexnow2018 <- read.csv("Z:\\Kiffer\\SexNow2018\\SN2018.csv")

##Ensure the data was read in as a dataframe
class(sexnow2018)

##Check the class of a variable
class(sexnow2018$Q11_age)
class(sexnow2018$Q15_money)

##Convert the age variable from integer to numeric
sexnow2018$Q11_age <- as.numeric(sexnow2018$Q11_age)

##Recheck the class of the age variable
class(sexnow2018$Q11_age)

##Summary of Age
summary(sexnow2018$Q11_age)

#Summary of PrEP Use Variable
table(sexnow2018$Q50_PrEP_ever_used)
table(sexnow2018$PrEPUse_Ever)

#Identifying levels in the PrEP Use Variable
levels(sexnow2018$Q50_PrEP_ever_used)

# Create a subset of the data containing only men who reported having HIV
PozObservations <- sexnow2018[which(sexnow2018$Q43_HIV_ever_diagnosed == 'Yes'), ]

# Create a new variable based on whether respondent reported ever using PrEP
sexnow2018$PrEPUse_Ever <- NA
sexnow2018$PrEPUse_Ever[sexnow2018$Q50_PrEP_ever_used == "Yes, Currently"] <- "Ever"
sexnow2018$PrEPUse_Ever[sexnow2018$Q50_PrEP_ever_used == "Yes, Previously"] <- "Ever"
sexnow2018$PrEPUse_Ever[sexnow2018$Q50_PrEP_ever_used == "No"] <- "Never"
sexnow2018$PrEPUse_Ever[is.na(sexnow2018$PrEPUse_Ever)] <- NA

# Examine frequency of new variable
table(sexnow2018$PrEPUse_Ever)

# Examine Cross tabulation between new and old variable versions
table(sexnow2018$Q50_PrEP_ever_used, sexnow2018$PrEPUse_Ever, useNA = "ifany")

# Create an empty column
sexnow2018$age_groups <- NA

# Collapse Ages 16 to 18 -- 1
sexnow2018$age_groups[sexnow2018$Q11_age <= 18] <- "16-18"
# Collapse Ages 19 to 29 -- 2
sexnow2018$age_groups[sexnow2018$Q11_age >= 19 & sexnow2018$Q11_age <= 29] <- "19-29"
# Collapse Ages 30 to 58 -- 3
sexnow2018$age_groups[sexnow2018$Q11_age >= 30 & sexnow2018$Q11_age <= 59] <- "30-59"
# Collapse Ages 60 or older -- 4
sexnow2018$age_groups[sexnow2018$SMD030 >= 60] <- "60+"

#recode the variable as a factor
sexnow2018$age_groups <- as.factor(sexnow2018$age_groups)

#examine your new variable using the table statement
table(sexnow2018$age_groups)

#Use the table statement to expplroe the province variable
table(sexnow2018$Q1_province)

#Use the table statement to expplroe the province variable
table(sexnow2018$Q1_province, sexnow2018$PrEPUse_Ever)

#Add dimension names
table(sexnow2018$Q1_province, sexnow2018$PrEPUse_Ever, dnn = c("Province","PrEP Use"))

#Produce proportions
prop.table(table(sexnow2018$Q1_province, sexnow2018$PrEPUse_Ever, dnn = c("Province","PrEP Use"))
)

#Produce proportions
prop.table(table(sexnow2018$Q1_province, sexnow2018$PrEPUse_Ever, dnn = c("Province","PrEP Use")), margin = 1)

#Calculate mean age by indigenity
by(data = sexnow2018$Q11_age, INDICES = sexnow2018$Q2_ethnicity_indigenous, FUN = mean, na.rm = TRUE)

