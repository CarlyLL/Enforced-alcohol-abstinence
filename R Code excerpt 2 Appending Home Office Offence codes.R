# R Code excerpt 2: Appending Home Office Offence codes

`#Having read in PROBATION data read in offence_group_classification

HOCODES <- read.csv("offence-group-classification-june-2022.csv")

HOCODES$Offence_code <- as.numeric(HOCODES$Offence_code)

PROBATION$mo_code <- as.numeric(PROBATION$mo_code)

library(dplyr)

PROBATION <- left_join(PROBATION, HOCODES, by=c("mo_code" = "Offence_code"))

PROBATION$Offence.group <- as.factor(PROBATION$Offence.group)

levels(PROBATION$Offence.group)

table(PROBATION$Offence.group, useNA = "ifany") #request NAs `
