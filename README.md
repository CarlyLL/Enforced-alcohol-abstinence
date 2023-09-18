# Enforced-alcohol-abstinence
## About this project
Data and codes used in my ADRUK Data First Fellowship as detailed [here](https://www.adruk.org/our-work/browse-all-projects/adr-uk-research-fellows-the-first-users-of-the-data-first-probation-and-criminal-justice-linked-datasets-new63fdc70162868654419743) and in the following OSF pre-registration.


## Guide to the R scripts
### R Code excerpt 1: Appending Home Office Offence codes

`#Having read in PROBATION data read in offence_group_classification

HOCODES <- read.csv("offence-group-classification-june-2022.csv")

HOCODES$Offence_code <- as.numeric(HOCODES$Offence_code)

PROBATION$mo_code <- as.numeric(PROBATION$mo_code)

library(dplyr)

PROBATION <- left_join(PROBATION, HOCODES, by=c("mo_code" = "Offence_code"))

PROBATION$Offence.group <- as.factor(PROBATION$Offence.group)

levels(PROBATION$Offence.group)

table(PROBATION$Offence.group, useNA = "ifany") #request NAs
`

### R Code excerpt 2: Classifying alcohol-defined offences

`HOCODES <- read.csv("offence-group-classification-june-2022.csv")

#create vector of stem words to search

stem_words <- c("drink", "drunk", "alcohol", "influence of", "intox")

#find indices of the text variable that contain these stem words

indices <- grep(paste(stem_words, collapse="|"), HOCODES$Detailed_offence, ignore.case=TRUE)

#use indices to extract associated offence codes

alc_offences <- HOCODES$Offence_code[indices]

#create new variable - binary indicator as to whether an offence is alcohol related or not

HOCODES$alcohol_defined <- ifelse(HOCODES$Offence_code %in% alc_offences, 1, 0)

table(HOCODES$alcohol_defined) #124 codes alcohol defined

#print text for alcohol defined offences

HOCODES$Detailed_offence[HOCODES$alcohol_defined==1]

HOCODES$alcohol_defined <- factor(HOCODES$alcohol_defined, levels=c(0,1), labels=c("Not alcohol defined", "Alcohol defined")) # or "Absent", "Present"

label(HOCODES$alcohol_defined) <- "Alcohol defined"

table(HOCODES$alcohol_defined)
`
