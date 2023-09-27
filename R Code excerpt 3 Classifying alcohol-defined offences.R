# R Code excerpt 3: Classifying alcohol-defined offences

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

table(HOCODES$alcohol_defined) `
