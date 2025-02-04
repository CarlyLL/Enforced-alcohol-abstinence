# # create binary variable in full file as to whether AAMR present/absent
FFRQS$AAMR <- ifelse(FFRQS$rqmnt_type_main_category_desc=="Alcohol_Abstinence_&_Electronic_Monitoring_(AAMR)", 1,0)
table(FFRQS$AAMR, useNA = "ifany")
FFRQS$AAMR <- as.factor(FFRQS$AAMR)


# # create binary variable in full file as to whether ATR present/absent
FFRQS$ATR <- ifelse(FFRQS$rqmnt_type_main_category_desc=="Alcohol_Treatment", 1,0)
table(FFRQS$ATR, useNA = "ifany")
FFRQS$ATR <- as.factor(FFRQS$ATR)

#ethnicity variable recode
ethnicity_desc<- table(FFRQS$ethnicity_desc, useNA = "ifany")
FFRQS$ethnicity_desc<- as.factor(FFRQS$ethnicity_desc)
ethnicity_desc <- round((prop.table(ethnicity_desc)*100),2)
ethnicity_desc
#recode
FFRQS$ethnic <- dplyr::recode(FFRQS$ethnicity_desc, "White : Irish" = "White", "White : Other" = "White", "White: British/English/Welsh/Scottish/Northern Irish" = "White", "White: Gypsy, Irish Traveller, Romany" = "White", "Arab" = "Other", "Asian or Asian British: Bangladeshi" = "Other", "Asian or Asian British: Chinese" = "Other", "Asian or Asian British: Indian" = "Other", "Asian or Asian British: Other" = "Other", "Asian or Asian British: Pakistani" = "Other", "Black or Black British: African" = "Other", "Black or Black British: Caribbean" = "Other", "Black or Black British: Other" = "Other", "Chinese" = "Other", "Mixed : Other" = "Other", "Mixed: White and Asian" = "Other", "Mixed: White and Black African" = "Other", "Mixed: White and Black Caribbean" = "Other", "Other Ethnic Group" = "Other")
FFRQS$ethnic[FFRQS$ethnic=="Refusal"] <- NA
FFRQS$ethnic[FFRQS$ethnic=="Z_Dummy Ethnicity 04"] <- NA
FFRQS$ethnic[FFRQS$ethnic=="Z_Dummy Ethnicity 05"] <- NA
FFRQS$ethnic[FFRQS$ethnic==""] <- NA
FFRQS$ethnic <- droplevels(FFRQS$ethnic)
ethnic <- table(FFRQS$ethnic, useNA = "ifany")
ethnic <- round((prop.table(ethnic)*100),2)
ethnic
FFRQS$ethnic <- as.factor(FFRQS$ethnic)

#age - addressing outliers above xxx
table(FFRQS$age_at_offence, useNA = "ifany")
# boxplot(FFRQS$age_at_offence)
FFRQS$age_at_offence[FFRQS$age_at_offence>=xxx] <- NA
summary(FFRQS$age_at_offence)
sd(FFRQS$age_at_offence, na.rm=TRUE)

# #offence variable recode based on lookup offence codes
# leftjoin keeps all observations from first data set and merges in any matching ones from the second
setwd("XXX")
HOCODES <- read.csv("offence-group-classification-june-2022.csv")
# Use HO classification 2022 as results in fewer missing.
# as per code excerpt 3...
#create vector of stem words to search
stem_words <- c("drink", "drunk", "alcohol", "influence of", "intox")
#find indices of the text variable that contain these stem words
indices <- grep(paste(stem_words, collapse="|"), HOCODES$Detailed_offence, ignore.case=TRUE)
# use indices to extract associated offence codes
alc_offences <- HOCODES$Offence_code[indices]
#create new variable - binary indicator as to whether an offence is alcohol related or not
HOCODES$alcohol_defined <- ifelse(HOCODES$Offence_code %in% alc_offences, 1, 0)
table(HOCODES$alcohol_defined) #123 codes alcohol defined
#print text for alcohol defined offences
HOCODES$Detailed_offence[HOCODES$alcohol_defined==1]
HOCODES$alcohol_defined <- factor(HOCODES$alcohol_defined, levels=c(0,1), labels=c("Not alcohol defined", "Alcohol defined")) # or "Absent", "Present"
label(HOCODES$alcohol_defined) <- "Alcohol defined"
table(HOCODES$alcohol_defined) 
HOCODES$Offence_code <- as.numeric(HOCODES$Offence_code)
FFRQS$mo_code <- as.numeric(FFRQS$mo_code)

library(dplyr)
FFRQS <- left_join(FFRQS, HOCODES, by=c("mo_code" = "Offence_code"))
# table(FFRQS$Offence.group, useNA = "ifany")
# table(FFRQS$Offence, useNA = "ifany")

library(table1)
caption <- "HO lookup classification 2022" 
table1(~ Offence.group, data = FFRQS, overall=c(right="Total"), caption=caption)

caption <- "Alcohol defined offences" 
table1(~ alcohol_defined, data = FFRQS, overall=c(right="Total"), caption=caption)

FFRQS$Offence.group <- as.factor(FFRQS$Offence.group)
levels(FFRQS$Offence.group)
prop.table(table(FFRQS$Offence.group, useNA = "ifany")) #request NAs
# # drop NAs if need be
# # FFRQS <- FFRQS[!is.na(FFRQS$Offence.group),]


#Final Offence group classification to avoid small numbers in analysis
FFRQS$Offence.group.Final <- dplyr::recode(FFRQS$Offence.group, "Criminal damage and arson" = "Criminal damage and arson", "Drug offences" = "Drug offences", "Fraud offences" = "Theft offences", "Miscellaneous crimes against society" = "Miscellaneous crimes against society", "Not known" = "Not known", "Possession of weapons" = "Violence against the person", "Public order offences" = "Public order offences", "Robbery" = "Violence against the person", " Sexual offences" = " Sexual offences", "Summary motoring" = "Summary motoring", "Summary non-motoring" = "Summary non-motoring", "Theft offences" = "Theft offences", "Violence against the person" = "Violence against the person")
table(FFRQS$Offence.group.Final, useNA = "ifany")
FFRQS$Offence.group.Final[FFRQS$Offence.group.Final=="Not known"] <- NA
FFRQS$Offence.group.Final <- droplevels(FFRQS$Offence.group.Final)
table(FFRQS$Offence.group.Final, useNA = "ifany")
levels(FFRQS$Offence.group.Final)

caption <- "Offence classification" 
table1(~ Offence.group.Final, data = FFRQS, overall=c(right="Total"), caption=caption)

#set summary non motoring offences as reference category
FFRQS$Offence.group.Final <- relevel(FFRQS$Offence.group.Final, ref = "Summary non-motoring")

caption <- "Offence type" 
table1(~ Offence.type, data = FFRQS, overall=c(right="Total"), caption=caption)


FFRQS$gender <- factor(FFRQS$gender, levels=c("F","M"), labels=c("Female", "Male"))
label(FFRQS$gender) <- "Sex"
label(FFRQS$age_at_offence) <- "Age"
units(FFRQS$age_at_offence) <- "years"
FFRQS$ethnic <- factor(FFRQS$ethnic, levels=c("Other","White"), labels=c("Other","White"))
label(FFRQS$ethnic) <- "Ethnicity"
FFRQS$AAMR <- factor(FFRQS$AAMR, levels=c(0,1), labels=c("No AAMR", "AAMR")) # or "Absent", "Present"
label(FFRQS$AAMR) <- "AAMR"
FFRQS$ATR <- factor(FFRQS$ATR, levels=c(0,1), labels=c("No ATR", "ATR"))
label(FFRQS$ATR) <- "ATR"
label(FFRQS$Offence.group) <- "Offence group"
label(FFRQS$IMDRank) <- "IMD Rank"
label(FFRQS$IMDDecil) <- "IMD Decile"


table(FFRQS$rqmnt_termination_reason_desc, useNA = "ifany")
caption <- "Requirement termination reason" 
table1(~ rqmnt_termination_reason_desc | AAMR, data = FFRQS, overall=c(right="Total"), caption=caption) 

FFRQS$Success <- ifelse(FFRQS$rqmnt_termination_reason_desc=="Requirement Completed" | FFRQS$rqmnt_termination_reason_desc=="Expired (Normal)", 1,0)
table(FFRQS$Success, useNA = "ifany") # are NAs those for whom outcome not known?
FFRQS$Success <- factor(FFRQS$Success, levels=c(0,1), labels=c("Not Completed","Completed"))
label(FFRQS$Success) <- "Requirement Completed"

caption <- "Descriptive statistics full Probation case load" 
# table1(~ gender + age_at_offence + ethnic + AAMR + ATR + Offence.group.Final + alcohol_defined, data = FFRQS, overall=c(right="Total"), caption=caption)
table1(~ gender + age_at_offence + ethnic + IMDDecil + AAMR + ATR + Offence.group.Final + alcohol_defined, data = FFRQS, overall=c(right="Total"), caption=caption) 


caption <- "Descriptive statistics by AAMR" # by AAMR 
# table1(~ gender + age_at_offence + ethnic + Offence.group.Final + alcohol_defined + Success | AAMR, data = FFRQS, overall=c(right="Total"), caption=caption)
table1(~ gender + age_at_offence + ethnic + IMDDecil + Offence.group.Final + alcohol_defined + Success | AAMR, data = FFRQS, overall=c(right="Total"), caption=caption) #, extra.col=list('P-value'=pvalue)

library(data.table)
FFRQS$IMDDecil <- as.numeric(FFRQS$IMDDecil)
wilcox.test(FFRQS$IMDDecil~FFRQS$AAMR)

table(FFRQS$alcohol_defined, FFRQS$AAMR)
chisq.test(table(FFRQS$alcohol_defined, FFRQS$AAMR))

table(FFRQS$Offence.group.Final, FFRQS$AAMR)
chisq.test(table(FFRQS$Offence.group.Final, FFRQS$AAMR))

table(FFRQS$gender, FFRQS$AAMR)
chisq.test(table(FFRQS$gender, FFRQS$AAMR))

table(FFRQS$ethnic, FFRQS$AAMR)
chisq.test(table(FFRQS$ethnic, FFRQS$AAMR))


caption <- "Descriptive statistics by ATR" # by ATR
table1(~ gender + age_at_offence + ethnic + IMDDecil + Offence.group.Final + alcohol_defined + Success | ATR, data = FFRQS, overall=c(right="Total"), caption=caption) #, extra.col=list('P-value'=pvalue)

table(FFRQS$gender, FFRQS$ATR)
chisq.test(table(FFRQS$gender, FFRQS$ATR))

table(FFRQS$ethnic, FFRQS$ATR)
chisq.test(table(FFRQS$ethnic, FFRQS$ATR))

wilcox.test(FFRQS$age_at_offence~FFRQS$ATR)

wilcox.test(FFRQS$IMDDecil~FFRQS$ATR)

chisq.test(FFRQS$Offence.group.Final, FFRQS$ATR)

chisq.test(FFRQS$alcohol_defined, FFRQS$ATR)



FFRQS$probation_area_desc1 <- recode_area_variable(FFRQS$probation_area_desc.x, allocation = c(
  "Altcourse (HMP)" = "North West", "Ashfield (HMYOI)" = "South West", "Askham Grange (HMP & YOI)" = "Yorkshire and the Humber",
  "Avon & Somerset" = "South West", "Aylesbury (HMYOI)" = "South Central", "Bedford (HMP)" = "East of England", "Bedfordshire" = "East of England",
  "Belmarsh (HMP)" = "London", "Berwyn (HMP)" = "Wales", "Birmingham (HMP)" = "West Midlands", "Brinsford (HMYOI)" = "West Midlands", 
  "Bristol (HMP)" = "South West", "Brixton (HMP)" = "London", "Bronzefield (HMP)" = "Kent, Surrey and Sussex", "Buckley Hall (HMP)" = "Greater Manchester",
  "Bullingdon (HMP)" = "South Central", "Bure (HMP)" = "East of England", "BVT CRC" = "West Midlands", "BVT NPS Division" = "West Midlands", 
  "Cambridgeshire &Peterborough" = "East of England", "Cardiff (HMP)" = "Wales", "Central Projects Team" = "Other", "Channings Wood (HMP)" = "South West",
  "Chelmsford (HMP)" = "East of England", "Cheshire" = "North West", "Coldingley (HMP)" = "Kent, Surrey and Sussex", "Commissioned Rehab Services" = "Other",
  "CPA BeNCH" = "Other", "CPA Brist Gloucs Somerset Wilts" = "South West", 
  "CPA Cheshire and Gtr Manchester" = "Cheshire and Gtr Manchester", 
  "CPA Cumbria and Lancashire" = "North West", "CPA Derby Leics Notts Rutland" = "East Midlands", "CPA Dorset Devon and Cornwall" = "South West", 
  "CPA Durham Tees Valley" = "North East", "CPA Essex" = "East of England", "CPA Hampshire and Isle of Wight" = "South Central", 
  "CPA Humber Lincs & N Yorks" = "Yorkshire and the Humber", "CPA Kent Surrey and Sussex" = "Kent, Surrey and Sussex", "CPA London" = "London", 
  "CPA Merseyside" = "North West", "CPA Norfolk and Suffolk" = "East of England", "CPA Northumbria" = "North East", "CPA South Yorkshire" = "Yorkshire and the Humber",
  "CPA Staffs and West Mids" = "West Midlands", "CPA Thames Valley" = "South East", "CPA Wales" = "Wales", "CPA Warwickshire and West Mercia" = "West Midlands",
  "CPA West Yorkshire" = "Yorkshire and the Humber", "Cumbria" = "North West", "Dartmoor (HMP)" = "South West", "Deerbolt (HMP & YOI)" = "North East",
  "Derbyshire" = "East Midlands", "Devon & Cornwall" = "South West", "Doncaster (HMP)" = "Yorkshire and the Humber", "Dorset" = "South West",
  "Dovegate (HMP)" = "West Midlands", "Downview (HMP)" = "London", "Drake Hall (HMP & YOI)" = "West Midlands", "Durham (HMP)" = "North East",
  "Durham and Tees Valley" = "North East", "East Midlands Region" = "East Midlands", "East of England" = "East of England", "East Sutton Park (HMP & YOI)" = "Kent, Surrey and Sussex",
  "Eastwood Park (HMP)" = "South West", "Elmley (HMP)" = "Kent, Surrey and Sussex", "Erlestoke (HMP)" = "South West", "Essex" = "East of England",
  "Exeter (HMP)" = "South West", "Ext - East of England" = "East of England", "Ext - North East Region" = "North East", "Ext - NPS Greater Manchester" = "Greater Manchester",
  "Ext - NPS North West Region" = "North West", "Ext - South West" = "South West", "Ext - West Midlands Region" = "West Midlands", "Ext - Yorkshire and The Humber" = "Yorkshire and the Humber",
  "External - London" = "London", 
  "External - NPS Midlands" = "Midlands", ###############
  "External - NPS North East" = "North East", "External - NPS North West" = "North West", "External - NPS South East & Estn" = "South East", "External - NPS South West & SC" = "South West",
  "External - Wales" = "Wales", "Featherstone (HMP)" = "West Midlands", "Feltham (HMP & YOI)" = "London", "Ford (HMP)" = "Kent, Surrey and Sussex", "Forest Bank (HMP & YOI)" = "Greater Manchester",
  "Foston Hall (HMP &YOI)" = "East Midlands", "Frankland (HMP)" = "North East", "Full Sutton (HMP)" = "Yorkshire and the Humber", "Garth (HMP)" = "North West",
  "Gartree (HMP)" = "East Midlands", "Gloucestershire" = "South West", "Greater Manchester" = "Greater Manchester", "Guys Marsh (HMP)" = "South West", "Hampshire" = "South Central",
  "Haverigg (HMP)" = "North West", "Hertfordshire" = "East of England", "Hewell (HMP)" = "West Midlands", "High Down (HMP)" = "London",
  "Highpoint (HMP)" = "East of England", "Hindley (HMP & YOI)" = "Greater Manchester", "Hollesley Bay (HMP)" = "East of England",
  "Holme House (HMP)" = "North East", "Hull (HMP)" = "Yorkshire and the Humber", "Humber (HMP)" = "Yorkshire and the Humber", "Humberside" = "Yorkshire and the Humber",
  "Huntercombe (HMP)" = "South Central", "IRC Morton Hall" = "East Midlands", "IRC The Verne" = "South West", "Isis (HMP & YOI)" = "London", "Isle of Wight (HMP)" = "South Central",
  "Kent" = "Kent, Surrey and Sussex", "Kent Surrey Sussex Region" = "Kent, Surrey and Sussex", "Kirkham (HMP)" = "North West", "Kirklevington Grange (HMP)" = "North East", "Lancashire" = "North West",
  "Lancaster Farms (HMYOI & RC)" = "North West", "Leeds (HMP)" = "Yorkshire and the Humber", "Leicester (HMP)" = "East Midlands", "Leicestershire & Rutland" = "East Midlands",
  "Lewes (HMP)" = "Kent, Surrey and Sussex", "Lincoln (HMP)" = "East Midlands", "Lincolnshire" = "Yorkshire and the Humber", "Lindholme (HMP)" = "Yorkshire and the Humber",
  "Littlehey (HMP)" = "East of England", "Liverpool (HMP)" = "North West", "London" = "London", "Long Lartin (HMP)" = "West Midlands", "Low Newton (HMP)" = "North East", "Lowdham Grange (HMP)" = "East Midlands",
  "Maidstone (HMP)" = "Kent, Surrey and Sussex", "Manchester (HMP)" = "Greater Manchester", "Merseyside" = "North West", "Moorland (HMP & YOI)" = "Yorkshire and the Humber", "National Responsibility Divison" = "Other",
  "National Security Division" = "Other", "New Hall (HMP & YOI)" = "Yorkshire and the Humber", "No Trust or Trust Unknown" = "Unknown", "Norfolk and Suffolk" = "East of England", "North East Region" = "North East",
  "Northamptonshire" = "East of England", "Northumberland (HMP)" = "North East", "Northumbria" = "North East", "Norwich (HMP & YOI)" = "East of England", "Nottingham (HMP)" = "East Midlands", "Nottinghamshire" = "East Midlands",
  "NPS Greater Manchester" = "Greater Manchester",
  "NPS Midlands" = "Midlands", ##################
  "NPS North East" = "North East", "NPS North West" = "North West", "NPS North West Region" = "North West", 
  "NPS South East and Eastern" = "South East and Eastern", ##############
  "NPS South West and South Central" = "South West and South Central", ################
  "Oakwood" = "West Midlands", "Onley (HMP)" = "West Midlands", "Parc (HMP & YOI)" = "Wales", "Pentonville (HMP)" = "London", "Peterborough" = "East of England", "Peterborough Female" = "East of England",
  "Portland (HMP & YOI)" = "South West", "Prescoed (HMP & YOI)" = "Wales", "Preston (HMP)" = "North West", "Ranby (HMP)" = "East Midlands", "Risley (HMP)" = "North West", "Rochester (HMYOI)" = "Kent, Surrey and Sussex",
  "Rye Hill (HMP)" = "West Midlands", "Send (HMP)" = "Kent, Surrey and Sussex", "South Central" = "South Central", "South West" = "South West", "South Yorkshire" = "Yorkshire and the Humber", "Stafford (HMP)" = "West Midlands",
  "Staffordshire and West Midlands" = "West Midlands", "Standford Hill (HMP)" = "Kent, Surrey and Sussex", "Stocken (HMP)" = "East Midlands", "Stoke Heath (HMP & YOI)" = "West Midlands", "Styal (HMP & YOI)" = "North West",
  "Surrey and Sussex" = "Kent, Surrey and Sussex", "Swaleside (HMP)" = "Kent, Surrey and Sussex", "Swansea (HMP)" = "Wales", "Swinfen Hall (HMP & YOI)" = "West Midlands", "Thames Valley" = "South East",
  "Thameside" = "London", "The Mount (HMP)" = "East of England", "Unallocated" = "Unknown", "Usk (HMP)" = "Wales", "Wakefield (HMP)" = "Yorkshire and the Humber", "Wales" = "Wales", "Wales Probation Trust" = "Wales",
  "Wandsworth (HMP)" = "London", "Warren Hill (HMP & YOI)" = "East of England", "Warwickshire" = "West Midlands", "Wayland (HMP)" = "East of England", "Wealstun (HMP)" = "Yorkshire and the Humber", "West Mercia" = "West Midlands",
  "West Midlands Region" = "West Midlands", "West Yorkshire" = "Yorkshire and the Humber", "Whatton (HMP)" = "East Midlands", "Whitemoor (HMP)" = "East of England", "Wiltshire" = "South West", "Winchester (HMP)" = "South Central",
  "Woodhill (HMP)" = "South Central", "Wormwood Scrubs (HMP)" = "London", "Wymott (HMP)" = "North West", "York and North Yorkshire" = "Yorkshire and the Humber", "Yorkshire and The Humber"= "Yorkshire and the Humber", "ZZ BAST Public Provider 1" = "Other"), missing_value = "Unknown")

caption <- "AAMR by Probation area" # by ATR
table1(~ probation_area_desc1 | AAMR, data = FFRQS, overall=c(right="Total"), caption=caption, format = "latex") 
chisq.test(table(FFRQS$probation_area_desc1, FFRQS$AAMR))

caption <- "ATR by Probation area" # by ATR
table1(~ probation_area_desc1 | ATR, data = FFRQS, overall=c(right="Total"), caption=caption, format = "latex") 
chisq.test(table(FFRQS$probation_area_desc1, FFRQS$ATR))


library(ggplot2)
#boxplot
ggplot(FFRQS, aes(x = AAMR , y = length.x, fill = AAMR)) + #or length_in_days
  geom_boxplot() +
  theme_classic()

summary(FFRQS$length.x)
sd(FFRQS$length.x)
caption <- "Disposal length"
table1(~ as.numeric(length.x) | AAMR, data = FFRQS, overall=c(right="Total"), caption=caption)

# ANOVA
anova_one_way <- aov(length.x~AAMR, data = FFRQS) # or length_in_days
summary(anova_one_way)
TukeyHSD(anova_one_way)  # perform a Tukey test identifies which group is different

# ANOVA
anova_one_way <- aov(length.x~ATR, data = FFRQS) # or length_in_days
summary(anova_one_way)
TukeyHSD(anova_one_way)  # perform a Tukey test identifies which group is different

caption <- "Disposal length"
table1(~ as.numeric(length.x) | ATR, data = FFRQS, overall=c(right="Total"), caption=caption)

###############################################
# # Look at other requirements alongside AMMRs
###############################################
library(dplyr)
library(tidyr)


# create list of IDs with AAMRs
AAMR_IDs <- unique(FFRQS$event_id_hash[FFRQS$rqmnt_type_main_category_desc=="Alcohol_Abstinence_&_Electronic_Monitoring_(AAMR)"])
# create binary indicator for this condition for each repeated entry of id
FFRQS$AAMRlong <- as.numeric(FFRQS$event_id_hash %in% AAMR_IDs)
# tabulate requirements for subset of ids with aamrs
#arrange factor levels based on freq of requirements
FFRQS$rqmnt_type_main_category_desc <- factor(FFRQS$rqmnt_type_main_category_desc, levels=as.factor(names(sort(table(FFRQS$rqmnt_type_main_category_desc), decreasing=T))))
table(FFRQS$rqmnt_type_main_category_desc[FFRQS$event_id_hash %in% AAMR_IDs])
# # or
# AAMRsubset <- FFRQS[FFRQS$AAMRlong==1,]
# #then drop aamrs
# AAMRsubset <- AAMRsubset[!AAMRsubset$rqmnt_type_main_category_desc=="Alcohol Abstinence and Monitoring",]
# caption <- "Frequency of requirements used alongside AAMRs" 
# table1(~ rqmnt_type_main_category_desc, data = AAMRsubset, overall=c(right="Total"), caption=caption) 


# ###############################################
# # # Look at other requirements alongside ATRs
# ###############################################

# create list of IDs with ATRs
ATR_IDs <- unique(FFRQS$event_id_hash[FFRQS$rqmnt_type_main_category_desc=="Alcohol_Treatment"])
# create binary indicator for this condition for each repeated entry of id
FFRQS$ATRlong <- as.numeric(FFRQS$event_id_hash %in% ATR_IDs)
# tabulate requirements for subset of ids with ATRs
#arrange factor levels based on freq of requirements
FFRQS$rqmnt_type_main_category_desc <- factor(FFRQS$rqmnt_type_main_category_desc, levels=as.factor(names(sort(table(FFRQS$rqmnt_type_main_category_desc), decreasing=T))))
table(FFRQS$rqmnt_type_main_category_desc[FFRQS$event_id_hash %in% ATR_IDs])
# # or
# ATRsubset <- FFRQS[FFRQS$ATRlong==1,]
# #then drop atrs
# ATRsubset <- ATRsubset[!ATRsubset$rqmnt_type_main_category_desc=="Alcohol Treatment",]
# caption <- "Frequency of requirements used alongside ATRs" 
# table1(~ rqmnt_type_main_category_desc, data = ATRsubset, overall=c(right="Total"), caption=caption) 


###############################
# # subsetting AMMRs
###############################
AAMR <- FFRQS[FFRQS$rqmnt_type_main_category_desc == "Alcohol_Abstinence_&_Electronic_Monitoring_(AAMR)",]
AAMR <- subset(AAMR, !(is.na(rqmnt_type_main_category_desc)))
dim(AAMR[duplicated(AAMR$estimated_offender_id.x),])[1]# number of duplicate IDs
summary(AAMR$length.y) # examining length of requirement ie AAMR
sd(AAMR$length.y) # examining length of requirement ie AAMR
hist(AAMR$length.y, ylim=c(0,700), breaks=8, col="#6BBBAE", labels=TRUE, main="", xlab="Length of AMMR (Days)", ylab="Number of AAMRs")


summary(AAMR$recall_count)

table(AAMR$mo_category_description)
offences <- table(AAMR$Offence.group.Final)
offences
offences <- round((prop.table(offences)*100),2)
offences

caption <- "Disposal types with AAMR"
table1(~ factor(disposal_type_description), data = AAMR, overall=c(right="Total"), caption=caption) # extra.col=list('P-value'=pvalue)


caption <- "Termination reason for AAMR"
table1(~ factor(rqmnt_termination_reason_desc), data = AAMR, overall=c(right="Total"), caption=caption) # extra.col=list('P-value'=pvalue)


# Not that many (e.g. compared to use of ATRs) so look at distribution of use of this requirement over time (e.g. since roll out / by year etc.) 
summary(AAMR$referral_date)
# extract year and count by year
class(AAMR$referral_date)
library(tidyverse)
AAMR$Year <- format(as.Date(AAMR$referral_date), format="%Y")
table(AAMR$Year, useNA = "ifany")
counts<-table(AAMR$Year, useNA = "ifany")
bar<-barplot(counts, ylim=c(0,max(counts)+55), col="#6BBBAE", xlab="Year", ylab="Number of AAMRs")
text(x=bar, y=counts+1, label=counts, pos=3)
abline(h=0, col="black", lwd=1)

# Look at which areas are using (i.e. pilot vs other areas).
caption <- "Probation areas using AAMR"
table1(~ factor(probation_area_desc1), data = AAMR, overall=c(right="Total"), caption=caption) # extra.col=list('P-value'=pvalue)


sex <- table(AAMR$gender)
sex <- prop.table(sex)*100

summary(AAMR$age_at_offence)
sd(AAMR$age_at_offence, na.rm=TRUE) # need to deal with NAs

table(AAMR$ethnic) # 


label(AAMR$gender) <- "Sex"
label(AAMR$Success) <- "Requirement Completed"
caption <- "Successful completion of AAMR by sex"
table1(~ Success | gender, data = AAMR, overall=c(right="Total"), caption=caption, extra.col=list('P-value'=pvalue))

library(dplyr)

FFRQS$count <- 1


###############################
# # subsetting ATRs
###############################
ATR <- FFRQS[FFRQS$rqmnt_type_main_category_desc == "Alcohol_Treatment",]
ATR <- subset(ATR, !(is.na(rqmnt_type_main_category_desc)))
dim(ATR[duplicated(ATR$estimated_offender_id.x),])[1]# number of duplicate IDs
summary(ATR$length.y) # examining length of requirement ie AAMR
sd(ATR$length.y) # examining length of requirement ie AAMR
hist(ATR$length.y, breaks=2, col="light blue", labels=TRUE, main="", xlab="Length of ATR (Months)", ylab="Number of ATRs")

names(ATR)

# create binary variable in ATR file as to whether AAMR successfully completed or not
table(ATR$Success, useNA = "ifany")
prop.table(table(ATR$Success, useNA = "ifany"))
ATR$Success <- as.factor(ATR$Success)

#drop missing cases on success to make below table
ATR2 <- ATR[!is.na(ATR$Success),]

caption <- "ATR success by Probation area" # by ATR
table1(~ probation_area_desc1 | Success, data = ATR2, overall=c(right="Total"), caption=caption)
table1(~ Success | probation_area_desc1, data = ATR2, overall=c(right="Total"), caption=caption) #, transpose=TRUE
table1(~ Success | probation_area_desc1, data = ATR2, overall=c(right="Total"), transpose=TRUE, caption=caption)


T1a<- table(ATR$probation_area_desc1, ATR$Success)
T1b<-percent_success<-prop.table(T1a, margin=1)*100 # margin=1 row% / margin=2 col%
# T1<-cbind(T1a,T1b)
T1<-data.frame(T1a,T1b)
library(gt)
gt(T1, rowname_col = "row", groupname_col = "group") 


table(ATR$Offence.group.Final)
offences <- table(ATR$Offence.group.Final)
offences <- round((prop.table(offences)*100),2)
barplot(offences, ylab = "Percentage of offences", xlab="Offence group")

table(ATR$Offence.type)
offences <- table(ATR$Offence.group.Final)
offences
offences <- round((prop.table(offences)*100),2)
offences
barplot(offences, ylab = "Percentage of offences", xlab="Offence type")


caption <- "Disposal types with ATR"
table1(~ factor(disposal_type_description), data = ATR, overall=c(right="Total"), caption=caption) # extra.col=list('P-value'=pvalue)


terminatation <- table(ATR$rqmnt_termination_reason_desc)
terminatation
terminatation <- round((prop.table(terminatation)*100),2)
terminatation

caption <- "Termination reason for ATR"
table1(~ factor(rqmnt_termination_reason_desc), data = ATR, overall=c(right="Total"), caption=caption) # extra.col=list('P-value'=pvalue)


# Look at distribution of use of this requirement over time (e.g. since roll out / by year etc.) 
summary(ATR$referral_date)
# extract year and count by year
class(ATR$referral_date)
library(tidyverse)
ATR$Year <- format(as.Date(ATR$referral_date), format="%Y")
table(ATR$Year, useNA = "ifany")
counts<-table(ATR$Year, useNA = "ifany")
bar<-barplot(counts, ylim=c(0,max(counts)+1000), col="#6BBBAE", xlab="Year", ylab="Number of ATRs")
text(x=bar, y=counts+1, label=counts, pos=3)
abline(h=0, col="black", lwd=1)

# Look at which areas are using (i.e. pilot vs other areas).
ProbArea <- table(ATR$probation_area_desc.x, useNA = "ifany")
ProbArea
ProbArea <- round((prop.table(ProbArea)*100),2)
ProbArea

sex <- table(ATR$gender)
sex
sex <- prop.table(sex)*100
sex

summary(ATR$age_at_offence) # may need to remove outliers

table(ATR$ethnic) # may need to collapse categories 

# examining rates of success by demographic characteristics
sexxtab <- table(ATR$Success, ATR$gender)
sexxtab
summary(sexxtab) #chi sq test
sexxtab <- round((prop.table(sexxtab)*100), 2)
sexxtab


###############################
#### logistic regression models#
###############################

#change reference category in offence group as needed
# levels(ATR$Offence.group.Final)
# levels(AAMR$Offence.group.Final)
# ATR$Offence.group.Final <- relevel(ATR$ Offence.group.Final, ref = 9) #Summary non-motoring"
# AAMR$Offence.group.Final <- relevel(AAMR$ Offence.group.Final, ref = 9)
# AAMR$Offence.group.Final <- relevel(AAMR$Offence.group.Final, ref = "Summary non-motoring")

#RUN LOG REG TO LOOK AT FACTORS ASSOCIATED WITH GETTING AMMR
m1 <- glm(AAMR ~ age_at_offence + gender + ethnic + IMDDecil + Offence.group.Final + alcohol_defined, family=binomial(link='logit'), data=FFRQS) 
summary(m1)
library(sjPlot)
tab_model(m1, show.aic=T, show.fstat=T, show.r2=F) # Use 'df_method="wald"' for faster computation of CIs.
plot_model(m1, vline.color = "black", show.values = TRUE, value.offset = .3, title="Odds of receiving an AAMR") + theme_classic() #theme_bw() # theme_mininal() # theme_light()
library("car")
vif(m1) #to check multicollinearity

#RUN LOG REG TO LOOK AT FACTORS ASSOCIATED WITH (UN)SUCCESSFUL COMPLETION [technically needs to account for right censoring]
m2.1 <- glm(Success ~ age_at_offence + gender + ethnic + IMDDecil + Offence.group.Final + alcohol_defined, family=binomial(link='logit'), data=AAMR) 
summary(m2.1)
tab_model(m2.1, show.aic=T, show.fstat=T, show.r2=F)
plot_model(m2.1, vline.color = "black", show.values = TRUE, value.offset = .3, title="Odds of successfully completing an AAMR") + theme_classic()
vif(m2.1) #to check multicollinearity

#RUN LOG REG TO LOOK AT FACTORS ASSOCIATED WITH GETTING ATR
m3 <- glm(ATR ~ age_at_offence + gender + ethnic + IMDDecil + Offence.group.Final + alcohol_defined, family=binomial(link='logit'), data=FFRQS) 
summary(m3)
tab_model(m3, show.aic=T, show.fstat=T, show.r2=F) # Use 'df_method="wald"' for faster computation of CIs.
plot_model(m3, vline.color = "black", show.values = TRUE, value.offset = .3, title="Odds of receiving an ATR") + theme_classic()
vif(m3) #to check multicollinearity

#RUN LOG REG TO LOOK AT FACTORS ASSOCIATED WITH (UN)SUCCESSFUL COMPLETION
m4 <- glm(Success ~ age_at_offence + gender + ethnic + IMDDecil + Offence.group.Final + alcohol_defined, family=binomial(link='logit'), data=ATR) 
summary(m4)
tab_model(m4, show.aic=T, show.fstat=T, show.r2=F) 
plot_model(m4, vline.color = "black", show.values = TRUE, value.offset = .3, title="Odds of successfully completing an ATR") + theme_classic()
vif(m4) #to check multicollinearity

###############################
# # temporal measures FFRQS
###############################
# rqmnt_start_date	The date the requirement is given or imposed (not the date the requirement started). It is often the same as the disposal date.
# rqmnt_commencement_date	The commencement date of the requirement.
# rqmnt_termination_date	The date the requirement was terminated. 

class(FFRQS$rqmnt_termination_date)
class(FFRQS$rqmnt_start_date)
class(FFRQS$rqmnt_commencement_date)


###check erroneous data entries xxx and remove as needed
# data collection date range: 1 January 2014 to 31 December 2020
FFRQS$rqmnt_termination_date<- as.Date(FFRQS$rqmnt_termination_date, format="%Y-%m-%d")
FFRQS$rqmnt_start_date <- as.Date(FFRQS$rqmnt_start_date, format="%Y-%m-%d")
FFRQS$rqmnt_commencement_date<- as.Date(FFRQS$rqmnt_commencement_date, format="%Y-%m-%d")

summary(FFRQS$rqmnt_termination_date)
summary(FFRQS$rqmnt_start_date) # rqmnt_start_date preferred date field in metadata
summary(FFRQS$rqmnt_commencement_date)


FFRQS$RQduration <- difftime(FFRQS$rqmnt_termination_date, FFRQS$rqmnt_commencement_date, units="days")
summary(as.numeric(FFRQS$RQduration))
hist(as.numeric(FFRQS$RQduration))
# lots of missing values if use rqmnt_commencement_date

caption <- "Duration in days elapsed between requirement commencement and termination"
table1(~ as.numeric(RQduration) | AAMR, data = FFRQS, overall=c(right="Total"), caption=caption)
table1(~ as.numeric(RQduration) | ATR, data = FFRQS, overall=c(right="Total"), caption=caption)

#RQMT completion/termination vs missing censor variable [not sure this will provide useful insight but needs to be recreated for reoffending]
# 0=censored, 1=not censored (i.e. outcome observed)
# recode NAs in this variable to 0
FFRQS$RQMTcensor[is.na(FFRQS$rqmnt_termination_date)] <-0 # use rqmnt_termination_date or rqmnt_termination_reason_desc or Success ?
FFRQS$RQMTcensor[is.na(FFRQS$RQMTcensor)] <- 1
table(FFRQS$RQMTcensor, FFRQS$AAMR, useNA = "ifany")

study_end_date <- as.Date("2020/12/31") #(based on the referral date), # disposal_date
FFRQS$rqmnt_start_date<-as.Date(FFRQS$rqmnt_start_date)
FFRQS$right_censored=ifelse(FFRQS$rqmnt_start_date > (study_end_date-120),0,1)
table(FFRQS$right_censored, FFRQS$AAMR, useNA = "ifany")

# length_in_days
# disposal_termination_date
FFRQS$Orderduration <-difftime(as.Date(FFRQS$disposal_termination_date), as.Date(FFRQS$disposal_date), units="days")
summary(as.numeric(FFRQS$Orderduration))
summary(FFRQS$length_in_days)
caption <- "Disposal length"
table1(~ length_in_days | AAMR, data = FFRQS, overall=c(right="Total"), caption=caption) #could add in type of disposal too
table1(~ length_in_days | ATR, data = FFRQS, overall=c(right="Total"), caption=caption)



##############################
# survival analysis - cross sectional
##############################
library(survival)
library(survminer)

table(AAMR$Success, useNA = "ifany")

# AAMR$rqmnt_termination_date <- as.Date(AAMR$rqmnt_termination_date, format="%Y-%m-$d")
# AAMR$rqmnt_commencement_date <- as.Date(AAMR$rqmnt_commencement_date, format="%Y-%m-$d")

AAMR$RQduration <- difftime(as.Date(AAMR$rqmnt_termination_date), as.Date(AAMR$rqmnt_commencement_date), units="days")
summary(as.numeric(AAMR$RQduration))
AAMR$RQduration <- as.numeric(AAMR$RQduration)
summary(AAMR$RQduration)

study_end_date <- as.Date("2020/12/31") #(based on the referral date), # disposal_date
AAMR$rqmnt_start_date<-as.Date(AAMR$rqmnt_start_date)
AAMR$right_censored=ifelse(AAMR$rqmnt_start_date > (study_end_date-120),0,1) # 0= censored
table(AAMR$right_censored, useNA = "ifany")

table(AAMR$right_censored, AAMR$Success, useNA = "ifany")

# create event indicator (failure to complete requirement)
#status variable should be 0=censored, 1=observed outcome
# AAMR$event_indicator <- ifelse(is.na(AAMR$Success) & AAMR$right_censored==0,0,1) # look reoff in linked data
AAMR$Success <- ifelse(AAMR$Success=="Completed",1,0)
table(AAMR$Success, useNA = "ifany")
AAMR <- AAMR %>% mutate(event_indicator=ifelse(!is.na(Success), Success, ifelse(right_censored==0,0, NA)))
# inverse code the event_indicator if want to look at time to failure
AAMR$event_indicator <- ifelse(AAMR$event_indicator==1,0,1)
table(AAMR$event_indicator, useNA = "ifany")
# #create survival time to event var
surv_obj <- Surv(time = AAMR$RQduration, event = AAMR$event_indicator) # Success?
head(surv_obj) # the + denotes if obs was right censored
# # time needs to be variable of duration until event (or censoring happens)
# # event needs to be binary var denoting whether event occured (1) or not (0)
#
# # fit cox prop haz model

coxmodel <- coxph(surv_obj ~ 1, data=AAMR) # survival curve for full cohort
coxmodel <- coxph(Surv(time = RQduration, event = event_indicator) ~ age_at_offence + gender + ethnic + IMDDecil + Offence.group.Final + alcohol_defined, data=AAMR) 
summary(coxmodel)
tab_model(coxmodel, show.aic=T, show.fstat=T, show.r2=F) # somehow only runs with full specification as in two lines above
#plot survival curves
#ggsurvplot(survfit(coxmodel), data=FFRQS, confint=TRUE, colour="#2E9FDF", ggtheme = theme_minimal())
ggsurvplot(survfit(coxmodel), data=AAMR, 
           fun="cumhaz", confint=TRUE, colour="#2E9FDF", ggtheme = theme_minimal()
           #xlab="Days", ylab="Proportion experiencing failure/success"
)


# save cum hazs
cox <- survfit(coxmodel)
# plot results
plot(cox, fun="cumhaz", conf.int=FALSE, xlab="Time (days)", ylab="Cumulative hazard")



#ATR

# create binary variable (with no NAs) in ATR file as to whether AAMR successfully completed or not
table(ATR$Success, useNA = "ifany")
# ATR$Success <- ifelse(ATR$rqmnt_termination_reason_desc=="Requirement Completed"| ATR$rqmnt_termination_reason_desc=="Expired (Normal)", 1,0)
# table(ATR$Success, useNA = "ifany") # NAs might be people for whom outcomes had not yet been observed?
# ATR$Success[is.na(ATR$Success)] <-0
# ATR$Success <- as.factor(ATR$Success)
ATR$Success <- ifelse(ATR$Success=="Completed",1,0)


#check date format
# ATR$rqmnt_termination_date <- as.Date(ATR$rqmnt_termination_date, format="%Y-%m-%d")
# ATR$rqmnt_commencement_date <- as.Date(ATR$rqmnt_commencement_date, format="%Y-%m-%d")

ATR$RQduration <- difftime(as.Date(ATR$rqmnt_termination_date), as.Date(ATR$rqmnt_commencement_date), units="days")
ATR$RQduration <- as.numeric(ATR$RQduration)
summary(ATR$RQduration)

study_end_date <- as.Date("2020/12/31") #(based on the referral date), # disposal_date
ATR$rqmnt_start_date<-as.Date(ATR$rqmnt_start_date)
ATR$right_censored=ifelse(ATR$rqmnt_start_date > (study_end_date-120),0,1)
table(ATR$right_censored, useNA = "ifany")

table(ATR$right_censored, ATR$Success, useNA = "ifany")

# create event indicator (failure to complete requirement)
# ATR$event_indicator <- ifelse(ATR$Success==0 & ATR$right_censored==0,0,1) # look reoff in linked data
# table(ATR$event_indicator, useNA = "ifany")
ATR <- ATR %>% mutate(event_indicator=ifelse(!is.na(Success), Success, ifelse(right_censored==0,0, NA)))
# inverse code the event_indicator if want to look at time to failure
ATR$event_indicator <- ifelse(ATR$event_indicator==1,0,1)
table(ATR$event_indicator, useNA = "ifany")
# #create survival time to event var
surv_obj <- Surv(time = ATR$RQduration, event = ATR$event_indicator)
head(surv_obj) # the + denotes if obs was right censored
# # time needs to be variable of duration until event (or censoring happens)
# # event needs to be binary var denoting whether event occured (1) or not (0)


# # fit cox prop haz model
coxmodel <- coxph(Surv(time = RQduration, event = event_indicator) ~ age_at_offence + gender + ethnic + IMDDecil + Offence.group.Final + alcohol_defined, data=ATR) # if running on reoffending / breach etc.
summary(coxmodel)
tab_model(coxmodel, show.aic=T, show.fstat=T, show.r2=F) # somehow only runs with full specification as in two lines above
#plot survival curves
ggsurvplot(survfit(coxmodel), data=ATR, 
           fun="cumhaz", confint=TRUE, colour="#2E9FDF", ggtheme = theme_minimal()
           #xlab="Days", ylab="Proportion experiencing failure/success"
)

# save cum hazs
cox <- survfit(coxmodel)

# plot results
plot(cox, fun="cumhaz", conf.int=FALSE, xlab="Time (days)", ylab="Cumulative hazard")
# # conf.int=TRUE is default if only one curve

