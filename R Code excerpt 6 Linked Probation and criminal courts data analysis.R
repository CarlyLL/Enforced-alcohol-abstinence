# Look at whether those with AAMRs turn up again in court data by identifying all those in the probation data that have a record in the court data where the offence_date is greater than the main_offence_date
# getwd()
packages <- c("dplyr", "data.table", "lubridate")
lapply(packages, require, character.only = TRUE)

setwd("XXX")

#####################################################
# read in combined data
# (in preparing data ensure dates are all in same format)
#####################################################
MAGPROBpeople <- read.csv(file = "MAGPROBpeople.csv")
setDT(MAGPROBpeople) # covert to data table

#Final Offence group classification to avoid small numbers in analysis
#recode
MAGPROBpeople$Offence.group.Final <- dplyr::recode(MAGPROBpeople$Offence.group, "Criminal damage and arson" = "Criminal damage and arson", "Drug offences" = "Drug offences", "Fraud offences" = "Theft offences", "Miscellaneous crimes against society" = "Miscellaneous crimes against society", "Not known" = "Not known", "Possession of weapons" = "Violence against the person", "Public order offences" = "Public order offences", "Robbery" = "Violence against the person", " Sexual offences" = " Sexual offences", "Summary motoring" = "Summary motoring", "Summary non-motoring" = "Summary non-motoring", "Theft offences" = "Theft offences", "Violence against the person" = "Violence against the person")
table(MAGPROBpeople$Offence.group.Final, useNA = "ifany")
MAGPROBpeople$Offence.group.Final[MAGPROBpeople$Offence.group.Final=="Not known"] <- NA
MAGPROBpeople$Offence.group.Final[MAGPROBpeople$Offence.group.Final==""] <- NA
#MAGPROBpeople$Offence.group.Final <- droplevels(MAGPROBpeople$Offence.group.Final)
table(MAGPROBpeople$Offence.group.Final, useNA = "ifany")
MAGPROBpeople$Offence.group.Final <- as.factor(MAGPROBpeople$Offence.group.Final)
levels(MAGPROBpeople$Offence.group.Final)
MAGPROBpeople$Offence.group.Final <- relevel(MAGPROBpeople$Offence.group.Final, ref = "Summary non-motoring")


##################################################################
# create binary flag for reoffending / representation in mags data
##################################################################
# subset probation_delius cases
MAGPROBpeople <- MAGPROBpeople %>%  filter (source_dataset == "probation_delius")
dim(MAGPROBpeople)
n_distinct(MAGPROBpeople$estimated_xjs_id) #row_id_hash

library(dplyr)
# # to subset based on offence_date from 01/01/2011
head(MAGPROBpeople$offence_date)
MAGPROBpeople$offence_date <-as.Date(MAGPROBpeople$offence_date, format="%Y-%m-%d")
MAGPROBpeople$main_offence_date[MAGPROBpeople$main_offence_date==""] <- NA # there are no NAs?
MAGPROBpeople$main_offence_date <-as.Date(MAGPROBpeople$main_offence_date, format="%Y-%m-%d")

MAGPROBpeople <- MAGPROBpeople %>% filter(offence_date >= as.Date("2011-01-01") & offence_date <= as.Date("2024-01-01"))
dim(MAGPROBpeople)
n_distinct(MAGPROBpeople$estimated_xjs_id) #row_id_hash

MAGPROBpeople$final_result_date<-as.Date(MAGPROBpeople$final_result_date, format="%Y-%m-%d")
MAGPROBpeople$disposal_date<-as.Date(MAGPROBpeople$disposal_date, format="%Y-%m-%d")
MAGPROBpeople$referral_date<-as.Date(MAGPROBpeople$referral_date, format="%Y-%m-%d")

###MAKE SURE REQ DATES ARE IN DATE FORMAT
class(MAGPROBpeople$rqmnt_commencement_date)
MAGPROBpeople$rqmnt_commencement_date<-as.Date(MAGPROBpeople$rqmnt_commencement_date, format="%Y-%m-%d")


#recode NAs for AAMRs and ATRs as 0
MAGPROBpeople$Alcohol.Abstinence.and.Monitoring <- is.na(MAGPROBpeople$Alcohol_Abstinence_._Electronic_Monitoring_.AAMR.) == 0
MAGPROBpeople$Alcohol.Treatment <- is.na(MAGPROBpeople$Alcohol_Treatment) == 0



# ##############
# # but first may need to subset analysis to include only those first referals so as not to double count reoffending outcomes and conflate offence types etc
# # order by id and date if need be
# MAGPROBpeopleSUBSET<-MAGPROBpeople[order(MAGPROBpeople$estimated_mc_cc_ps_dp, MAGPROBpeople$referral_date),]
n_distinct(MAGPROBpeople$case_id_hash) 
MAGPROBpeople <- MAGPROBpeople[!duplicated(MAGPROBpeople$case_id_hash)]
# #########

# data frame approach (faster)
MAGPROBpeople[, reoffendedEHA := ifelse(any(offence_date > referral_date[1L]), 1,0), by = estimated_xjs_id] # if I add [1L] then all returned as NA
finaloffenders <- MAGPROBpeople[, .I[.N], by=estimated_xjs_id]$V1
MAGPROBpeople[finaloffenders, reoffendedEHA := 0]

# now create variable counting how many times reoffended.
MAGPROBpeople[, count_reoffending := sum(offence_date > referral_date[1L], na.rm=TRUE), by=estimated_xjs_id]


################################################################################
# numeric duration elapsed in days between PROB date and next date in court data
################################################################################
# data table approach faster
library(data.table)

MAGPROBpeople[, duration := sapply(referral_date, function(x) {
  next_dates<-offence_date[offence_date>x]
  if(length(next_dates)>0) as.integer(min(next_dates)-x) else NA_integer_
}), by=estimated_xjs_id]

# MAGPROBpeople$duration <- as.numeric(MAGPROBpeople$duration)
summary(as.numeric(MAGPROBpeople$duration))
hist(as.numeric(MAGPROBpeople$duration)) #check distr


#new duration/time variable for EHA
setkey(MAGPROBpeople, estimated_xjs_id, offence_date, referral_date) 
study_end_date <- as.Date("2020-12-31")


MAGPROBpeople[, durationEHA := sapply(referral_date, function(x) {
  next_dates<-offence_date[offence_date>x]
  if(length(next_dates)>0) {
    min_next_date <- min(next_dates)
    as.integer(min(min_next_date, study_end_date)-x)
  } else {
    as.integer(study_end_date - x)
  }
}), by = estimated_xjs_id]

class(MAGPROBpeople$durationEHA)
summary(as.numeric(MAGPROBpeople$durationEHA))


# look at duration by AAMR / ATR
summary(as.numeric(MAGPROBpeople$duration))
MAGPROBpeople[, as.list(summary(as.numeric(duration))), by=Alcohol.Abstinence.and.Monitoring] 
wilcox.test(as.numeric(duration) ~ Alcohol.Abstinence.and.Monitoring, data=MAGPROBpeople)
boxplot(duration~Alcohol.Abstinence.and.Monitoring, data=MAGPROBpeople, main="title", xlab = "x axis title", ylab = "Count of representations")
hist(MAGPROBpeople$duration, by=MAGPROBpeople$Alcohol.Abstinence.and.Monitoring) #check distr

MAGPROBpeople[, as.list(summary(as.numeric(duration))), by=Alcohol.Treatment] 
wilcox.test(as.numeric(duration) ~ Alcohol.Treatment, data=MAGPROBpeople)
boxplot(duration~Alcohol.Treatment, data=MAGPROBpeople, main="title", xlab = "x axis title", ylab = "Count of representations")


###########
# # but  also need to subset analysis to include only unique people so dont double count reoffending outcomes and conflate offence types etc
n_distinct(MAGPROBpeople$estimated_xjs_id) #was case_id_hash
MAGPROBpeopleUNIQUE <- MAGPROBpeople[!duplicated(MAGPROBpeople$estimated_xjs_id)]


###REOFF BINARY DESCRIPTIVE ANALYSIS
table(MAGPROBpeopleUNIQUE$reoffendedEHA, useNA = "ifany")
prop.table(table(MAGPROBpeopleUNIQUE$reoffendedEHA, useNA = "ifany"))*100


table(MAGPROBpeopleUNIQUE$Alcohol.Abstinence.and.Monitoring, useNA = "ifany")
table(MAGPROBpeopleUNIQUE$Alcohol.Treatment, useNA = "ifany")


# AAMR
table(MAGPROBpeopleUNIQUE$Alcohol.Abstinence.and.Monitoring, useNA = "ifany")
table(MAGPROBpeopleUNIQUE$reoffendedEHA, MAGPROBpeopleUNIQUE$Alcohol.Abstinence.and.Monitoring, useNA = "ifany")
prop.table(table(MAGPROBpeopleUNIQUE$reoffendedEHA, MAGPROBpeopleUNIQUE$Alcohol.Abstinence.and.Monitoring, useNA = "ifany"), margin = 2)*100
chisq.test(MAGPROBpeopleUNIQUE$reoffendedEHA, MAGPROBpeopleUNIQUE$Alcohol.Abstinence.and.Monitoring, correct=FALSE)

# ATR
table(MAGPROBpeopleUNIQUE$Alcohol.Treatment, useNA = "ifany")
table(MAGPROBpeopleUNIQUE$reoffendedEHA, MAGPROBpeopleUNIQUE$Alcohol.Treatment, useNA = "ifany")
prop.table(table(MAGPROBpeopleUNIQUE$reoffendedEHA, MAGPROBpeopleUNIQUE$Alcohol.Treatment, useNA = "ifany"), margin = 2)*100
chisq.test(MAGPROBpeopleUNIQUE$reoffendedEHA, MAGPROBpeopleUNIQUE$Alcohol.Treatment, correct=FALSE)

#REOFF COUNT DESCRIPTIVE ANALYSIS
#look at by group 
summary(MAGPROBpeopleUNIQUE$count_reoffending)
hist(MAGPROBpeopleUNIQUE$count_reoffending) #check distr

MAGPROBpeopleUNIQUE[, as.list(summary(count_reoffending)), by='Alcohol.Abstinence.and.Monitoring'] 
wilcox.test(count_reoffending ~ Alcohol.Abstinence.and.Monitoring, data=MAGPROBpeopleUNIQUE)


MAGPROBpeopleUNIQUE[, as.list(summary(count_reoffending)), by='Alcohol.Treatment'] 
wilcox.test(count_reoffending ~ Alcohol.Treatment, data=MAGPROBpeopleUNIQUE)


########################################################################################
# Tables reoffending by crime type and AAMR/ATR etc as per org prob data file analysis
#######################################################################################
str(MAGPROBpeople)
names(MAGPROBpeople)
MAGPROBpeople$reoffended <- as.factor(MAGPROBpeople$reoffended)
MAGPROBpeople$reoffendedEHA <- as.factor(MAGPROBpeople$reoffendedEHA)
MAGPROBpeople$gender <- as.factor(MAGPROBpeople$gender)
MAGPROBpeople$alcohol_defined <- as.factor(MAGPROBpeople$alcohol_defined)
MAGPROBpeople$Alcohol.Abstinence.and.Monitoring <- as.factor(MAGPROBpeople$Alcohol.Abstinence.and.Monitoring)
MAGPROBpeople$Offence.group <- as.factor(MAGPROBpeople$Offence.group)
MAGPROBpeople$Offence.group.Final <- as.factor(MAGPROBpeople$Offence.group.Final)
MAGPROBpeople$duration <- as.numeric(MAGPROBpeople$duration)
#REPEAT MAGPROBpeopleUNIQUE
MAGPROBpeopleUNIQUE$reoffended <- as.factor(MAGPROBpeopleUNIQUE$reoffended)
MAGPROBpeopleUNIQUE$reoffendedEHA <- as.factor(MAGPROBpeopleUNIQUE$reoffendedEHA)
MAGPROBpeopleUNIQUE$gender <- as.factor(MAGPROBpeopleUNIQUE$gender)
MAGPROBpeopleUNIQUE$alcohol_defined <- as.factor(MAGPROBpeopleUNIQUE$alcohol_defined)
MAGPROBpeopleUNIQUE$Alcohol.Abstinence.and.Monitoring <- as.factor(MAGPROBpeopleUNIQUE$Alcohol.Abstinence.and.Monitoring)
MAGPROBpeopleUNIQUE$Offence.group <- as.factor(MAGPROBpeopleUNIQUE$Offence.group)
MAGPROBpeopleUNIQUE$Offence.group.Final <- as.factor(MAGPROBpeopleUNIQUE$Offence.group.Final)
MAGPROBpeopleUNIQUE$duration <- as.numeric(MAGPROBpeopleUNIQUE$duration)


MAGPROBpeople$ethnic[MAGPROBpeople$ethnic==""] <- NA
MAGPROBpeople$ethnic <- droplevels(MAGPROBpeople$ethnic)
MAGPROBpeople$ethnic <- as.factor(MAGPROBpeople$ethnic)
table(MAGPROBpeople$ethnic, useNA="ifany")
table(MAGPROBpeople$breach_marker, useNA = "ifany")
table(MAGPROBpeople$breach_marker, MAGPROBpeople$Alcohol.Abstinence.and.Monitoring, useNA = "ifany")
prop.table(table(MAGPROBpeople$breach_marker, MAGPROBpeople$Alcohol.Abstinence.and.Monitoring, useNA = "ifany"))
table(MAGPROBpeople$proceedings_type_desc, useNA = "ifany")
table(MAGPROBpeople$proceeding_type, useNA = "ifany")
#REPEAT MAGPROBpeopleUNIQUE
MAGPROBpeopleUNIQUE$ethnic[MAGPROBpeopleUNIQUE$ethnic==""] <- NA
MAGPROBpeopleUNIQUE$ethnic <- droplevels(MAGPROBpeopleUNIQUE$ethnic)
MAGPROBpeopleUNIQUE$ethnic <- as.factor(MAGPROBpeopleUNIQUE$ethnic)
table(MAGPROBpeopleUNIQUE$ethnic, useNA="ifany")
table(MAGPROBpeopleUNIQUE$breach_marker, useNA = "ifany")
table(MAGPROBpeopleUNIQUE$breach_marker, MAGPROBpeopleUNIQUE$Alcohol.Abstinence.and.Monitoring, useNA = "ifany")
prop.table(table(MAGPROBpeopleUNIQUE$breach_marker, MAGPROBpeopleUNIQUE$Alcohol.Abstinence.and.Monitoring, useNA = "ifany"))
table(MAGPROBpeopleUNIQUE$proceedings_type_desc, useNA = "ifany")
table(MAGPROBpeopleUNIQUE$proceeding_type, useNA = "ifany")
# table(MAGPROBpeople$jsas_result_group_desc, MAGPROBpeople$Alcohol.Abstinence.and.Monitoring, useNA = "ifany")



######################
# Research questions #
######################

############################################
##Q1 Do those with AAMR/ATRs reoffend? (Prevalance)
############################################
MAGPROBpeople$alcohol_defined <- dplyr::recode(MAGPROBpeople$alcohol_defined, "0"="Not alcohol-defined", "1"="Alcohol-defined")
MAGPROBpeople$Alcohol.Abstinence.and.Monitoring <- MAGPROBpeople$Alcohol.Abstinence.and.Monitoring %>% recode_factor("FALSE"="No AAMR", "TRUE"="AAMR")
MAGPROBpeople$Alcohol.Treatment<-as.factor(MAGPROBpeople$Alcohol.Treatment)
MAGPROBpeople$Alcohol.Treatment <- MAGPROBpeople$Alcohol.Treatment %>% recode_factor("FALSE"="No ATR", "TRUE"="ATR")
MAGPROBpeople$gender <- MAGPROBpeople$gender %>% recode_factor("F"="Female", "M"="Male")
# MAGPROBpeople$reoffendedEHA<-as.factor(MAGPROBpeople$reoffendedEHA)
#REPEAT MAGPROBpeopleUNIQUE
MAGPROBpeopleUNIQUE$alcohol_defined <- dplyr::recode(MAGPROBpeopleUNIQUE$alcohol_defined, "0"="Not alcohol-defined", "1"="Alcohol-defined")
MAGPROBpeopleUNIQUE$Alcohol.Abstinence.and.Monitoring <- MAGPROBpeopleUNIQUE$Alcohol.Abstinence.and.Monitoring %>% recode_factor("FALSE"="No AAMR", "TRUE"="AAMR")
MAGPROBpeopleUNIQUE$Alcohol.Treatment<-as.factor(MAGPROBpeopleUNIQUE$Alcohol.Treatment)
MAGPROBpeopleUNIQUE$Alcohol.Treatment <- MAGPROBpeopleUNIQUE$Alcohol.Treatment %>% recode_factor("FALSE"="No ATR", "TRUE"="ATR")
MAGPROBpeopleUNIQUE$gender <- MAGPROBpeopleUNIQUE$gender %>% recode_factor("F"="Female", "M"="Male")


#as a log reg model in first instance - reoffending binary

m1a <- glm(reoffendedEHA ~ age_at_offence + gender + ethnic + IMDDecil + Offence.group.Final + alcohol_defined + Alcohol.Abstinence.and.Monitoring, family=binomial(link='logit'), data=MAGPROBpeopleUNIQUE)
# Offence.group.Final / Offence.group
summary(m1a)
library(sjPlot)
library(ggplot2)
tab_model(m1a, show.aic=T, show.fstat=T, show.r2=F) # Use 'df_method="wald"' for faster computation of CIs.
#Use 'df_method="wald"' for faster computation of CIs.
plot_model(m1a, vline.color = "black", show.values = TRUE, value.offset = .3, title="Odds of re-offending following an AAMR") + theme_classic()
library("car")
vif(m1a) #to check multicollinearity

m1b <- glm(reoffendedEHA ~ age_at_offence + gender + ethnic + IMDDecil + alcohol_defined + Offence.group.Final + Alcohol.Treatment, family=binomial(link='logit'), data=MAGPROBpeopleUNIQUE) 
summary(m1b)
tab_model(m1b, show.aic=T, show.fstat=T, show.r2=F) # Use 'df_method="wald"' for faster computation of CIs.
plot_model(m1b, vline.color = "black", show.values = TRUE, value.offset = .3, title="Odds of re-offending following an ATR") + theme_classic()

###########################################
##Q2 How often those with AAMR/ATRs reoffend? (Incidence)
############################################

# -neg binominal model (reoffending count)

library(MASS)
m2a <- glm.nb(count_reoffending ~ age_at_offence + gender + ethnic + IMDDecil + alcohol_defined + Offence.group.Final + Alcohol.Abstinence.and.Monitoring, data=MAGPROBpeopleUNIQUE) 
summary(m2a)
tab_model(m2a, show.aic=T, show.fstat=T, show.r2=F) # Use 'df_method="wald"' for faster computation of CIs.
plot_model(m2a, vline.color = "black", show.values = TRUE, value.offset = .3, title="Incidence rate ratio of re-offending following an AAMR") + theme_classic()


m2b <- glm.nb(count_reoffending ~ age_at_offence + gender + ethnic + IMDDecil + alcohol_defined + Offence.group.Final + Alcohol.Treatment, data=MAGPROBpeopleUNIQUE) 
summary(m2b)
tab_model(m2b, show.aic=T, show.fstat=T, show.r2=F) # Use 'df_method="wald"' for faster computation of CIs.
plot_model(m2b, vline.color = "black", show.values = TRUE, value.offset = .3, title="Incidence rate ratio of re-offending following an ATR") + theme_classic()

###########################################
##Q3 How long does it take for those with AAMR/ATRs to reoffend? (Duration, EHA)
############################################

# neg binominal model (Duration)

m3a <- glm.nb(duration ~ age_at_offence + gender + ethnic + IMDDecil + alcohol_defined + Offence.group.Final + Alcohol.Abstinence.and.Monitoring, data=MAGPROBpeople) 
summary(m3a)
tab_model(m3a, show.aic=T, show.fstat=T, show.r2=F) # Use 'df_method="wald"' for faster computation of CIs.
plot_model(m3a, vline.color = "black", show.values = TRUE, value.offset = .3, title="Incidence rate ratio of duration until re-offending following an AAMR") + theme_classic()

m3b <- glm.nb(duration ~ age_at_offence + gender + ethnic + IMDDecil + alcohol_defined + Offence.group.Final + Alcohol.Treatment, data=MAGPROBpeople) 
# m3b <- glm.nb(duration ~ age_at_offence + gender + ethnic + IMDDecil + alcohol_defined + Offence.group.Final + Alcohol.Treatment, data=MAGPROBpeopleUNIQUE) 
summary(m3b)
tab_model(m3b, show.aic=T, show.fstat=T, show.r2=F) # Use 'df_method="wald"' for faster computation of CIs.
plot_model(m3b, vline.color = "black", show.values = TRUE, value.offset = .3, title="Incidence rate ratio of duration until re-offending following an ATR") + theme_classic()


