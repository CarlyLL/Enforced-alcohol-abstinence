###############################################################################
# Probation and magistrates person-event data linkage and reappearance measures
###############################################################################


#load packages
library(dplyr)
library(data.table)
library(lubridate)

# Steps 1-3
# First read in my pre-processed magistrates’ courts data (MAGALL), probation data (PROB) and linked Cross-justice system linking dataset (LINKCJS).
# And subset variables of interest from the magistrates courts dataset, to include key ID, date and offence variables

# Step 4
#Add linking information to magistrates data
MAGLINK <- LINKCJS[LINKCJS$source_dataset=="mags_hocas",]
MAGALL <-  dplyr::left_join(MAGALL, MAGLINK, by = "row_id_hash")

# Step 5
#Then using the imported variables from the linking dataset to add probation linking information
PROBLINK <- LINKCJS[LINKCJS$source_dataset=="probation_delius",] 
MAGPROBLINK <- dplyr::left_join(MAGALL, PROBLINK, by="estimated_xjs_id")  
MAGPROBLINK <- dplyr::distinct(MAGPROBLINK, case_id_hash, .keep_all="TRUE") # Only distinct matches were retained for adding probation linking info
#line 24 mitigates against the duplication that results from line 23
PROB <-  dplyr::left_join(PROB, PROBLINK, by = "row_id_hash")

# Step 6
MAGPROBpeople <- MAGPROBLINK %>%
  dplyr::left_join(PROB, by = "estimated_xjs_id") %>%
  dplyr::filter(is.na(main_offence_date) | offence_date == main_offence_date)

# Step 7
#Subsetting these data for only those with probation records 
MAGPROBpeople<- MAGPROBpeople %>% dplyr::filter (source_dataset == "probation_delius")

#Step 8
#Exclude offence dates out of range
MAGPROBpeople <- MAGPROBpeople %>% dplyr::filter(offence_date >= as.Date("2011-01-01") & offence_date <= as.Date("2021-01-01 "))


## Calculating reappearance measures

# de-duplicate if needed
n_distinct(MAGPROBpeople$case_id_hash)
MAGPROBpeople <- MAGPROBpeople[!duplicated(MAGPROBpeople$case_id_hash)]

#derive reappearance binary outcome
MAGPROBpeople[, reoffended := ifelse(offence_date > referral_date[1L], 1,0), by=estimated_xjs_id]  
finaloffenders <- MAGPROBpeople[, .I[.N], by=estimated_xjs_id]$V1
MAGPROBpeople[finaloffenders, reoffended:=0]

#derive reappearance count outcome
MAGPROBpeople[, count_reoffending := sum(offence_date > referral_date[1L], na.rm=TRUE), by=estimated_xjs_id]  

#derive duration to reappearance outcome
MAGPROBpeople[, duration := sapply(referral_date, function(x) {
  next_dates<-offence_date[offence_date>x]
  if(length(next_dates)>0) as.integer(min(next_dates)-x) else NA_integer_
}), by=estimated_xjs_id]  


# duration for EHA
MAGPROBpeople[, durationEHA := sapply(referral_date, function(x) {
  next_dates <- offence_date[offence_date > x]
  if (length(next_dates) > 0) as.integer(min(min(next_dates), study_end_date) - x) else NA_integer_
}), by = estimated_xjs_id]
