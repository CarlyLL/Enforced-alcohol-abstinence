###############################################################################
# Probation and magistrates person-event data linkage and reappearance measures
###############################################################################


#load packages
library(dplyr)
library(data.table)
library(lubridate)

# Steps 1-3
# First read in magistrates’ courts data (MAGALL), probation data (PROB) and linked criminal courts, prisons and probation dataset (LINKCJS).
# And subset variables of interest from the magistrates courts dataset, to include key ID, date and offence variables

# Step 4
#Add linking information to magistrates data
MAGLINK <- LINKCJS[LINKCJS$source_dataset=="mags_hocas",]
MAGALL <-  dplyr::left_join(MAGALL, MAGLINK, by = "row_id_hash")

# Step 5
#Then using the imported variables from the linking dataset, link probation data
PROBLINK <- LINKCJS[LINKCJS$source_dataset=="probation_delius",] 
MAGPROBLINK <-  dplyr::left_join(MAGALL, PROBLINK, by="estimated_mc_cc_ps_dp")  
MAGPROBLINK <-  dplyr::distinct(MAGPROBLINK, case.id.hash, .keep_all="TRUE") # Upon linking only distinct matches were retained for linking with probation data.
PROB <-  dplyr::left_join(PROB, PROBLINK, by = "row_id_hash")

# Step 6
MAGPROBpeople <- MAGPROBLINK %>%
  dplyr::left_join(PROB, by = "estimated_mc_cc_ps_dp") %>%
  dplyr::filter(is.na(main_offence_date) | offence_date == main_offence_date)

# Step 7
#Subsetting these data for only those with probation records 
MAGPROBpeople<- MAGPROBpeople %>% dplyr::filter (source_dataset == "probation_delius")

#Step 8
#Exclude offence dates out of range
MAGPROBpeople <- MAGPROBpeople %>% dplyr::filter(offence_date >= as.Date("2011-01-01") & offence_date <= as.Date("2021-01-01 "))


## Calculating reappearance measures

#derive reappearance binary outcome
MAGPROBpeople[, reoffended := ifelse(offence_date > referral_date[1L], 1,0)]  

#derive reappearance count outcome
MAGPROBpeople[, count_reoffending := sum(offence_date > referral_date[1L], na.rm=TRUE), by=estimated_mc_cc_ps_dp]  

#derive duration to reappearance outcome
data.table::setkey(MAGPROBpeople, estimated_mc_cc_ps_dp, final_result_date)
MAGPROBpeople[, duration := lubridate::difftime(offence_date, referral_date[1L], units = "days"), by = estimated_mc_cc_ps_dp]
MAGPROBpeople[duration <=0, duration := NA] # Nas for incidents where offence date is on or before main offence date or negative values of duration
