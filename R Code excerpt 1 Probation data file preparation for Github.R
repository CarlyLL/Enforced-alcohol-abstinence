#################           
# Probation data
#################

########################################################################## 
# Reading in probation files
########################################################################## 
# setwd("XXX")
FFALL <- read.csv(file="flatfile.csv")
#read in requirements 
RQALL <-  read.csv(file="rqmnts.csv")

########################################################################## 
# Linking IMD 2019 E&W deprivation data
########################################################################## 
IMD <- read.csv(file = "Lower_Super_Output_Area_(LSOA)_IMD_2019__(OSGB1936).csv")
IMD <- IMD[,2:5]
library(dplyr)
FFALLIMD <- left_join(FFALL, IMD, by = c("lsoa_residence" = "lsoa11cd")) 
#check if zeros in these data (mostly in relation to welsh areas are actually missing values)
FFALLIMD$IMDRank[FFALLIMD$IMDRank == 0] <-NA
FFALLIMD$IMDDecil[FFALLIMD$IMDDecil == 0] <- NA


############################################################ 
### change RQs to wide format
############################################################ 
#only really need vars: event_id_hash, rqmnt_type_main_category_code
RQALL$estimated_offender_id <- as.factor(RQALL$estimated_offender_id)
RQALL$event_id_hash <- as.factor(RQALL$event_id_hash)
RQALL$rqmnt_type_main_category_desc <- as.factor(RQALL$rqmnt_type_main_category_desc)

library(tidyr)
levels(RQALL$rqmnt_type_main_category_desc) <- gsub(" ", "_", levels(RQALL$rqmnt_type_main_category_desc))
RQALLwide <- pivot_wider(RQALL, id_cols = c("estimated_offender_id", "event_id_hash"),
                         names_from = rqmnt_type_main_category_desc, 
                         values_from = rqmnt_type_main_category_desc, #rqmnt_commencement_date, rqmnt_termination_date
                         values_fn = list(rqmnt_type_main_category_desc = function(x)1),
                         names_prefix = " " # can only get to work when use this and later trim variable names?
) 
names(RQALLwide) <- trimws(names(RQALLwide), "left")

dim(RQALLwide)
n_distinct(RQALLwide$estimated_offender_id) 

#add in columns again
identifier_columns <- c("estimated_offender_id", "event_id_hash")
non_identifier_columns <- names(RQALL)[!(names(RQALL) %in% identifier_columns)]
non_pivoted_data <- RQALL[,c(identifier_columns, non_identifier_columns), drop=FALSE]
RQALLwide <- left_join(RQALLwide, non_pivoted_data, by=c("estimated_offender_id", "event_id_hash"))
# subset only unique events
dim(RQALLwide)
n_distinct(RQALLwide$event_id_hash)
RQALLwide <- RQALLwide[!duplicated(RQALLwide$event_id_hash),]

rm(IMD, non_pivoted_data, identifier_columns, non_identifier_columns)

########################################################################## 
# Linking  probation requirements to flat file
# to return a data frame with multiple requirements per each offender-event
########################################################################## 
# # library(dplyr)
FFALLIMD$estimated_offender_id<-as.factor(FFALLIMD$estimated_offender_id)
FFALLIMD$event_id_hash<-as.factor(FFALLIMD$event_id_hash)

# join based on event_id_hash and estimated_offender_id to use for cross sec analyses here
FFRQS <- inner_join(FFALLIMD, RQALL, by = c("event_id_hash" = "event_id_hash"))
#or
# FFRQS3 <- left_join(FFALLIMD, RQALLwide, by = c("event_id_hash" = "event_id_hash", "estimated_offender_id" = "estimated_offender_id")) 
