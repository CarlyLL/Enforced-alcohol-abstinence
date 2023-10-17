###########################             
#Probation data prepation #
###########################


# Reading in and link flat files

setwd("xxxx")

getwd()

# install.packages("Rtools")
# library(Rtools)
# install.packages("arrow")
library(arrow)
# FFALL2 <- arrow::read_parquet("xxx") # where all files are in the folder


FF01 <- read_parquet("moj_probation_flatfile_01")
FF02 <- read_parquet("moj_probation_flatfile_02")
FF03 <- read_parquet("moj_probation_flatfile_03")
FF04 <- read_parquet("moj_probation_flatfile_04")
FF05 <- read_parquet("moj_probation_flatfile_05")
FF06 <- read_parquet("moj_probation_flatfile_06")
FF07 <- read_parquet("moj_probation_flatfile_07")
FF08 <- read_parquet("moj_probation_flatfile_08")
FF09 <- read_parquet("moj_probation_flatfile_09")
FF10 <- read_parquet("moj_probation_flatfile_10")
FF11 <- read_parquet("moj_probation_flatfile_11")
FF12 <- read_parquet("moj_probation_flatfile_12")
FF13 <- read_parquet("moj_probation_flatfile_13")
FF14 <- read_parquet("moj_probation_flatfile_14")
FF15 <- read_parquet("moj_probation_flatfile_15")
FF16 <- read_parquet("moj_probation_flatfile_16")
FF17 <- read_parquet("moj_probation_flatfile_17")
FF18 <- read_parquet("moj_probation_flatfile_18")
FF19 <- read_parquet("moj_probation_flatfile_19")
FF20 <- read_parquet("moj_probation_flatfile_20")
FF21 <- read_parquet("moj_probation_flatfile_21")
FF22 <- read_parquet("moj_probation_flatfile_22")
FF23 <- read_parquet("moj_probation_flatfile_23")
FF24 <- read_parquet("moj_probation_flatfile_24")
FF25 <- read_parquet("moj_probation_flatfile_25")
FF26 <- read_parquet("moj_probation_flatfile_26")
FF27 <- read_parquet("moj_probation_flatfile_27")
FF28 <- read_parquet("moj_probation_flatfile_28")
FF29 <- read_parquet("moj_probation_flatfile_29")
FF30 <- read_parquet("moj_probation_flatfile_30")

FFALL <- rbind(FF01, FF02, FF03, FF04, FF05, FF06, FF07, FF08, FF09, FF10, FF11, FF12, FF13, FF14, FF15, FF16, FF17, FF18, FF19, FF20, FF21, FF22, FF23, FF24, FF25, FF26, FF27, FF28, FF29, FF30)

rm(FF01, FF02, FF03, FF04, FF05, FF06, FF07, FF08, FF09, FF10, FF11, FF12, FF13, FF14, FF15, FF16, FF17, FF18, FF19, FF20, FF21, FF22, FF23, FF24, FF25, FF26, FF27, FF28, FF29, FF30)


# Deal with duplicate entries 

#see how many unique estimated_defendant_id s
dim(FFALL[duplicated(FFALL$estimated_offender_id),])[1]# number of duplicate ids 
#see how many unique events
dim(FFALL[duplicated(FFALL$event_id_hash),])[1]# number of duplicate events
#drop duplicate events
FFALL <- FFALL[!duplicated(FFALL$event_id_hash),]


# Append IMD 2019 data

setwd("xxxx")
IMD <- read.csv(file = "Lower_Super_Output_Area_(LSOA)_IMD_2019__(OSGB1936).csv")
IMD <- IMD[,2:5]
# code in IMD = "lsoa11cd"
# code in FFALL = "lsoa_residence"
library(dplyr)
FFALLIMD <- left_join(FFALL, IMD, by = c("lsoa_residence" = "lsoa11cd")) 

#check if zeros (welsh areas, which are actually missing values)
FFALLIMD$IMDRank[FFALLIMD$IMDRank == 0] <-NA
FFALLIMD$IMDDecil[FFALLIMD$IMDDecil == 0] <- NA


# read in and link  probation requirements
setwd("xxx")

RQ01 <- read_parquet("moj_probation_rqmnts_01")
RQ02 <- read_parquet("moj_probation_rqmnts_02")
RQ03 <- read_parquet("moj_probation_rqmnts_03")
RQ04 <- read_parquet("moj_probation_rqmnts_04")
RQ05 <- read_parquet("moj_probation_rqmnts_05")
RQ06 <- read_parquet("moj_probation_rqmnts_06")
RQ07 <- read_parquet("moj_probation_rqmnts_07")
RQ08 <- read_parquet("moj_probation_rqmnts_08")
RQ09 <- read_parquet("moj_probation_rqmnts_09")
RQ10 <- read_parquet("moj_probation_rqmnts_10")
RQ11 <- read_parquet("moj_probation_rqmnts_11")
RQ12 <- read_parquet("moj_probation_rqmnts_12")
RQ13 <- read_parquet("moj_probation_rqmnts_13")
RQ14 <- read_parquet("moj_probation_rqmnts_14")
RQ15 <- read_parquet("moj_probation_rqmnts_15")
RQ16 <- read_parquet("moj_probation_rqmnts_16")
RQ17 <- read_parquet("moj_probation_rqmnts_17")
RQ18 <- read_parquet("moj_probation_rqmnts_18")
RQ19 <- read_parquet("moj_probation_rqmnts_19")
RQ20 <- read_parquet("moj_probation_rqmnts_20")
RQ21 <- read_parquet("moj_probation_rqmnts_21")
RQ22 <- read_parquet("moj_probation_rqmnts_22")
RQ23 <- read_parquet("moj_probation_rqmnts_23")
RQ24 <- read_parquet("moj_probation_rqmnts_24")
RQ25 <- read_parquet("moj_probation_rqmnts_25")
RQ26 <- read_parquet("moj_probation_rqmnts_26")
RQ27 <- read_parquet("moj_probation_rqmnts_27")
RQ28 <- read_parquet("moj_probation_rqmnts_28")
RQ29 <- read_parquet("moj_probation_rqmnts_29")
RQ30 <- read_parquet("moj_probation_rqmnts_30")

RQALL <- rbind(RQ01, RQ02, RQ03, RQ04, RQ05, RQ06, RQ07, RQ08, RQ09, RQ10, RQ11, RQ12, RQ13, RQ14, RQ15, RQ16, RQ17, RQ18, RQ19, RQ20, RQ21, RQ22, RQ23, RQ24, RQ25, RQ26, RQ27, RQ28, RQ29, RQ30)
rm(RQ01, RQ02, RQ03, RQ04, RQ05, RQ06, RQ07, RQ08, RQ09, RQ10, RQ11, RQ12, RQ13, RQ14, RQ15, RQ16, RQ17, RQ18, RQ19, RQ20, RQ21, RQ22, RQ23, RQ24, RQ25, RQ26, RQ27, RQ28, RQ29, RQ30)


# change requirements to wide format
RQALL2 <- RQALL[, c("estimated_offender_id", "event_id_hash", "rqmnt_type_main_category_desc")] # to keep only vars of interest
RQALL2$estimated_offender_id <- as.factor(RQALL2$estimated_offender_id)
RQALL2$event_id_hash <- as.factor(RQALL2$event_id_hash)
RQALL2$rqmnt_type_main_category_desc <- as.factor(RQALL2$rqmnt_type_main_category_desc)

library(tidyr)
RQALLwide <- pivot_wider(RQALL2, id_cols = c("estimated_offender_id", "event_id_hash"),
                         names_from = rqmnt_type_main_category_desc, values_from = rqmnt_type_main_category_desc,
                         values_fn = list(rqmnt_type_main_category_desc = function(x)1),
                         #names_prefix = "factor."
)

dim(RQALLwide[duplicated(RQALLwide$estimated_offender_id),])[1]# number of duplicate ids



#link requirements to flatfile info - to return return a data frame with multiple requirements per each offender-event

# library(dplyr)
FFRQS <- inner_join(FFALLIMD, RQALL, by = c("event_id_hash" = "event_id_hash")) 
#OR
#FFRQS2 <- left_join(FFALLIMD, RQALLwide, by = c("event_id_hash" = "event_id_hash")) 
