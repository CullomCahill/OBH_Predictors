## DATA SETUP SCRIPT FOR Potential Predictors of OBH Client Success

# Data set from National Association of Therapeutic Schools and Programs Practive Research Network (NATSAP PRN)
# Access to this dataset requires a Research Use Agreement because it includes sensitive adolescent 
# mental health records.  Thus, a link to the dataset is not included in this script.


## DATA SETUP ----

library(tidyverse)


## Load full aggregated NATSAP dataset for OBH programs (as of  6-3-21)
org_obh <- read.csv("OBH_CF_CC/data/Wilderness Data Only for Elements 6.3.21.csv")


# Select only data interested in: 
# Demographic data, specific NATSAP surveys (client, parent, staff), YOQ (client and parent), 

sub_obh <- org_obh %>% 
  # Rename first column
  rename(ep01_gender = `ï..ep01_gender`) %>% 
  # Select only needed information
  select(# Demotraphic data
    ep01_gender:ep01_rel02_relationship, 
    # NATSAP: Client
    ep01_client_NATSAP_AQ_D_19_D0_completeDate:ep01_client_NATSAP_AQ_PD_19_D365_Q9,   # Lines 246:328
    # NATSAP: Relative 1 
    ep01_rel01_NATSAP_PQ_D_19_D0_completeDate:ep01_rel01_NATSAP_PQ_PD_19_D365_Q3a_a,  # 581:722
    # NATSAP: Relative 2 
    ep01_rel02_NATSAP_PQ_D_19_D0_completeDate:ep01_rel02_NATSAP_PQ_PD_19_D365_Q3a_a,    # 891:1032, NOTE: Relative 3 etc have minimal data, NOT INCLUDED 
    # NATSAP: STAFF
    ep01_staff_NATSAP_SQ_D_19_D0_completeDate:ep01_staff_NATSAP_SQ_I_19_A0_Q5,   #1453:1504
    # Y-OQ-SR: Client
    ep01_client_YOQ_20SR_A0_completeDate:ep01_client_YOQ_20SR_D365_SCORE,  # 413:532
    # Y-OQ: Relative 1
    ep01_rel01_YOQ_201_A0_completeDate:ep01_rel01_YOQ_201_D365_SCORE, # 723:842
    # Y-OQ: Relative 2
    ep01_rel02_YOQ_201_A0_completeDate:ep01_rel02_YOQ_201_D365_SCORE) # 1033:1152


## CLEAN ----

# Change blank spaces to NA's
sub_obh <- sub_obh %>% mutate_all(na_if," ")

# Substitute ep01_ prefix with nothing (""), will only be looking at episode 1 cases, minimal data after that
colnames(sub_obh) <- gsub("ep01_", "", colnames(sub_obh))
colnames(sub_obh) <- gsub("has_filter_", "", colnames(sub_obh)) # Remove "has_filer_" prefix as well


# Convert variables to factors
to_fact <- c("gender", "ethnicity", "age_group_at_admit", "rel01_rrd_relationship", # List of factors
             "rel01_relationship", "rel02_rrd_relationship", "rel02_relationship")
sub_obh[to_fact] <- lapply(sub_obh[to_fact], factor)

# Convert dates 
sub_obh$doa <- as.Date(sub_obh$doa, "%m/%d/%Y") # doa = Date of Admission
sub_obh$dod <- as.Date(sub_obh$dod, "%m/%d/%Y") # dod = Date of Discharge
sub_obh$dob <- as.Date(sub_obh$dob, "%m/%d/%Y") # dob = Date of Birth


# Create reference object of column names 
obhfull_names <- as.data.frame(names(sub_obh))
obhfull <- sub_obh # rename data frame




## ROUTINE OUTCOME MONITORING VARIABLE CREATION (rom)----

# Some programs collect YOQ data at additional points between intake and discharge
# The columns are coded to reflect the  timepoint of data collection
## ex. A0 = Day of Admission, A15 = 15 days after Admission, D0 = Day of Discharge, etc.
# Here we find the cases that have ROM data available, i.e. an data entry in the columns between A0 and D0
# Some programs will record a client's discharge data in both the D0 entry and the time point closest to when they leave
## ex. if a client leaves after 1 month, a program may record their discharge YOQ as both A30 and D0
# These cases did not meet the criteria for the ROM

# Create new columns with only routine outcome monitoring 
obhfull$rom15 <- ifelse(obhfull$client_YOQ_20SR_A15_SCORE != obhfull$client_YOQ_20SR_D0_SCORE & 
                      !is.na(obhfull$client_YOQ_20SR_A15_SCORE), 1, 0)
obhfull$rom30 <- ifelse(obhfull$client_YOQ_20SR_A30_SCORE != obhfull$client_YOQ_20SR_D0_SCORE & 
                        !is.na(obhfull$client_YOQ_20SR_A30_SCORE), 1, 0)
obhfull$rom45 <- ifelse(obhfull$client_YOQ_20SR_A45_SCORE != obhfull$client_YOQ_20SR_D0_SCORE & 
                      !is.na(obhfull$client_YOQ_20SR_A45_SCORE), 1, 0)
obhfull$rom60 <- ifelse(obhfull$client_YOQ_20SR_A60_SCORE != obhfull$client_YOQ_20SR_D0_SCORE & 
                      !is.na(obhfull$client_YOQ_20SR_A60_SCORE), 1, 0)
obhfull$rom <- ifelse(obhfull$rom15 == 1 | obhfull$rom30 == 1 | obhfull$rom45 == 1 | obhfull$rom60 == 1, 1, 0)
table(obhfull$rom) # 537 before taking only A0-D0 


# Select and Rename variables of interest
obh1 <- obhfull %>% 
  # Select variables we will be using
  select(
    # Demographic data
    gender, dob, doa, dod, adopted:trauma_related_issue, rom, 
    # NATSAP Questions
    # Add AQI Q9-Q12, openness to experience variables
    client_NATSAP_AQ_I_19_A0_Q9,  # It makes sense for me to be at this program 0-10
    client_NATSAP_AQ_I_19_A0_Q10, # I would like to make a positive change in my life
    client_NATSAP_AQ_I_19_A0_Q11, # How much prior progress
    client_NATSAP_AQ_I_19_A0_Q12, # How much progress do you need to make
    client_NATSAP_AQ_I_19_A0_Q13, # Transport service (y/n)
    rel01_NATSAP_PQ_I_19_A0_Q4,   # Household income
    rel01_NATSAP_PQ_I_19_A0_Q1a, # survey subject
    rel01_NATSAP_PQ_I_19_A0_Q2, # Ethnicity
    rel01_NATSAP_PQ_I_19_A0_Q3a,  # residency
    # Client YOQ: DOA, DOD, MO6
    client_YOQ_20SR_A0_SCORE, client_YOQ_20SR_D0_SCORE, client_YOQ_20SR_D180_SCORE,
    # Relative 1 YOQ: DOA, DOD, MO6
    rel01_YOQ_201_A0_SCORE, rel01_YOQ_201_D0_SCORE, rel01_YOQ_201_D180_SCORE,
    # Relative 2 YOQ: DOA, DOD, MO6
    rel02_YOQ_201_A0_SCORE, rel02_YOQ_201_D0_SCORE, rel02_YOQ_201_D180_SCORE,
  # NATSAP discharge variables
    client_NATSAP_AQ_D_19_D0_Q6, # program satisfaction
    client_NATSAP_AQ_D_19_D0_Q7, # effort into program
    client_NATSAP_AQ_D_19_D0_Q8, # Describe problems
  ) %>% 
  rename(
    # Rename NATSAP questions for usability
    program_need = client_NATSAP_AQ_I_19_A0_Q9,
    change_desire = client_NATSAP_AQ_I_19_A0_Q10,
    prior_prog = client_NATSAP_AQ_I_19_A0_Q11,
    progress_need = client_NATSAP_AQ_I_19_A0_Q12,
    transport = client_NATSAP_AQ_I_19_A0_Q13,
    income = rel01_NATSAP_PQ_I_19_A0_Q4,
    quality_treat = client_NATSAP_AQ_D_19_D0_Q6,
    effort_treat  = client_NATSAP_AQ_D_19_D0_Q7,
    prob_change   = client_NATSAP_AQ_D_19_D0_Q8,
    survey_sub    = rel01_NATSAP_PQ_I_19_A0_Q1a,
    ethnicity     = rel01_NATSAP_PQ_I_19_A0_Q2,
    residency     = rel01_NATSAP_PQ_I_19_A0_Q3a,
  )

# Remove 'unknown' gender
obh1 <- obh1 %>% mutate(gender = na_if(gender, "UNKNOWN")) %>% droplevels()


## COMPLETE CASES
# Drop cases that do not have both Admission and Discharge data
obh <- obh1 %>% 
  drop_na(client_YOQ_20SR_A0_SCORE, client_YOQ_20SR_D0_SCORE)


# DEFINE VARIABLES ----

# Define DELTA from intake to discharge::delta_client
obh$delta_client <- obh$client_YOQ_20SR_D0_SCORE - obh$client_YOQ_20SR_A0_SCORE

# Define Reliable Change Index::rci 
obh$rci <- ifelse(obh$delta_client <= -18, 1, 0)

# Define Clinical Change Category:: change_cat_client
obh <- obh %>% 
  mutate(change_cat_client =
           case_when(delta_client <= -18 & client_YOQ_20SR_D0_SCORE <= 46 ~ "rec", # recovered
                     delta_client <= -18 & client_YOQ_20SR_D0_SCORE >= 47 ~ "imp", # improved
                     delta_client >= -17 & delta_client <= 0 & 
                       client_YOQ_20SR_D0_SCORE >= 47 ~ "unch", # unchanged
                     delta_client >= 1 & client_YOQ_20SR_D0_SCORE >= 47 ~ "det", # deteriorated
                     delta_client >= -17 & delta_client <= 0 & client_YOQ_20SR_D0_SCORE <= 46 ~ "nocl", # non-clinical
                     delta_client >= 1 & client_YOQ_20SR_D0_SCORE <= 46 ~ "nocl")) # non-clinical
# View table of change categories
change_cat_tab <- obh %>% group_by(change_cat_client) %>% tally()




## FIX DATA TYPES 
# Change string variables to factor
to_factor <- c(colnames(obh[,25:29]), "change_cat_client")
obh[,to_factor] <- lapply(obh[,to_factor], factor)
# str(obh)

## Create an age at intake column
obh$age_intake <- floor(as.numeric(difftime(obh$doa, obh$dob, units = "weeks"))/52.25)


# Change order of factors for clinical change category
obh$change_cat_client <- factor(obh$change_cat_client, 
                                levels = c("rec", "imp", "unch", "nocl", "det"))





### LIKERT SCALE CONVERSION -----

### Likert Scale Conversion:: program_need, change_desire, prior_prog, progress_need
# Convert program_need likert to low, med, high (1,2,3)

obh <- obh %>% 
  # program_need
  mutate(program_need_bin = case_when(program_need <= 3 ~ 1,
                                      program_need >= 4 & program_need <= 6 ~ 2,
                                      program_need >= 7 ~ 3)) %>% 
  # change_desire
  mutate(change_desire_bin = case_when(change_desire <= 3 ~ 1,
                                       change_desire >= 4 & change_desire <= 6 ~ 2,
                                       change_desire >= 7 ~ 3)) %>% 
  # prior_prog
  mutate(prior_prog_bin = case_when(prior_prog <= 3 ~ 1,
                                    prior_prog >= 4 & prior_prog <= 6 ~ 2,
                                    prior_prog >= 7 ~ 3)) %>% 
  # progress_need
  mutate(progress_need_bin = case_when(progress_need <= 3 ~ 1,
                                       progress_need >= 4 & progress_need <= 6 ~ 2,
                                       progress_need >= 7 ~ 3))



## SAVE AS RDS----
saveRDS(obh, "C:/Users/applesaint/Desktop/One Cloud/Resume 2022/Github Portfolio/obh_6_3_21_clean.rds")

