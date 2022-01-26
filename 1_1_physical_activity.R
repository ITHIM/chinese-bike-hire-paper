## Script to calculate health burden when scenario PA is compared with baseline's
# Using Comparative Risk Assessment (CRA)

#Load libraries
library(tidyverse)

# Check if drpa package is installed, if not then install it
if (!require("drpa",character.only = TRUE)) {
  print('Installing "drpa" package...')
  remotes::install_github("meta-analyses/drpa")
}

library(drpa)
library(ithimr)

# Import mmets per person
mmets <- read_csv("data/Merged MET result_Individual.csv")

# Rename mmets column
mmets <- mmets %>% rename(base_mmets = METnoshared, scen_mmets = METshared)

## Calculate RR using DRPA package for all-cause-mortality
# Baseline
rr_base <- drpa::dose_response(cause = "all-cause-mortality", outcome_type = "fatal", mmets$base_mmets)
rr_base_mean <- mean(rr_base$rr)
# Scenario
rr_scen <- drpa::dose_response(cause = "all-cause-mortality", outcome_type = "fatal", mmets$scen_mmets)
rr_scen_mean <- mean(rr_scen$rr)

# Create demographics dataset out of mmets
demographics <- mmets %>% dplyr::select(age, Idind) %>% rename(participant_id = Idind) %>% 
  mutate(sex = ifelse(mmets$GENDER == 1, "male", "female"), age_cat = mmets$agegroup)

## Create a single dataset with baseline and scenario per person overall RR for MMETs
rr_pp <- cbind(demographics, rr_base) %>% rename(base_mmet = rr) %>% cbind(rr_scen) %>% rename(scen_mmet = rr)

# Create short names for baseline and scenario
SCEN_SHORT_NAME <- c("base", "scen")

# Demography
# sex   age   population
DEMOGRAPHIC <<- mmets %>% dplyr::select(GENDER, agegroup, popsize) %>% rename(sex = GENDER, age_cat = agegroup, population = popsize)
DEMOGRAPHIC$sex <- ifelse(DEMOGRAPHIC$sex == 1, "male", "female")
DEMOGRAPHIC <- distinct(DEMOGRAPHIC)
DEMOGRAPHIC$dem_index <- 1:nrow(DEMOGRAPHIC)

CHRONIC_DISEASE_SCALAR <- 1
# measure    sex   age      cause population min_age max_age         rate     burden
# 1                    Deaths   male 15-19 All causes       9168      15      19 0.0005779854   5.298970
# 2 YLLs (Years of Life Lost)   male 15-19 All causes       9168      15      19 0.0412215015 377.918726

# Define reference scenario as baseline
SCEN <- REFERENCE_SCENARIO <- "base"

## copied from ithimr ithim_load_data
global_path_ithimr <- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global/')
## for windows??
global_path_ithimr <- paste0(global_path_ithimr, "/")

# Read updated disease outcomes lookup from ITHIM-R package
# Use only all-cause-mortaliy as an example
DISEASE_INVENTORY <- read.csv(paste0(global_path_ithimr,"dose_response/disease_outcomes_lookup.csv"))
# Make AP 0 for now
DISEASE_INVENTORY$air_pollution <- 0
# Keep only all-cause-mortality
DISEASE_INVENTORY <- DISEASE_INVENTORY %>% mutate(physical_activity = ifelse(acronym == "all_cause", 1, 0))
DISEASE_INVENTORY <- DISEASE_INVENTORY[1,]

# Create an example burden dataset
DISEASE_BURDEN <- data.frame (measure  = c(rep("YLLs (Years of Life Lost)", each = 10), rep("Deaths", each = 10)), 
                               sex = c("male", "female"),
                               age = c("<25","25-30", "31-35","36-40","41-59"),
                               cause= "All causes",
                               population = "0",
                               min_age = c(0, 25, 31, 36, 41),
                               max_age = c(25, 30, 35, 40, 59),
                               burden = (15931.384 / 10))

# Calcualte RR for PA
ind_pa <- ithimr::gen_pa_rr(rr_pp)

# # Calculate RR for AP
# ind_ap <- ithimr::gen_ap_rr(rr_pp)
# 
# # Calculate overall health burden
# RR_PA_AP_calculations <- ithimr::combined_rr_ap_pa(ind_pa = ind_pa,ind_ap = ind_ap)

# Calculate health burden for deaths and YLLs
health_burden_list <- ithimr::health_burden(ind_pa, combined_AP_PA = F)
