## Script to calculate health burden when scenario PA is compared with baseline's
# Using Comparative Risk Assessment (CRA)

#Load libraries
library(tidyverse)
library(stringr)

# Check if drpa package is installed, if not then install it
if (!require("drpa",character.only = TRUE)) {
  print('Installing "drpa" package...')
  remotes::install_github("meta-analyses/drpa")
}
library(drpa)

# Check if ithimr package (from latam_paper) is installed, if not then install it
if (!require("ithimr",character.only = TRUE)) {
  print('Installing ITHIMR package from the most latest developments in the latam_paper branch')
  # Uncomment line below if install_github fails in Windows
  # options(download.file.method = "wininet") ## seen at https://github.com/forestgeo/learn/issues/102#issuecomment-524264784
  remotes::install_github("ITHIM/ITHIM-R@latam_paper")
}
library(ithimr)
library(writexl)

all_mmets <- list.files(path = "data/regions_data", pattern = "*merged_Individual result.csv", full.names = TRUE) %>%
  setNames(., sub("\\.csv$", "", basename(.))) %>% 
  map(read_csv)

# Read burden data
INPUT_DISEASE_BURDEN <- readxl::read_excel("data/GBD_Province-include other final.xlsx")

output_df <- list()

for (i in 1:length(all_mmets)){
  
  # Import mmets per person
  mmets <- all_mmets[i][[1]]
  
  # Extract region's name
  region <- sapply(strsplit(names(all_mmets)[i], "_"), "[", 1)
  
  # Print region's short name
  print(region)
  
  # Rename mmets column
  mmets <- mmets %>% rename(base_mmet = METnoshared, scen_mmet = METshared)
  
  # Create demographics dataset out of mmets
  mmets <- mmets %>% rename(participant_id = Idind) %>% 
    mutate(sex = ifelse(mmets$GENDER == 1, "male", "female"), age_cat = mmets$agegroup)
  
  # Create short names for baseline and scenario
  SCEN_SHORT_NAME <- c("base", "scen")
  
  # Demography
  # sex   age   population
  DEMOGRAPHIC <<- mmets %>% 
    dplyr::select(GENDER, agegroup, popsize) %>% 
    rename(sex = GENDER, age_cat = agegroup, population = popsize) %>% 
    group_by(age_cat, sex) %>% summarise(population = first(population))
  DEMOGRAPHIC$sex <- ifelse(DEMOGRAPHIC$sex == 1, "male", "female")
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
  DISEASE_INVENTORY <- read.csv(paste0(global_path_ithimr,"dose_response/disease_outcomes_lookup.csv"))
  
  # Keep only those disease with PA relationship
  DISEASE_INVENTORY <- DISEASE_INVENTORY %>% filter(physical_activity == 1)
  
  # Remove missing diseases (that don't exist in the GBD burden data)
  DISEASE_INVENTORY <- DISEASE_INVENTORY %>% filter(!GBD_name %in% c("Myeloma", "Head and neck cancer", "Gastric cardia cancer"))
  
  # Make AP 0 for now
  DISEASE_INVENTORY$air_pollution <- 0
  
  # Filter it for China - as an example, rename columns to expected ones
  # Also mutate YLLs to its full form
  # Only keep those causes that exist in the DISEASE_INVENTORY
  DISEASE_BURDEN <<- INPUT_DISEASE_BURDEN %>% rename(sex = sex_name, cause = cause_name, burden = val, age_cat = age_name, 
                                               measure = measure_name) %>% 
    mutate(measure = ifelse(measure == "YLLs", "YLLs (Years of Life Lost)", measure)) %>% 
    mutate(age_cat = case_when(str_detect(age_cat, "40-60") ~ "41-59", 
                               str_detect(age_cat, "25-30") ~ "26-30",
                               TRUE ~ age_cat)) %>% 
    mutate(sex = ifelse(sex == 1, "male", "female"))%>%
    filter(age_cat %in% mmets$age_cat) %>% 
    filter(cause %in% DISEASE_INVENTORY$GBD_name & location_name == region)
  
  # Fix case
  DISEASE_BURDEN[DISEASE_BURDEN$measure == "deaths",]$measure <- "Deaths"
  
  # Fix case
  DISEASE_BURDEN$sex <- tolower(DISEASE_BURDEN$sex)
  
  # Calcualte RR for PA
  ind_pa <- ithimr::gen_pa_rr(mmets, conf_int = TRUE)
  
  # # Calculate RR for AP
  # ind_ap <- ithimr::gen_ap_rr(rr_pp)
  # 
  # # Calculate overall health burden
  # RR_PA_AP_calculations <- ithimr::combined_rr_ap_pa(ind_pa = ind_pa,ind_ap = ind_ap)
  
  # Calculate health burden for deaths and YLLs
  health_burden_list <- ithimr::health_burden(ind_pa, conf_int = TRUE, combined_AP_PA = F)
  
  # Print plots of health burden - by age and gender groups
  for (type in names(health_burden_list)){
    
    # Read burden data
    burden_df <- health_burden_list[[type]]
    
    # Rename columns - remove unnecessary bits
    plot_cols <- sapply(names(burden_df),function(x)grepl('scen',x))
    col_names <- str_replace_all(names(burden_df[plot_cols]), paste0(paste0("scen_", type, "_"), '|pa_ap_|ap_|pa_|_lb|_ub'), '')
    names(burden_df)[3:ncol(burden_df)] <- col_names
    
    # Change form
    burden_df <- pivot_longer(burden_df, cols = -c(sex, age_cat))
    
    # Add region's name
    burden_df$location <- region
    
    if (length(output_df[[type]]) == 0)
      output_df[[type]] <- burden_df
    else
      output_df[[type]] <- rbind(output_df[[type]], burden_df)
    
    # Using ggplot, create a box plot
    burden_graph <- ggplot(burden_df) +
      aes(x = name, y = value, fill = age_cat) +
      geom_boxplot() +
      scale_fill_hue(direction = 1) +
      coord_flip() +
      theme_light() +
      facet_wrap(vars(sex)) +
      labs(
        x = "Disease/cause",
        y = ifelse(type == "deaths", "# of deaths", "Years of Life Lost (YLLs)"),
        title = paste("Region - ", region, " - Health Burden - ", ifelse(type == "deaths", "Averted number of deaths", "Reduction in Years of Life Lost (YLLs)"))
      )
      
    print(burden_graph)
    
  }
}

# Write burden output in a single xlsx for deaths and ylls
writexl::write_xlsx(list(averted_deaths = output_df$deaths,
                         reduction_ylls = output_df$ylls),
                    path = "./output/health_burden.xlsx")
