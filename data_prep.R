# Data Preparation for ACO Data

library(readr)
library(janitor)
library(tidyverse)
library(stringr)


# ACO Dataset -------------------------------------------------------------

aco <-
  
  # Read in CSV data
  read.csv(
    "data/2018_Shared_Savings_Program__SSP__Accountable_Care_Organizations__ACO__PUF.csv"
  ) %>%
  
  # Selected Columns
  select(
    ACO_ID,
    Initial_Start_Date,
    Agree_Type,
    ACO_State,
    QualScore,
    Current_Track_1,
    Current_Track_1_Plus,
    Current_Track_2,
    Current_Track_3,
    Initial_Track_1,
    Initial_Track_1_Plus,
    Initial_Track_2,
    Initial_Track_3,
    N_AB,
    contains("N_Ben_"),
    Sav_rate, # Savings rate: (Benchmark Minus Expenditures) as a percent of Total Expenditures
    BnchmkMinExp, # Benchmark minus Expenditures: If +, represents total savings. If -, represents total losses
    GenSaveLoss, # Generated Total Savings and Losses: (Benchmark Minus Expenditures) for ACOs whose savings rate met or exceeded minimum savings rate. 0 otherwise 
    N_PCP,
    N_Spec,
    N_NP,
    N_PA,
    N_CNS
  ) %>%
  
  # Turning ACO_State into iterable lists
  mutate(ACO_State = map(ACO_State,
                         ~ unlist(str_split(.x, ", ")))) %>%
  
  # Counting the number of States each ACO operates within
  mutate(num_states = map(ACO_State, length),
         num_states = as.integer(num_states)) %>%
  
  # Collapsing Dummy Variables around Current Track
  gather(current, flag, Current_Track_1:Current_Track_3) %>%
  filter(flag == 1) %>%
  select(-flag) %>%
  mutate(current = str_replace_all(current, "_", " "),
         current = str_replace(current, "Current ", "")) %>%
  
  # Collapsing Dummy Variables around Initial Track
  gather(initial, flag2, Initial_Track_1:Initial_Track_3) %>%
  filter(flag2 == 1) %>%
  select(-flag2) %>%
  mutate(initial = str_replace_all(initial, "_", " "),
         initial = str_replace(initial, "Initial ", "")) %>%
  
  # Changing Start Date into date type, and extracting year
  mutate(
    Initial_Start_Date = as.Date(Initial_Start_Date, format = "%m/%d/%Y"),
    Initial_Start_Year = as.factor(format(Initial_Start_Date, "%Y"))
  ) %>%
  
  # Calculating total providers, and ratio of Assigned Beneficiaries to Providers
  mutate(
    N_total_providers = N_PCP + N_Spec + N_NP + N_PA + N_CNS,
    ratio_AB_to_providers = N_AB/N_total_providers
  ) %>%
  
  # Creating Categorical Variable for GenSaveLoss
  mutate(GenSaveLoss_categorical = 
           case_when(
             GenSaveLoss > 0 ~ "Savings",
             GenSaveLoss < 0 ~ "Losses",
             GenSaveLoss == 0 ~ "No Savings/Losses"
           )) %>%
  
  # Creating percentage values for PCPs and Specialists
  mutate(perc_PCP = N_PCP/N_total_providers,
         perc_Spec = N_Spec/N_total_providers) %>%
  
  # Calculate percentages of AB ages
  mutate(perc_Age_0_64 = N_Ben_Age_0_64 / N_AB,
         perc_Age_65_74 = N_Ben_Age_65_74 / N_AB,
         perc_Age_75_84 = N_Ben_Age_75_84 / N_AB,
         perc_Age_85plus = N_Ben_Age_85plus / N_AB) %>%
  
  # Calculate percentage for gender
  mutate(perc_male = N_Ben_Male / (N_Ben_Male + N_Ben_Female)) %>%
  
  # Remove the number of beneficiaries now that we have percentages
  select(-contains("N_Ben_"), -N_PCP, -N_Spec, -N_NP, -N_PA, -N_CNS) %>%
  
  # Turns Sav_rate into whole percentages
  mutate(Sav_rate = Sav_rate * 100)


saveRDS(aco, file = "clean_data/aco.RDS")
saveRDS(aco, file = "shiny/data/aco.RDS")



# Regressions ----------------------------------------------------------------


acos <- aco %>%
  rep_sample_n(size = nrow(aco),
               reps = 100,
               replace = TRUE) %>%
  group_by(replicate) %>%
  nest() %>%
  mutate(mod = map(
    data,
    ~ lm(Sav_rate ~ Initial_Start_Year + ratio_AB_to_providers + current + QualScore,
         data = .
    )
  ),
  reg_results = map(mod, ~ tidy(., conf.int = TRUE))) %>%
  unnest(reg_results) %>%
  select(-data,-mod) %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "Initial_Start_Year2013" ~ "Initial Starting Year in 2013",
      term == "Initial_Start_Year2014" ~ "Initial Starting Year in 2014",
      term == "Initial_Start_Year2015" ~ "Initial Starting Year in 2015",
      term == "Initial_Start_Year2016" ~ "Initial Starting Year in 2016",
      term == "Initial_Start_Year2017" ~ "Initial Starting Year in 2017",
      term == "Initial_Start_Year2018" ~ "Initial Starting Year in 2018",
      term == "ratio_AB_to_providers" ~ "Ratio of Assigned Beneficiaries to Providers",
      term == "currentTrack 1 Plus" ~ "Track 1 Plus",
      term == "currentTrack 2" ~ "Track 2",
      term == "currentTrack 3" ~ "Track 3",
      term == "QualScore" ~ "Data Quality Score"
    ),
    term = as.factor(term)
  )

saveRDS(acos, file = "clean_data/acos_regression.RDS")
saveRDS(acos, file = "shiny/data/acos_regression.RDS")


provider <- aco %>%
  mutate(N_total_providers = N_total_providers / 100) %>%
  rep_sample_n(size = nrow(aco),
               reps = 100,
               replace = TRUE) %>%
  group_by(replicate) %>%
  nest() %>%
  mutate(mod = map(
    data,
    ~ lm(Sav_rate ~ perc_PCP + perc_Spec + N_total_providers,
         data = .
    )
  ),
  reg_results = map(mod, ~ tidy(., conf.int = TRUE))) %>%
  unnest(reg_results) %>%
  select(-data,-mod) %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "perc_PCP" ~ "% PCP Providers",
      term == "perc_Spec" ~ "% Speciality Providers",
      term == "N_total_providers" ~ "Number of Total Providers"
    ),
    term = as.factor(term)
  )

saveRDS(provider, file = "clean_data/provider_regression.RDS")
saveRDS(provider, file = "shiny/data/provider_regression.RDS")


patient <- aco %>%
  rep_sample_n(size = nrow(aco),
               reps = 100,
               replace = TRUE) %>%
  group_by(replicate) %>%
  nest() %>%
  mutate(mod = map(
    data,
    ~ lm(
      Sav_rate ~ perc_Age_65_74 + perc_Age_75_84 + perc_Age_85plus + perc_male,
      data = .
    )
  ),
  reg_results = map(mod, ~ tidy(., conf.int = TRUE))) %>%
  unnest(reg_results) %>%
  select(-data,-mod) %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "perc_Age_65_74" ~ "% Beneficiaries Age 65-74",
      term == "perc_Age_75_84" ~ "% Beneficiaries Age 75-84",
      term == "perc_Age_85plus" ~ "% Beneficiaries Age 85 +",
      term == "perc_male" ~ "% Beneficiaries Male",
    ),
    term = as.factor(term)
  )


saveRDS(patient, file = "clean_data/patient_regression.RDS")
saveRDS(patient, file = "shiny/data/patient_regression.RDS")


# County Dataset ----------------------------------------------------------

county <-
  
  # Read in CSV data
  read.csv("data/2018_County-level_FFS_Data_for_Shared_Savings_Program_Benchmark_PUF.csv") %>%
  
  # Clean column headers
  clean_names() %>%
  
  # Tidy data into longer format
  pivot_longer(
    cols = per_capita_esrd_expenditures:aged_non_dual_person_years,
    names_to = "Names",
    values_to = "Values"
  ) %>%
  
  # Create new columns in more readable format
  mutate(
    Beneficiary = case_when(
      str_detect(Names, "disabled") ~ "Disabled",
      str_detect(Names, "esrd") ~ "End Stage Renal Disease",
      str_detect(Names, "aged_dual") ~ "Aged Dual",
      str_detect(Names, "aged_non_dual") ~ "Aged Non-Dual"
    ),
    Category = case_when(
      str_detect(Names, "per_capita") ~ "Per Capita Expenditures",
      str_detect(Names, "risk_score") ~ "Average HCC Risk Score",
      str_detect(Names, "person_years") ~ "Person Years"
    )
  ) %>%
  
  mutate(
    Values = as.numeric(as.character(Values))
  ) %>%
  
  # Re-order column names
  select(-Names)


# Add FIPS codes to the counties
fips <- read.csv("data/FIPS.csv") %>% 
  
  # Ensure that FIPS codes are legnth 5
  mutate(FIPS = str_pad(FIPS, width = 5, pad = "0")) %>%
  
  # Match state abbreviation to state name
  mutate(state_name = state.name[match(x = State, state.abb)]) %>%
  
  # Rename columns to match table to merge
  rename("county_name" = Name,
         "fips" = FIPS)

# Merging FIPS to county data
county <- county %>% 
  left_join(fips, by = c("state_name", "county_name")) %>%
  select(-state_id, -county_id)

saveRDS(county, file = "clean_data/county.RDS")
saveRDS(county, file = "shiny/data/county.RDS")


