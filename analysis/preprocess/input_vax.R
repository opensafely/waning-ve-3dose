################################################################################

# This script:
# - imports input_vax.R
# - cleans, summarises and saves data for applying eligibility criteria (data_processed.R)
# - cleans, summarises and saves vaccination data (data_vax_wide.R)

################################################################################

## setup
library(tidyverse)

## source functions
source(here::here("analysis", "functions", "utility_functions.R"))

## create folders for outputs
fs::dir_create(here::here("output", "data"))
fs::dir_create(here::here("output", "summaries"))

## import study_parameters
study_parameters <- readr::read_rds(
  here::here("analysis", "lib", "study_parameters.rds"))

## regions
regions <- readr::read_csv(
  here::here("analysis", "lib", "regions.csv")
)

################################################################################
# initial pre-processing
cat("#### extract data ####\n")

data_extract <- arrow::read_feather(file = here::here("output", "input_vax.feather")) 

my_skim(data_extract)

### summarise raw data

cat("#### process extracted data ####\n")
data_processed_0 <- data_extract %>%
  # because date types are not returned consistently by cohort extractor
  mutate(across(c(contains("_date")), 
                ~ lubridate::floor_date(
                  as.Date(., format="%Y-%m-%d"),
                  unit = "days"))) %>%
  # derive ethnicity variable
  mutate(
    # Region
    region = factor(region, levels = regions$region),
    # Ethnicity
    ethnicity = if_else(is.na(ethnicity_6), ethnicity_6_sus, ethnicity_6),
    ethnicity = fct_case_when(
      ethnicity == "1" ~ "White",
      ethnicity == "4" ~ "Black",
      ethnicity == "3" ~ "South Asian",
      ethnicity == "2" ~ "Mixed",
      ethnicity == "5" ~ "Other",
      TRUE ~ NA_character_
    ),
    # IMD quintile
    imd = factor(
      if_else(imd %in% "Unknown", NA_character_, imd)
    ),
    # Sex
    sex = fct_case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male",
      TRUE ~ NA_character_
    ),
    # First recording of end of life care
    endoflife_date = pmin(endoflife_date, midazolam_date, na.rm = TRUE),
    
    #Subgroup
    subgroup = fct_case_when(
      jcvi_group %in% c("04b", "06") & age_1 < 65 ~ "18-64 years and clinically vulnerable",
      jcvi_group %in% c("11", "12") ~ "18-39 years",
      jcvi_group %in% c("07", "08", "09", "10") ~ "40-64 years",
      jcvi_group %in% c("02", "03", "04a", "04b", "05") ~ "65+ years",
      TRUE ~ NA_character_
    )
    
  ) %>%
  select(-ethnicity_6, -ethnicity_6_sus, -midazolam_date) %>%
  droplevels()

################################################################################
# process vaccine data
data_vax_wide <- local({
  
  data_vax_pfizer <- data_processed_0 %>%
    select(patient_id, matches("covid\\_vax\\_pfizer\\_\\d+\\_date")) %>%
    pivot_longer(
      cols = -patient_id,
      names_to = c(NA, "vax_pfizer_index"),
      names_pattern = "^(.*)_(\\d+)_date",
      values_to = "date",
      values_drop_na = TRUE
    ) %>%
    arrange(patient_id, date)
  
  data_vax_az <- data_processed_0 %>%
    select(patient_id, matches("covid\\_vax\\_az\\_\\d+\\_date")) %>%
    pivot_longer(
      cols = -patient_id,
      names_to = c(NA, "vax_az_index"),
      names_pattern = "^(.*)_(\\d+)_date",
      values_to = "date",
      values_drop_na = TRUE
    ) %>%
    arrange(patient_id, date)
  
  data_vax_moderna <- data_processed_0 %>%
    select(patient_id, matches("covid\\_vax\\_moderna\\_\\d+\\_date")) %>%
    pivot_longer(
      cols = -patient_id,
      names_to = c(NA, "vax_moderna_index"),
      names_pattern = "^(.*)_(\\d+)_date",
      values_to = "date",
      values_drop_na = TRUE
    ) %>%
    arrange(patient_id, date)
  
  data_vax_disease <- data_processed_0 %>%
    select(patient_id, matches("covid\\_vax\\_disease\\_\\d+\\_date")) %>%
    pivot_longer(
      cols = -patient_id,
      names_to = c(NA, "vax_disease_index"),
      names_pattern = "^(.*)_(\\d+)_date",
      values_to = "date",
      values_drop_na = TRUE
    ) %>%
    arrange(patient_id, date)
  
  
  data_vax <- data_processed_0 %>% # to get the unvaccinated
    # filter(if_all(starts_with("covid_vax"), ~ is.na(.))) %>%
    filter_at(vars(starts_with("covid_vax")), all_vars(is.na(.))) %>%
    select(patient_id) %>% 
    full_join(
      data_vax_pfizer %>%
        full_join(data_vax_az, by=c("patient_id", "date")) %>%
        full_join(data_vax_moderna, by=c("patient_id", "date")) %>%
        full_join(data_vax_disease, by=c("patient_id", "date")),
      by = "patient_id"
    ) %>%
    mutate(
      brand = fct_case_when(
        (!is.na(vax_az_index)) & is.na(vax_pfizer_index) & is.na(vax_moderna_index) ~ "az",
        is.na(vax_az_index) & (!is.na(vax_pfizer_index)) & is.na(vax_moderna_index) ~ "pfizer",
        is.na(vax_az_index) & is.na(vax_pfizer_index) & (!is.na(vax_moderna_index)) ~ "moderna",
        (!is.na(vax_az_index)) + (!is.na(vax_pfizer_index)) + (!is.na(vax_moderna_index)) > 1 ~ "duplicate",
        !is.na(vax_disease_index) ~ "unknown",
        TRUE ~ NA_character_
      )
    ) %>%
    arrange(patient_id, date) %>%
    group_by(patient_id) %>%
    mutate(
      vax_index=row_number()
    ) %>%
    ungroup() %>%
    droplevels()
  
  data_vax_wide <- data_vax %>%
    pivot_wider(
      id_cols= patient_id,
      names_from = c("vax_index"),
      values_from = c("date", "brand"),
      names_glue = "covid_vax_{vax_index}_{.value}"
    )
  
  data_vax_wide
  
})

my_skim(data_vax_wide)

readr::write_rds(
  data_vax_wide,
  here::here("output", "data", "data_wide_vax_dates.rds"), 
  compress="gz")

###############################################################################
# save data_processed 
# (remove variables that are saved elsewhere)

data_processed <- data_processed_0 %>%
  select(-contains("_vax_"))

my_skim(data_processed)

## add the following to the data processed summary
cat("## check subgroups as desired ##\n")
data_processed_0 %>%
  group_by(subgroup, jcvi_group) %>%
  count() %>%
  ungroup()


readr::write_rds(
  data_processed,
  here::here("output", "data", "data_processed.rds"), 
  compress="gz")
