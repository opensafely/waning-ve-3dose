######################################

# This script:
# - creates dummy data for study_definition.py

######################################

library(tidyverse)
# library(lubridate)
# library(glue)

# load metadata
study_parameters <- readr::read_rds(here::here("analysis", "lib", "study_parameters.rds"))

set.seed(study_parameters$seed)

n <- study_parameters$n

K <- study_parameters$K

start_date <- study_parameters$start_date

regions <- readr::read_csv(here::here("analysis", "lib", "regions.csv"))

# load dummy data functions
source(here::here("analysis", "functions", "dummy_data_functions.R"))

# date vars 
# set these to have occured since start of pandemic
date_vars_recent <- c(
  # "positive_test_0_date", 
  # "primary_care_covid_case_0_date", 
  # "covidadmitted_0_date",
  # "covidemergency_0_date",
  "death_date",
  "longres_date",
  "endoflife_date", 
  "midazolam_date",
  # "coviddeath_date", 
  "dereg_date"
  )

jcvi_group_patterns <- readr::read_csv(here::here("analysis", "lib", "jcvi_groups.csv")) %>%
  mutate(across(definition, ~str_extract(.x, "age_. >=\\d{2}"))) %>%
  # add dummy conditions for groups 1, 4b and 6, as longres and atrisk data not available here (done correctly in real data)
  # will fix later in this script
  mutate(across(definition, ~case_when(group == "01" ~ "age_1 >=110", 
                                       group %in% c("04b","06") ~ "age_1 >=120",
                                       !is.na(.x) ~ .x,
                                       TRUE ~ "TRUE")))

# conditions for eligibility dates
elig_date_patterns <- readr::read_csv(here::here("analysis", "lib", "elig_dates.csv")) %>%
  mutate(across(description, ~str_replace_all(.x, "p=", "p=="))) %>%
  mutate(across(description, ~str_replace_all(.x, "OR", "|"))) %>%
  mutate(across(description, ~str_replace_all(.x, "AND", "&"))) %>%
  mutate(across(description, ~str_replace_all(.x, "DEFAULT", "TRUE")))

dummy_data_elig <- tibble(patient_id = 1:n) %>%
  mutate(age_1 = as.integer(runif(nrow(.), 16, 90), 0),
         age_2 = age_1,
         imd = sample(
           x = c("Unknown", "1 most deprived", as.character(2:4), "5 least deprived"),
           prob = c(0.01, rep(0.99/5, 5)),
           size = n,
           replace = TRUE)) %>%
  var_category(sex, categories = c("F", "M")) %>%
  var_category(ethnicity_6, 
               categories = c(as.character(1:5), NA_character_),
               ratios = c(rep(0.99/5,5), 0.01)) %>%
  var_category(ethnicity_6_sus, 
               categories = c(as.character(1:5), NA_character_),
               ratios = c(rep(0.99/5,5), 0.01)) %>%
  var_category(region, categories = regions$region, ratios = regions$ratio) %>%
  # binary vars for exclusion criteria
  mutate(hscworker = rbernoulli(n = nrow(.), p=0.01)) %>%
  mutate(housebound = rbernoulli(n = nrow(.), p=0.01)) %>%
  # jcvi_group
  var_category(
    name = jcvi_group, 
    categories = jcvi_group_patterns$group, 
    conditions = jcvi_group_patterns$definition
    ) %>%
  mutate(
    cev = rbernoulli(n=nrow(.), p=0.1),
    atrisk = rbernoulli(n=nrow(.), p=0.2)
    ) %>%
  mutate(across(jcvi_group, 
                ~ case_when(
                  age_1 >= 16 & age_1 < 70 & cev ~ "04b",
                  age_1 >= 16 & age_1 < 65 & atrisk ~ "06",
                  TRUE ~ .x
                ))) %>%
  select(-atrisk, -cev) %>%
  # elig_date
  var_category(
    name = elig_date,
    categories = elig_date_patterns$date,
    conditions = elig_date_patterns$description) 

# fix vaccine dates so that they have roughly correct distribution
dummy_data_vax <- dummy_data_elig %>%
  select(patient_id, elig_date) %>%
  mutate(
    covid_vax_1_date = as.Date(elig_date) + lubridate::days(round(rnorm(nrow(.), mean = 2*7, sd = 3))),
    covid_vax_2_date = covid_vax_1_date + lubridate::days(round(rnorm(nrow(.), mean = 10*7, sd = 3))),
    covid_vax_3_date = covid_vax_2_date + lubridate::days(round(rnorm(nrow(.), mean = 28*7, sd = 3))),
    covid_vax_4_date = covid_vax_3_date + lubridate::days(round(rnorm(nrow(.), mean = 28*7, sd = 3))),
    covid_vax_1_brand = sample(
      x = c(NA_character_, "disease", "pfizer", "az", "moderna"), 
      size = nrow(.),
      replace = TRUE,
      prob = c(0.28, 0.01, 0.35, 0.35, 0.01)
      ),
    covid_vax_2_brand = if_else(covid_vax_1_brand=="disease",NA_character_,covid_vax_1_brand),
    covid_vax_3_brand = sample(
      x = c(NA_character_, "pfizer", "az", "moderna"), 
      size = nrow(.),
      replace = TRUE,
      prob = c(0.09, 0.45, 0.01, 0.45)
    ),
    covid_vax_4_brand = sample(
      x = c(NA_character_, "pfizer", "az", "moderna"), 
      size = nrow(.),
      replace = TRUE,
      prob = c(0.89, 0.05, 0.01, 0.05)
    )
  ) %>%
  mutate(across(c(covid_vax_2_brand, covid_vax_3_brand, covid_vax_4_brand),
                ~ if_else(
                  is.na(covid_vax_1_brand),
                  NA_character_,
                  .x
                ))) %>%
  select(-elig_date)

dummy_data_vax_long <- dummy_data_vax %>%
  pivot_longer(
    cols = -patient_id,
    names_to = c("sequence", ".value"),
    names_pattern = "covid_vax_(.)_(.*)",
  ) %>%
  filter(!is.na(brand)) 

# check sequences
# dummy_data_vax_long %>%
#   group_by(patient_id) %>%
#   summarise(brand_sequence = str_c(brand, collapse = ", ")) %>%
#   ungroup() %>%
#   group_by(brand_sequence) %>%
#   count() %>%
#   ungroup() %>%
#   arrange(n) %>%
#   print(n=Inf)

# transform to wide
dummy_data_vax_wide <- dummy_data_vax_long %>%
  pivot_wider(
    names_from = c(brand, sequence),
    values_from = "date",
    names_glue = "covid_vax_{brand}_{sequence}_date"
  ) 

# final dummy data
dummy_data <- dummy_data_elig %>%
  left_join(dummy_data_vax_wide, by = "patient_id") %>%
  # date vars recent
  bind_cols(
    pmap(
      list(a = date_vars_recent, 
           b = rep(0.2, length(date_vars_recent))),
      function(a,b) 
        var_date(
          .data = ., 
          name = !! a,
          incidence = b,
          earliest=study_parameters$pandemic_start,
          latest=study_parameters$end_date,
          keep_vars = FALSE
        ))) %>%
  # add death_date if coviddeath_date
  # mutate(across(death_date, 
  #               ~if_else(
  #                 !is.na(coviddeath_date), 
  #                 coviddeath_date,
  #                 .x))) %>%
  mutate(across(contains("_date"), as.POSIXct)) %>%
  mutate(across(ends_with("date"), as.POSIXct)) %>%
  mutate(across(c(ethnicity_6, ethnicity_6_sus, jcvi_group, region, sex),
                as.factor)) %>%
  droplevels()

arrow::write_feather(dummy_data, here::here("analysis", "dummy_data_vax.feather"))
