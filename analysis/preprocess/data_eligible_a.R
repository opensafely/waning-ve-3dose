################################################################################

# This script:
# - reads the processed data
# - applies initial eligibility criteria

################################################################################

## setup
library(tidyverse)

## source functions
source(here::here("analysis", "functions", "utility_functions.R"))

# read processed covariates data
data_processed <- readr::read_rds(
  here::here("output", "data", "data_processed.rds")) 

# read processed vaccination data
data_vax_wide <- readr::read_rds(
  here::here("output", "data", "data_wide_vax_dates.rds")) 

data_vax_long <- data_vax_wide %>%
  pivot_longer(
    cols = starts_with("covid_vax"),
    names_to = c("dose", ".value"),
    names_pattern = "covid_vax_(.)_(.*)",
    values_drop_na = TRUE # removes missing doses
  ) %>%
  mutate(across(dose, as.integer))

# count the number of patients in the extracted data
eligibility_count <- tribble(
  ~description, ~n, ~stage,
  "Extracted using study_definition", n_distinct(data_processed$patient_id), "a-in"
)

################################################################################
# create folder for metadata and flow data
fs::dir_create(here::here("output", "lib"))
fs::dir_create(here::here("output", "flow"))

################################################################################
cat("#### apply exclusion criteria from box a to processed data ####\n")
# remove dummy jcvi_group
data_eligible_a <- data_processed %>%
  filter(age_2 >= 18)

eligibility_count <- eligibility_count %>%
  add_row(
    description = "Samples with age_2 < 18 removed",
    n =  n_distinct(data_eligible_a$patient_id),
    stage = "a-in"
  )

# remove if aged over 120 (to avoid probable errors)
data_eligible_a <- data_eligible_a %>%
  filter(age_2 <= 120)

eligibility_count <- eligibility_count %>%
  add_row(
    description = "Samples with age_2 > 120 removed.",
    n =  n_distinct(data_eligible_a$patient_id),
    stage = "a-ex"
  )

# remove if any missing data for key variables
data_eligible_a <- data_eligible_a %>%
  filter(
    !is.na(sex),
    !is.na(region),
    !is.na(ethnicity),
    !is.na(imd)
    )

eligibility_count <- eligibility_count %>%
  add_row(
    description = "Samples with missing ethnicity, sex, imd, region removed.",
    n =  n_distinct(data_eligible_a$patient_id),
    stage = "a-ex"
  )

# carehome
data_eligible_a <- data_eligible_a %>%
  filter(
    ! (!is.na(longres_date) & (longres_date <= elig_date + lubridate::days(42))),
    jcvi_group != "01"
    )

eligibility_count <- eligibility_count %>%
  add_row(
    description = "Samples with record of being in care home removed.",
    n =  n_distinct(data_eligible_a$patient_id),
    stage = "a-ex"
  )

# housebound
data_eligible_a <- data_eligible_a %>%
  filter(!housebound)

eligibility_count <- eligibility_count %>%
  add_row(
    description = "Samples with record of being currently housebound removed.",
    n =  n_distinct(data_eligible_a$patient_id),
    stage = "a-ex"
  )

# duplicate those with brands for doses 1-3
remove_duplicates <- data_vax_long %>%
  filter(brand %in% "duplicate" & dose <= 3) %>%
  distinct(patient_id)

data_eligible_a <- data_eligible_a %>%
  anti_join(remove_duplicates, by = "patient_id") 

eligibility_count <- eligibility_count %>%
  add_row(
    description = "Samples with a record of >1 vaccine brands on one date for doses 1-3 removed.",
    n =  n_distinct(data_eligible_a$patient_id),
    stage = "a-ex"
  )

# remove those with unknown brand for doses 1-3
remove_unknowns <- data_vax_long %>%
  filter(brand %in% "unknown") %>%
  distinct(patient_id)

data_eligible_a <- data_eligible_a %>%
  anti_join(remove_unknowns, by = "patient_id") 

eligibility_count <- eligibility_count %>%
  add_row(
    description = "Samples with a record of unknown vaccine brand for doses 1-3 removed.",
    n =  n_distinct(data_eligible_a$patient_id),
    stage = "a-ex"
  )

# number of people removed at each stage
eligibility_count <- eligibility_count %>%
  # round 
  mutate(across(n, ~roundmid_any(.x, to = 7))) %>%
  mutate(n_removed = lag(n) - n)

# save data from eligible individuals
readr::write_rds(data_eligible_a %>%
                   select(patient_id, jcvi_group, elig_date, region, ethnicity) %>%
                   droplevels(),
                 here::here("output", "data", "data_eligible_a.rds"),
                 compress="gz")

# save eligibility count data
readr::write_csv(
  eligibility_count,
  here::here("output", "flow", "eligibility_count_a.csv"))

# ################################################################################
# jcvi_group, elig_date combos ----
fix_age <- data_processed %>%
  mutate(age = if_else(
    jcvi_group %in% c("10", "11", "12"),
    age_2,
    age_1)) %>%
  select(patient_id, age)

group_age_ranges <- data_eligible_a %>%
  left_join(fix_age,
            by = "patient_id") %>%
  group_by(jcvi_group, elig_date) %>%
  summarise(min = min(age), max = max(age), .groups = "keep") %>%
  ungroup()

# check none of the min / max correspond to < 5 individuals
check <- function(x) {
  fix_age %>%
    filter(age %in% x) %>%
    distinct(patient_id) %>%
    nrow(.)
}

group_age_ranges <- group_age_ranges %>%
  mutate(
    check_min = sapply(
      group_age_ranges$min,
      check),
    check_max = sapply(
      group_age_ranges$max,
      check)
  ) %>%
  mutate(age_range = case_when(
    max > 80 ~ glue::glue("{min}+"),
    check_min < 5 | check_max < 5 ~ "[REDACTED]",
    TRUE ~ glue::glue("{min} - {max}")
  )) %>%
  mutate(across(age_range, as.character)) %>%
  select(-ends_with(c("min", "max")))

# save as csv so that it can be checked
readr::write_csv(group_age_ranges,
                 here::here("output", "lib", "group_age_ranges.csv"))
