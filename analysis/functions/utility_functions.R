library(tidyverse)

fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

ceiling_any <- function(x, to=1){
  # round to nearest 100 millionth to avoid floating point errors
  ceiling(plyr::round_any(x/to, 1/100000000))*to
}

roundmid_any <- function(x, to=1){
  # like ceiling_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}

my_skim <- function(
  .data, # dataset to be summarised
  id_var = "patient_id" # name of id column (set to NULL if no id column)
) {
  
  # specify summary function for each class
  my_skimmers <- list(
    logical = skimr::sfl(
    ),
    # numeric applied to numeric and integer
    numeric = skimr::sfl(
      mean = mean,
      sd = sd,
      min = ~ min(.x, na.rm=TRUE),
      p10 = ~ quantile(.x, p=0.1, na.rm=TRUE, type=1),
      p25 = ~ quantile(.x, p=0.25, na.rm=TRUE, type=1),
      p50 = ~ quantile(.x, p=0.5, na.rm=TRUE, type=1),
      p75 = ~ quantile(.x, p=0.75, na.rm=TRUE, type=1),
      p90 = ~ quantile(.x, p=0.9, na.rm=TRUE, type=1),
      max = ~ max(.x, na.rm=TRUE)
    ),
    character = skimr::sfl(),
    factor = skimr::sfl(),
    Date = skimr::sfl(
      min = ~ min(.x, na.rm=TRUE),
      p50 = ~ quantile(.x, p=0.5, na.rm=TRUE, type=1),
      max = ~ max(.x, na.rm=TRUE)
    ),
    POSIXct = skimr::sfl(
      min = ~ min(.x, na.rm=TRUE),
      p50 = ~ quantile(.x, p=0.5, na.rm=TRUE, type=1),
      max = ~ max(.x, na.rm=TRUE)
    )
  )
  
  my_skim_fun <- skimr::skim_with(
    !!!my_skimmers,
    append = FALSE
  )
  
  # summarise factors as the printing is not very nice or flexible in skim
  summarise_factor <- function(var) {
    
    .data %>%
      group_by(across(all_of(var))) %>%
      count() %>%
      ungroup() %>%
      # mutate(across(n, ~roundmid_any(.x, to = 7))) %>% # comment out as not releasing
      mutate(percent = round(100*n/sum(n),2)) %>%
      knitr::kable(format = "pipe") %>% 
      print()
    
  }
  
  vars <- .data %>% 
    select(-all_of(id_var)) %>% 
    select(where(~ is.factor(.x) | is.character(.x))) %>%
    names()
  
  data_name <- deparse(substitute(.data))
  
  options(width = 120)
  capture.output(
    {
      print(my_skim_fun(.data, -all_of(id_var)))
      cat("\n")
      cat("--- counts for factor and character variables ---")
      for (v in vars) {
        summarise_factor(v)
      }
    },
    file = here::here("output", "summaries", glue::glue("{data_name}.txt")),
    append = FALSE
  )
    
}

