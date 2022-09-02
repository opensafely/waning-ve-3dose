library(tidyverse)

fs::dir_create(here::here("output", "vax-dates"))
 
source(here::here("analysis", "functions", "utility_functions.R"))
source(here::here("analysis", "functions", "ggplot2-extensions", "utilities.R"))
source(here::here("analysis", "functions", "ggplot2-extensions", "bin.R"))
source(here::here("analysis", "functions", "ggplot2-extensions", "stat-bin.R"))

# test StatBinRounded
# tibble(x = runif(n=1000, min=1, max=10)) %>%
#   ggplot(aes(x)) +
#   geom_freqpoly(
#     stat = StatBin,
#     binwidth = 1,
#     colour = "red"
#   ) +
#   geom_freqpoly(
#     stat = StatBinRounded,
#     binwidth = 1,
#     colour = "blue"
#   )


data_eligible_a <- readr::read_rds(here::here("output", "data", "data_eligible_a.rds")) %>%
  select(patient_id, jcvi_group, elig_date, region, ethnicity) %>%
  droplevels()

data_vax_wide <- readr::read_rds(here::here("output", "data", "data_wide_vax_dates.rds"))

regions <- levels(data_eligible_a$region)
regions_trunc <- str_trunc(regions, width = 12, side = "right")

plot_data <- data_eligible_a %>%
  select(patient_id, jcvi_group, elig_date, region) %>%
  mutate(across(region, factor, levels = regions, labels = regions_trunc)) %>%
  left_join(
    data_vax_wide,
    by = "patient_id"
  ) %>%
  filter(if_any(starts_with("covid_vax"), ~!is.na(.x))) %>%
  pivot_longer(
    cols = starts_with("covid_vax"),
    names_to = c("sequence", ".value"),
    names_pattern = "covid_vax_(.)_(.*)",
    values_drop_na = TRUE
  ) %>%
  filter(brand != "unknown", sequence < 4) %>%
  rename(dose = sequence)

elig_dates <- sort(unique(plot_data$elig_date))

plot_vax_dates <- function(elig_date_select) {
  
  elig_date_select <- as.Date(elig_date_select)
  
  plot_data <- plot_data %>%
    filter(elig_date %in% elig_date_select) %>%
    droplevels()
  
  end_date <- elig_date_select + lubridate::years(1)
  
  plot_out <- plot_data %>%
    ggplot(aes(x = date, colour = brand, linetype = dose)) +
    geom_freqpoly(
      stat = StatBinRounded,
      binwidth = 1
    ) +
    facet_grid(region~.) +
    scale_x_date(
      name = "vaccination date",
      date_breaks = "2 months",
      limits = c(elig_date_select, end_date)
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 8),# angle = 45, vjust = 0.5),
      axis.title.x = margin ###
      text = element_text(size = 10)
    )
  
  ggsave(
    filename = here::here("output", "vax-dates", glue::glue("vax-dates-{elig_date_select}.svg")),
    height = 20, width = 15, units = "cm"
  )
  
}

plot_vax_dates(elig_dates[1])



