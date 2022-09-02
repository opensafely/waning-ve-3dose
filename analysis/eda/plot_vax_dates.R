library(tidyverse)

fs::dir_create(here::here("output", "eda"))
 
source(here::here("analysis", "functions", "utility_functions.R"))
source(here::here("analysis", "functions", "ggplot2-extensions", "utilities.R"))
source(here::here("analysis", "functions", "ggplot2-extensions", "bin.R"))
source(here::here("analysis", "functions", "ggplot2-extensions", "stat-bin.R"))

# StatBinRounded:
# rounds all counts up to nearest 7, and center on (integer) midpoint of multiples of 7
# see analysis/functions/ggplot2-extensions

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

# read in data from those satisfying eligibility criteria A
data_eligible_a <- readr::read_rds(here::here("output", "data", "data_eligible_a.rds")) %>%
  select(patient_id, jcvi_group, elig_date, region, ethnicity) %>%
  droplevels()

# read in vaccination data
data_vax_wide <- readr::read_rds(here::here("output", "data", "data_wide_vax_dates.rds"))

# read in JCVI group age ranges
group_age_ranges <- readr::read_csv(here::here("output", "lib", "group_age_ranges.csv"))

# create truncated region labels for plotting
regions <- levels(data_eligible_a$region)
regions_trunc <- str_trunc(regions, width = 12, side = "right")

# prepare data for plotting
plot_data <- data_eligible_a %>%
  select(patient_id, jcvi_group, elig_date, region) %>%
  mutate(across(region, factor, levels = regions, labels = regions_trunc)) %>%
  left_join(
    data_vax_wide,
    by = "patient_id"
  ) %>%
  # remove those with unknown brand (if present will always be in position 1)
  filter(covid_vax_1_brand != "unknown") %>%
  # reshape to long
  pivot_longer(
    cols = starts_with("covid_vax"),
    names_to = c("dose", ".value"),
    names_pattern = "covid_vax_(.)_(.*)",
    values_drop_na = TRUE # removes unvaccinated individuals
  ) %>%
  # relabel brand
  mutate(across(brand,
                factor,
                levels = c("pfizer", "az", "moderna"),
                labels = c("BNT162b2", "ChAdOx1", "mRNA-1273"))) 

# function for plotting vaccination dates
plot_vax_dates <- function(jcvi_group_select, elig_date_select) {
  
  # elig_date_select <- as.Date(elig_date_select)
  # filter data to those who became eligible on elig_date_select
  plot_data <- plot_data %>%
    filter(
      jcvi_group %in% jcvi_group_select,
      elig_date %in% elig_date_select
      ) %>%
    droplevels()

  # age range in plot
  age_range_select <- group_age_ranges %>%
    filter(
      jcvi_group %in% jcvi_group_select,
      elig_date %in% elig_date_select
    )
  age_range_select <- age_range_select$age_range
  
  # clean JCVI group label for plot
  jcvi_group_select_clean <- str_remove(jcvi_group_select, "^0")
  
  # set upper x limit for plot
  xlim_upper <- elig_date_select + lubridate::years(1)
  
  colour_palette <- RColorBrewer::brewer.pal(3, "Dark2")
  names(colour_palette) <- levels(plot_data$brand)
  
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
      limits = c(elig_date_select, xlim_upper)
    ) +
    scale_colour_discrete(
      type = colour_palette
    ) +
    labs(
      subtitle = glue::glue("JCVI group {jcvi_group_select_clean}, eligible on {elig_date_select}, aged {age_range_select} years")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      text = element_text(size = 10)
    )
  
  ggsave(
    filename = here::here("output", "eda", glue::glue("vax_dates_{elig_date_select}.svg")),
    height = 20, width = 15, units = "cm"
  )
  
}

# create plots
for (i in 1:nrow(group_age_ranges)) {
  plot_vax_dates(
    jcvi_group_select = group_age_ranges[i,]$jcvi_group,
    elig_date_select = group_age_ranges[i,]$elig_date
  )
}
