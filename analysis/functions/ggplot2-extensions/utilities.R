# Wrapping vctrs data_frame constructor with no name repair
data_frame0 <- function(...) data_frame(..., .name_repair = "minimal")
# data_frame0 cannot be accessed via ggplot2:::data_frame0