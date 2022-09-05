# Compute bins ------------------------------------------------------------

bin_vector_rounded <- function(x, bins, weight = NULL, pad = FALSE) {
  if (!ggplot2:::is_bins(bins)) {
    cli::cli_abort("{.arg bins} must be a {.cls ggplot2_bins} object")
  }
  
  if (all(is.na(x))) {
    return(ggplot2:::bin_out(length(x), NA, NA, xmin = NA, xmax = NA))
  }
  
  if (is.null(weight)) {
    weight <- rep(1, length(x))
  } else {
    weight[is.na(weight)] <- 0
  }
  
  bin_idx <- cut(x, bins$fuzzy, right = bins$right_closed,
                 include.lowest = TRUE)
  bin_count <- as.numeric(tapply(weight, bin_idx, sum, na.rm = TRUE))
  bin_count[is.na(bin_count)] <- 0
  bin_count <- roundmid_any(bin_count, to = 7)
  
  bin_x <- (bins$breaks[-length(bins$breaks)] + bins$breaks[-1]) / 2
  bin_widths <- diff(bins$breaks)
  
  # Pad row of 0s at start and end
  if (pad) {
    bin_count <- c(0, bin_count, 0)
    
    width1 <- bin_widths[1]
    widthn <- bin_widths[length(bin_widths)]
    
    bin_widths <- c(width1, bin_widths, widthn)
    bin_x <- c(bin_x[1] - width1, bin_x, bin_x[length(bin_x)] + widthn)
  }
  
  # Add row for missings
  if (any(is.na(bins))) {
    bin_count <- c(bin_count, sum(is.na(bins)))
    bin_widths <- c(bin_widths, NA)
    bin_x <- c(bin_x, NA)
  }
  
  ggplot2:::bin_out(bin_count, bin_x, bin_widths)
}
