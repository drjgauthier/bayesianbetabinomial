#Prop_model function by Rasmus Baath http://www.sumsar.net/blog/2018/12/visualizing-the-beta-binomial/
prop_model <- function(data = c(), prior_prop = c(1, 1), n_draws = 10000) {
  library(tidyverse)
  data <- as.logical(data)
  # data_indices decides what densities to plot between the prior and the posterior
  # For 20 datapoints and less we're plotting all of them.
  data_indices <- round(seq(0, length(data), length.out = min(length(data) + 1, 20)))
  
  # dens_curves will be a data frame with the x & y coordinates for the 
  # denities to plot where x = proportion_success and y = probability
  proportion_success <- c(0, seq(0, 1, length.out = 100), 1)
  dens_curves <- map_dfr(data_indices, function(i) {
    value <- ifelse(i == 0, "Prior", ifelse(data[i], "Success", "Failure"))
    label <- paste0("n=", i)
    probability <- dbeta(proportion_success,
                         prior_prop[1] + sum(data[seq_len(i)]),
                         prior_prop[2] + sum(!data[seq_len(i)]))
    probability <- probability / max(probability)
    data_frame(value, label, proportion_success, probability)
  })
  # Turning label and value into factors with the right ordering for the plot
  dens_curves$label <- fct_rev(factor(dens_curves$label, levels =  paste0("n=", data_indices )))
  dens_curves$value <- factor(dens_curves$value, levels = c("Prior", "Success", "Failure"))
  
  p <- ggplot(dens_curves, aes(x = proportion_success, y = label,
                               height = probability, fill = value)) +
    ggridges::geom_density_ridges(stat="identity", color = "white", alpha = 0.8,
                                  panel_scaling = FALSE, size = 1) +
    scale_y_discrete("", expand = c(0.01, 0)) +
    scale_x_continuous("Posterior probablity of response") +
    scale_fill_manual(values = hcl(120 * 2:0 + 15, 100, 65), name = "", drop = FALSE,
                      labels =  c("Prior   ", "Response   ", " No Response   ")) +
    ggtitle(paste0(
      "Binomial model - Data: ", sum(data),  " responses, " , sum(!data), " failures")) +
    theme_light() +
    theme(legend.position = "top")
  print(p)
  
  # Returning a sample from the posterior distribution that can be further 
  # manipulated and inspected
  posterior_sample <- rbeta(n_draws, prior_prop[1] + sum(data), prior_prop[2] + sum(!data))
  invisible(posterior_sample)
}