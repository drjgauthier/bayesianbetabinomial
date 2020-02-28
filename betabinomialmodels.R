data <- c(0:15)
prop_model(data, prior_prop = c(0.4,0.6))
data <- as.logical(data)
# data_indices decides what densities to plot between the prior and the posterior
# For 20 datapoints and less we're plotting all of them.
data_indices <- round(seq(0, length(data), length.out = min(length(data) + 1, 20)))
prior_prop = c(1, 1)
# dens_curves will be a data frame with the x & y coordinates for the 
# denities to plot where x = proportion_success and y = probability
proportion_success <- c(0, seq(0, 1, length.out = 100), 1)
dens_curves <- map_dfr(data_indices, function(i) {
  prior_prop = c(0.4,0.6)
  value <- ifelse(i == 0, "Prior", ifelse(data[i], "Success", "Failure"))
  label <- paste0("n=", i)
  probability <- dbeta(proportion_success,
                       prior_prop[1] + sum(data[seq_len(i)]),
                       prior_prop[2] + sum(!data[seq_len(i)]))
  probability <- probability / max(probability)
  data_frame(value, label, proportion_success, probability)
})
