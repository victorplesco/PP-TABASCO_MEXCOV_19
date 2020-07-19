source("~/TABASCO-MEXCOV-19/src/packages/install-packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw_0719.R")

# defaultW <- getOption("warn") 
# options(warn = -1) 
# options(warn = defaultW)

tmp <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive"));
time_of_death <- as.numeric(tmp$FECHA_DEF - tmp$FECHA_SINTOMAS); time_of_death <- time_of_death[-which(is.na(time_of_death))]; rm(tmp);

#################################################################################################################################################################################################################################################################################
## Bootstrap - PROBABLY OVERFITTED ##############################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

n <- length(time_of_death); B <- 10^4; mean_vect <- rep(0, B);
for(i in 1:B)
{
  ind = sample(1:n, n, replace = TRUE);
  mean_vect[i] = mean(time_of_death[ind]);
}
# Wald-type 95% Confidence Interval for the Mean Statistic;
SE_boot <- sd(mean_vect); 
cat("99% Bootstrap CI: ", mean(time_of_death) + c(-1, 1) * 2.58 * SE_boot, " (Blue)\n99th Quantile:    ", quantile(mean_vect, 0.99), "          (Red)");
hist(mean_vect, breaks = 400, main = "Boostrap Sampling for Time of Death from Sars-Cov-19 since first Symptoms", xlab = "Mean (in days)"); 
abline(v = mean(mean_vect), col = "dodgerblue3", lwd = 3);
abline(v = mean(time_of_death) + c(-1) * 2.58 * SE_boot, col = "dodgerblue3", lwd = 3, lty = 2);
abline(v = mean(time_of_death) + c( 1) * 2.58 * SE_boot, col = "dodgerblue3", lwd = 3, lty = 2);
abline(v = quantile(mean_vect, 0.99), col = "tomato3", lwd = 3, lty = 2);

#################################################################################################################################################################################################################################################################################
## Bayesian Update - (EDAD, DEATH_TIME) #########################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

par(mfrow = c(1, 1));
plot(density(time_of_death), main = "Histogram of Time of Death from Sars-Cov-19 since first Symptoms", xlab = "Time of Death (in days)");
curve(dchisq(x, 15), col = "tomato3", lwd = 3, add = TRUE);

{rm(.Random.seed, envir=globalenv());
par(mfrow = c(3, 2));
iterations <- 100; counter <- 0; 
param_a <- 10; param_b <- 5; 
domain <- round(qnorm(seq(0.01, 0.99, 0.01), param_a, param_b)); prior <- dnorm(domain, param_a, param_b);
for(i in 1:iterations)
{
  # SAMPLE;
  ind  <- sample(1:length(time_of_death), 5, replace = FALSE);
  data <- time_of_death[i];
  # cat(data, "\n");
  
  # PRIOR;
  if(i == 1)
  {
    # POSTERIOR CONTAINER;
    density_posterior_theta <- c();
    
    # PRIOR DOMAIN;
    domain_theta <- domain;
    
    # PRIOR DENSITY;
    density_prior_theta <- prior;
    
    plot(domain_theta, density_prior_theta, type = "l", lwd = 3, col = "tomato3");
  }
  else
  {density_prior_theta <- density_posterior_theta;}
  
  # LIKELIHOOD;
  lik <- c();
  for(j in 1:length(domain_theta))
  {lik[j] = sum(log(dpois(data, domain_theta[j])));};
  
  tmp <- c(which(is.na(lik)), which(is.infinite(lik)));
  # cat(tmp, "\n");
  counter = counter + length(tmp);
  
  if(length(tmp) != 0)
  {
    density_prior_theta <- density_prior_theta[-tmp];
    lik <- lik[-tmp];
    domain_theta <- domain_theta[-tmp];
  }

  # POSTERIOR;
  density_posterior_theta <- density_prior_theta * exp(lik/sum(lik)); # P(THETA) * P(X|THETA) / P(X);
  
  n_plot <- round(cumsum(rep(iterations/5, 5)));
  if(i %in% n_plot)
  {
    # PLOT;
    plot(domain_theta,  density_posterior_theta, type = "l", lwd = 3, col = "tomato3"); 
    abline(v = domain_theta[which(density_posterior_theta == max(density_posterior_theta))]);
    axis(side = 1, at = domain_theta[which(density_posterior_theta == max(density_posterior_theta))], 
         labels = domain_theta[which(density_posterior_theta == max(density_posterior_theta))]);
  }
};};

par(mfrow = c(1, 1));
plot(density(time_of_death), main = "Histogram of Time of Death from Sars-Cov-19 since first Symptoms", xlab = "Time of Death (in days)");
curve(dchisq(x, 15), col = "tomato3", lwd = 3, add = TRUE);

#################################################################################################################################################################################################################################################################################
## Bayesian Update - Beta-Binomial Model (FACTORS) ##############################################################################################################################################################################################################################|
#################################################################################################################################################################################################################################################################################

# This function takes a number of successes and failuers coded as a TRUE/FALSE or 0/1 vector. This should be given as the data argument.
# The result is a visualization of the how a Beta-Binomial model gradualy learns the underlying proportion of successes using this data. 
# The function also returns a sample from the posterior distribution that can be further manipulated and inspected. The default prior is 
# a Beta(1,1) distribution, but this can be set using the prior_prop argument.

plot(qbeta(seq(0.01, 0.99, length.out = 100), 1, 1), dbeta(qbeta(seq(0.01, 0.99, length.out = 100), 1, 1), 1, 1), 
     type = "l", col = "tomato3", lwd = 3, xlab = "Proportion", ylab = "Density"); # Prior: Beta(1,1);

prop_model <- function(data = c(), prior_prop = c(1, 1), n_draws = 10000) 
{

  data <- as.logical(data);
  
  data_indices <- round(seq(0, length(data), length.out = min(length(data) + 1, 20)));
  # Decides what densities to plot between the prior and the posterior (for 20 data points and less we're plotting all of them).
  
  proportion_success <- seq(0.01, 0.99, length.out = 100);
  dens_curves <- map_dfr(data_indices, function(i) 
    {
      value <- ifelse(i == 0, "Prior", ifelse(data[i], "Success", "Failure"));
      label <- paste0("n=", i);
      probability <- dbeta(proportion_success,
                           prior_prop[1] + sum(data[seq_len(i)]),
                           prior_prop[2] + sum(!data[seq_len(i)]));
      probability <- probability / max(probability);
      data_frame(value, label, proportion_success, probability);
    }); # Data frame with the x & y coordinates for the denities to plot where x = proportion_success and y = probability;
  
  
  # Turning label and value into factors with the right ordering for the plot;
  dens_curves$label <- fct_rev(factor(dens_curves$label, levels = paste0("n=", data_indices)));
  dens_curves$value <- factor(dens_curves$value, levels = c("Prior", "Success", "Failure"));
  
  p <- ggplot(dens_curves, aes(x = proportion_success, y = label,
                               height = probability, fill = value)) +
    ggridges::geom_density_ridges(stat = "identity", color = "white", alpha = 0.8,
                                  panel_scaling = TRUE, size = 1) +
    scale_y_discrete("", expand = c(0.01, 0)) +
    scale_x_continuous("Underlying proportion of success") +
    scale_fill_manual(values = hcl(120 * 2:0 + 15, 100, 65), name = "", drop = FALSE,
                      labels =  c("Prior   ", "Success   ", "Failure   ")) +
    ggtitle(paste0("Binomial model - Data: ", sum(data),  " successes, " , sum(!data), " failures")) +
    theme_light() +
    theme(legend.position = "top");
  print(p);
  
  # Returning a sample from the posterior distribution that can be further manipulated and inspected;
  posterior_sample <- rbeta(n_draws, prior_prop[1] + sum(data), prior_prop[2] + sum(!data));
  # invisible(posterior_sample);
}

b_data <- sample(c(0, 1), 100, prob = c(0.75, 0.25), replace = TRUE) 
b_data <- ifelse(b_data == 1, TRUE, FALSE);  
prop_model(b_data)  
  
  
swabspos <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive" & DECEASED == "Yes")); swabspos <- swabspos[order(swabspos$FECHA_INGRESO),];
data <- ifelse(as.character(na.omit(swabspos$HIPERTENSION[1:10000])) == "Yes", TRUE, FALSE); posterior <- prop_model(data);

# rm(dtf, swabsraw_07-09, B, i, ind, mean_vect, n, SE_boot_mean, time_of_death, defaultW);