source("~/TABASCO-MEXCOV-19/src/packages/install-packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw_0719.R")

# defaultW <- getOption("warn") 
# options(warn = -1) 
# options(warn = defaultW)

tmp <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive"));
DEATH_TIME <- as.numeric(tmp$FECHA_DEF - tmp$FECHA_SINTOMAS); DEATH_TIME <- DEATH_TIME[-which(is.na(DEATH_TIME))]; rm(tmp);
                 
#################################################################################################################################################################################################################################################################################
## Bootstrap - PROBABLY OVERFITTED ##############################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

n <- length(DEATH_TIME); B <- 10^4; theta_vect <- rep(0, B);
for(i in 1:B)
{
  ind = sample(1:n, n, replace = TRUE);
  theta_vect[i] = mean(DEATH_TIME[ind]);
}
# Wald-type 95% Confidence Interval for the Mean Statistic;
SE_boot <- sd(theta_vect); 
cat("99% Bootstrap CI: ", mean(DEATH_TIME) + c(-1, 1) * 2.58 * SE_boot, " (Blue)\n99th Quantile:    ", quantile(theta_vect, 0.99), "          (Red)");
hist(theta_vect, breaks = 400, main = "Boostrap Sampling for Time of Death from Sars-Cov-19 since first Symptoms", xlab = "Mean (in days)"); 
abline(v = mean(theta_vect), col = "dodgerblue3", lwd = 3);
abline(v = mean(DEATH_TIME) + c(-1) * 2.58 * SE_boot, col = "dodgerblue3", lwd = 3, lty = 2);
abline(v = mean(DEATH_TIME) + c( 1) * 2.58 * SE_boot, col = "dodgerblue3", lwd = 3, lty = 2);
abline(v = quantile(theta_vect, 0.99), col = "tomato3", lwd = 3, lty = 2); rm(B, i, ind, theta_vect, n, SE_boot); # 12;

#################################################################################################################################################################################################################################################################################
## Bayesian Update - DEATH_TIME #################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

{
  rm(.Random.seed, envir=globalenv());
  par(mfrow = c(3, 2));
  iterations <- 100; counter <- 0; 
  param_a <- 40; param_b <- 20; 
  domain <- round(qt(seq(0.01, 0.99, 0.01), param_a, param_b)); prior <- dt(domain, param_a, param_b);
  for(i in 1:iterations)
  {
    # SAMPLE;
    ind  <- sample(1:length(DEATH_TIME), 5, replace = FALSE);
    data <- DEATH_TIME[i];
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
    };
  };
}; rm(counter, data, density_posterior_theta, density_prior_theta, domain, domain_theta, i, ind, iterations, j, lik, n_plot, param_a, param_b, prior, tmp);

par(mfrow = c(1, 1));
plot(density(DEATH_TIME), main = "Histogram of Time of Death from Sars-Cov-19 since first Symptoms", xlab = "Time of Death (in days)");
curve(dchisq(x + 10, 20), col = "tomato3", lwd = 3, add = TRUE); # 22;

#################################################################################################################################################################################################################################################################################
## Histogram - DEATH_TIME #######################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

ggplot() +
  
  # DEATH_TIME DENSITY;
  geom_line(aes(x = density(DEATH_TIME)$x, y = density(DEATH_TIME)$y),
               col = "tomato3",
               size = 0.75) +
  
  # CHI-SQUARED(20) DENSITY;
  geom_line(aes(x = qchisq(seq(0.0001, 0.9999999999999, length = 1000), 20) - 9.5, y = dchisq(qchisq(seq(0.0001, 0.999999999999, length = 1000), 20), 20)),
            col = "black",
            size = 0.75) + 
  
  # 95-th / 83-th PERCENTILE;
  geom_line(aes(x = rep(quantile(DEATH_TIME, 0.95), 2), y = c(0, 0.067)),
            col = "black",
            size = 1.05,
            linetype = "dashed") +
  
  # Custom Label
  geom_text(aes(x = 35.5, y = 0.066),  label = "95-th Percentile of Observed Data", size = 5) +
  geom_text(aes(x = 32.45, y = 0.063),  label = expression(paste("98-th Percentile of  ", chi[20])), size = 5) +
  
  geom_line(aes(x = c(46, 47), y = rep(0.066, 2)), size = 2, col = "tomato3") +
  geom_line(aes(x = c(46, 47), y = rep(0.063, 2)), size = 2, col = "black") +
  
  labs(title = "Histogram of Decease Time from Sars-Cov-19 since first Symptoms",
       subtitle = "",
       x = "Decease Time (in days)",
       y = "Density") +
  theme_bw(base_size = 17.5, base_family = "Times");
        