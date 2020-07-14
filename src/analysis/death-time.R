source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw.R")
defaultW <- getOption("warn") 
options(warn = -1) 

dtf <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive"));
time_of_death <- as.numeric(dtf$FECHA_DEF - dtf$FECHA_SINTOMAS); time_of_death <- time_of_death[-which(is.na(time_of_death))];

n <- length(time_of_death); B <- 10^4; mean_vect <- rep(0, B)
for(i in 1:B)
{
  ind = sample(1:n, n, replace = TRUE)
  mean_vect[i] = mean(time_of_death[ind])
}
# Wald-type 95% Confidence Interval for the Mean Statistic;
SE_boot_mean <- sd(mean_vect); 
cat("99% Bootstrap CI: ", mean(time_of_death) + c(-1, 1) * 2.58 * SE_boot_mean, " (Blue)\n99th Quantile:    ", quantile(mean_vect, 0.99), "          (Red)");
hist(mean_vect, breaks = 400, main = "Boostrap Sampling for Time of Death from Sars-Cov-19 since first Symptoms", xlab = "Mean (in days)"); 
abline(v = mean(mean_vect), col = "dodgerblue3", lwd = 3);
abline(v = mean(time_of_death) + c(-1) * 2.58 * SE_boot_mean, col = "dodgerblue3", lwd = 3, lty = 2);
abline(v = mean(time_of_death) + c( 1) * 2.58 * SE_boot_mean, col = "dodgerblue3", lwd = 3, lty = 2);
abline(v = quantile(mean_vect, 0.99), col = "tomato3", lwd = 3, lty = 2);

options(warn = defaultW)
rm(dtf, swabsraw, B, i, ind, mean_vect, n, SE_boot_mean, time_of_death, defaultW);