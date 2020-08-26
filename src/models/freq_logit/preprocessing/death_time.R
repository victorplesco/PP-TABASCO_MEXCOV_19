source("~/TABASCO-MEXCOV-19/src/packages/install-packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw_0719.R")

# defaultW <- getOption("warn") 
# options(warn = -1) 
# options(warn = defaultW)

tmp <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive"));
DEATH_TIME <- as.numeric(tmp$FECHA_DEF - tmp$FECHA_SINTOMAS); DEATH_TIME <- DEATH_TIME[-c(which(is.na(DEATH_TIME)), which(DEATH_TIME <= 0))]; rm(tmp);

#################################################################################################################################################################################################################################################################################
## DEATH_TIME | Estimation ######################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

descdist(DEATH_TIME, boot = 100, discrete = FALSE)

# “mle”, “mme”, “qme”, “mge”, “mse”; CHECK FOR ALL;
fit.lognorm <- fitdist(DEATH_TIME, "lnorm", method = c("mge"));
fit.weibull <- fitdist(DEATH_TIME, "weibull", method = c("mge"));

ggplot() +
  
  # DEATH_TIME;
  geom_histogram(aes(x = DEATH_TIME, y = ..density..),
            col = "black",
            fill = "white",
            bins = 100) +
  
  # LOG-NORMAL
  geom_line(aes(x = qlnorm(seq(0.0000001, 0.9995, length = 10000), fit.lognorm$estimate[1], fit.lognorm$estimate[2]), 
                y = dlnorm(qlnorm(seq(0.00001, 0.9995, length = 10000), fit.lognorm$estimate[1], fit.lognorm$estimate[2]), 
                                                                        fit.lognorm$estimate[1], fit.lognorm$estimate[2])),
            col = "tomato3",
            size = 0.75) +
  
  geom_line(aes(x = rep(qlnorm(0.99, fit.lognorm$estimate[1], fit.lognorm$estimate[2]), 2), y = c(0, 0.065)),
            size = 1.25,
            col  = "tomato3") 
  
  # WEIBULL
  geom_line(aes(x = qweibull(seq(0.000000001, 0.9999999, length = 10000), fit.weibull$estimate[1], fit.weibull$estimate[2]), 
                y = dweibull(qweibull(seq(0.0000001, 0.9999999, length = 10000), fit.weibull$estimate[1], fit.weibull$estimate[2]), 
                                                                                 fit.weibull$estimate[1], fit.weibull$estimate[2])),
            col = "dodgerblue3",
            size = 0.75) +
  
  geom_line(aes(x = rep(qweibull(0.99, fit.weibull$estimate[1], fit.weibull$estimate[2]), 2), y = c(0, 0.065)),
            size = 1.25,
            col  = "dodgerblue3") +
  
  # Custom Labels;
  labs(title = "Histogram of Decease Time from Sars-Cov-19 since first Symptoms",
       subtitle = "",
       x = "Decease Time (in days)",
       y = "Density") +
  theme_bw(base_size = 17.5, base_family = "Times");


