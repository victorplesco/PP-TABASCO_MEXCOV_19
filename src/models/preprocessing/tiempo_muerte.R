source("~/TABASCO-MEXCOV-19/src/packages/install.packages.R");
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw_0718.R");

# defaultW <- getOption("warn") 
# options(warn = -1) 
# options(warn = defaultW)

tmp <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive"));
TIEMPO_MUERTE <- as.numeric(tmp$FECHA_DEF - tmp$FECHA_SINTOMAS); TIEMPO_MUERTE <- TIEMPO_MUERTE[-c(which(is.na(TIEMPO_MUERTE)), which(TIEMPO_MUERTE <= 0))]; rm(tmp);

#################################################################################################################################################################################################################################################################################
## TIEMPO_MUERTE | Estimation ######################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

descdist(TIEMPO_MUERTE, boot = 100, discrete = FALSE);

# Possible methods: “mle”, “mme”, “qme”, “mge”, “mse”; 
fit.lognorm <- fitdist(TIEMPO_MUERTE, "lnorm", method = c("mge"));
fit.weibull <- fitdist(TIEMPO_MUERTE, "weibull", method = c("mse"));


exp(fit.lognorm$estimate[1] + fit.lognorm$estimate[2]^2/2)
hist(log(TIEMPO_MUERTE))
hist(TIEMPO_MUERTE)
mean(log(TIEMPO_MUERTE))


ggplot() +
  
  # TIEMPO_MUERTE;
  geom_histogram(aes(x = TIEMPO_MUERTE, y = ..density..),
            col = "black",
            fill = "white",
            bins = 50) +
  
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
