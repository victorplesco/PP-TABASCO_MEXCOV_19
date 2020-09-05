source("~/TABASCO-MEXCOV-19/src/packages/install.packages.R");
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw_0718.R");

tmp <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive"));
TIEMPO_MUERTE <- as.numeric(tmp$FECHA_DEF - tmp$FECHA_SINTOMAS); TIEMPO_MUERTE <- TIEMPO_MUERTE[-c(which(is.na(TIEMPO_MUERTE)), which(TIEMPO_MUERTE <= 0))]; rm(tmp);

#################################################################################################################################################################################################################################################################################
## TIEMPO_MUERTE | fitdistrplus #################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

descdist(TIEMPO_MUERTE, boot = 100, discrete = FALSE);

# Possible methods: “mle”, “mme”, “qme”, “mge”, “mse”; 
mle.fit <- fitdist(TIEMPO_MUERTE, "lnorm", method = c("mle")); mle <- c(mle.fit$estimate[1], mle.fit$estimate[2]); 
mme.fit <- fitdist(TIEMPO_MUERTE, "lnorm", method = c("mme")); mme <- c(mme.fit$estimate[1], mme.fit$estimate[2]); 
mge.fit <- fitdist(TIEMPO_MUERTE, "lnorm", method = c("mge")); mge <- c(mge.fit$estimate[1], mge.fit$estimate[2]); 
mse.fit <- fitdist(TIEMPO_MUERTE, "lnorm", method = c("mse")); mse <- c(mse.fit$estimate[1], mse.fit$estimate[2]); 

plot_dtf <- data.frame(x     = sort(unique(TIEMPO_MUERTE)),
                       CDF   = c(cumsum(as.numeric(table(TIEMPO_MUERTE)))/sum(as.numeric(table(TIEMPO_MUERTE))),
                                 cumsum(dlnorm(unique(TIEMPO_MUERTE), mle.fit$estimate[1], mle.fit$estimate[2])),
                                 cumsum(dlnorm(unique(TIEMPO_MUERTE), mme.fit$estimate[1], mme.fit$estimate[2])),
                                 cumsum(dlnorm(unique(TIEMPO_MUERTE), mge.fit$estimate[1], mge.fit$estimate[2])),
                                 cumsum(dlnorm(unique(TIEMPO_MUERTE), mse.fit$estimate[1], mse.fit$estimate[2]))),
                       Label = c(rep("ECDF", length(sort(unique(TIEMPO_MUERTE)))),
                                 rep("MLE",  length(sort(unique(TIEMPO_MUERTE)))),
                                 rep("MME",  length(sort(unique(TIEMPO_MUERTE)))),
                                 rep("MGE",  length(sort(unique(TIEMPO_MUERTE)))),
                                 rep("MSE",  length(sort(unique(TIEMPO_MUERTE))))));

mse_dtf  <- c(1/length(unique(TIEMPO_MUERTE)) *  
                sum(abs(cumsum(dlnorm(unique(TIEMPO_MUERTE), mle.fit$estimate[1], mle.fit$estimate[2])) +
                       -cumsum(as.numeric(table(TIEMPO_MUERTE)))/sum(as.numeric(table(TIEMPO_MUERTE))))),
              1/length(unique(TIEMPO_MUERTE)) *  
                sum(abs(cumsum(dlnorm(unique(TIEMPO_MUERTE), mme.fit$estimate[1], mme.fit$estimate[2])) +
                       -cumsum(as.numeric(table(TIEMPO_MUERTE)))/sum(as.numeric(table(TIEMPO_MUERTE))))),
              1/length(unique(TIEMPO_MUERTE)) *  
                sum(abs(cumsum(dlnorm(unique(TIEMPO_MUERTE), mge.fit$estimate[1], mge.fit$estimate[2])) +
                       -cumsum(as.numeric(table(TIEMPO_MUERTE)))/sum(as.numeric(table(TIEMPO_MUERTE))))),
              1/length(unique(TIEMPO_MUERTE)) *  
                sum(abs(cumsum(dlnorm(unique(TIEMPO_MUERTE), mse.fit$estimate[1], mse.fit$estimate[2])) +
                       -cumsum(as.numeric(table(TIEMPO_MUERTE)))/sum(as.numeric(table(TIEMPO_MUERTE))))))

ggplot() +
  
  geom_line(data = plot_dtf, aes(x = x, y = CDF, col = Label),
            size = 0.75) +
  
  geom_text(aes(x = 90, y = 0.30), label = paste0("MLE: ", round(mse_dtf[1], 3)), size = 4) +
  geom_text(aes(x = 90, y = 0.25), label = paste0("MME: ", round(mse_dtf[2], 3)), size = 4) +
  geom_text(aes(x = 90, y = 0.20), label = paste0("MGE: ", round(mse_dtf[3], 3)), size = 4) +
  geom_text(aes(x = 90, y = 0.15), label = paste0("MSE: ", round(mse_dtf[4], 3)), size = 4) +
  
  
  scale_color_manual(values = c("black", "tomato3", "forestgreen", "#009dd0", "#f58f3b")) +
  
  # Custom Labels;
  labs(title = "Cumulative Density Functions of Proposed Distributions for Decease Time",
       subtitle = "Bottom-Left: MAE between the ECDF and Proposals",
       x = "Decease Time (in days)",
       y = "Cumulative Density") +
  theme_bw(base_size = 17.5, base_family = "Times") +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "bottom");

#################################################################################################################################################################################################################################################################################
## TIEMPO_MUERTE | Final distribution ###########################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################
  
ggplot() +
  
  # TIEMPO_MUERTE;
  geom_histogram(aes(x = TIEMPO_MUERTE, y = ..density..),
            col = "black",
            fill = "white",
            bins = 50) +
  
  # LOG-NORMAL
  geom_line(aes(x = qlnorm(seq(0.0000001, 0.9995, length = 10000), mme.fit$estimate[1], mme.fit$estimate[2]), 
                y = dlnorm(qlnorm(seq(0.00001, 0.9995, length = 10000), mme.fit$estimate[1], mme.fit$estimate[2]), 
                                                                        mme.fit$estimate[1], mme.fit$estimate[2])),
            col = "#009dd0",
            size = 0.75) +
  
  geom_line(aes(x = rep(qlnorm(0.99, mme.fit$estimate[1], mme.fit$estimate[2]), 2), y = c(0, 0.065)),
            size = 0.75,
            col  = "#009dd0",
            linetype = "dashed") +
  geom_text(aes(x = 44, y = 0.0625), label = "99-th Percentile", size = 4) +
  
  # Custom Labels;
  labs(title = "Histogram of Decease Time from Sars-Cov-19 since first Symptoms",
       subtitle = "",
       x = "Decease Time (in days)",
       y = "Density") +
  theme_bw(base_size = 17.5, base_family = "Times");
