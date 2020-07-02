source("~/TABASCO-MEXCOV-19/src/packages/autoinstall.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/dtf_arima.R")


#################################################################################################################################################################################################################################################################################
## TS - Aggregated ##############################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################


ts_a_train <- ts(train_set[, c("aTotal")], frequency = 7);
#### plot(ts_a_train);

dec_a_train <- decompose(ts_a_train, "multiplicative"); 
# plot(dec_a_train)
# boxplot(ts_a_train ~ cycle(ts_a_train), xlab = "Time")

param <- auto.arima(ts_a_train, trace = TRUE, stepwise = FALSE, approximation = FALSE);
arima.fit <- Arima(ts_a_train, order = c(1, 2, 3), method = "ML", include.drift = FALSE);
# arima.fit %>% residuals() %>% ggtsdisplay() 

# plot(diff(ts_a_train, lag = 1, differences = 1));
# plot(movavg(diff(ts_a_train, lag = 1, differences = 1), 7, type = "s"));

# plot(diff(ts_a_train, lag = 1, differences = 2));
# plot(movavg(diff(ts_a_train, lag = 1, differences = 2), 7, type = "s"));
  
predict.fit <- forecast(arima.fit, level = c(95), h = 8)
#### plot(predict.fit)

dev.off()
ggplot() +
  
  # test_set;
  geom_line(aes(x = as.data.frame(rbind(train_set, test_set))$Date, y = as.data.frame(rbind(train_set, test_set))$aTotal), col = "black") +
  
  # Prediction;
  geom_line(aes(x = test_set$Date[1:8], y = predict.fit$lower), col = "gray", linetype = "dashed") +
  geom_line(aes(x = test_set$Date[1:8], y = predict.fit$upper), col = "gray", linetype = "dashed") +
  geom_line(aes(x = test_set$Date[1:8], y = predict.fit$mean), col = "red2") +
  
  # Confidence;
  geom_ribbon(aes(x = test_set$Date[1:8], ymin = predict.fit$lower, ymax = predict.fit$upper), fill = "gray", alpha = 0.2) +
  
  ## Custom Label
  labs(title = "Prediction of cumulative confirmed cases: ARIMA",
       subtitle = "",
       x = "Date",
       y = "Cumulative Confirmed") +
  theme_bw(base_size = 10, base_family = "Times")

source("~/TABASCO-MEXCOV-19/src/support/metrica.R")
metrica(test_set$aTotal, predict.fit$mean)

##
## New.Prediction
##

predict.fit <- forecast(arima.fit, level = c(95), h = 64)
#### plot(predict.fit)

ggplot() +
  
  # Reality;
  geom_line(aes(x = as.data.frame(rbind(train_set, test_set, real_set))$Date, y = as.data.frame(rbind(train_set, test_set, real_set))$aTotal), col = "black") +
  
  # Prediction;
  geom_line(aes(x = as.data.frame(rbind(test_set, real_set))$Date[1:64], y = predict.fit$lower), col = "gray", linetype = "dashed") +
  geom_line(aes(x = as.data.frame(rbind(test_set, real_set))$Date[1:64], y = predict.fit$upper), col = "gray", linetype = "dashed") +
  geom_line(aes(x = as.data.frame(rbind(test_set, real_set))$Date[1:64], y = predict.fit$mean), col = "red2") +
  
  # Confidence;
  geom_ribbon(aes(x = as.data.frame(rbind(test_set, real_set))$Date[1:64], ymin = predict.fit$lower, ymax = predict.fit$upper), fill = "gray", alpha = 0.2) +
  
  ## Custom Label
  labs(title = "Prediction of cumulative confirmed cases: ARIMA",
       subtitle = "",
       x = "Date",
       y = "Cumulative Confirmed") +
  theme_bw(base_size = 10, base_family = "Times")

##
## Old.Prediction
##

ts_a_train <- ts(rbind(train_set, test_set)[, c("aTotal")], frequency = 7);
#### plot(ts_a_train);

dec_a_train <- decompose(ts_a_train, "multiplicative"); 
# plot(dec_a_train)
# boxplot(ts_a_train ~ cycle(ts_a_train), xlab = "Time")

param <- auto.arima(ts_a_train, trace = TRUE, stepwise = FALSE, approximation = FALSE);
arima.fit <- Arima(ts_a_train, order = c(1, 2, 3), method = "CSS-ML", include.drift = FALSE);
# arima.fit %>% residuals() %>% ggtsdisplay() 

# plot(diff(ts_a_train, lag = 1, differences = 1));
# plot(movavg(diff(ts_a_train, lag = 1, differences = 1), 7, type = "s"));

# plot(diff(ts_a_train, lag = 1, differences = 2));
# plot(movavg(diff(ts_a_train, lag = 1, differences = 2), 7, type = "s"));

predict.fit <- forecast(arima.fit, level = c(95), h = 58)
#### plot(predict.fit)

ggplot() +
  
  # Reality;
  geom_line(aes(x = as.data.frame(rbind(train_set, test_set, real_set))$Date, y = as.data.frame(rbind(train_set, test_set, real_set))$aTotal), col = "black") +
  
  # Prediction;
  geom_line(aes(x = real_set$Date[1:58], y = predict.fit$lower), col = "gray", linetype = "dashed") +
  geom_line(aes(x = real_set$Date[1:58], y = predict.fit$upper), col = "gray", linetype = "dashed") +
  geom_line(aes(x = real_set$Date[1:58], y = predict.fit$mean), col = "red2") +
  
  # Confidence;
  geom_ribbon(aes(x = real_set$Date[1:58], ymin = predict.fit$lower, ymax = predict.fit$upper), fill = "gray", alpha = 0.2) +
  
  ## Custom Label
  labs(title = "Prediction of cumulative confirmed cases: ARIMA",
       subtitle = "",
       x = "Date",
       y = "Cumulative Confirmed") +
  theme_bw(base_size = 10, base_family = "Times")


#################################################################################################################################################################################################################################################################################
## TS - Daily ###################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################


ts_d_train <- ts(train_set[, c("dTotal")], frequency = 7)
#### plot(ts_d_train);

dec_d_train <- decompose(ts_d_train, "multiplicative"); 
# plot(dec_d_train)
# boxplot(ts_d_train ~ cycle(ts_d_train), xlab = "Time")

param <- auto.arima(ts_d_train, trace = TRUE, stepwise = FALSE, approximation = FALSE)
arima.fit <- Arima(ts_d_train, order = c(1, 1, 2), seasonal = c(2, 0, 0), method = "CSS-ML", include.drift = TRUE)
# arima.fit %>% residuals() %>% ggtsdisplay()

predict.fit <- forecast(arima.fit, level = c(95), h = 8)
# plot(predict.fit)

dev.off()
ggplot() +
  
  # Reality;
  geom_line(aes(x = as.data.frame(rbind(train_set, test_set))$Date, y = as.data.frame(rbind(train_set, test_set))$dTotal), col = "black") +
  
  # Prediction;
  geom_line(aes(x = test_set$Date[1:8], y = predict.fit$lower), col = "gray", linetype = "dashed") +
  geom_line(aes(x = test_set$Date[1:8], y = predict.fit$upper), col = "gray", linetype = "dashed") +
  geom_line(aes(x = test_set$Date[1:8], y = predict.fit$mean), col = "red2") +
  
  # Confidence;
  geom_ribbon(aes(x = test_set$Date[1:8], ymin = predict.fit$lower, ymax = predict.fit$upper), fill = "gray", alpha = 0.2) +
  
  ## Custom Label
  labs(title = "Prediction of daily confirmed cases: ARIMA",
       subtitle = "",
       x = "Date",
       y = "Daily Confirmed") +
  theme_bw(base_size = 10, base_family = "Times")

source("~/TABASCO-MEXCOV-19/src/support/metrica.R")
metrica(test_set$dTotal, predict.fit$mean)

##
## New.Prediction
##

predict.fit <- forecast(arima.fit, level = c(95), h = 64)
#### plot(predict.fit)

ggplot() +
  
  # Reality;
  geom_line(aes(x = as.data.frame(rbind(train_set, test_set, real_set))$Date, y = as.data.frame(rbind(train_set, test_set, real_set))$dTotal), col = "black") +
  
  # Prediction;
  geom_line(aes(x = as.data.frame(rbind(test_set, real_set))$Date[1:64], y = predict.fit$lower), col = "gray", linetype = "dashed") +
  geom_line(aes(x = as.data.frame(rbind(test_set, real_set))$Date[1:64], y = predict.fit$upper), col = "gray", linetype = "dashed") +
  geom_line(aes(x = as.data.frame(rbind(test_set, real_set))$Date[1:64], y = predict.fit$mean), col = "red2") +
  
  # Confidence;
  geom_ribbon(aes(x = as.data.frame(rbind(test_set, real_set))$Date[1:64], ymin = predict.fit$lower, ymax = predict.fit$upper), fill = "gray", alpha = 0.2) +
  
  ## Custom Label
  labs(title = "Prediction of cumulative confirmed cases: ARIMA",
       subtitle = "",
       x = "Date",
       y = "Daily Confirmed") +
  theme_bw(base_size = 10, base_family = "Times")

##
## Old.Prediction
##

ts_d_train <- ts(rbind(train_set, test_set)[, c("dTotal")], frequency = 7);
#### plot(ts_a_train);

dec_d_train <- decompose(ts_d_train, "multiplicative"); 
# plot(dec_a_train)
# boxplot(ts_a_train ~ cycle(ts_a_train), xlab = "Time")

param <- auto.arima(ts_d_train, trace = TRUE, stepwise = FALSE, approximation = FALSE);
arima.fit <- Arima(ts_d_train, order = c(1, 1, 2), seasonal = c(2, 0, 0), method = "CSS-ML", include.drift = TRUE)
# arima.fit %>% residuals() %>% ggtsdisplay() 

# plot(diff(ts_a_train, lag = 1, differences = 1));
# plot(movavg(diff(ts_a_train, lag = 1, differences = 1), 7, type = "s"));

# plot(diff(ts_a_train, lag = 1, differences = 2));
# plot(movavg(diff(ts_a_train, lag = 1, differences = 2), 7, type = "s"));

predict.fit <- forecast(arima.fit, level = c(95), h = 58)
#### plot(predict.fit)

ggplot() +
  
  # Reality;
  geom_line(aes(x = as.data.frame(rbind(train_set, test_set, real_set))$Date, y = as.data.frame(rbind(train_set, test_set, real_set))$dTotal), col = "black") +
  
  # Prediction;
  geom_line(aes(x = real_set$Date[1:58], y = predict.fit$lower), col = "gray", linetype = "dashed") +
  geom_line(aes(x = real_set$Date[1:58], y = predict.fit$upper), col = "gray", linetype = "dashed") +
  geom_line(aes(x = real_set$Date[1:58], y = predict.fit$mean), col = "red2") +
  
  # Confidence;
  geom_ribbon(aes(x = real_set$Date[1:58], ymin = predict.fit$lower, ymax = predict.fit$upper), fill = "gray", alpha = 0.2) +
  
  ## Custom Label
  labs(title = "Prediction of cumulative confirmed cases: ARIMA",
       subtitle = "",
       x = "Date",
       y = "Daily Confirmed") +
  theme_bw(base_size = 10, base_family = "Times")
  