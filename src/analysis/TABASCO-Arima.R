source("~/TABASCO-MEXCOV-19/src/packages/autoinstall.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/ts_buffers.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/buffersraw.R")

states  <- unique(as.character(buffersraw$State[which(buffersraw$Region == "Center")]))
dtf.reg <- ts_buffers[, c(1, which(colnames(ts_buffers) %in% states))]
dtf.reg$dTotal <- rowSums(ts_buffers[, 2:19]);
dtf.reg$aTotal <- cumsum(dtf.reg$dTotal); dtf.reg <- dtf.reg[which(dtf.reg$Date <= "2020-06-15"),];

##
## Train / Test
##

train_set <- dtf.reg[which(dtf.reg$Date <= "2020-04-18" & dtf.reg$Date > "2020-03-07"),];
plot(train_set$Date, train_set$dTotal)
plot(train_set$Date, train_set$aTotal)

test_set  <- dtf.reg[which(dtf.reg$Date >  "2020-04-18"),];

# Daily
# ggplot() + 
#   geom_line(data = train_set, aes(x = Date, y = dTotal), col = "red2") + 
#   theme_bw()

# Aggregated
# ggplot() + 
#   geom_line(data = train_set, aes(x = Date, y = aTotal), col = "red2") + 
#   theme_bw()

##
## TS - Aggregated
##

ts_a_train <- ts(train_set[, c("aTotal")], 
                 frequency = 7)
# plot(ts_a_train);

dec_a_train <- decompose(ts_a_train, "multiplicative"); 
# plot(dec_a_train) SI
# boxplot(ts_a_train ~ cycle(ts_a_train), xlab = "Time")

param <- auto.arima(ts_a_train, trace = TRUE, stepwise = FALSE, approximation = FALSE)
# plot.ts(param$residuals)
# acf(ts(param$residuals), main = "ACF residuals")
# pacf(ts(param$residuals), main = "PACF residuals")

arima.fit <- Arima(ts_a_train, order = c(1, 2, 3), method = "CSS-ML", include.drift = FALSE)
# arima.fit %>% residuals() %>% ggtsdisplay() SI

# plot(diff(ts_a_train, lag = 1, differences = 2));
# plot(movavg(diff(ts_a_train, lag = 1, differences = 2), 7, type = "s"));
  
predict.fit <- forecast(arima.fit, level = c(95), h = 58)
# plot(predict.fit)

dev.off()
ggplot() +
  geom_line(aes(x = dtf.reg$Date, y = dtf.reg$aTotal), col = "black") +
  geom_line(aes(x = test_set$Date[1:58], y = predict.fit$lower), col = "red2") +
  geom_line(aes(x = test_set$Date[1:58], y = predict.fit$upper), col = "red2") +
  geom_line(aes(x = test_set$Date[1:58], y = predict.fit$mean), col = "red2") +
  theme_bw()

##
## TS - Daily
##

ts_d_train <- ts(train_set[, c("dTotal")], 
                 frequency = 7)
# plot(ts_d_train);

dec_d_train <- decompose(ts_d_train, "multiplicative"); 
# plot(dec_d_train)
# boxplot(ts_d_train ~ cycle(ts_d_train), xlab = "Time")

param <- auto.arima(ts_d_train, trace = TRUE, stepwise = FALSE, approximation = FALSE)
# plot.ts(param$residuals)
# acf(ts(param$residuals), main = "ACF residuals")
# pacf(ts(param$residuals), main = "PACF residuals")

arima.fit <- Arima(ts_d_train, order = c(1, 1, 2), seasonal = c(2, 0, 0), method = "CSS-ML", include.drift = TRUE)
# arima.fit %>% residuals() %>% ggtsdisplay()

predict.fit <- forecast(arima.fit, level = c(95), h = 58)
# plot(predict.fit)

dev.off()
ggplot() +
  geom_line(aes(x = dtf.reg$Date, y = dtf.reg$dTotal), col = "black") +
  geom_line(aes(x = test_set$Date[1:58], y = predict.fit$lower), col = "red2") +
  geom_line(aes(x = test_set$Date[1:58], y = predict.fit$upper), col = "red2") +
  geom_line(aes(x = test_set$Date[1:58], y = predict.fit$mean), col = "red2") +
  theme_bw()
