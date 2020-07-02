source("~/TABASCO-MEXCOV-19/src/packages/autoinstall.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/dtf_arima.R")

taim <- 1:42; taim2 <- 43:100;
glm.poisson.fit <- glm(dTotal ~ log(1 + taim) +
                                log(1 + lag_1_Confirmed),
                       family = poisson, data = train_set)

test_set$taim <- taim2;
glm.poisson.predict <- predict(glm.poisson.fit, newdata = test_set, type = "response");

ggplot() +
  
  # Reality;
  geom_line(aes(x = as.data.frame(rbind(train_set, test_set[, -25]))$Date,
                y = as.data.frame(rbind(train_set, test_set[, -25]))$dTotal),
            col = "black") +
  
  # Prediction;
  geom_line(aes(x = test_set$Date - 1,
                y = as.numeric(glm.poisson.predict)), 
            col = "red2") +

  ## Custom Label
  labs(title = "Prediction of daily confirmed cases: Poisson Regression",
       subtitle = "",
       x = "Date",
       y = "Daily Confirmed") +
    theme_bw(base_size = 15, base_family = "Times")


source("~/TABASCO-MEXCOV-19/src/support/metrica.R")
metrica(test_set$dTotal, as.numeric(glm.poisson.predict))
