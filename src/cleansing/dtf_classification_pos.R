source("~/TABASCO-MEXCOV-19/src/packages/install-packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw.R")

swabspos <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive") %>% select(-FECHA_DEF, -RESULTADO, -OTRO_CASO,
                                                                                  -FECHA_SINTOMAS, -INTUBADO, -UCI, 
                                                                                  -TIPO_PACIENTE, -ID_REGISTRO, -ENTIDAD_UM,
                                                                                  -FECHA_INGRESO))


ind <- sample(1:nrow(swabspos), 0.8 * nrow(swabspos), replace = FALSE)
train_set <- swabspos[ind,]
test_set <- swabspos[-ind,]

##
## glm.logit.fit
##

glm.logit.fit <- glm(DECEASED ~ ., family = binomial(link = "logit"), data = train_set, na.action = na.omit)
summary(glm.logit.fit)
anova(glm.logit.fit, test = "Chisq")
pscl::pR2(glm.logit.fit)["McFadden"]

##
## confusion-matrix
##

cutoff <- seq(0.01, 1, 0.01);
indexes <- data.frame(Sensitivity = rep(NA, length(cutoff)),
                      Specificity = rep(NA, length(cutoff)),
                      Accuracy    = rep(NA, length(cutoff)))
glm.logit.predict <- as.vector(predict(glm.logit.fit, newdata = test_set, type = "response")); 
tmp <- train_set$DECEASED;
for(i in 1:length(cutoff))
{
  predicted.classes <- as.factor(ifelse(glm.logit.predict > cutoff[i], "Yes", "No")); 
  tmp2 = confusionMatrix(data = predicted.classes, reference = tmp, positive = "Yes");
  
  indexes$Sensitivity[i] = as.numeric(tmp2$byClass[1]);
  indexes$Specificity[i] = as.numeric(tmp2$byClass[2]);
  indexes$Accuracy[i]    = as.numeric(tmp2$overall[1]);
  indexes$Precision[i]   = as.numeric(tmp2$byClass[5]);
}

indexes$Optimal <- indexes$Sensitivity - indexes$Specificity;

ggplot() + 
  
  ## Sensitivity
  geom_line(aes(x = cutoff, y = indexes$Sensitivity), col = "indianred") +
  
  ## Specificity
  geom_line(aes(x = cutoff, y = indexes$Specificity), col = "black") +
  
  ## Accuracy
  geom_line(aes(x = cutoff, y = indexes$Accuracy), col = "dodgerblue") +
  
  ## Cut-off
  geom_point(aes(x = cutoff[which(abs(indexes$Optimal) == min(abs(indexes$Optimal), na.rm = TRUE))] + 0.005, 
                 y = indexes$Sensitivity[which(abs(indexes$Optimal) == min(abs(indexes$Optimal), na.rm = TRUE))] - 0.006), 
             col = "black",
             size = 3) +
  
  ## Custom Label
  labs(title = "",
       subtitle = "",
       x = "Cut-off",
       y = "Classification Metric") +
  theme_bw(base_size = 15, base_family = "Times") # +
  
  ## Custom Labels
  
  geom_text(aes(x = 0.83, y = 0.375),  label = "Sensitivity", size = 5) +
  geom_line(aes(x = seq(0.735, 0.765, length = 10), y = rep(0.375, length(seq(0.735, 0.765, length = 10)))), col = "indianred", size = 2) +
  
  geom_text(aes(x = 0.83, y = 0.35), label = "Specificity",    size = 5) +
  geom_line(aes(x = seq(0.735, 0.765, length = 10), y = rep(0.35, length(seq(0.735, 0.765, length = 10)))), col = "black", size = 2) +
  
  geom_text(aes(x = 0.83, y = 0.325), label = "Accuracy",    size = 5) +
  geom_line(aes(x = seq(0.735, 0.765, length = 10), y = rep(0.325, length(seq(0.735, 0.765, length = 10)))), col = "dodgerblue", size = 2)
