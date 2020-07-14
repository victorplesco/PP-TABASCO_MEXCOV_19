source("~/TABASCO-MEXCOV-19/src/packages/install-packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw.R")

#######################################################################################################################################################################################################################################################
## SEXO: Male #########################################################################################################################################################################################################################################
#######################################################################################################################################################################################################################################################

swabspos <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive" & SEXO == "Male") %>%
                                       mutate(INFECTION_TIME = as.numeric(as.Date("2020-07-09") - FECHA_SINTOMAS))); # Time being infected. After 12d we estimate recovering.
ind      <- which(swabspos$DECEASED == "No" & swabspos$INFECTION_TIME <= 12); swabspos <- swabspos[-ind,];                                                
swabspos <- as.data.frame(swabspos %>% select(-c(ID_REGISTRO, ENTIDAD_UM, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, RESULTADO, TIPO_PACIENTE, UCI, INTUBADO,
                                                 SEXO, EMBARAZO, OTRO_CASO, INFECTION_TIME)));

##
## Train & Test Set;
##

set.seed(123);
ind <- sample(1:nrow(swabspos), 0.8 * nrow(swabspos), replace = FALSE);
train_set <- swabspos[ind,]; test_set <- swabspos[-ind,];
rm(.Random.seed, envir=globalenv());

##
## glm.logit.fit
##

glm.logit.fit <- glm(DECEASED ~ ., family = binomial(link = "logit"), data = train_set, na.action = na.omit);
summary(glm.logit.fit);
anova(glm.logit.fit, test = "Chisq");
pscl::pR2(glm.logit.fit)["McFadden"];

##
## confusion-matrix
##

cutoff <- seq(0.01, 1, 0.01);
indexes <- data.frame(Sensitivity = rep(NA, length(cutoff)),
                      Specificity = rep(NA, length(cutoff)),
                      Accuracy    = rep(NA, length(cutoff)));
glm.logit.predict <- as.vector(predict(glm.logit.fit, newdata = train_set, type = "response")); 
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
  
  ## Accuracy
  geom_line(aes(x = cutoff, y = indexes$Accuracy), col = "dodgerblue") +
  
  ## Cut-off
  geom_point(aes(x = cutoff[which(abs(indexes$Optimal) == min(abs(indexes$Optimal), na.rm = TRUE))], 
                 y = indexes$Sensitivity[which(abs(indexes$Optimal) == min(abs(indexes$Optimal), na.rm = TRUE))]), 
             col = "black",
             size = 3) +
  
  ## Custom Label
  labs(title = "",
       subtitle = "",
       x = "Cut-off",
       y = "Classification Metric") +
  theme_bw(base_size = 15, base_family = "Times")

##
## Prediction - Aggregated
## 

glm.logit.predict <- as.vector(predict(glm.logit.fit, newdata = test_set, type = "response")); 
predicted.classes <- as.factor(ifelse(glm.logit.predict > cutoff[which(abs(indexes$Optimal) == min(abs(indexes$Optimal), na.rm = TRUE))], "Yes", "No")); tmp <- test_set$DECEASED;
confusionMatrix(data = predicted.classes, reference = tmp, positive = "Yes")
