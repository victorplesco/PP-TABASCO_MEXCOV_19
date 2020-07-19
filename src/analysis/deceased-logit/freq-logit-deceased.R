source("~/TABASCO-MEXCOV-19/src/packages/install-packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw_0719.R")
    
##########################################################################################################################################################################################################################################################
## Logit - DECEASED ######################################################################################################################################################################################################################################
##########################################################################################################################################################################################################################################################

swabspos <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive") %>%
                                       mutate(INFECTION_TIME = as.numeric(as.Date("2020-07-18") - FECHA_SINTOMAS))); # Time being infected. After 12d we estimate recovering.
ind      <- which(swabspos$DECEASED == "No" & swabspos$INFECTION_TIME <= 26); swabspos <- swabspos[-ind,];                                                
swabspos <- as.data.frame(swabspos %>% select(-c(ID_REGISTRO, NEUMONIA, EMBARAZO, OTRO_CASO, INFECTION_TIME, ENTIDAD_UM, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, RESULTADO, TIPO_PACIENTE, UCI, INTUBADO))); rm(ind);
swabspos <- na.omit(swabspos); # summary(swabspos);

##
## train & test set;
##

set.seed(123);
flds <- createFolds(as.numeric(rownames(swabspos)), k = 5, list = TRUE, returnTrain = FALSE)

##
## variable-selection;
##

PARAMETERS    <- matrix(0, nrow = 12, ncol = 10);
DEVIANCE      <- matrix(0, nrow = 12, ncol = 10);
AIC           <- matrix(0, nrow = 12, ncol = 10);
BIC           <- matrix(0, nrow = 12, ncol = 10);
Pseudo_R      <- matrix(0, nrow = 12, ncol = 10);
Cross-Entropy <- matrix(0, nrow = 12, ncol = 10);

for(j in 1:5)
{
  index <- c();

  for(k in 1:12)
  {
    dev = c(); variables = c(1:12); 

    for(i in 1:12)
    {
      if(variables[i] == 0) {next;};
      
      glm.logit.fit = glm(DECEASED ~ ., family = binomial(link = "logit"), data = swabspos[-flds[[j]], c(index, variables[i], 13)], na.action = na.omit);
      dev[i] = (-2 * as.numeric(logLik(glm.logit.fit))) # Deviance;
      
      if(i == 12)
      {
        tmp = which(dev == min(dev, na.rm = TRUE)); index[k] <- tmp; variables[tmp] <- 0;
        glm.logit.fit = glm(DECEASED ~ ., family = binomial(link = "logit"), data = swabspos[-flds[[j]], c(index, 13)], na.action = na.omit);
        
        PARAMETERS[k, j] = tmp;
        DEVIANCE[k, j] = (-2 * as.numeric(logLik(glm.logit.fit))) # Deviance;
        AIC[k, j]      = ( 2 * length(glm.logit.fit$coefficients)) + (-2 * as.numeric(logLik(glm.logit.fit))) # AIC;
        BIC[k, j]      = (log(length(flds[[1]])) * length(glm.logit.fit$coefficients)) + (-2 * as.numeric(logLik(glm.logit.fit))) # BIC;
        Pseudo_R[k, j] = 1 - glm.logit.fit$deviance/glm.logit.fit$null.deviance # Pseudo R;
      }
    }
  }
};
  
colnames(PARAMETERS) <- c("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9", "K10");
colnames(DEVIANCE)   <- c("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9", "K10");
colnames(AIC)        <- c("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9", "K10");
colnames(BIC)        <- c("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9", "K10");
colnames(Pseudo_R)   <- c("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9", "K10");

rownames(PARAMETERS) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12");
rownames(DEVIANCE)   <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12");
rownames(AIC)        <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12");
rownames(BIC)        <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12");
rownames(Pseudo_R)   <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12");

# write_csv(as.data.frame(PARAMETERS), "~/TABASCO-MEXCOV-19/src/analysis/deceased-logit/freq-cv/PARAMETERS.csv")
# write_csv(as.data.frame(DEVIANCE),   "~/TABASCO-MEXCOV-19/src/analysis/deceased-logit/freq-cv/DEVIANCE.csv"  )
# write_csv(as.data.frame(AIC),        "~/TABASCO-MEXCOV-19/src/analysis/deceased-logit/freq-cv/AIC.csv"       )
# write_csv(as.data.frame(BIC),        "~/TABASCO-MEXCOV-19/src/analysis/deceased-logit/freq-cv/BIC.csv"       )
# write_csv(as.data.frame(Pseudo_R),   "~/TABASCO-MEXCOV-19/src/analysis/deceased-logit/freq-cv/Pseudo_R.csv"  )

# PARAMETERS <- read_csv("~/TABASCO-MEXCOV-19/src/analysis/deceased-logit/freq-cv/PARAMETERS.csv")
# DEVIANCE   <- read_csv("~/TABASCO-MEXCOV-19/src/analysis/deceased-logit/freq-cv/DEVIANCE.csv"  )
# AIC        <- read_csv("~/TABASCO-MEXCOV-19/src/analysis/deceased-logit/freq-cv/AIC.csv"       )
# BIC        <- read_csv("~/TABASCO-MEXCOV-19/src/analysis/deceased-logit/freq-cv/BIC.csv"       )
# Pseudo_R   <- read_csv("~/TABASCO-MEXCOV-19/src/analysis/deceased-logit/freq-cv/Pseudo_R.csv"  )

summary_cv <- data.frame(Complexity = c(1:12),
                         DEVIANCE   = as.numeric(apply(X = DEVIANCE, MARGIN = 1, FUN = mean)),
                         AIC        = as.numeric(apply(X = AIC, MARGIN = 1, FUN = mean)),
                         BIC        = as.numeric(apply(X = BIC, MARGIN = 1, FUN = mean)),
                         Pseudo_R   = as.numeric(apply(X = Pseudo_R, MARGIN = 1, FUN = mean)));

par(mfrow = c(2, 2));
plot(summary_cv$Complexity, summary_cv$DEVIANCE, type = "l", col = "tomato3", lwd = 2, xlab = "Complexity", ylab = "Deviance");
plot(summary_cv$Complexity, summary_cv$AIC, type = "l", col = "tomato3", lwd = 2, xlab = "Complexity", ylab = "AIC");
plot(summary_cv$Complexity, summary_cv$BIC, type = "l", col = "tomato3", lwd = 2, xlab = "Complexity", ylab = "BIC");
plot(summary_cv$Complexity, summary_cv$Pseudo_R, type = "l", col = "tomato3", lwd = 2, xlab = "Complexity", ylab = "Pseudo-R");

##
## test-error;
##

cross_entropy <- function(y, y_hat, N)
{return(-1/N * sum(y * log(y_hat) + (1 - y) * log(1 - y_hat)));}

CROSSENTROPY <- matrix(0, nrow = 5, ncol = 10);
# https://towardsdatascience.com/entropy-how-decision-trees-make-decisions-2946b9c18c8
# https://towardsdatascience.com/cross-entropy-for-dummies-5189303c7735
# WHY ADDING COVARIATES IS DANNOSO?

for(k in 1:10)
{
  iterator = 1; index = c(1, 2, 4, 3, 11);
  for(i in 1:5)
  {
    glm.logit.fit     = glm(DECEASED ~ ., family = binomial(link = "logit"), data = swabspos[-flds[[k]], c(index[1:iterator], 13)], na.action = na.omit);
    glm.logit.predict = as.vector(predict(glm.logit.fit, newdata = swabspos[flds[[k]], c(index[1:iterator], 13)], type = "response")); 
    CROSSENTROPY[i, k] = cross_entropy(ifelse(swabspos[flds[[k]], c(13)] == "Yes", 1, 0), glm.logit.predict, length(swabspos[flds[[1]], c(13)]));  
    
    iterator = iterator + 1;
  }
}

colnames(CROSSENTROPY) <- c("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9", "K10");
rownames(CROSSENTROPY) <- c("V1", "V2", "V3", "V4", "V5");

par(mfrow = c(1, 1));
plot(1:5, as.numeric(apply(X = CROSSENTROPY, MARGIN = 1, FUN = mean)), type = "l", col = "tomato3", lwd = 2, xlab = "Complexity", ylab = "Cross-Entropy");

##
## confusion-matrix;
##
  
targe * log(predizione  )
source("~/TABASCO-MEXCOV-19/src/support/confmatrix.R")
tmp <- confmatrix(train_set, glm.logit.fit)
tmp[[1]]; tmp[[2]];
  
##
## Prediction - Aggregated
## 

glm.logit.fit     <- glm(DECEASED ~ ., family = binomial(link = "logit"), data = swabspos[flds[[3]], c(2, 3, 13)], na.action = na.omit);
glm.logit.predict <- as.vector(predict(glm.logit.fit, newdata = na.omit(swabspos[-flds[[3]], c(2, 3, 13)]), type = "response")); 
predicted.classes <- as.factor(ifelse(glm.logit.predict > 0.5, "Yes", "No")); tmp <- na.omit(swabspos[-flds[[3]], c(2, 3, 13)])$DECEASED;
confusionMatrix(data = predicted.classes, reference = tmp, positive = "Yes")

##########################################################################################################################################################################################################################################################
## SEXO: Male ############################################################################################################################################################################################################################################
##########################################################################################################################################################################################################################################################

swabspos_male <- as.data.frame(swabspos %>% filter(SEXO == "Male") %>%
                                            select(-c(SEXO, EMBARAZO)));
# summary(swabspos_male)

##
## Train & Test Set;
##

set.seed(123);
ind <- sample(1:nrow(swabspos_male), 0.8 * nrow(swabspos_male), replace = FALSE);
train_set <- swabspos_male[ind,]; test_set <- swabspos_male[-ind,];
# summary(train_set)
rm(.Random.seed, envir=globalenv()); rm(ind);

##
## glm.logit.fit
##

glm.logit.fit <- glm(DECEASED ~ ., family = binomial(link = "logit"), data = train_set[, -4], na.action = na.omit);
summary(glm.logit.fit);
anova(glm.logit.fit, test = "Chisq");
pscl::pR2(glm.logit.fit)["McFadden"];
# mCoef <- glm.logit.fit$coefficients;

##
## confusion-matrix
##

source("~/TABASCO-MEXCOV-19/src/support/confmatrix.R")
tmp <- confmatrix(train_set, glm.logit.fit)
tmp[[1]]; tmp[[2]];

##
## Prediction - Aggregated
## 

glm.logit.predict <- as.vector(predict(glm.logit.fit, newdata = test_set, type = "response")); 
predicted.classes <- as.factor(ifelse(glm.logit.predict > tmp[[2]], "Yes", "No")); tmp <- test_set$DECEASED;
confusionMatrix(data = predicted.classes, reference = tmp, positive = "Yes")

##########################################################################################################################################################################################################################################################
## SEXO: Female ##########################################################################################################################################################################################################################################
##########################################################################################################################################################################################################################################################

swabspos_female <- as.data.frame(swabspos %>% filter(SEXO == "Female") %>%
                                              select(-SEXO));
# summary(swabspos_female)

##
## Train & Test Set;
##

set.seed(123);
ind <- sample(1:nrow(swabspos_female), 0.8 * nrow(swabspos_female), replace = FALSE);
train_set <- swabspos_female[ind,]; test_set <- swabspos_female[-ind,];
# summary(train_set)
rm(.Random.seed, envir=globalenv()); rm(ind);

##
## glm.logit.fit
##

glm.logit.fit <- glm(DECEASED ~ ., family = binomial(link = "logit"), data = train_set[, -4], na.action = na.omit);
summary(glm.logit.fit);
anova(glm.logit.fit, test = "Chisq");
pscl::pR2(glm.logit.fit)["McFadden"];
# fCoef <- glm.logit.fit$coefficients;

##
## confusion-matrix
##

source("~/TABASCO-MEXCOV-19/src/support/confmatrix.R")
tmp <- confmatrix(train_set, glm.logit.fit)
tmp[[1]]; tmp[[2]];

##
## Prediction - Aggregated
## 

glm.logit.predict <- as.vector(predict(glm.logit.fit, newdata = test_set, type = "response")); 
predicted.classes <- as.factor(ifelse(glm.logit.predict > tmp[[2]], "Yes", "No")); tmp <- test_set$DECEASED;
confusionMatrix(data = predicted.classes, reference = tmp, positive = "Yes")

##########################################################################################################################################################################################################################################################
## RESULTs ###############################################################################################################################################################################################################################################
##########################################################################################################################################################################################################################################################

SEXOS <- ggplot() +
  
  geom_line(aes(x = 1:length(tCoef), y = as.numeric(tCoef)), col = "black") +
  geom_line(aes(x = 1:length(mCoef), y = as.numeric(mCoef)), col = "dodgerblue3") +
  geom_line(aes(x = 1:length(fCoef), y = as.numeric(fCoef)), col = "tomato3") +
  
  ## Custom Label
  labs(title = "Prediction of daily confirmed cases: Logistic Regression",
       subtitle = "",
       x = "Date",
       y = "Daily Confirmed") +
  theme_bw(base_size = 12, base_family = "Times") +
  
  scale_x_continuous(breaks = c(1:length(tCoef)), 
                     labels = names(tCoef))
  
