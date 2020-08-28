source("~/TABASCO-MEXCOV-19/src/packages/install.packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw_0718.R")
    
swabspos <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive") %>%
                                       mutate(TIEMPO_INFECCION = as.numeric(as.Date("2020-07-18") - FECHA_SINTOMAS))); # Time being infected; 
ind      <- which(swabspos$FALLECIDO == "No" & swabspos$TIEMPO_INFECCION <= 40); swabspos <- swabspos[-ind,]; # After 40d we estimate recovering;                                                
swabspos <- as.data.frame(swabspos %>% select(-c(ID_REGISTRO, NEUMONIA, EMBARAZO, OTRO_CASO, TIEMPO_INFECCION, ENTIDAD_UM, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, RESULTADO, TIPO_PACIENTE, UCI, INTUBADO))); rm(ind);
swabspos <- na.omit(swabspos); # summary(swabspos);

#################################################################################################################################################################################################################################################################################
## K-Fold Cross Validation ######################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

set.seed(123);
flds <- createFolds(as.numeric(rownames(swabspos)), k = 10, list = TRUE, returnTrain = FALSE)

#################################################################################################################################################################################################################################################################################
## Model Fit ####################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

rnms <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11");
cnms <- c("K1", "K2", "K3", "K4", "K5");

indices <- lapply(X = list(PARAMETERS = matrix(0, nrow = 11, ncol = 5),
                           DEVIANCE   = matrix(0, nrow = 11, ncol = 5),
                           AIC        = matrix(0, nrow = 11, ncol = 5),
                           BIC        = matrix(0, nrow = 11, ncol = 5),
                           CHI        = matrix(0, nrow = 11, ncol = 5)),
                  FUN = function(x){rownames(x) = rnms; colnames(x) = cnms; x;}); rm(rnms, cnms);

for(i in 1:5)
{index = c(); variables = c(1:11); cat(paste0("K-", i), "Cross Validation\n");
  
  for(j in 1:11)
  {dev = c(); cat(paste0(" j-", j), " \n");
  
    for(k in 1:11)
    {if(variables[11] != 0) {if(variables[k] == 0){next;}}

      cat(paste0("   k-", k), " \n");
      glm.logit.fit = glm(FALLECIDO ~ ., family = binomial(link = "logit"), data = swabspos[-flds[[i]], c(as.numeric(na.omit(index)), variables[k], 12)], na.action = na.omit);
      dev[k] = (-2 * as.numeric(logLik(glm.logit.fit))) # Key for variables selection;
      
      cat("    ", dev, "\n");
      if(k == 11)
      {
        if(j == 1){prior.glm.fit = glm(FALLECIDO ~ 1, family = binomial(link = "logit"), data = swabspos[-flds[[i]],], na.action = na.omit);}
        else{prior.glm.fit = glm(FALLECIDO ~ ., family = binomial(link = "logit"), data = swabspos[-flds[[i]], c(as.numeric(na.omit(index)), 12)], na.action = na.omit);}
        tmp = which(dev == min(dev, na.rm = TRUE)); index[j] = tmp; variables[tmp] = 0;
        glm.logit.fit = glm(FALLECIDO ~ ., family = binomial(link = "logit"), data = swabspos[-flds[[i]], c(as.numeric(na.omit(index)), 12)], na.action = na.omit);
        
        indices$PARAMETERS[j, i] = tmp;
        indices$DEVIANCE[j, i]   = (-2 * as.numeric(logLik(glm.logit.fit))); # Deviance;
        indices$AIC[j, i]        = ( 2 * length(glm.logit.fit$coefficients)) + (-2 * as.numeric(logLik(glm.logit.fit))); # AIC;
        indices$BIC[j, i]        = (log(length(flds[[i]])) * length(glm.logit.fit$coefficients)) + (-2 * as.numeric(logLik(glm.logit.fit))); # BIC;
        indices$CHI[j, i]        = -2 * (as.numeric(logLik(prior.glm.fit) - as.numeric(logLik(glm.logit.fit)))); # CHI;
        
        cat(" variables: ", variables, "\n", "index: ", index, "\n", "tmp: ", tmp, "\n");
      }
    };
  };
};

# write_csv(as.data.frame(Pseudo_R),   "~/TABASCO-MEXCOV-19/src/analysis/FALLECIDO-logit/freq-cv/Pseudo_R.csv"  )
# Pseudo_R   <- read_csv("~/TABASCO-MEXCOV-19/src/analysis/FALLECIDO-logit/freq-cv/Pseudo_R.csv"  )

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
    glm.logit.fit     = glm(FALLECIDO ~ ., family = binomial(link = "logit"), data = swabspos[-flds[[k]], c(index[1:iterator], 13)], na.action = na.omit);
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

glm.logit.fit     <- glm(FALLECIDO ~ ., family = binomial(link = "logit"), data = swabspos[flds[[3]], c(2, 3, 13)], na.action = na.omit);
glm.logit.predict <- as.vector(predict(glm.logit.fit, newdata = na.omit(swabspos[-flds[[3]], c(2, 3, 13)]), type = "response")); 
predicted.classes <- as.factor(ifelse(glm.logit.predict > 0.5, "Yes", "No")); tmp <- na.omit(swabspos[-flds[[3]], c(2, 3, 13)])$FALLECIDO;
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

glm.logit.fit <- glm(FALLECIDO ~ ., family = binomial(link = "logit"), data = train_set[, -4], na.action = na.omit);
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
predicted.classes <- as.factor(ifelse(glm.logit.predict > tmp[[2]], "Yes", "No")); tmp <- test_set$FALLECIDO;
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

glm.logit.fit <- glm(FALLECIDO ~ ., family = binomial(link = "logit"), data = train_set[, -4], na.action = na.omit);
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
predicted.classes <- as.factor(ifelse(glm.logit.predict > tmp[[2]], "Yes", "No")); tmp <- test_set$FALLECIDO;
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
  
