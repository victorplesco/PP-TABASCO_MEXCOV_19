source("~/TABASCO-MEXCOV-19/src/packages/install-packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw.R")

##########################################################################################################################################################################################################################################################
## Logit - DECEASED ######################################################################################################################################################################################################################################
##########################################################################################################################################################################################################################################################

swabspos <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive") %>%
                                       mutate(INFECTION_TIME = as.numeric(as.Date("2020-07-09") - FECHA_SINTOMAS))); # Time being infected. After 12d we estimate recovering.
ind      <- which(swabspos$DECEASED == "No" & swabspos$INFECTION_TIME <= 12); swabspos <- swabspos[-ind,];                                                
swabspos <- as.data.frame(swabspos %>% select(-c(ID_REGISTRO, OTRO_CASO, INFECTION_TIME, ENTIDAD_UM, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, RESULTADO, TIPO_PACIENTE, UCI, INTUBADO)));
# summary(swabspos)

##
## Train & Test Set;
##

set.seed(123);
ind <- sample(1:nrow(swabspos), 0.8 * nrow(swabspos), replace = FALSE);
train_set <- swabspos[ind,]; test_set <- swabspos[-ind,];
# summary(train_set)
rm(.Random.seed, envir=globalenv()); rm(ind);

##
## glm.logit.fit
##

glm.logit.fit <- glm(DECEASED ~ ., family = binomial(link = "logit"), data = train_set[, -4], na.action = na.omit);
summary(glm.logit.fit);
anova(glm.logit.fit, test = "Chisq");
pscl::pR2(glm.logit.fit)["McFadden"];

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
