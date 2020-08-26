source("~/TABASCO-MEXCOV-19/src/packages/install-packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw_0709.R")

#################################################################################################################################################################################################################################################################################
## Logit - DECEASED #############################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

swabspos <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive") %>%
                                       mutate(INFECTION_TIME = as.numeric(as.Date("2020-07-09") - FECHA_SINTOMAS))); # Time being infected. After 12d we estimate recovering.
ind      <- which(swabspos$DECEASED == "No" & swabspos$INFECTION_TIME <= 12); swabspos <- swabspos[-ind,];                                                
swabspos <- as.data.frame(swabspos %>% select(-c(ID_REGISTRO, OTRO_CASO, INFECTION_TIME, ENTIDAD_UM, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, RESULTADO, TIPO_PACIENTE, UCI, INTUBADO))); rm(ind);
# summary(swabspos);

#################################################################################################################################################################################################################################################################################
## glm.logit.fit - LOO Validation ###############################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

# (help(priors, package = 'rstanarm'))

options(mc.cores = parallel::detectCores())
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
glm.logit.fit <- stan_glm(DECEASED ~ ., data = swabspos,
                  family = binomial(link = "logit"), 
                  prior = t_prior, prior_intercept = t_prior, QR = TRUE,
                  seed = 1)
# summary(glm.logit.fit);
# saveRDS(glm.logit.fit, "~/TABASCO-MEXCOV-19/src/analysis/deceased-logit/models/logit-bayes-t7025.rds");
# glm.logit.fit <- readRDS("~/TABASCO-MEXCOV-19/src/analysis/deceased-logit/models/logit-bayes-t7025.rds");
glm.logit.fit$
# ATTENTION!
# loo <- loo(x = glm.logit.fit, save_psis = TRUE);

#################################################################################################################################################################################################################################################################################
## glm.logit.fit - K-fold Validation ############################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

##
## Train & Test Set;
##

set.seed(123);
ind <- sample(1:nrow(swabspos), 0.8 * nrow(swabspos), replace = FALSE);
train_set <- na.omit(swabspos[ind, -4]); test_set <- na.omit(swabspos[-ind, -4]);
# summary(train_set)
rm(.Random.seed, envir=globalenv()); rm(ind);

glm.logit.predict <- posterior_linpred(glm.logit.fit, transform = TRUE, newdata = test_set)
pred <- colMeans(glm.logit.predict)
pr <- as.integer(pred >= 0.40)
pr <- as.factor(pr)
levels(pr) <- c("No", "Yes")
caret::confusionMatrix(pr, test_set[, 13])



pplot<-plot(glm.logit.fit, "areas", prob = 0.50, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)







summary(linpred)
linpred <- posterior_linpred(glm.logit.fit)
preds <- posterior_linpred(glm.logit.fit, transform=TRUE)
pred <- colMeans(preds)
pr <- as.integer(pred >= 0.5)

# confusion matrix
caret::confusionMatrix(as.factor(as.numeric(pr>0.5)), y)[2]
# posterior classification accuracy
round(mean(xor(pr,as.integer(y==0))),2)
# posterior balanced classification accuracy
round((mean(xor(pr[y==0]>0.5,as.integer(y[y==0])))+mean(xor(pr[y==1]<0.5,as.integer(y[y==1]))))/2,2)














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
