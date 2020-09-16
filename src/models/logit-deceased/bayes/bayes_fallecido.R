source("~/TABASCO-MEXCOV-19/src/packages/install.packages.R");
swabspos <- read.csv(gzfile("~/TABASCO-MEXCOV-19/data/cleansed/0718/swabspos_log_0718.csv.gz"));

#################################################################################################################################################################################################################################################################################
## Train/Test Set ###############################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

set.seed(123);
ind <- sample(1:nrow(swabspos), 0.8 * nrow(swabspos), replace = F); 
train.set <- swabspos[ind, -c(5, 8, 11)]; test.set  <- swabspos[-ind, -c(5, 8, 11)]; rm(ind);

#################################################################################################################################################################################################################################################################################
## Models #######################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

tstudent.stan.glm.model.8x2000    <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_1/tstudent.stan.glm.model.8x2000.rds");
# write.table(tstudent.stan.glm.model.8x2000$stan_summary[, c(1, 3, 4, 10:12)], "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_1/COEFFICIENTS.txt");
# launch_shinystan(tstudent.stan.glm.model.8x2000);

boot.normal.stan.glm.model.8x2000 <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_2/boot.normal.stan.glm.model.8x2000.rds");
# write.table(boot.normal.stan.glm.model.8x2000$stan_summary[, c(1, 3, 4, 10:12)], "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_2/COEFFICIENTS.txt");
# launch_shinystan(boot.normal.stan.glm.model.8x2000);

#################################################################################################################################################################################################################################################################################
## Priors #######################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

tmp_model1 <- data.frame(tstudent.stan.glm.model.8x2000);    colnames(tmp_model1)[1] <- "INTERCEPT";
tmp_model2 <- data.frame(boot.normal.stan.glm.model.8x2000); colnames(tmp_model2)[1] <- "INTERCEPT";

ggplot() + 

  # MODEL 1  
  geom_line(aes(x = c(min(tmp_model1[, 1]), max(tmp_model1[, 1])), y = rep(9, 2)), col = "#009dd0") +
  geom_point(aes(x = mean(tmp_model1[, 1]), y = 9), col = "#009dd0") +
  
  geom_line(aes(x = c(min(tmp_model1[, 2]), max(tmp_model1[, 2])), y = rep(8, 2)), col = "#009dd0") +
  geom_point(aes(x = mean(tmp_model1[, 2]), y = 8), col = "#009dd0") +
  
  geom_line(aes(x = c(min(tmp_model1[, 3]), max(tmp_model1[, 3])), y = rep(7, 2)), col = "#009dd0") +
  geom_point(aes(x = mean(tmp_model1[, 3]), y = 7), col = "#009dd0") +
  
  geom_line(aes(x = c(min(tmp_model1[, 4]), max(tmp_model1[, 4])), y = rep(6, 2)), col = "#009dd0") +
  geom_point(aes(x = mean(tmp_model1[, 4]), y = 6), col = "#009dd0") +
  
  geom_line(aes(x = c(min(tmp_model1[, 5]), max(tmp_model1[, 5])), y = rep(5, 2)), col = "#009dd0") +
  geom_point(aes(x = mean(tmp_model1[, 5]), y = 5), col = "#009dd0") +

  geom_line(aes(x = c(min(tmp_model1[, 6]), max(tmp_model1[, 6])), y = rep(4, 2)), col = "#009dd0") +
  geom_point(aes(x = mean(tmp_model1[, 6]), y = 4), col = "#009dd0") +
  
  geom_line(aes(x = c(min(tmp_model1[, 7]), max(tmp_model1[, 7])), y = rep(3, 2)), col = "#009dd0") +
  geom_point(aes(x = mean(tmp_model1[, 7]), y = 3), col = "#009dd0") +
  
  geom_line(aes(x = c(min(tmp_model1[, 8]), max(tmp_model1[, 8])), y = rep(2, 2)), col = "#009dd0") +
  geom_point(aes(x = mean(tmp_model1[, 8]), y = 2), col = "#009dd0") +
  
  geom_line(aes(x = c(min(tmp_model1[, 9]), max(tmp_model1[, 9])), y = rep(1, 2)), col = "#009dd0") +
  geom_point(aes(x = mean(tmp_model1[, 9]), y = 1), col = "#009dd0") +
  
  # MODEL 2
  geom_line(aes(x = c(min(tmp_model2[, 1]), max(tmp_model2[, 1])), y = rep(9, 2)), col = "#f58f3b") +
  geom_point(aes(x = mean(tmp_model2[, 1]), y = 9), col = "#f58f3b") +
  
  geom_line(aes(x = c(min(tmp_model2[, 2]), max(tmp_model2[, 2])), y = rep(8, 2)), col = "#f58f3b") +
  geom_point(aes(x = mean(tmp_model2[, 2]), y = 8), col = "#f58f3b") +
  
  geom_line(aes(x = c(min(tmp_model2[, 3]), max(tmp_model2[, 3])), y = rep(7, 2)), col = "#f58f3b") +
  geom_point(aes(x = mean(tmp_model2[, 3]), y = 7), col = "#f58f3b") +
  
  geom_line(aes(x = c(min(tmp_model2[, 4]), max(tmp_model2[, 4])), y = rep(6, 2)), col = "#f58f3b") +
  geom_point(aes(x = mean(tmp_model2[, 4]), y = 6), col = "#f58f3b") +
  
  geom_line(aes(x = c(min(tmp_model2[, 5]), max(tmp_model2[, 5])), y = rep(5, 2)), col = "#f58f3b") +
  geom_point(aes(x = mean(tmp_model2[, 5]), y = 5), col = "#f58f3b") +
  
  geom_line(aes(x = c(min(tmp_model2[, 6]), max(tmp_model2[, 6])), y = rep(4, 2)), col = "#f58f3b") +
  geom_point(aes(x = mean(tmp_model2[, 6]), y = 4), col = "#f58f3b") +
  
  geom_line(aes(x = c(min(tmp_model2[, 7]), max(tmp_model2[, 7])), y = rep(3, 2)), col = "#f58f3b") +
  geom_point(aes(x = mean(tmp_model2[, 7]), y = 3), col = "#f58f3b") +
  
  geom_line(aes(x = c(min(tmp_model2[, 8]), max(tmp_model2[, 8])), y = rep(2, 2)), col = "#f58f3b") +
  geom_point(aes(x = mean(tmp_model2[, 8]), y = 2), col = "#f58f3b") +
  
  geom_line(aes(x = c(min(tmp_model2[, 9]), max(tmp_model2[, 9])), y = rep(1, 2)), col = "#f58f3b") +
  geom_point(aes(x = mean(tmp_model2[, 9]), y = 1), col = "#f58f3b") +
  
  # MODEL 1 - LEGEND
  geom_line(aes(x = c(-5.45, -5.55), y = rep(2.65, 2)), col = "#009dd0", size = 2) +
  geom_text(aes(x = -5.20, y = 2.65), label = "Model 1", size = 5) +
  
  # MODEL 2 - LEGEND
  geom_line(aes(x = c(-5.45, -5.55), y = rep(2.35, 2)), col = "#f58f3b", size = 2) +
  geom_text(aes(x = -5.20, y = 2.35), label = "Model 2", size = 5) +
  
  scale_y_continuous(breaks = c(9:1), labels = colnames(tmp_model1)) +
  # Custom Labels;
  labs(x = "",
       y = "") +
  theme_bw(base_size = 17.5, base_family = "Times"); rm(tmp_model1, tmp_model2);

#################################################################################################################################################################################################################################################################################
## Model Selection - loo ########################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

#################################################################################################################################################################################################################################################################################
## Prediction - Training Data ####################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

target  = factor(train.set$FALLECIDO, levels = c("Yes", "No"));
cutoff  = seq(min(tstudent.stan.glm.model.8x2000$fitted.values), max(tstudent.stan.glm.model.8x2000$fitted.values), 0.01);
indices = data.frame(specificity   = rep(NA, length(cutoff)),
                     sensitivity   = rep(NA, length(cutoff)),
                     accuracy      = rep(NA, length(cutoff)));

for(i in 1:length(cutoff))
{
  predicted.classes = factor(ifelse(tstudent.stan.glm.model.8x2000$fitted.values > cutoff[i], "Yes", "No"), levels = c("Yes", "No")); 
  tmp = confusionMatrix(predicted.classes, target, positive = "Yes");
  
  indices$specificity[i]   = as.numeric(tmp$byClass[2]);
  indices$sensitivity[i]   = as.numeric(tmp$byClass[1]);
  indices$accuracy[i]      = as.numeric(tmp$overall[1]);
}

indices$optimal    = abs(indices$sensitivity - indices$specificity); opt = which(abs(indices$optimal) == min(abs(indices$optimal), na.rm = TRUE))[1];
predicted.classes  = factor(ifelse(tstudent.stan.glm.model.8x2000$fitted.values > cutoff[opt], "Yes", "No"), levels = c("Yes", "No"));
optconfusionmatrix = table(Actual = target, Predicted = predicted.classes);
out_model1 = list(Cutoff = cutoff[opt], Specificity = indices$specificity[opt], Sensitivity = indices$sensitivity[opt], Accuracy = indices$accuracy[opt], "Confusion Matrix" = optconfusionmatrix);
# saveRDS(out_model1, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_1/out_train_model1.rds"); # rm(out_model1);
out_train_model1 = readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_1/out_train_model1.rds");

##
## MODEL 2
##

target  = factor(train.set$FALLECIDO, levels = c("Yes", "No"));
cutoff  = seq(min(boot.normal.stan.glm.model.8x2000$fitted.values), max(boot.normal.stan.glm.model.8x2000$fitted.values), 0.01);
indices = data.frame(specificity   = rep(NA, length(cutoff)),
                     sensitivity   = rep(NA, length(cutoff)),
                     accuracy      = rep(NA, length(cutoff)));

for(i in 1:length(cutoff))
{
  predicted.classes = factor(ifelse(boot.normal.stan.glm.model.8x2000$fitted.values > cutoff[i], "Yes", "No"), levels = c("Yes", "No")); 
  tmp = confusionMatrix(predicted.classes, target, positive = "Yes");
  
  indices$specificity[i]   = as.numeric(tmp$byClass[2]);
  indices$sensitivity[i]   = as.numeric(tmp$byClass[1]);
  indices$accuracy[i]      = as.numeric(tmp$overall[1]);
}

indices$optimal    = abs(indices$sensitivity - indices$specificity); opt = which(abs(indices$optimal) == min(abs(indices$optimal), na.rm = TRUE))[1];
predicted.classes  = factor(ifelse(boot.normal.stan.glm.model.8x2000$fitted.values > cutoff[opt], "Yes", "No"), levels = c("Yes", "No"));
optconfusionmatrix = table(Actual = target, Predicted = predicted.classes);
out_model2 = list(Cutoff = cutoff[opt], Specificity = indices$specificity[opt], Sensitivity = indices$sensitivity[opt], Accuracy = indices$accuracy[opt], "Confusion Matrix" = optconfusionmatrix);
# saveRDS(out_model2, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_2/out_train_model2.rds"); # rm(out_model2);
out_train_model2 = readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_2/out_train_model2.rds");

#################################################################################################################################################################################################################################################################################
## Prediction - Testing Data ####################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

##
## MODEL 1
##
install.packages("fst")
require(fst)


# predict_model1 <- posterior_epred(tstudent.stan.glm.model.8x2000, newdata = test.set, draws = 500); # Too big to be stored on GitHub;
# saveRDS(predict_model1, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_1/predict_modelAAA.rds", compress = TRUE); # rm(predict_model1);
predict_model1 <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_1/predict_model1.rds");

predict_means1 = colMeans(predict_model1); 
target  = factor(test.set$FALLECIDO, levels = c("Yes", "No"));
cutoff  = seq(min(predict_means1), max(predict_means1), 0.01);
indices = data.frame(specificity   = rep(NA, length(cutoff)),
                     sensitivity   = rep(NA, length(cutoff)),
                     accuracy      = rep(NA, length(cutoff)));

for(i in 1:length(cutoff))
{
  predicted.classes = factor(ifelse(predict_means1 > cutoff[i], "Yes", "No"), levels = c("Yes", "No")); 
  tmp = confusionMatrix(predicted.classes, target, positive = "Yes");
  
  indices$specificity[i]   = as.numeric(tmp$byClass[2]);
  indices$sensitivity[i]   = as.numeric(tmp$byClass[1]);
  indices$accuracy[i]      = as.numeric(tmp$overall[1]);
}

indices$optimal    = abs(indices$sensitivity - indices$specificity); opt = which(abs(indices$optimal) == min(abs(indices$optimal), na.rm = TRUE))[1];
predicted.classes  = factor(ifelse(predict_means1 > cutoff[opt], "Yes", "No"), levels = c("Yes", "No"));
optconfusionmatrix = table(Actual = target, Predicted = predicted.classes);
out_model1 = list(Cutoff = cutoff[opt], Specificity = indices$specificity[opt], Sensitivity = indices$sensitivity[opt], Accuracy = indices$accuracy[opt], "Confusion Matrix" = optconfusionmatrix);
# saveRDS(out_model1, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_1/out_test_model1.rds"); # rm(out_model1);
out_test_model1 = readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_1/out_test_model1.rds");

##
## MODEL 2
##

# predict_model2 <- posterior_epred(boot.normal.stan.glm.model.8x2000, newdata = test.set, draws = 500); # Too big to be stored on GitHub;
# saveRDS(predict_model2, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_2/predict_model2.rds"); # rm(predict_model2);
predict_model2 <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_2/predict_model2.rds");

predict_means2 = colMeans(predict_model2); 
target  = factor(test.set$FALLECIDO, levels = c("Yes", "No"));
cutoff  = seq(min(predict_means2), max(predict_means2), 0.01);
indices = data.frame(specificity   = rep(NA, length(cutoff)),
                     sensitivity   = rep(NA, length(cutoff)),
                     accuracy      = rep(NA, length(cutoff)));

for(i in 1:length(cutoff))
{
  predicted.classes = factor(ifelse(predict_means2 > cutoff[i], "Yes", "No"), levels = c("Yes", "No")); 
  tmp = confusionMatrix(predicted.classes, target, positive = "Yes");
  
  indices$specificity[i]   = as.numeric(tmp$byClass[2]);
  indices$sensitivity[i]   = as.numeric(tmp$byClass[1]);
  indices$accuracy[i]      = as.numeric(tmp$overall[1]);
}

indices$optimal    = abs(indices$sensitivity - indices$specificity); opt = which(abs(indices$optimal) == min(abs(indices$optimal), na.rm = TRUE))[1];
predicted.classes  = factor(ifelse(predict_means2 > cutoff[opt], "Yes", "No"), levels = c("Yes", "No"));
optconfusionmatrix = table(Actual = target, Predicted = predicted.classes);
out_model2 = list(Cutoff = cutoff[opt], Specificity = indices$specificity[opt], Sensitivity = indices$sensitivity[opt], Accuracy = indices$accuracy[opt], "Confusion Matrix" = optconfusionmatrix);
# saveRDS(out_model2, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_2/out_test_model2.rds"); # rm(out_model2);
out_test_model2 = readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_2/out_test_model2.rds");
