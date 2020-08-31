source("~/TABASCO-MEXCOV-19/src/packages/install.packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw_0718.R")
    
swabspos <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive") %>%
                                       mutate(TIEMPO_INFECCION = as.numeric(as.Date("2020-07-18") - FECHA_SINTOMAS))); # Time being infected; 
ind      <- which(swabspos$TIEMPO_INFECCION <= 40); swabspos <- swabspos[-ind,]; # After 40d we estimate recovering;                                                
swabspos <- as.data.frame(swabspos %>% select(-c(ID_REGISTRO, EMBARAZO, NEUMONIA, OTRO_CASO, ENTIDAD_UM, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, RESULTADO, TIPO_PACIENTE, UCI, INTUBADO, TIEMPO_INFECCION))); rm(ind, swabsraw);
swabspos <- na.omit(swabspos); # summary(swabspos);

#################################################################################################################################################################################################################################################################################
## 5-fold Cross Validation ######################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

set.seed(123);
flds <- createFolds(as.numeric(rownames(swabspos)), k = 5, list = TRUE, returnTrain = FALSE);

#################################################################################################################################################################################################################################################################################
## Model Selection - Training Data ##############################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

rnms <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11");
cnms <- c("K1", "K2", "K3", "K4", "K5");

indices <- lapply(X = list(PARAMETERS = matrix(0, nrow = 11, ncol = 5),
                           DEVIANCE   = matrix(0, nrow = 11, ncol = 5),
                           AIC        = matrix(0, nrow = 11, ncol = 5),
                           BIC        = matrix(0, nrow = 11, ncol = 5),
                           LRT        = matrix(0, nrow = 11, ncol = 5),
                           WALD       = matrix(0, nrow = 11, ncol = 5)),
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
        indices$AIC[j, i]        = (2 * length(glm.logit.fit$coefficients)) + (-2 * as.numeric(logLik(glm.logit.fit))); # AIC;
        indices$BIC[j, i]        = (log(length(flds[[i]])) * length(glm.logit.fit$coefficients)) + (-2 * as.numeric(logLik(glm.logit.fit))); # BIC;
        indices$LRT[j, i]        = pchisq(-2 * (as.numeric(logLik(prior.glm.fit) - as.numeric(logLik(glm.logit.fit)))), 1, lower.tail = FALSE); # Likelihood Ratio Test;
        indices$WALD[j, i]       = pnorm(as.numeric(glm.logit.fit$coefficients[j + 1])/sqrt(summary(glm.logit.fit)$cov.scaled[j + 1, j + 1]), 0, 1, lower.tail = FALSE); # Wald Test;
        
        cat(" variables: ", variables, "\n", "index: ", index, "\n", "tmp: ", tmp, "\n");
      } 
    };
  };
}; rm(glm.logit.fit, dev, i, index, j, k, variables);

# saveRDS(indices, "~/TABASCO-MEXCOV-19/src/models/logit/freq/5-fold_Cross_Validation/training_indices.rds"); rm(indices);
training_indices <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit/freq/5-fold_Cross_Validation/training_indices.rds");

ggplot() +
  
  # 5-fold CV Deviance;
  geom_line(aes(x = c(1:11), y = apply(X = training_indices$DEVIANCE, MARGIN = 1, FUN = mean)),
            col = "black",
            size = 0.75) +
  
  # 5-fold CV Max/Min;
  geom_ribbon(aes(x = c(1:11),
                  ymin = apply(X = training_indices$DEVIANCE, MARGIN = 1, FUN = min), 
                  ymax = apply(X = training_indices$DEVIANCE, MARGIN = 1, FUN = max)), 
              alpha    = 0.1,
              linetype = "dashed",
              colour   = "black",
              size     = 0.75,
              fill     = "tomato3") +
  
  # Custom Labels;
  labs(title = "5-fold CV",
       subtitle = "",
       x = "Complexity",
       y = "Deviance") +
  theme_bw(base_size = 17.5, base_family = "Times");

#################################################################################################################################################################################################################################################################################
## Model Selection - Testing Data ###############################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

source("~/TABASCO-MEXCOV-19/src/support/confmatrix.R");

rnms <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11");
cnms <- c("K1", "K2", "K3", "K4", "K5");

indices <- lapply(X = list(CUTOFF      = matrix(0, nrow = 11, ncol = 5),
                           SENSITIVITY = matrix(0, nrow = 11, ncol = 5),
                           SPECIFICITY = matrix(0, nrow = 11, ncol = 5),
                           ACCURACY    = matrix(0, nrow = 11, ncol = 5)),
                  FUN = function(x){rownames(x) = rnms; colnames(x) = cnms; x;}); rm(rnms, cnms);

for(i in 1:5)
{cat(paste0("K-", i), "Cross Validation\n");

  for(j in 1:11)
  {cat(paste0(" j-", j), " \n");
    
    glm.model = glm(FALLECIDO ~ ., family = binomial(link = "logit"), data = swabspos[-flds[[i]], c(training_indices$PARAMETERS[1:j, i], 12)], na.action = na.omit);
    test.set  = swabspos[-flds[[i]], training_indices$PARAMETERS[1:j, i], drop = FALSE]; 
    target    = swabspos[-flds[[i]], 12];
    tmp       = confmatrix(test.set, target, glm.model, show.plot = FALSE);
    
    indices$CUTOFF[j, i]      = tmp$Cutoff;
    indices$SENSITIVITY[j, i] = tmp$Sensitivity;
    indices$SPECIFICITY[j, i] = tmp$Specificity;
    indices$ACCURACY[j, i]    = tmp$Accuracy;
  };
}; rm(flds, glm.model, test.set, training_indices, i, j, target, "confmatrix");

# saveRDS(indices, "~/TABASCO-MEXCOV-19/src/models/logit/freq/5-fold_Cross_Validation/test_error.rds"); rm(indices);
test_error <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit/freq/5-fold_Cross_Validation/test_error.rds");

# saveRDS(indices, "~/TABASCO-MEXCOV-19/src/models/logit/freq/5-fold_Cross_Validation/train_error.rds"); rm(indices);
train_error <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit/freq/5-fold_Cross_Validation/train_error.rds");

# Complexity Vs (Cutoff, Accuracy, Sensitivity, Specificity) on TRAIN and TEST sets;
{
cut.plot <- ggplot() +
  
  # 5-fold CV Cutoff;
  geom_line(aes(x = c(1:11), y = apply(X = test_error$CUTOFF, MARGIN = 1, FUN = mean)),
            col = "black",
            size = 0.75) +
  geom_line(aes(x = c(1:11), y = apply(X = train_error$CUTOFF, MARGIN = 1, FUN = mean)),
            col = "tomato3",
            size = 0.75) +
  
  geom_ribbon(aes(x = c(1:11),
                  ymin = apply(X = test_error$CUTOFF, MARGIN = 1, FUN = min), 
                  ymax = apply(X = test_error$CUTOFF, MARGIN = 1, FUN = max)), 
              alpha    = 0.1,
              linetype = "dashed",
              colour   = "black",
              size     = 0.75,
              fill     = "black") +
  geom_ribbon(aes(x = c(1:11),
                  ymin = apply(X = train_error$CUTOFF, MARGIN = 1, FUN = min), 
                  ymax = apply(X = train_error$CUTOFF, MARGIN = 1, FUN = max)), 
              alpha    = 0.1,
              linetype = "dashed",
              colour   = "tomato3",
              size     = 0.75,
              fill     = "tomato3") +
  
  # Custom Labels;
  labs(title = "",
       subtitle = "",
       x = "Complexity",
       y = "Cutoff") +
  theme_bw(base_size = 17.5, base_family = "Times");

acc.plot <- ggplot() +
  
  # 5-fold CV Accuracy;
  geom_line(aes(x = c(1:11), y = apply(X = test_error$ACCURACY, MARGIN = 1, FUN = mean)),
            col = "black",
            size = 0.75) +
  geom_line(aes(x = c(1:11), y = apply(X = train_error$ACCURACY, MARGIN = 1, FUN = mean)),
            col = "tomato3",
            size = 0.75) +
  
  geom_ribbon(aes(x = c(1:11),
                  ymin = apply(X = test_error$ACCURACY, MARGIN = 1, FUN = min), 
                  ymax = apply(X = test_error$ACCURACY, MARGIN = 1, FUN = max)), 
              alpha    = 0.1,
              linetype = "dashed",
              colour   = "black",
              size     = 0.75,
              fill     = "black") +
  geom_ribbon(aes(x = c(1:11),
                  ymin = apply(X = train_error$ACCURACY, MARGIN = 1, FUN = min), 
                  ymax = apply(X = train_error$ACCURACY, MARGIN = 1, FUN = max)), 
              alpha    = 0.1,
              linetype = "dashed",
              colour   = "tomato3",
              size     = 0.75,
              fill     = "tomato3") +
  
  # Custom Labels;
  labs(title = "",
       subtitle = "",
       x = "Complexity",
       y = "Accuracy") +
  theme_bw(base_size = 17.5, base_family = "Times");

sen.plot <- ggplot() +
  
  # 5-fold CV Sensitivity;
  geom_line(aes(x = c(1:11), y = apply(X = test_error$SENSITIVITY, MARGIN = 1, FUN = mean)),
            col = "black",
            size = 0.75) +
  geom_line(aes(x = c(1:11), y = apply(X = train_error$SENSITIVITY, MARGIN = 1, FUN = mean)),
            col = "tomato3",
            size = 0.75) +
  
  geom_ribbon(aes(x = c(1:11),
                  ymin = apply(X = test_error$SENSITIVITY, MARGIN = 1, FUN = min), 
                  ymax = apply(X = test_error$SENSITIVITY, MARGIN = 1, FUN = max)), 
              alpha    = 0.1,
              linetype = "dashed",
              colour   = "black",
              size     = 0.75,
              fill     = "black") +
  geom_ribbon(aes(x = c(1:11),
                  ymin = apply(X = train_error$SENSITIVITY, MARGIN = 1, FUN = min), 
                  ymax = apply(X = train_error$SENSITIVITY, MARGIN = 1, FUN = max)), 
              alpha    = 0.1,
              linetype = "dashed",
              colour   = "tomato3",
              size     = 0.75,
              fill     = "tomato3") +
  
  # Custom Labels;
  labs(title = "",
       subtitle = "",
       x = "Complexity",
       y = "Sensitivity") +
  theme_bw(base_size = 17.5, base_family = "Times");

spe.plot <- ggplot() +
  
  # 5-fold CV Specificity;
  geom_line(aes(x = c(1:11), y = apply(X = test_error$SPECIFICITY, MARGIN = 1, FUN = mean)),
            col = "black",
            size = 0.75) +
  geom_line(aes(x = c(1:11), y = apply(X = train_error$SPECIFICITY, MARGIN = 1, FUN = mean)),
            col = "tomato3",
            size = 0.75) +
  
  geom_ribbon(aes(x = c(1:11),
                  ymin = apply(X = test_error$SPECIFICITY, MARGIN = 1, FUN = min), 
                  ymax = apply(X = test_error$SPECIFICITY, MARGIN = 1, FUN = max)), 
              alpha    = 0.1,
              linetype = "dashed",
              colour   = "black",
              size     = 0.75,
              fill     = "black") +
  geom_ribbon(aes(x = c(1:11),
                  ymin = apply(X = train_error$SPECIFICITY, MARGIN = 1, FUN = min), 
                  ymax = apply(X = train_error$SPECIFICITY, MARGIN = 1, FUN = max)), 
              alpha    = 0.1,
              linetype = "dashed",
              colour   = "tomato3",
              size     = 0.75,
              fill     = "tomato3") +
  
  # Custom Labels;
  labs(title = "",
       subtitle = "",
       x = "Complexity",
       y = "Specificty") +
  theme_bw(base_size = 17.5, base_family = "Times");

grid.arrange(cut.plot, acc.plot, sen.plot, spe.plot, nrow = 2); rm(acc.plot, cut.plot, sen.plot, spe.plot, test_error, train_error);
}

#################################################################################################################################################################################################################################################################################
## Final Model ##################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

glm.logit.fit     <- glm(FALLECIDO ~ ., family = binomial(link = "logit"), data = swabspos[, -c(5, 8, 11)], na.action = na.omit);
glm.logit.predict <- as.vector(predict(glm.logit.fit, newdata = swabspos, type = "response")); 
predicted.classes <- factor(ifelse(glm.logit.predict > 0.5, "Yes", "No"), levels = c("Yes", "No")); target <- factor(swabspos$FALLECIDO, levels = c("Yes", "No"));
confusionMatrix(data = predicted.classes, reference = target, positive = "Yes");
