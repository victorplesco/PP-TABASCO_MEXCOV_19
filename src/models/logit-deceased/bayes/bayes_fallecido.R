source("~/TABASCO-MEXCOV-19/src/packages/install.packages.R");
source("~/TABASCO-MEXCOV-19/src/cleansing/swabspos_0718.R");







ind <- sample(1:nrow(swabspos), 1000, replace = F); 

test.data.simple <- swabspos[ind, c(1, 12)];  
test.data.complex <- swabspos[ind,];
# summary(test.data);

stan.glm.model.simple <- stan_glm(FALLECIDO ~ ., family = binomial(link = "logit"), data = test.data.simple);
stan.glm.model.complex <- stan_glm(FALLECIDO ~ ., family = binomial(link = "logit"), data = test.data.complex);

coef(stan.glm.model.simple);
coef(stan.glm.model.complex);

as.data.frame(stan.glm.model); # DISTRIBUTION of PARAMETERS;
summary(stan.glm.model);
coef(stan.glm.model); # purrr::map_dbl(as.data.frame(stan.glm.model), median);
posterior_interval(stan.glm.model);


pplot<-plot(tstudent.stan.glm.model.8x2000, "areas", prob = 0.95, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)


linpred <- posterior_linpred(tstudent.stan.glm.model.8x2000);
preds   <- posterior_linpred(tstudent.stan.glm.model.8x2000, transform = TRUE, newdata = test.set)
pred    <- colMeans(preds)
pr      <- factor(ifelse(as.integer(pred >= 0.1610234) == 1, "Yes", "No"), levels = c("Yes", "No")); target <- factor(test.set$FALLECIDO, levels = c("Yes", "No"));
confusionMatrix(data = pr, reference = target, positive = "Yes");

cutoff  = seq(min(pred), max(pred), 0.001);
indices = data.frame(specificity   = rep(NA, length(cutoff)),
                     sensitivity   = rep(NA, length(cutoff)),
                     accuracy      = rep(NA, length(cutoff)));

for(i in 1:length(cutoff))
{
  predicted.classes = factor(ifelse(pred > cutoff[i], "Yes", "No"), levels = c("Yes", "No")); 
  tmp = confusionMatrix(predicted.classes, target, positive = "Yes");
  
  indices$specificity[i]   = as.numeric(tmp$byClass[2]);
  indices$sensitivity[i]   = as.numeric(tmp$byClass[1]);
  indices$accuracy[i]      = as.numeric(tmp$overall[1]);
}

indices$optimal    = abs(indices$sensitivity - indices$specificity); opt = which(abs(indices$optimal) == min(abs(indices$optimal), na.rm = TRUE))[1];
predicted.classes  = factor(ifelse(pred > cutoff[opt], "Yes", "No"), levels = c("Yes", "No"));
optconfusionmatrix = table(Actual = target, Predicted = predicted.classes);
show.plot = TRUE
if(show.plot)
{
  p.plot = ggplot() +
    
    ## Specificity;
    geom_line(aes(x = cutoff, y = indices$specificity), col = "dodgerblue3") +
    
    ## Sensitivity
    geom_line(aes(x = cutoff, y = indices$sensitivity), col = "tomato3") +
    
    ## Accuracy
    geom_line(aes(x = cutoff, y =  indices$accuracy), col = "forestgreen") +
    
    ## Cut-off
    geom_point(aes(x = cutoff[opt],
                   y = indices$sensitivity[opt]),
               col = "black",
               size = 3) +
    
    ## Custom Label
    labs(title = "",
         subtitle = "",
         x = "Cut-off",
         y = "Classification Metric") +
    theme_bw(base_size = 15, base_family = "Times");
  print(p.plot);
};

out = list(Cutoff = cutoff[opt], Specificity = indices$specificity[opt], Sensitivity = indices$sensitivity[opt], Accuracy = indices$accuracy[opt], "Confusion Matrix" = optconfusionmatrix); 
return(out);

?stan_glm()
end - start

stan.glm.model1 <- stan_glm(FALLECIDO ~ EDAD, 
                            family = binomial(link = "logit"), 
                            data = train.data,
                            prior_intercept = student_t(df = 7, location = 0, scale = 2.5),
                            prior           = student_t(df = 7, location = 0, scale = 2.5),
);
stan.glm.model2 <- stan_glm(FALLECIDO ~ EDAD + SEXO, 
                            family = binomial(link = "logit"), 
                            data = train.data,
                            prior_intercept = student_t(df = 7, location = 0, scale = 2.5),
                            prior           = student_t(df = 7, location = 0, scale = 2.5),
);
stan.glm.model3 <- stan_glm(FALLECIDO ~ EDAD + SEXO + DIABETES, 
                            family = binomial(link = "logit"), 
                            data = train.data,
                            prior_intercept = student_t(df = 7, location = 0, scale = 2.5),
                            prior           = student_t(df = 7, location = 0, scale = 2.5),
);
stan.glm.model4 <- stan_glm(FALLECIDO ~ ., 
                            family = binomial(link = "logit"), 
                            data = train.data,
                            prior_intercept = student_t(df = 7, location = 0, scale = 2.5),
                            prior           = student_t(df = 7, location = 0, scale = 2.5),
);

linpred <- posterior_linpred(post1)
preds <- posterior_linpred(post1, transform=TRUE)
pred <- colMeans(preds)
pr <- as.integer(pred >= 0.5)



loo0 <- loo(stan.glm.model)

loo1 <- loo(stan.glm.model1);
loo2 <- loo(stan.glm.model2);
loo3 <- loo(stan.glm.model3);
loo4 <- loo(stan.glm.model4);

loo_compare(loo1, loo2, loo3, loo4, loo0);

launch_shinystan(stan.glm.model, ppd = FALSE)
tmp <- posterior_predict(stan.glm.model)
summary(tmp)


stan_hist(stan.glm.model, bins = 40)
posterior_vs_prior(stan.glm.model, group_by_parameter = TRUE, pars=c("(Intercept)"))+
  posterior_vs_prior(stan.glm.model, group_by_parameter = TRUE)

?posterior_linpred
pred <- colMeans(preds)
pr <- factor(ifelse(as.integer(pred >= 0.5) == 1, "Yes", "No"), levels = c("No", "Yes"))
confusionMatrix(pr, test.data$FALLECIDO)
# confusion matrix
caret::confusionMatrix(as.factor(as.numeric(pr>0.5)), y)[2]



loo1 <- loo(stan.glm.model, k_threshold = 0.7)


pp_check(stan.glm.model)









coef(stan.glm.model);
as.data.frame(stan.glm.model); # DISTRIBUTION of PARAMETERS;
summary(stan.glm.model);
coef(stan.glm.model); # purrr::map_dbl(as.data.frame(stan.glm.model), median);
posterior_interval(stan.glm.model);
plot(stan.glm.model, "areas", prob = 0.95, prob_outer = 1) + geom_vline(xintercept = 0)

stan_glm.logit.predict <- as.vector(predict(stan.glm.model, newdata = swabspos, type = "response")); 
predicted.classes <- factor(ifelse(glm.logit.predict > 0.1610234, "Yes", "No"), levels = c("Yes", "No")); target <- factor(swabspos$FALLECIDO, levels = c("Yes", "No"));
confusionMatrix(data = predicted.classes, reference = target, positive = "Yes");
