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
