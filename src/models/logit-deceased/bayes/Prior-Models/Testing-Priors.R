source("~/TABASCO-MEXCOV-19/src/packages/install.packages.R");
swabspos <- read.csv(gzfile("~/TABASCO-MEXCOV-19/data/cleansed/0718/swabspos_0718.csv.gz"));

#################################################################################################################################################################################################################################################################################
## Train/Test Set ###############################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

set.seed(123);
ind <- sample(1:nrow(swabspos), 0.8 * nrow(swabspos), replace = F); 
train.set <- swabspos[ind,]; test.set  <- swabspos[-ind,]; rm(ind);

#################################################################################################################################################################################################################################################################################
## Model 1 - tstudent 8x2000 ####################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

{
start <- Sys.time();
tstudent.stan.glm.model.8x2000 <- stan_glm(FALLECIDO ~ ., 
                                           family = binomial(link = "logit"), 
                                           data = train.set,
                                           prior_intercept = student_t(df = 7, location = 0, scale = 2.5),
                                           prior           = student_t(df = 7, location = 0, scale = 2.5),
                                           chains = 8, cores = 8
                                           );
end <- Sys.time();
runtime <- end - start; cat("Runtime: ", runtime);
write_feather(runtime, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/tstudent.stan.glm.model.8x2000.feather");
};

# saveRDS(tstudent.stan.glm.model.8x2000.rds, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/tstudent.stan.glm.model.8x2000.rds"); rm(tstudent.stan.glm.model.8x2000.rds);
tstudent.stan.glm.model.8x2000 <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/tstudent.stan.glm.model.8x2000.rds");

#################################################################################################################################################################################################################################################################################
## Model 2 - normal with Bootstrap parameters 8x2000 ############################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

results  <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Parameters/results.rds");
boot_location <- as.numeric(apply(X = apply(X = results, MARGIN = 2, FUN = log), MARGIN = 2, FUN = mean)); boot_location[2] <- 0;
boot_scale    <- as.numeric(apply(X = apply(X = results, MARGIN = 2, FUN = log), MARGIN = 2, FUN = sd)); boot_scale[2] <- 1;

{
  start <- Sys.time();
  tstudent.stan.glm.model.8x2000 <- stan_glm(FALLECIDO ~ ., 
                                             family = binomial(link = "logit"), 
                                             data = train.set,
                                             prior_intercept = normal(location = boot_location[12],  scale = boot_scale[12]),
                                             prior           = normal(location = boot_location[-12], scale = boot_scale[-12]),
                                             chains = 8, cores = 8
  );
  end <- Sys.time();
  runtime <- end - start; cat("Runtime: ", runtime);
  write_feather(runtime, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/tstudent.stan.glm.model.8x2000.feather");
};

# saveRDS(boot.normal.stan.glm.model.8x2000.rds, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/boot.normal.stan.glm.model.8x2000.rds"); rm(boot.normal.stan.glm.model.8x2000.rds);
boot.normal.stan.glm.model.8x2000.rds <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/boot.normal.stan.glm.model.8x2000.rds");
