source("~/TABASCO-MEXCOV-19/src/packages/install.packages.R");
swabspos <- read.csv(gzfile("~/TABASCO-MEXCOV-19/data/cleansed/0718/swabspos_log_0718.csv.gz"));

#################################################################################################################################################################################################################################################################################
## Train/Test Set ###############################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

set.seed(123);
ind <- sample(1:nrow(swabspos), 0.01 * nrow(swabspos), replace = F); 
train.set <- swabspos[ind, -c(5, 8, 11)]; test.set  <- swabspos[-ind, -c(5, 8, 11)]; rm(ind);

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
write_feather(as.data.frame(runtime), "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_1/tstudent.stan.glm.model.8x2000.feather");
};

# saveRDS(tstudent.stan.glm.model.8x2000, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_1/tstudent.stan.glm.model.8x2000.rds"); rm(tstudent.stan.glm.model.8x2000.rds);
tstudent.stan.glm.model.8x2000 <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_1/tstudent.stan.glm.model.8x2000.rds");

#################################################################################################################################################################################################################################################################################
## Model 2 - normal with Bootstrap parameters 8x2000 ############################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

results  <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Parameters/results.rds");
boot_location <- as.numeric(apply(X = apply(X = results[, -c(5, 8, 11)], MARGIN = 2, FUN = log), MARGIN = 2, FUN = mean)); boot_location[2] <- 0;
boot_scale    <- as.numeric(apply(X = apply(X = results[, -c(5, 8, 11)], MARGIN = 2, FUN = log), MARGIN = 2, FUN = sd)); boot_scale[2] <- 2.5;

{
  start <- Sys.time();
  boot.normal.stan.glm.model.8x2000 <- stan_glm(FALLECIDO ~ ., 
                                             family = binomial(link = "logit"), 
                                             data = train.set,
                                             prior_intercept = normal(location = boot_location[ 9],  scale = boot_scale[ 9]),
                                             prior           = normal(location = boot_location[-9],  scale = boot_scale[-9]),
                                             chains = 8, cores = 8
  );
  end <- Sys.time();
  runtime <- end - start; cat("Runtime: ", runtime);
  write_feather(runtime, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_2/tstudent.stan.glm.model.8x2000.feather");
}; # rm(results, boot_location, boot_scale, train.set, test.set, swabspos);

# saveRDS(boot.normal.stan.glm.model.8x2000, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_2/boot.normal.stan.glm.model.8x2000.rds"); rm(boot.normal.stan.glm.model.8x2000);
boot.normal.stan.glm.model.8x2000 <- readRDS("~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Prior-Models/Model_2/boot.normal.stan.glm.model.8x2000.rds");
