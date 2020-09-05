source("~/TABASCO-MEXCOV-19/src/packages/install.packages.R");
swabspos <- read.csv(gzfile("~/TABASCO-MEXCOV-19/data/cleansed/0718/swabspos_0718.csv.gz"));

#################################################################################################################################################################################################################################################################################
## Bootstrap | FACTOR Regressors ################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

col <- c(1, 3:11); B <- 10^4; n <- nrow(swabspos);
fact_matrix <- matrix(0, ncol = ncol(swabspos), nrow = B); colnames(fact_matrix) <- colnames(swabspos);

for(i in col)
{cat(paste0("i-", i), "\n");
  for(j in 1:B)
  {cat(paste0("  j-", j, "\n"));
    ind = sample(1:n, n, replace = TRUE);
    fact_matrix[j, i] = OddsRatio(table(swabspos[ind, i], swabspos[ind, 12]), method = "wald");
  };
}; rm(i, j);

# saveRDS(fact_matrix, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Bootstrap-Priors/fact_matrix.rds"); 
fact_matrix <- readRDS( "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Bootstrap-Priors/fact_matrix.rds");

#################################################################################################################################################################################################################################################################################
## Bootstrap | Intercept ########################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

for(j in 1:B)
{cat(paste0("  j-", j, "\n"));
  ind = sample(1:n, n, replace = TRUE);
  fact_matrix[j, 12] = length(which(swabspos$FALLECIDO[ind] == "Yes"))/length(which(swabspos$FALLECIDO[ind] == "No"))
}; rm(i);

# saveRDS(fact_matrix, "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Bootstrap-Priors/fact_matrix.rds"); 
fact_matrix <- readRDS( "~/TABASCO-MEXCOV-19/src/models/logit-deceased/bayes/Bootstrap-Priors/fact_matrix.rds");