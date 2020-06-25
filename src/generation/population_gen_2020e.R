source("~/TABASCO-MEXCOV-19/src/cleansing/demographics_2020e.R")
males <- data.frame(Age        = demographics_2020e$Age,
                    Gender     = rep(0, nrow(demographics_2020e)),
                    Percentage = demographics_2020e$Percentage_Male)

females <- data.frame(Age        = demographics_2020e$Age,
                      Gender     = rep(1, nrow(demographics_2020e)),
                      Percentage = demographics_2020e$Percentage_Female)

total <- rbind(males, females)

source("~/TABASCO-MEXCOV-19/src/cleansing/population.R")
population <- population[-33,]
tot_pop    <- sum(population$Population[-33])

population_2020e <- data.frame(State  = rep(NA, tot_pop),
                               Age    = rep(NA, tot_pop),
                               Gender = rep(NA, tot_pop))

n <- 1;
k <- population[1, 2];
for(i in 1:nrow(population))
{
  if(i > 1){k = k + population[i, 2];}
  cat("BEGIN: ", n, " END ", k, "\n");
  index = sample(1:nrow(total), size = population[i, 2], prob = total$Percentage, replace = TRUE);
  population_2020e[n:k, 2] = as.character(total[index, 1]);
  population_2020e[n:k, 3] = total[index, 2];
  population_2020e[n:k, 1] = as.character(population$State[i]);
  n = n + population[i, 2];
}

n <- c()
for(i in 1:nrow(population))
{
  n[i] = length(which(population_2020e$Age == "unknown" & population_2020e$State == population[i, 1]));
  cat("Iteration: ", i, "\n");
}

list_of_vecs <- list()
for(i in 1:nrow(population)){list_of_vecs[[i]] = c(NA);}
for(i in 1:nrow(population))
{
  list_of_vecs[[i]] = which(population_2020e$Age  == "unknown" & population_2020e$State == population[i, 1]);  
  cat("Iteration: ", i, "\n");
}

for(i in 1:nrow(population))
{
  population_2020e[list_of_vecs[[i]], 2] = as.character(sample(total$Age[-c(22, 44)], size = n[i], prob = total$Percentage[-c(22, 44)]/sum(total$Percentage[-c(22, 44)]), replace = TRUE));
  cat("Iteration: ", i, "\n");
}

population_2020e$State  <- as.factor(population_2020e$State)
population_2020e$Age    <- as.factor(population_2020e$Age)
population_2020e$Gender <- as.factor(as.character(population_2020e$Gender))

# write_feather(population_2020e, "~/TABASCO-MEXCOV-19/data/metadata/population/population_2020e.feather") 