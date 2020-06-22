population <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_population_MX.csv", 
                       header = TRUE)
# {Estado = State, Poblacion = Population}
colnames(population) <- c("State", "Population")
# {State, Age} = as.factor
population[, 1] <- as.factor(population[, 1])