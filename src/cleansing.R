install.packages("knitr")
require(knitr)

##
## population
##

population <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_population_MX.csv", 
                       header = TRUE)
# View(population)
knitr::kable(summary(population))
# which(is.na(population))

# {Estado = State, Poblacion = Population}
colnames(population) <- c("State", "Population")

# {State, Age} = as.factor
population[, 1] <- lapply(population[, 1], factor)

# knitr::kable(summary(population))

##
## confirmedraw
##

confirmedraw <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_confirmedraw.csv", 
                       header = TRUE) 
# View(confirmedraw)
knitr::kable(summary(confirmedraw))
# which(is.na(confirmedraw))

# {Estado = State, Sexo = Gender, Edad = Age, Fecha.de.Inicio.de.sintomas = Date}
colnames(confirmedraw) <- c("State", "Gender", "Age", "Date")

# {State, Age} = as.factor
confirmedraw[, 1:2] <- lapply(confirmedraw[, 1:2], factor)

# {Date} = as.Date(%d/%m/%Y)
confirmedraw$Date <- as.Date(confirmedraw$Date, "%d/%m/%Y")

# knitr::kable(summary(confirmedraw))

ts_confirmed <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_time_series_confirmed_MX.csv",
                         header = TRUE)
View(ts_confirmed)

ts_deaths <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_time_series_deaths_MX.csv",
                      header = TRUE)
View(ts_deaths)
