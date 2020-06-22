confirmedraw <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_confirmedraw.csv", 
                         header = TRUE) 
# {Estado = State, Sexo = Gender, Edad = Age, Fecha.de.Inicio.de.sintomas = Date}
colnames(confirmedraw) <- c("State", "Gender", "Age", "Date")
# {State, Age} = as.factor
confirmedraw[, 1:2] <- lapply(confirmedraw[, 1:2], factor)
# {Date} = as.Date(%d/%m/%Y)
confirmedraw$Date <- as.Date(confirmedraw$Date, "%d/%m/%Y")