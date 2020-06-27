confirmedraw <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_confirmedraw.csv", 
                         header = TRUE) 
# {Estado = State, Sexo = Gender, Edad = Age, Fecha.de.Inicio.de.sintomas = Date}
colnames(confirmedraw) <- c("State", "Gender", "Age", "Date")
# {Gender} = as.factor
levels(confirmedraw$Gender) <- c("1", "0")
# {Date} = as.Date(%d/%m/%Y)
confirmedraw$Date <- as.Date(confirmedraw$Date, "%d/%m/%Y")
# {Age} = categorize
breaks <- seq(0, 100, 4)
age_classes <- c(rep(NA, length(breaks) - 1))
for(i in 1:(length(breaks)-1)) {age_classes[i] <- paste0(paste0(breaks[i], "-"), breaks[i + 1]);}
confirmedraw$Labels <- cut(confirmedraw$Age, breaks = breaks, labels = age_classes, include.lowest = TRUE)
confirmedraw <- confirmedraw[, c(1, 5, 2, 4)]
colnames(confirmedraw)[colnames(confirmedraw) == "Labels"] <- "Age"
rm(age_classes, breaks, i)
