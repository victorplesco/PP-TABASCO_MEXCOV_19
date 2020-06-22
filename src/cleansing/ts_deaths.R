ts_deaths <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_time_series_deaths_MX.csv",
                      header = TRUE)
# {Estado = State}
colnames(ts_deaths)[colnames(ts_deaths) == "Estado"] <- "State"
# {ts_deaths[, -1]} = as.Date(%d/%m/%Y)
colnames(ts_deaths)[-1] <- sub("X*", "", colnames(ts_deaths)[-1])
colnames(ts_deaths)[-1] <- gsub("[.]", "/", colnames(ts_deaths)[-1])
names(ts_deaths)[-1] <- as.character(as.Date(colnames(ts_deaths)[-1], "%m/%d/%Y"))
