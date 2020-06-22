ts_confirmed <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_time_series_confirmed_MX.csv",
                         header = TRUE)
# {Estado = State}
colnames(ts_confirmed)[colnames(ts_confirmed) == "Estado"] <- "State"
# {ts_confirmed[, -1]} = as.Date(%d/%m/%Y)
colnames(ts_confirmed)[-1] <- sub("X*", "", colnames(ts_confirmed)[-1])
colnames(ts_confirmed)[-1] <- gsub("[.]", "/", colnames(ts_confirmed)[-1])
names(ts_confirmed)[-1] <- as.character(as.Date(colnames(ts_confirmed)[-1], "%m/%d/%Y"))
