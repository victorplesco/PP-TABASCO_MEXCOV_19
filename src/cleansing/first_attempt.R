source("~/TABASCO-MEXCOV-19/src/cleansing/trans_ts_confirmed.R") 
source("~/TABASCO-MEXCOV-19/src/cleansing/trans_ts_deaths.R") 

first_attempt <- as.data.frame(matrix(NA, nrow = (ncol(trans_ts_confirmed) - 2) * nrow(trans_ts_confirmed), ncol = 4))

j = 1;
for(i in 1:32) 
{
  first_attempt[j:(i * nrow(trans_ts_confirmed)), 1] = rep(colnames(trans_ts_confirmed)[i], nrow(trans_ts_confirmed))
  first_attempt[j:(i * nrow(trans_ts_confirmed)), 2] = c(trans_ts_confirmed[, i]);
  first_attempt[j:(i * nrow(trans_ts_confirmed)), 3] = c(trans_ts_deaths[, i]);
  first_attempt[j:(i * nrow(trans_ts_confirmed)), 4] = rep(trans_ts_confirmed$Date);
  j = j + nrow(trans_ts_confirmed);
}
colnames(first_attempt) <- c("State", "Confirmed", "Deaths", "Date");
first_attempt$Date <- as.Date(first_attempt$Date, "%Y-%m-%d")
first_attempt <- first_attempt[order(first_attempt[, 4]),]
rm(trans_ts_confirmed, trans_ts_deaths, i, j)

