source("~/TABASCO-MEXCOV-19/src/cleansing/ts_confirmed.R") 
trans_ts_confirmed <- as.data.frame(t(as.matrix(ts_confirmed[, 2:ncol(ts_confirmed)])))
for(i in 1:ncol(trans_ts_confirmed)) {colnames(trans_ts_confirmed)[i] = as.character(ts_confirmed[i, 1]);}
trans_ts_confirmed$Date <- rownames(trans_ts_confirmed); rownames(trans_ts_confirmed) <- seq(1, nrow(trans_ts_confirmed), 1)
rm(ts_confirmed, i)
