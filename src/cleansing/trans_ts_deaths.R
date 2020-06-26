source("~/TABASCO-MEXCOV-19/src/cleansing/ts_deaths.R") 
trans_ts_deaths <- as.data.frame(t(as.matrix(ts_deaths[, 2:ncol(ts_deaths)])))
for(i in 1:ncol(trans_ts_deaths)) {colnames(trans_ts_deaths)[i] = as.character(ts_deaths[i, 1]);}
trans_ts_deaths$Date <- rownames(trans_ts_deaths); rownames(trans_ts_deaths) <- seq(1, nrow(trans_ts_deaths), 1)
rm(ts_deaths, i)
