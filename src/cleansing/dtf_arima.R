source("~/TABASCO-MEXCOV-19/src/cleansing/buffers_ts.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/buffersraw.R")
states  <- unique(as.character(buffersraw$State[which(buffersraw$Region == "Center")]));
dtf.reg <- buffers_ts[, c(1, which(colnames(buffers_ts) %in% states))];
dtf.reg$dTotal <- rowSums(buffers_ts[, 2:19]);
dtf.reg$aTotal <- cumsum(dtf.reg$dTotal); 

dtf.reg   <- dtf.reg[which(dtf.reg$Date > "2020-03-07" & dtf.reg$Date <= "2020-06-15"),];
dtf.reg$lag_1_Confirmed <- c(0, dtf.reg$dTotal[-100])
dtf.reg$lag_2_Confirmed <- c(rep(0, 2), dtf.reg$dTotal[-c(99:100)])
dtf.reg$lag_3_Confirmed <- c(rep(0, 3), dtf.reg$dTotal[-c(98:100)])

train_set <- dtf.reg[which(dtf.reg$Date <= "2020-04-18"),];
test_set  <- dtf.reg[which(dtf.reg$Date >  "2020-04-18"),];
rm(buffersraw, dtf.reg, buffers_ts, states);
