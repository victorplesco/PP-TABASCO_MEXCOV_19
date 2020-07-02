source("~/TABASCO-MEXCOV-19/src/cleansing/buffers_ts.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/buffersraw.R")
states  <- unique(as.character(buffersraw$State[which(buffersraw$Region == "Center")]));
dtf.reg <- buffers_ts[, c(1, which(colnames(buffers_ts) %in% states))];
dtf.reg$dTotal <- rowSums(buffers_ts[, 2:19]);
dtf.reg$aTotal <- cumsum(dtf.reg$dTotal); 

dtf.reg   <- dtf.reg[which(dtf.reg$Date <= "2020-06-15"),];
train_set <- dtf.reg[which(dtf.reg$Date <= "2020-04-18" & dtf.reg$Date > "2020-03-07"),];
test_set  <- dtf.reg[which(dtf.reg$Date >  "2020-04-18"),];
rm(buffersraw, dtf.reg, buffers_ts, states);
