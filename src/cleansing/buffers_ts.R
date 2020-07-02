source("~/TABASCO-MEXCOV-19/src/cleansing/buffersraw.R")
buffers_ts <- data.frame(Date = sort(unique(buffersraw$dConfirmed)))
colnames   <- sort(as.character(unique(buffersraw$State)))
for(i in 1:length(unique(buffersraw$State)))
{
  tmp = buffersraw %>% filter(Result == "Positive" & State == sort(as.character(unique(buffersraw$State)))[i]) %>%
                       group_by(dConfirmed) %>%
                       summarise(Confirmed = n())
  buffers_ts = merge(x = buffers_ts, y = tmp, by.x = "Date", by.y = "dConfirmed", all.x = TRUE);
  colnames(buffers_ts)[i + 1] = colnames[i];
}
buffers_ts[is.na(buffers_ts)] <- 0;
for(i in 1:nrow(buffers_ts)) {buffers_ts$NACIONAL[i] <- sum(buffers_ts[i, -1]);}
rm(i, buffersraw, tmp, colnames);
