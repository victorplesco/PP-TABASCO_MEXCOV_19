source("~/TABASCO-MEXCOV-19/src/cleansing/buffersraw.R")
ts_buffers <- data.frame(Date = sort(unique(buffersraw$dConfirmed)))
colnames   <- sort(as.character(unique(buffersraw$State)))
for(i in 1:length(unique(buffersraw$State)))
{
  tmp = buffersraw %>% filter(Result == "Positive" & State == sort(as.character(unique(buffersraw$State)))[i]) %>%
                       group_by(dConfirmed) %>%
                       summarise(Confirmed = n())
  ts_buffers = merge(x = ts_buffers, y = tmp, by.x = "Date", by.y = "dConfirmed", all.x = TRUE);
  colnames(ts_buffers)[i + 1] = colnames[i];
}
ts_buffers[is.na(ts_buffers)] <- 0;
for(i in 1:nrow(ts_buffers)) {ts_buffers$NACIONAL[i] <- sum(ts_buffers[i, -1]);}
rm(i, buffersraw, tmp, colnames);
