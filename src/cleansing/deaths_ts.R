source("~/TABASCO-MEXCOV-19/src/cleansing/buffersraw.R")
deaths_ts <- data.frame(Date = sort(unique(buffersraw$dConfirmed)))
colnames   <- sort(as.character(unique(buffersraw$State)))
buffersraw <- buffersraw[-c(which(is.na(buffersraw$dDeaths))),]
for(i in 1:length(unique(buffersraw$State)))
{
  tmp = buffersraw %>% filter(Result == "Positive" & State == sort(as.character(unique(buffersraw$State)))[i]) %>%
                       group_by(dDeaths) %>%
                       summarise(Deaths = n())
  deaths_ts = merge(x = deaths_ts, y = tmp, by.x = "Date", by.y = "dDeaths", all.x = TRUE);
  colnames(deaths_ts)[i + 1] = colnames[i];
}
deaths_ts[is.na(deaths_ts)] <- 0;
for(i in 1:nrow(deaths_ts)) {deaths_ts$NACIONAL[i] <- sum(deaths_ts[i, -1]);}
rm(i, buffersraw, tmp, colnames)
