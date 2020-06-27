source("~/TABASCO-MEXCOV-19/src/cleansing/buffersraw.R")
new_deaths <- data.frame(Date = sort(unique(buffersraw$Date_Confirmed)))
colnames   <- sort(as.character(unique(buffersraw$State)))
buffersraw <- buffersraw[-c(which(is.na(buffersraw$Date_Death))),]
for(i in 1:length(unique(buffersraw$State)))
{
  tmp = buffersraw %>% filter(Result == "Positive" & State == sort(as.character(unique(buffersraw$State)))[i]) %>%
                       group_by(Date_Death) %>%
                       summarise(Deaths = n())
  new_deaths = merge(x = new_deaths, y = tmp, by.x = "Date", by.y = "Date_Death", all.x = TRUE);
  colnames(new_deaths)[i + 1] = colnames[i];
}
new_deaths[is.na(new_deaths)] <- 0;
for(i in 1:nrow(new_deaths)) {new_deaths$NACIONAL[i] <- sum(new_deaths[i, -1]);}
rm(i, buffersraw, tmp, colnames)
