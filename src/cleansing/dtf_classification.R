source("~/TABASCO-MEXCOV-19/src/cleansing/ts_buffers.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/new_deaths.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/buffersraw.R")

dtf.class <- buffersraw[which(buffersraw$Result != "Pending"),]; dtf.class <- dtf.class[, c(1, 2, 6, 3, 7, 4, 5)];
dtf.class$Result <- droplevels(dtf.class$Result); levels(dtf.class$Result) <- c("1", "0"); dtf.class$Result <- factor(dtf.class$Result, levels = c("0", "1"));
dtf.class$WeekDays <- as.factor(weekdays(as.POSIXct(as.character(dtf.class$dConfirmed), format = "%Y-%m-%d"), abbreviate = F));
levels(dtf.class$WeekDays) <- c("Sunday", "Thursday", "Monday", "Tuesday", "Wednesday", "Saturday", "Friday");
dtf.class$WeekDays <- factor(dtf.class$WeekDays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"));
dtf.class$Region <- factor(dtf.class$Region, levels = c("South", "Center", "North"));

source("~/TABASCO-MEXCOV-19/src/support/age_intervals.R")
# Last Update: granularity = 2;
dtf.class$Age.Labels <- cut(dtf.class$Age, breaks = breaks, labels = age_classes, include.lowest = TRUE)

# Merge - Confirmed;
ts_buffers$Aggregated <- cumsum(ts_buffers$NACIONAL)
pre.merge.confirmed <- ts_buffers[, c("Date", "Aggregated")]
dtf.class <- merge(dtf.class, pre.merge.confirmed, by.x = "dConfirmed", by.y = "Date", all.x = TRUE)

# Merge - Deaths;
new_deaths$Aggregated <- cumsum(new_deaths$NACIONAL)
pre.merge.deaths <- new_deaths[, c("Date", "Aggregated")]
dtf.class <- merge(dtf.class, pre.merge.deaths, by.x = "dDeaths", by.y = "Date", all.x = TRUE)

train_set <- dtf.class[dtf.class$dConfirmed <= "2020-04-18",]
test_set  <- dtf.class[dtf.class$dConfirmed > "2020-04-18",]

rm(buffersraw, dtf.class, new_deaths, pre.merge.confirmed, pre.merge.deaths, ts_buffers, age_classes, breaks);