source("~/TABASCO-MEXCOV-19/src/cleansing/buffers_ts.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/deaths_ts.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/buffersraw.R")

dtf.class <- buffersraw[which(buffersraw$Result != "Pending"),]; dtf.class <- dtf.class[, c(1, 2, 6, 3, 7, 4, 5)];
dtf.class$Result <- droplevels(dtf.class$Result); dtf.class$Result <- factor(dtf.class$Result, levels = c("Negative", "Positive")); 
dtf.class$WeekDays <- as.factor(weekdays(as.POSIXct(as.character(dtf.class$dConfirmed), format = "%Y-%m-%d"), abbreviate = F));
levels(dtf.class$WeekDays) <- c("Sunday", "Thursday", "Monday", "Tuesday", "Wednesday", "Saturday", "Friday");
dtf.class$WeekDays <- factor(dtf.class$WeekDays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"));
dtf.class$Region <- factor(dtf.class$Region, levels = c("South", "Center", "North"));

source("~/TABASCO-MEXCOV-19/src/support/age_intervals.R")
# Last Update: granularity = 2;
dtf.class$Age.Labels <- cut(dtf.class$Age, breaks = breaks, labels = age_classes, include.lowest = TRUE)

# Merge - Confirmed;
pre.merge.confirmed <- buffers_ts[, c("Date", "NACIONAL")]
dtf.class <- merge(dtf.class, pre.merge.confirmed, by.x = "dConfirmed", by.y = "Date", all.x = TRUE)
colnames(dtf.class)[colnames(dtf.class) == "NACIONAL"] <- "daily_Confirmed";

# Merge - Deaths;
pre.merge.deaths <- deaths_ts[, c("Date", "NACIONAL")]
dtf.class <- merge(dtf.class, pre.merge.deaths, by.x = "dDeaths", by.y = "Date", all.x = TRUE)
colnames(dtf.class)[colnames(dtf.class) == "NACIONAL"] <- "daily_Deaths";

train_set   <- dtf.class[which(dtf.class$dConfirmed <= "2020-04-18"),];
test_set    <- dtf.class[which(dtf.class$dConfirmed > "2020-04-18"),];

rm(buffersraw, dtf.class, deaths_ts, pre.merge.confirmed, pre.merge.deaths, buffers_ts, age_classes, breaks);
