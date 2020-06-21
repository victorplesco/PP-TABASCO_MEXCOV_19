population <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_population_MX.csv", 
                       header = TRUE)
View(population)

confirmed <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_confirmedraw.csv", 
                       header = TRUE) 
View(confirmed)

ts_confirmed <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_time_series_confirmed_MX.csv",
                         header = TRUE)
View(ts_confirmed)

ts_deaths <- read.csv("~/TABASCO-MEXCOV-19/data/original/datasets_587393_1092718_time_series_deaths_MX.csv",
                      header = TRUE)
View(ts_deaths)
