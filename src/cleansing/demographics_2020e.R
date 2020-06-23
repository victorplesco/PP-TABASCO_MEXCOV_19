demographics_2020e <- read.csv("~/TABASCO-MEXCOV-19/data/metadata/demographics/Mexico_Demographics_2010.csv")
demographics_2020e$Percentage_Male   <- demographics_2020e$Male/sum(demographics_2020e$Total)
demographics_2020e$Percentage_Female <- demographics_2020e$Female/sum(demographics_2020e$Total)
demographics_2020e$Percentage_Total  <- demographics_2020e$Total/sum(demographics_2020e$Total)
source("~/TABASCO-MEXCOV-19/src/cleansing/population.R")
tot_pop <- sum(population$Population[-33])
demographics_2020e$Male   <- demographics_2020e$Percentage_Male * tot_pop
demographics_2020e$Female <- demographics_2020e$Percentage_Female * tot_pop
demographics_2020e$Total  <- demographics_2020e$Percentage_Total * tot_pop
rm(tot_pop, population);
