list <- read.csv("~/TABASCO-MEXCOV-19/src/packages/list.csv")
list$package <- as.character(list$package)
for(i in 1:nrow(list))
{
  if(!require(list[i, 1], character.only = TRUE))
  {
    install.packages(as.character(list[i, 1]), character.only = TRUE)
    library(as.character(list[i, 1]), character.only = TRUE)
  }
}
rm(i, list)
