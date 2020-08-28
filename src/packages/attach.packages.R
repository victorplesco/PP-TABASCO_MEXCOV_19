check.packages <- function(pkg)
{
  for(i in pkg) 
  {
    if(!require(as.character(i), character.only = TRUE))
    {
      install.packages(as.character(i), character.only = TRUE);
      library(as.character(i), character.only = TRUE);
    }
     else {library(as.character(i), character.only = TRUE)}
  };
};