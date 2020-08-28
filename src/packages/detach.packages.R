detach.packages <- function(pkg)
{
  search_item <- paste("package", pkg, sep = ":");
  
  while(search_item %in% search())
  {detach(search_item, unload = TRUE, character.only = TRUE);};
};
