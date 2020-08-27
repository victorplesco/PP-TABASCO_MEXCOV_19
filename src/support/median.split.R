minor <- function(x)
{return(x[which(x <= median(x))]);}

major <- function(x)
{return(x[which(x >= median(x))]);}

n <- 2; # outputs n ^ 2 intervals;
dtf.indexes <- data.frame(empty = rep(NA, 3))
raw_interval <- function(n, direction, x)
{
  index = c(min(x), median(x), max(x))
  # print(index);
  
  minor = minor(x);
  major = major(x);
  
  colname = paste0((min(x) + median(x) + max(x)), direction);
  dtf.indexes[colname] <<- index;
  # print(dtf.indexes);
  
  if(n == 0){return(dtf.indexes)}
  
  n = n - 1;
  
  minorname = "minor";
  majorname = "major";
  
  raw_interval(n, minorname, minor)
  raw_interval(n, majorname, major)
}

breaks <- sort(unique(as.vector(t(raw_interval(n, "start", swabspos$EDAD)[, -1]))));
age_classes <- c(rep(NA, length(breaks) - 1));
for(i in 1:(length(breaks)-1)) 
{
  if(i == 1)
  {
    age_classes[i] = paste0(paste0(breaks[i], "-"), breaks[i + 1]);
    next;
  }
  
  if(i == (length(breaks) - 1))
  {
    age_classes[i] = paste0(breaks[i], "+");
    break;
  }
  
  age_classes[i] = paste0(paste0((breaks[i] + 1), "-"), breaks[i + 1]);
}

rm(dtf.indexes, i, n, major, minor, raw_interval);