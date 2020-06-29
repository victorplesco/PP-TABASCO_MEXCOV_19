

source("~/TABASCO-MEXCOV-19/src/packages/autoinstall.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/buffersraw.R")

dtf.class <- buffersraw[which(buffersraw$Result != "Pending" & buffersraw$Date_Confirmed <= "2020-04-18"),]
dtf.class <- dtf.class[, c(1, 2, 6, 3, 7)]
dtf.class$Result <- droplevels(dtf.class$Result);
# dtf.class


#################################################################################################################################################################################################################################################################################
## Age ##########################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################


# str(buffersraw)
# hist(buffersraw$Age)

source("~/TABASCO-MEXCOV-19/src/support/age_intervals.R")

# Last Update: granularity = 2;
dtf.class$Age.Labels <- cut(dtf.class$Age, breaks = breaks, labels = age_classes, include.lowest = TRUE)


#################################################################################################################################################################################################################################################################################
## Contingency Analysis: Independence ###########################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################


##
## Age.Labels
##

age.table <- with(dtf.class, table(Result, Age.Labels))
chisq.test(age.table)
CramerV(age.table)

## Plot
mosaicplot(t(age.table), col = c("indianred", "dodgerblue"), cex.axis = 1, sub = "", ylab = "", main = "")
  
####
#### x Region
####

  # FIRST ATTEMPT - WORKS #
  # dump("mantelhaen.test", file = "~/TABASCO-MEXCOV-19/src/support/my_mh.R")
  source("~/TABASCO-MEXCOV-19/src/support/my_mh.R")
    
   age.Region.table <- with(dtf.class, table(Result, Age.Labels, Region))
  
#### Mantel-Haen
    with(dtf.class, my_mantelhaen.test(age.Region.table, alternative = "two.sided"))
    
#### Plot
    doubledecker(Result ~ Region + Age.Labels, data = dtf.class)
  
####
#### x State
####
    
    age.state.table <- with(dtf.class, table(State, Result, Age.Labels))
    
#### Mantel-Haen
    with(dtf.class, my_mantelhaen.test(age.state.table, alternative = "two.sided"))

##  
## Gender
##
    
gender.table <- with(dtf.class, table(Result, Gender))
chisq.test(gender.table)
CramerV(gender.table)

## Plot
mosaicplot(t(gender.table), col = c("indianred", "dodgerblue"), cex.axis = 1, sub = "", ylab = "", main = "")

####
#### x Region
####

   gender.Region.table <- with(dtf.class, table(Result, Gender, Region))
  
#### Mantel-Haen
    with(dtf.class, my_mantelhaen.test(gender.Region.table, alternative = "two.sided"))
  
#### Plot
    doubledecker(Result ~ Region + Gender, data = dtf.class)
  
####
#### x State
####
    
    gender.state.table <- with(dtf.class, table(Result, Gender, State))
  
#### Mantel-Haen
    with(dtf.class, my_mantelhaen.test(gender.state.table, alternative = "two.sided"))
 
## 
## State
##
    
state.table <- with(dtf.class, table(Result, State))
chisq.test(state.table)
CramerV(state.table)

## Plot
mosaicplot(t(state.table), col = c("indianred", "dodgerblue"), cex.axis = 1, sub = "", ylab = "", main = "")

##
## Region
##

Region.table <- with(dtf.class, table(Result, Region))
chisq.test(Region.table)
CramerV(Region.table)

## Plot
mosaicplot(t(Region.table), col = c("indianred", "dodgerblue"), cex.axis = 1, sub = "", ylab = "", main = "")

