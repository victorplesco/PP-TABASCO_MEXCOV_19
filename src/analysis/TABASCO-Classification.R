source("~/TABASCO-MEXCOV-19/src/packages/autoinstall.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/dtf_classification.R")

#################################################################################################################################################################################################################################################################################
## Contingency Analysis: Independence ###########################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

##
## Age
##

ggplot(data = train_set, aes(x = Result, y = Age)) +
  geom_boxplot(aes(col = Result), fill = "white") + 
  scale_color_manual(values = c("indianred", "black")) +
  
  # Custom Label
  labs(title = "",
       subtitle = "",
       x = "Result",
       y = "Age") +
  theme_bw(base_size = 10, base_family = "Times") +
  theme(legend.position = "top")

## t.test

#   Normality Assumption
par(mfrow = c(1, 2))
qqnorm(train_set$Age[train_set$Result == "Positive"])
qqline(train_set$Age[train_set$Result == "Positive"], col = "red2")
qqnorm(train_set$Age[train_set$Result == "Negative"])
qqline(train_set$Age[train_set$Result == "Negative"], col = "red2")

t.test(Age ~ Result, data = train_set, var.equal = FALSE)

####
#### x Region
####

#### Plot
    north <- ggplot(data = train_set[train_set$Region == "North",], aes(x = Result, y = Age)) +
               geom_boxplot(aes(col = Result), fill = "white") + 
               scale_color_manual(values = c("indianred", "black")) +
      
               # Custom Label
               labs(title = "",
                    subtitle = "",
                    x = "Result | North",
                    y = "") +
               theme_bw(base_size = 15, base_family = "Times") +
               theme(legend.position = "none")
    
    south <- ggplot(data = train_set[train_set$Region == "South",], aes(x = Result, y = Age)) +
               geom_boxplot(aes(col = Result), fill = "white") + 
               scale_color_manual(values = c("indianred", "black")) +
              
               # Custom Label
               labs(title = "",
                    subtitle = "",
                    x = "Result | South",
                    y = "Age") +
              theme_bw(base_size = 15, base_family = "Times") +
              theme(legend.position = "none")
    
    center <- ggplot(data = train_set[train_set$Region == "Center",], aes(x = Result, y = Age)) +
                geom_boxplot(aes(col = Result), fill = "white") + 
                scale_color_manual(values = c("indianred", "black")) +
                
                # Custom Label
                labs(title = "",
                     subtitle = "",
                     x = "Result | Center",
                     y = "") +
                theme_bw(base_size = 15, base_family = "Times") +
                theme(legend.position = "top")

    grid.arrange(south, center, north, nrow = 1)
    
#### t.test
    t.test(Age ~ Result, data = train_set[train_set$Region == "South",], var.equal = FALSE)
    t.test(Age ~ Result, data = train_set[train_set$Region == "Center",], var.equal = FALSE)
    t.test(Age ~ Result, data = train_set[train_set$Region == "North",], var.equal = FALSE)
    
##
## Age.Labels
##

age.table <- with(train_set, table(Result, Age.Labels))
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
    
   age.Region.table <- with(train_set, table(Result, Age.Labels, Region))
  
#### Mantel-Haen
    with(train_set, my_mantelhaen.test(age.Region.table, alternative = "two.sided"))
    
#### Plot
    doubledecker(Result ~ Region + Age.Labels, data = train_set)
  
####
#### x State
####
    
    age.state.table <- with(train_set, table(State, Result, Age.Labels))
    
#### Mantel-Haen
    with(train_set, my_mantelhaen.test(age.state.table, alternative = "two.sided"))

##  
## Gender
##
    
gender.table <- with(train_set, table(Result, Gender))
chisq.test(gender.table)
CramerV(gender.table)

## Plot
mosaicplot(t(gender.table), col = c("indianred", "dodgerblue"), cex.axis = 1, sub = "", ylab = "", main = "")

####
#### x Region
####

   gender.Region.table <- with(train_set, table(Result, Gender, Region))
  
#### Mantel-Haen
    with(train_set, my_mantelhaen.test(gender.Region.table, alternative = "two.sided"))
  
#### Plot
    doubledecker(Result ~ Region + Gender, data = train_set)
  
####
#### x State
####
    
    gender.state.table <- with(train_set, table(Result, Gender, State))
  
#### Mantel-Haen
    with(train_set, my_mantelhaen.test(gender.state.table, alternative = "two.sided"))
 
## 
## State
##
    
state.table <- with(train_set, table(Result, State))
chisq.test(state.table)
CramerV(state.table)

## Plot
mosaicplot(t(state.table), col = c("indianred", "dodgerblue"), cex.axis = 1, sub = "", ylab = "", main = "")

##
## Region
##

Region.table <- with(train_set, table(Result, Region))
chisq.test(Region.table)
CramerV(Region.table)

## Plot
mosaicplot(t(Region.table), col = c("indianred", "dodgerblue"), cex.axis = 1, sub = "", ylab = "", main = "")


#################################################################################################################################################################################################################################################################################
## Contingency Analysis: Logistic Regression ####################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################


glm.logit.fit <- glm(Result ~ Age + Region + Gender + WeekDays, family = binomial(link = "logit"), data = train_set)
# summary(glm.logit.fit)

ts_buffers$Aggregated <- cumsum(ts_buffers$NACIONAL)
pre.merge <- ts_buffers[, c("Date", "Aggregated")]

test_set  <- as.data.frame(buffersraw %>% filter(Result != "Pending" & dConfirmed > "2020-04-18"))
test_set$Result <- droplevels(test_set$Result); levels(test_set$Result) <- c("0", "1");
test_set$WeekDays <- as.factor(weekdays(as.POSIXct(test_set$dConfirmed, format = "%Y-%m-%d"), abbreviate = F));
test_set$WeekEnds <- as.factor(ifelse(as.character(test_set$WeekDays) != "sabato" & as.character(test_set$WeekDays) != "domenica", "0", "1"));

source("~/TABASCO-MEXCOV-19/src/support/age_intervals.R")
# Last Update: granularity = 2;
test_set$Age.Labels <- cut(test_set$Age, breaks = breaks, labels = age_classes, include.lowest = TRUE)

test_set <- merge(test_set, pre.merge, by.x = "dConfirmed", by.y = "Date", all.x = TRUE)

# install.packages("caret")
# require(caret)  

glm.logit.predict <- predict(glm.logit.fit, newdata = test_set)
confusionMatrix(data = as.factor(as.character(as.numeric(glm.logit.predict > 0.5))), reference = test_set$Result)
