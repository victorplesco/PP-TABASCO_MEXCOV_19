source("~/TABASCO-MEXCOV-19/src/packages/autoinstall.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/dtf_classification.R")


#################################################################################################################################################################################################################################################################################
## Contingency Analysis: Independence ###########################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################


##
## WeekDays
##

weekdays.table <- with(train_set, table(Result, WeekDays))
chisq.test(weekdays.table)
CramerV(weekdays.table)

## Plot
mosaicplot(t(weekdays.table), col = c("indianred", "dodgerblue"), cex.axis = 1, sub = "", ylab = "", main = "")

source("~/TABASCO-MEXCOV-19/src/cleansing/ts_buffers.R")
ts_buffers$WeekDays <- as.factor(weekdays(as.POSIXct(as.character(ts_buffers$Date), format = "%Y-%m-%d"), abbreviate = F));
levels(ts_buffers$WeekDays) <- c("Sunday", "Thursday", "Monday", "Tuesday", "Wednesday", "Saturday", "Friday");
ts_buffers$WeekDays <- factor(ts_buffers$WeekDays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"));

dev.off()
ggplot() + 
  geom_line(data = ts_buffers, aes(x = Date, y = NACIONAL), col = "red2") +
  geom_point(data = ts_buffers[ts_buffers$WeekDays == "Saturday",], aes(x = Date, y = NACIONAL), col = "dodgerblue") +
  geom_point(data = ts_buffers[ts_buffers$WeekDays == "Sunday",], aes(x = Date, y = NACIONAL), col = "black") +
  theme_bw()

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

# Normality Assumption
par(mfrow = c(1, 2))
qqnorm(train_set$Age[train_set$Result == "1"], sub = "Result | Positive")
qqline(train_set$Age[train_set$Result == "1"], col = "red2")
qqnorm(train_set$Age[train_set$Result == "0"], sub = "Result | Negative")
qqline(train_set$Age[train_set$Result == "0"], col = "red2")
par(mfrow = c(1, 1))

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

  
glm.logit.fit <- glm(Result ~ Age.Labels + Gender + WeekDays, family = binomial(link = "logit"), data = train_set)
# summary(glm.logit.fit)
anova(glm.logit.fit, test = "Chisq")

# install.packages("pscl")
require(pscl)

list(model1 = pscl::pR2(glm.logit.fit1)["McFadden"],
     model2 = pscl::pR2(glm.logit.fit2)["McFadden"],
     model  = pscl::pR2(glm.logit.fit )["McFadden"])

glm.logit.fit1 <- glm(Result ~ Age.Labels, family = binomial(link = "logit"), data = train_set)
glm.logit.fit2 <- glm(Result ~ Age.Labels + WeekDays, family = binomial(link = "logit"), data = train_set)
anova(glm.logit.fit1, glm.logit.fit2, glm.logit.fit, test = "Chisq")


cutoff <- seq(0.01, 1, 0.01);
indexes <- data.frame(Sensitivity = rep(NA, length(cutoff)),
                      Specificity = rep(NA, length(cutoff)),
                      Accuracy    = rep(NA, length(cutoff)))
glm.logit.predict <- as.vector(predict(glm.logit.fit, newdata = test_set[, c("Age.Labels", "Gender", "Region", "WeekDays")], type = "response")); 
tmp <- test_set$Result;
for(i in 1:length(cutoff))
{
  predicted.classes <- as.factor(ifelse(glm.logit.predict > cutoff[i], "Positive", "Negative")); 
  tmp2 = confusionMatrix(data = predicted.classes, reference = tmp);
  
  indexes$Sensitivity[i] = tmp2$table[1, 1] / sum(tmp2$table[1, 1:2]);
  indexes$Specificity[i] = tmp2$table[2, 1] / sum(tmp2$table[2, 1:2]);
  indexes$Accuracy[i]    = sum(tmp2$table[1, 1], tmp2$table[2, 2]) / sum(tmp2$table[1:2, 1:2]);
}

ggplot() + geom_line(aes(x = cutoff, y = indexes$Sensitivity), col = "indianred") +
           geom_line(aes(x = cutoff, y = (1 - indexes$Specificity)), col = "dodgerblue") +
           geom_line(aes(x = cutoff, y = indexes$Accuracy), col = "black") +
           theme_bw()
          
## Aggregated
glm.logit.predict <- as.vector(predict(glm.logit.fit, newdata = test_set[, c("Age.Labels", "Gender", "Region", "WeekDays")], type = "response")); 
predicted.classes <- as.factor(ifelse(glm.logit.predict > 0.3125, "Positive", "Negative")); tmp <- test_set$Result;
confusionMatrix(data = predicted.classes, reference = tmp)$table/sum(confusionMatrix(data = predicted.classes, reference = tmp)$table);

## Daily
dIndex <- data.frame(Date = unique(test_set$dConfirmed),
                     Confirmed = rep(NA, length(unique(test_set$dConfirmed))))
minDate <- c()
for(i in 1:nrow(dIndex)) {minDate[i] = min(which(test_set$dConfirmed == dIndex[i, 1]))}
maxDate <- c()
for(i in 1:nrow(dIndex)) {maxDate[i] = max(which(test_set$dConfirmed == dIndex[i, 1]))}
for(i in 1:nrow(dIndex))
{
  tmp = as.data.frame(test_set %>% filter(test_set$dConfirmed == dIndex[i, 1]) %>%
                        select(Age, Gender, Region, WeekDays, Result));
  glm.logit.predict = as.vector(predict(glm.logit.fit, newdata = tmp[, -5], type = "response")); 
  tmp2 = as.factor(ifelse(glm.logit.predict > 0.3125, "Positive", "Negative"));
  dIndex[i, 2] = confusionMatrix(data = tmp2, reference = tmp$Result)$table[2, 2]; 
}

tmp <- as.data.frame(test_set %>% group_by(dConfirmed) %>% summarise(COUNT = n()))
ggplot() + geom_line(data = tmp, aes(x = dConfirmed, y = COUNT), col = "red2") +
           geom_line(data = dIndex, aes(x = Date, y = Confirmed), col = "black") +
  theme_bw()


#################################################################################################################################################################################################################################################################################
## Contingency Analysis: Tree Model #############################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################








