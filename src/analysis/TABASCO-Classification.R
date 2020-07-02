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
  theme_bw(base_size = 15, base_family = "Times") +
  theme(legend.position = "top")

## t.test

####
#### Normality Assumption
####

    par(mfrow = c(1, 2))
    qqnorm(train_set$Age[train_set$Result == "Positive"], sub = "Result | Positive")
    qqline(train_set$Age[train_set$Result == "Positive"], col = "red2")
    qqnorm(train_set$Age[train_set$Result == "Negative"], sub = "Result | Negative")
    qqline(train_set$Age[train_set$Result == "Negative"], col = "red2")
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

##
## Region
##

region.table <- with(train_set, table(Result, Region))
chisq.test(region.table)
CramerV(region.table)

## Plot

mosaicplot(t(region.table), col = c("indianred", "dodgerblue"), cex.axis = 1, sub = "", ylab = "", main = "")

##
## WeekDays
##

weekdays.table <- with(train_set, table(Result, WeekDays));
chisq.test(weekdays.table);
CramerV(weekdays.table);

## Plot

mosaicplot(t(weekdays.table), col = c("indianred", "dodgerblue"), cex.axis = 1, sub = "", ylab = "", main = "");

source("~/TABASCO-MEXCOV-19/src/cleansing/buffers_ts.R")
buffers_ts$WeekDays <- as.factor(weekdays(as.POSIXct(as.character(buffers_ts$Date), format = "%Y-%m-%d"), abbreviate = F));
levels(buffers_ts$WeekDays) <- c("Sunday", "Thursday", "Monday", "Tuesday", "Wednesday", "Saturday", "Friday");
buffers_ts$WeekDays <- factor(buffers_ts$WeekDays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"));

dev.off()
ggplot() + 
  geom_line(data = buffers_ts, aes(x = Date, y = NACIONAL), col = "red2") +
  geom_point(data = buffers_ts[buffers_ts$WeekDays == "Saturday",], aes(x = Date, y = NACIONAL), col = "dodgerblue") +
  geom_point(data = buffers_ts[buffers_ts$WeekDays == "Sunday",], aes(x = Date, y = NACIONAL), col = "black") +
  theme_bw()


#################################################################################################################################################################################################################################################################################
## Contingency Analysis: Logistic Regression ####################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################


##
## Train / Test
##

train_set <- train_set[which(train_set$Region == "Center"),]; train_set$State <- droplevels(train_set$State);
test_set <- test_set[which(test_set$Region == "Center"),]; test_set$State <- droplevels(test_set$State);

##
## glm.logit.fit
##

glm.logit.fit4 <- glm(Result ~ Age.Labels + Gender + WeekDays + State, family = binomial(link = "logit"), data = train_set)
# summary(glm.logit.fit4)
anova(glm.logit.fit4, test = "Chisq")

glm.logit.fit1 <- glm(Result ~ Age.Labels, family = binomial(link = "logit"), data = train_set)
glm.logit.fit2 <- glm(Result ~ Age.Labels + Gender, family = binomial(link = "logit"), data = train_set)
glm.logit.fit3 <- glm(Result ~ Age.Labels + Gender + WeekDays, family = binomial(link = "logit"), data = train_set)

anova(glm.logit.fit1, glm.logit.fit2, glm.logit.fit3, glm.logit.fit4, test = "Chisq")

list(model1 = pscl::pR2(glm.logit.fit1)["McFadden"],
     model2 = pscl::pR2(glm.logit.fit2)["McFadden"],
     model3 = pscl::pR2(glm.logit.fit3)["McFadden"],
     model4 = pscl::pR2(glm.logit.fit4)["McFadden"])

##
## confusion-matrix
##

cutoff <- seq(0.01, 1, 0.01);
indexes <- data.frame(Sensitivity = rep(NA, length(cutoff)),
                      Specificity = rep(NA, length(cutoff)),
                      Accuracy    = rep(NA, length(cutoff)))
glm.logit.predict <- as.vector(predict(glm.logit.fit4, newdata = test_set, type = "response")); 
tmp <- test_set$Result;
for(i in 1:length(cutoff))
{
  predicted.classes <- as.factor(ifelse(glm.logit.predict > cutoff[i], "Positive", "Negative")); 
  tmp2 = confusionMatrix(data = predicted.classes, reference = tmp, positive = "Positive");
  
  indexes$Sensitivity[i] = as.numeric(tmp2$byClass[1]);
  indexes$Specificity[i] = as.numeric(tmp2$byClass[2]);
  indexes$Accuracy[i]    = as.numeric(tmp2$overall[1]);
  indexes$Precision[i]   = as.numeric(tmp2$byClass[5]);
}

indexes$Optimal <- indexes$Sensitivity - indexes$Specificity;
  
ggplot() + 
  
  ## Sensitivity
  geom_line(aes(x = cutoff, y = indexes$Sensitivity), col = "indianred") +
  
  ## Specificity
  geom_line(aes(x = cutoff, y = indexes$Specificity), col = "black") +
  
  ## Accuracy
  geom_line(aes(x = cutoff, y = indexes$Accuracy), col = "dodgerblue") +
           
  ## Cut-off
  geom_point(aes(x = cutoff[which(abs(indexes$Optimal) == min(abs(indexes$Optimal), na.rm = TRUE))] - 0.003, 
                 y = indexes$Sensitivity[which(abs(indexes$Optimal) == min(abs(indexes$Optimal), na.rm = TRUE))] + 0.006), 
                      col = "black",
                      size = 3) +
           
  ## Custom Label
  labs(title = "",
      subtitle = "",
      x = "Cut-off",
      y = "Classification Metric") +
  theme_bw(base_size = 15, base_family = "Times") +

  ## Custom Labels
  
  geom_text(aes(x = 0.80, y = 0.375),  label = "Sensitivity", size = 5) +
  geom_line(aes(x = seq(0.735, 0.765, length = 10), y = rep(0.375, length(seq(0.735, 0.765, length = 10)))), col = "indianred", size = 2) +
  
  geom_text(aes(x = 0.80, y = 0.35), label = "Specificity",    size = 5) +
  geom_line(aes(x = seq(0.735, 0.765, length = 10), y = rep(0.35, length(seq(0.735, 0.765, length = 10)))), col = "black", size = 2) +
  
  geom_text(aes(x = 0.80, y = 0.325), label = "Accuracy",    size = 5) +
  geom_line(aes(x = seq(0.735, 0.765, length = 10), y = rep(0.325, length(seq(0.735, 0.765, length = 10)))), col = "dodgerblue", size = 2)

##
## Prediction - Aggregated
## 

glm.logit.predict <- as.vector(predict(glm.logit.fit4, newdata = test_set, type = "response")); 
predicted.classes <- as.factor(ifelse(glm.logit.predict > cutoff[which(abs(indexes$Optimal) == min(abs(indexes$Optimal), na.rm = TRUE))] - 0.003, "Positive", "Negative")); tmp <- test_set$Result;
confusionMatrix(data = predicted.classes, reference = tmp, positive = "Positive")

##
## Prediction - Daily
##

dIndex <- data.frame(Date = unique(test_set$dConfirmed),
                     Confirmed = rep(NA, length(unique(test_set$dConfirmed))))
minDate <- c()
for(i in 1:nrow(dIndex)) {minDate[i] = min(which(test_set$dConfirmed == dIndex[i, 1]))}
maxDate <- c()
for(i in 1:nrow(dIndex)) {maxDate[i] = max(which(test_set$dConfirmed == dIndex[i, 1]))}
for(i in 1:nrow(dIndex))
{
  tmp = as.data.frame(test_set %>% filter(test_set$dConfirmed == dIndex[i, 1] & test_set$Region == "Center") %>%
                        select(Age.Labels, Gender, Region, WeekDays, Result, State));
  glm.logit.predict = as.vector(predict(glm.logit.fit4, newdata = tmp[, -5], type = "response")); 
  tmp2 = as.factor(ifelse(glm.logit.predict > cutoff[which(abs(indexes$Optimal) == min(abs(indexes$Optimal), na.rm = TRUE))] - 0.003, "Positive", "Negative"));
  dIndex[i, 2] = confusionMatrix(data = tmp2, reference = tmp$Result)$table[2, 2]; 
}

tmp <- as.data.frame(test_set %>% group_by(dConfirmed) %>% summarise(COUNT = n()))

ggplot() + 
  geom_line(data = tmp, aes(x = dConfirmed, y = COUNT), col = "red2") +
  geom_line(data = dIndex, aes(x = Date, y = Confirmed), col = "black") +
  
  ## Custom Label
  labs(title = "Prediction of daily confirmed cases: Logistic Regression",
       subtitle = "",
       x = "Date",
       y = "Daily Confirmed") +
  theme_bw(base_size = 15, base_family = "Times")

source("~/TABASCO-MEXCOV-19/src/support/metrica.R")
metrica(tmp$COUNT, dIndex$Confirmed)
