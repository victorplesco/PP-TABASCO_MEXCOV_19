mex <- as.data.frame(read.csv("~/TABASCO-MEXCOV-19/data/metadata/WHO-COVID-19-MEXICO.csv") %>% filter(Country == "Mexico") %>% select(Date_reported,    
                                                                                                                                      New_cases, Cumulative_cases,
                                                                                                                                      New_deaths, Cumulative_deaths));

#################################################################################################################################################################################################################################################################################
## Daily ########################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

tmp_up   <- mex[, c(1, 2)]; tmp_up$Label   <- as.factor(1); 
tmp_down <- mex[, c(1, 4)]; tmp_down$Label <- as.factor(2);
colnames(tmp_up)[2] <- colnames(tmp_down)[2]; tmp <- rbind(tmp_up, tmp_down); colnames(tmp)[2] <- "Daily";

options(scipen = 999);
ggplot(data = tmp) +
  
  geom_bar(aes(x = as.Date(Date_reported), y = Daily, fill = Label),
           stat     = "identity",
           position = "stack",
           col      = "black",
           alpha    = 1) +
  
  scale_x_date(breaks = as.Date(c("2020-03-01",
                                  "2020-04-01",
                                  "2020-05-01",
                                  "2020-06-01",
                                  "2020-07-01",
                                  "2020-08-01",
                                  "2020-09-01"), 
                                format = "%Y-%m-%d")) +
  
  scale_fill_manual(values = c("#009dd0", "#f58f3b"),
                    labels = c("Confirmed", "Deceased")) +
  
  # Custom Labels;
  labs(title = "Bar Plot of Daily Confirmed and Deceased Cases from Sars-Cov-19",
       subtitle = "",
       x = "Date",
       y = "Daily Cases") +
  theme_bw(base_size = 17.5, base_family = "Times") +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "bottom");

#################################################################################################################################################################################################################################################################################
## Cumulative ###################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

tmp_up   <- mex[, c(1, 3)]; tmp_up$Label   <- as.factor(1); 
tmp_down <- mex[, c(1, 5)]; tmp_down$Label <- as.factor(2);
colnames(tmp_up)[2] <- colnames(tmp_down)[2]; tmp <- rbind(tmp_up, tmp_down); colnames(tmp)[2] <- "Cumulative";

options(scipen = 999);
ggplot(data = tmp) +
  
  geom_bar(aes(x = as.Date(Date_reported), y = Cumulative, fill = Label),
           stat     = "identity",
           position = "stack",
           col      = "black",
           alpha    = 1) +
  
  scale_x_date(breaks = as.Date(c("2020-03-01",
                                  "2020-04-01",
                                  "2020-05-01",
                                  "2020-06-01",
                                  "2020-07-01",
                                  "2020-08-01",
                                  "2020-09-01"), 
                                format = "%Y-%m-%d")) +
  
  scale_fill_manual(values = c("#009dd0", "#f58f3b"),
                    labels = c("Confirmed", "Deceased")) +
  
  # Custom Labels;
  labs(title = "Bar Plot of Cumulative Confirmed and Deceased Cases from Sars-Cov-19",
       subtitle = "",
       x = "Date",
       y = "Cumulative Cases") +
  theme_bw(base_size = 17.5, base_family = "Times") +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "bottom");