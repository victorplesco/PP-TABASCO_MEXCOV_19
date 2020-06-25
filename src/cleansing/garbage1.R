source("~/TABASCO-MEXCOV-19/src/packages/autoinstall.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/population_2020e.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/confirmedraw.R")

population_2020e$Infected <- 0; confirmedraw$Infected <- 1;
confirmedraw <- confirmedraw[, c(1, 3, 2, 5)];
pop_test <- rbind(population_2020e, confirmedraw)

# write_feather(pop_test, "~/TABASCO-MEXCOV-19/pop_test.R")
# pop_test <- read_feather("~/TABASCO-MEXCOV-19/pop_test.R")

cont_tot_gender <- table(pop_test$Infected, pop_test$Gender)
table_tot_gender <- data.frame(Male   = c(cont_tot_gender[, 1], sum(cont_tot_gender[, 1])),
                               Female = c(cont_tot_gender[, 2], sum(cont_tot_gender[, 2])),
                               Tot    = c(sum(cont_tot_gender[1, ]), sum(cont_tot_gender[2, ]), nrow(pop_test)))
rownames(table_tot_gender) <- c("Non Infected", "Infected", "Tot")
table_frq_gender <- table_tot_gender/nrow(pop_test)

cont_tot_age <- table(pop_test$Infected, pop_test$Age)
table_tot_age <- data.frame()





table_tot_age <- data.frame(Male   = c(cont_tot_age[, 1], sum(cont_tot_age[, 1])),
                            Female = c(cont_tot_age[, 2], sum(cont_tot_age[, 2])),
                            Tot    = c(sum(cont_tot_age[1, ]), sum(cont_tot_age[2, ]), nrow(pop_test)))
rownames(table_tot_age) <- c("Non Infected", "Infected", "Tot")
table_frq_age <- table_tot_age/nrow(pop_test)

cont_tot_state <- table(pop_test$Infected, pop_test$State)
table_tot_state <- data.frame(Male   = c(cont_tot_state[, 1], sum(cont_tot_state[, 1])),
                              Female = c(cont_tot_state[, 2], sum(cont_tot_state[, 2])),
                              Tot    = c(sum(cont_tot_state[1, ]), sum(cont_tot_state[2, ]), nrow(pop_test)))
rownames(table_tot_state) <- c("Non Infected", "Infected", "Tot")
table_frq_state <- table_tot_state/nrow(pop_test)

cont_list <- list(list(table_tot_gender, table_frq_gender),
                  list(table_tot_age,    table_frq_age),
                  list(table_tot_state,  table_frq_state))
