source("~/TABASCO-MEXCOV-19/src/packages/install-packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw_0709.R")

swabspos <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive") %>%
                            mutate(INFECTION_TIME = as.numeric(as.Date("2020-07-09") - FECHA_SINTOMAS))); # Time being infected. After 12d we estimate recovering.
ind      <- which(swabspos$DECEASED == "No" & swabspos$INFECTION_TIME <= 12); swabspos <- swabspos[-ind,];                                                
swabspos <- as.data.frame(swabspos %>% select(-c(ID_REGISTRO, OTRO_CASO, INFECTION_TIME, ENTIDAD_UM, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, RESULTADO, TIPO_PACIENTE, UCI, INTUBADO))); rm(ind);
# summary(swabspos);

#################################################################################################################################################################################################################################################################################
## Contingency Analysis: Independence ###########################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

#################
## CATEGORICAL ##
#################

indipmatrix <- matrix(0, nrow = (ncol(swabspos) - 1), ncol = (ncol(swabspos) - 1));
colnames(indipmatrix) <- colnames(swabspos)[-2];
rownames(indipmatrix) <- colnames(swabspos)[-2];

for(i in 1:nrow(indipmatrix))
{
  for(j in 1:ncol(indipmatrix))
  {
    if(i == 1 & j == 3 | i == 3 & j == 1)
    {
      indipmatrix[i, j] = NA;
      next;
    }
    
    data  = as.data.frame(na.omit(swabspos[, c(rownames(indipmatrix)[i], colnames(indipmatrix)[j])]));
    table = table(data[, 1], data[, 2]);
    indipmatrix[i, j] = as.numeric(chisq.test(table)[3]);
  }
}

assocmatrix <- matrix(0, nrow = (ncol(swabspos) - 1), ncol = (ncol(swabspos) - 1));
colnames(assocmatrix) <- colnames(swabspos)[-2];
rownames(assocmatrix) <- colnames(swabspos)[-2];

for(i in 1:nrow(assocmatrix))
{
  for(j in 1:ncol(assocmatrix))
  {
    if(i == 1 & j == 3 | i == 3 & j == 1)
    {
      assocmatrix[i, j] = NA;
      next;
    }
    
    data  = as.data.frame(na.omit(swabspos[, c(rownames(assocmatrix)[i], colnames(assocmatrix)[j])]));
    table = table(data[, 1], data[, 2]);
    assocmatrix[i, j] = as.numeric(Assocs(table)[3]);
  }
}

indipData <- melt(indipmatrix); colnames(indipData)[3] <- "p-value"; 
assocData <- melt(assocmatrix); colnames(assocData)[3] <- "Cramer's V";

enc_deceased <- assocData[c(93, 95, 106, 108, 112, 119, 121, 125, 132, 134, 138, 158, 160, 164), 1:3];

ggplot(data = indipData, aes(x = Var1, y = Var2)) + 
  
  geom_raster(data = indipData, aes(fill = indipData[, 3])) + 
  scale_fill_gradient(low = "grey90", high = "tomato3") +
  
  geom_point(data = assocData, aes(size = assocData[, 3]),
             color = "black") +
  
  # geom_text(data = indipData, aes(label = round(as.numeric(assocData[, 3]), 2))) +
  scale_size(range = c(0, 7.5)) +
  
  geom_point(data = enc_deceased, aes(x = Var1, y = Var2, size = enc_deceased[, 3]), 
             color = "dodgerblue3") +
  
  guides(size = guide_legend(override.aes = list(colour = "black"))) + 

  ## Custom Label
  labs(title = "",
       subtitle = "",
       x = "",
       y = "") +
  theme_bw(base_size = 12.5, base_family = "Times") +
  labs(fill = "p-value", size = "Cramer's V");


###############
## NUMERICAL ##
###############

ggplot(data = swabspos, aes(x = DECEASED, y = EDAD)) +
  geom_boxplot(aes(col = DECEASED), fill = "white") + 
  scale_color_manual(values = c("dodgerblue3", "tomato3")) +
  
  # Custom Label
  labs(title = "",
       subtitle = "",
       x = "Result",
       y = "Age") +
  theme_bw(base_size = 15, base_family = "Times") +
  theme(legend.position = "top")

par(mfrow = c(1, 2))
qqnorm(swabspos$EDAD[swabspos$DECEASED == "Yes"], sub = "DECEASED | Yes")
qqline(swabspos$EDAD[swabspos$DECEASED == "Yes"], col = "tomato3")
qqnorm(swabspos$EDAD[swabspos$DECEASED == "No"], sub = "DECEASED | No")
qqline(swabspos$EDAD[swabspos$DECEASED == "No"], col = "dodgerblue3")
par(mfrow = c(1, 1))

t.test(EDAD ~ DECEASED, data = swabspos, var.equal = FALSE)