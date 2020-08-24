source("~/TABASCO-MEXCOV-19/src/packages/install-packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw_0719.R")

# defaultW <- getOption("warn") 
# options(warn = -1) 
# options(warn = defaultW)

swabspos <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive") %>%
                            mutate(INFECTION_TIME = as.numeric(as.Date("2020-07-18") - FECHA_SINTOMAS))); # Time being infected. After 26d we estimate recovering.
ind      <- which(swabspos$FALLECIDO == "No" & swabspos$INFECTION_TIME <= 26); swabspos <- swabspos[-ind,];                                                
swabspos <- as.data.frame(swabspos %>% select(-c(ID_REGISTRO, NEUMONIA, OTRO_CASO, INFECTION_TIME, ENTIDAD_UM, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, RESULTADO, TIPO_PACIENTE, UCI, INTUBADO))); rm(ind);

#################################################################################################################################################################################################################################################################################
## Categorical ##################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

indipmatrix <- matrix(0, nrow = (ncol(swabspos) - 1), ncol = (ncol(swabspos) - 1));
tmp <- colnames(swabspos)[-which(colnames(swabspos) == "EDAD")]; 

findindex <- function(vector, x) 
{
  indexes = c();
  for(i in 1:length(x))
  {indexes[i] = which(vector == x[i])};
  return(indexes);
};

tmp <- tmp[findindex(tmp, c("FALLECIDO", "SEXO", "EMBARAZO", "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO"))];
colnames(indipmatrix) <- tmp; rownames(indipmatrix) <- tmp; rm(tmp, findindex);

for(i in 1:nrow(indipmatrix))
{
  for(j in 1:ncol(indipmatrix))
  {
    if(i == 2 & j == 3 | i == 3 & j == 2)
    {
      indipmatrix[i, j] = NA;
      next;
    }
    
    data  = as.data.frame(na.omit(swabspos[, c(rownames(indipmatrix)[i], colnames(indipmatrix)[j])]));
    table = table(data[, 1], data[, 2]);
    indipmatrix[i, j] = as.numeric(chisq.test(table)[3]);
  }
}; rm(data, table, i, j);

assocmatrix <- matrix(0, nrow = (ncol(swabspos) - 1), ncol = (ncol(swabspos) - 1));
tmp <- colnames(swabspos)[-which(colnames(swabspos) == "EDAD")]; 

findindex <- function(vector, x) 
{
  indexes = c();
  for(i in 1:length(x))
  {indexes[i] = which(vector == x[i])};
  return(indexes);
};

tmp <- tmp[findindex(tmp, c("FALLECIDO", "SEXO", "EMBARAZO", "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO"))];
colnames(assocmatrix) <- tmp; rownames(assocmatrix) <- tmp; rm(tmp, findindex);

for(i in 1:nrow(assocmatrix))
{
  for(j in 1:ncol(assocmatrix))
  {
    if(i == 2 & j == 3 | i == 3 & j == 2)
    {
      assocmatrix[i, j] = NA;
      next;
    }
    
    data  = as.data.frame(na.omit(swabspos[, c(rownames(assocmatrix)[i], colnames(assocmatrix)[j])]));
    table = table(data[, 1], data[, 2]);
    assocmatrix[i, j] = as.numeric(Assocs(table)[3]); 
  }
}; rm(data, table, i, j);

indipData <- melt(indipmatrix); colnames(indipData)[3] <- "p-value"; 
assocData <- melt(assocmatrix); colnames(assocData)[3] <- "Cramer's V";

ggplot(data = indipData, aes(x = Var1, y = Var2)) + 
  
  geom_raster(data = indipData, aes(fill = indipData[, 3])) + 
  scale_fill_gradient(low = "grey90", high = "tomato3") +
  
  geom_point(data = assocData, aes(size = assocData[, 3]),
             color = "black") +
  
  scale_size(range = c(0, 10)) +
  
  # TEXT Solution;
  # geom_text(data = indipData, aes(label = round(as.numeric(assocData[, 3]), 2))) + 

  # COLOR Solution;
  # geom_point(...  )
  # guides(size = guide_legend(override.aes = list(colour = "black"))) + 

  ## Custom Label
  labs(title = "",
       subtitle = "",
       x = "",
       y = "") +
  theme_bw(base_size = 12.5, base_family = "Times") +
  labs(fill = "p-value", size = "Cramer's V");

#################################################################################################################################################################################################################################################################################
## Numerical ####################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

ggplot(data = swabspos, aes(x = FALLECIDO, y = EDAD)) +
  geom_boxplot(aes(col = FALLECIDO), fill = "white") + 
  scale_color_manual(values = c("black", "tomato3")) +
  
  # Custom Label
  labs(title = "",
       subtitle = "",
       x = "FALLECIDO",
       y = "EDAD") +
  theme_bw(base_size = 15, base_family = "Times") +
  theme(legend.position = "top");

# par(mfrow = c(1, 2));
# qqnorm(swabspos$EDAD[swabspos$FALLECIDO == "Yes"], sub = "FALLECIDO | Yes"); qqline(swabspos$EDAD[swabspos$FALLECIDO == "Yes"], col = "tomato3");
# qqnorm(swabspos$EDAD[swabspos$FALLECIDO == "No"], sub = "FALLECIDO | No"); qqline(swabspos$EDAD[swabspos$FALLECIDO == "No"], col = "dodgerblue3");
# par(mfrow = c(1, 1));

t.test(EDAD ~ FALLECIDO, data = swabspos, var.equal = FALSE)