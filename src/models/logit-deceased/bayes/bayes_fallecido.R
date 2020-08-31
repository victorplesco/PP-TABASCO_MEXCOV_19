source("~/TABASCO-MEXCOV-19/src/packages/install.packages.R")
source("~/TABASCO-MEXCOV-19/src/cleansing/swabsraw_0718.R")

swabspos <- as.data.frame(swabsraw %>% filter(RESULTADO == "Positive") %>%
                            mutate(TIEMPO_INFECCION = as.numeric(as.Date("2020-07-18") - FECHA_SINTOMAS))); # Time being infected; 
ind      <- which(swabspos$TIEMPO_INFECCION <= 40); swabspos <- swabspos[-ind,]; # After 40d we estimate recovering;                                                
swabspos <- as.data.frame(swabspos %>% select(-c(ID_REGISTRO, EMBARAZO, NEUMONIA, OTRO_CASO, ENTIDAD_UM, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, RESULTADO, TIPO_PACIENTE, UCI, INTUBADO, TIEMPO_INFECCION))); rm(ind, swabsraw);
swabspos <- na.omit(swabspos); # summary(swabspos);
