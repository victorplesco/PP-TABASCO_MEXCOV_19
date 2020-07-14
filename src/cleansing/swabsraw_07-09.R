pkg <- c("tidyverse");
for(i in pkg) 
{
  if(!require(as.character(i), character.only = TRUE))
  {
    install.packages(as.character(i), character.only = TRUE)
    library(as.character(i), character.only = TRUE)
  }
  else {library(as.character(i), character.only = TRUE);}
};

mex <- read.csv(unzip("~/TABASCO-MEXCOV-19/data/original/datos_abiertos_covid19.zip"))
# str(mex)

##
## select();
##

swabsraw <- as.data.frame(mex %>% 
                            
  select(
  
  # PRIMARY KEY;
  ID_REGISTRO, # <ID> 
  
  # DEMOGRAPHICS;
  SEXO, # <GENDER> 
  EDAD, # <AGE>
  
  # TREATMENTS;
  ENTIDAD_UM, # <STATE>
  TIPO_PACIENTE, # <HOSPITALIZED>
  UCI, # <ICU>
  INTUBADO, # <INTUBED>
  
  # DIAGNOSTICS;
  NEUMONIA,
  EMBARAZO, 
  DIABETES,
  EPOC, 
  ASMA, 
  INMUSUPR, 
  HIPERTENSION, 
  CARDIOVASCULAR, 
  OBESIDAD, 
  RENAL_CRONICA, 
  TABAQUISMO, 
  
  # SUSCEPTIBILITY;
  OTRO_CASO,
  
  # SARS-CoV-2 SCREEN TEST;
  RESULTADO, # <RESULT> 
  
  # DATES;
  FECHA_INGRESO, 
  FECHA_SINTOMAS, 
  FECHA_DEF
  
  ) %>% 
  
  filter(RESULTADO != "3")) # Not considering the "pending" results of the SARS-CoV-2 screen test;

##
## <DEMOGRAPHICS>: {SEXO, EDAD};
## 

# [NEW VALUE]: GENDER <> SEXO = Male {2}, Female {1}, NA {else};
ind <- which(swabsraw$SEXO != "1" & swabsraw$SEXO != "2");
swabsraw$SEXO <- as.factor(swabsraw$SEXO); 
levels(swabsraw$SEXO)[levels(swabsraw$SEXO) == "2"] <- "Male";
levels(swabsraw$SEXO)[levels(swabsraw$SEXO) == "1"] <- "Female";
swabsraw$SEXO[ind] <- NA; swabsraw$SEXO <- droplevels(swabsraw$SEXO);
## summary(swabsraw$SEXO)

# EDAD;

##
## <TREATMENT>: {ENTIDAD_UM, TIPO_PACIENTE, UCI, INTUBADO};
## 

# [NEW VALUE]: STATE <> ENTIDAD_UM - Consult "Catalogo_de_Entidas_2020.csv";
entidas <- read.csv("~/TABASCO-MEXCOV-19/data/metadata/Catalogo_de_Entidas_2020.csv", header = TRUE); 
ind <- which(swabsraw$ENTIDAD_UM %in% entidas$CLAVE_ENTIDAD[1:32]);
swabsraw <- merge(x = swabsraw, y = entidas, by.x = "ENTIDAD_UM", by.y = "CLAVE_ENTIDAD", all.x = TRUE);
swabsraw$ENTIDAD_FEDERATIVA[-ind] <- NA; swabsraw$ENTIDAD_FEDERATIVA <- droplevels(swabsraw$ENTIDAD_FEDERATIVA);
swabsraw$ENTIDAD_UM <- swabsraw$ENTIDAD_FEDERATIVA; swabsraw$ENTIDAD_FEDERATIVA <- NULL;

# [NEW VALUE]: HOSPITALIZED <> TIPO_PACIENTE = Yes {2}, No {1}, NA {else};
ind <- which(swabsraw$TIPO_PACIENTE != "1" & swabsraw$TIPO_PACIENTE != "2");
swabsraw$TIPO_PACIENTE <- as.factor(swabsraw$TIPO_PACIENTE); 
levels(swabsraw$TIPO_PACIENTE)[levels(swabsraw$TIPO_PACIENTE) == "2"] <- "Yes";
levels(swabsraw$TIPO_PACIENTE)[levels(swabsraw$TIPO_PACIENTE) == "1"] <- "No";
swabsraw$TIPO_PACIENTE[ind] <- NA; swabsraw$TIPO_PACIENTE <- droplevels(swabsraw$TIPO_PACIENTE);
swabsraw$TIPO_PACIENTE <- factor(swabsraw$TIPO_PACIENTE, levels = c("No", "Yes"));
## summary(swabsraw$TIPO_PACIENTE)

# [NEW VALUE]: ICU <> UCI = Yes {1}, No {2}, NA {else};
ind <- which(swabsraw$UCI != "1" & swabsraw$UCI != "2");
swabsraw$UCI <- as.factor(swabsraw$UCI); 
levels(swabsraw$UCI)[levels(swabsraw$UCI) == "2"] <- "No";
levels(swabsraw$UCI)[levels(swabsraw$UCI) == "1"] <- "Yes";
swabsraw$UCI[ind] <- NA; swabsraw$UCI <- droplevels(swabsraw$UCI);
swabsraw$UCI <- factor(swabsraw$UCI, levels = c("No", "Yes"));
## summary(swabsraw$UCI)

# [NEW VALUE]: INTUBED <> INTUBADO = Yes {1}, No {2}, NA {else};
ind <- which(swabsraw$INTUBADO != "1" & swabsraw$INTUBADO != "2");
swabsraw$INTUBADO <- as.factor(swabsraw$INTUBADO); 
levels(swabsraw$INTUBADO)[levels(swabsraw$INTUBADO) == "2"] <- "No";
levels(swabsraw$INTUBADO)[levels(swabsraw$INTUBADO) == "1"] <- "Yes";
swabsraw$INTUBADO[ind] <- NA; swabsraw$INTUBADO <- droplevels(swabsraw$INTUBADO);
swabsraw$INTUBADO <- factor(swabsraw$INTUBADO, levels = c("No", "Yes"));
## summary(swabsraw$INTUBADO)

##
## <SARS-CoV-2 SCREEN TEST>: {RESULTADO};
## 

# [NEW VALUE]: RESULT <> RESULTADO = Positive {1}, Negative {2}, NA {else};
ind <- which(swabsraw$RESULTADO != "1" & swabsraw$RESULTADO != "2");
swabsraw$RESULTADO <- as.factor(swabsraw$RESULTADO); 
levels(swabsraw$RESULTADO)[levels(swabsraw$RESULTADO) == "2"] <- "Negative";
levels(swabsraw$RESULTADO)[levels(swabsraw$RESULTADO) == "1"] <- "Positive";
swabsraw$RESULTADO[ind] <- NA; swabsraw$RESULTADO <- droplevels(swabsraw$RESULTADO);
swabsraw$RESULTADO <- factor(swabsraw$RESULTADO, levels = c("Negative", "Positive"));
## summary(swabsraw$RESULTADO)

##
## <DIAGNOSTICS>: {GENERAL};
##

# [NEW VALUE]: GENERAL = Yes {1}, No {2}, NA {else};
ind <- c("NEUMONIA", "EMBARAZO", "DIABETES", "ASMA", "INMUSUPR", "HIPERTENSION", 
         "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO", "EPOC")

for(i in ind) 
{
  ind <- which(swabsraw[, i] != "1" & swabsraw[, i] != "2");
  swabsraw[, i] <- as.factor(swabsraw[, i]); 
  levels(swabsraw[, i])[levels(swabsraw[, i]) == "2"] <- "No";
  levels(swabsraw[, i])[levels(swabsraw[, i]) == "1"] <- "Yes";
  swabsraw[ind, i] <- NA; swabsraw[, i] <- droplevels(swabsraw[, i]);
  swabsraw[, i] <- factor(swabsraw[, i], levels = c("No", "Yes"));
};

##
## <SUSCEPTIBILITY>: {OTRO_CASO};
##

# [NEW VALUE]: OTRO_CASO = Yes {1}, No {2}, NA {else};
ind <- which(swabsraw$OTRO_CASO != "1" & swabsraw$OTRO_CASO != "2");
swabsraw$OTRO_CASO <- as.factor(swabsraw$OTRO_CASO); 
levels(swabsraw$OTRO_CASO)[levels(swabsraw$OTRO_CASO) == "2"] <- "No";
levels(swabsraw$OTRO_CASO)[levels(swabsraw$OTRO_CASO) == "1"] <- "Yes";
swabsraw$OTRO_CASO[ind] <- NA; swabsraw$OTRO_CASO <- droplevels(swabsraw$OTRO_CASO);
swabsraw$OTRO_CASO <- factor(swabsraw$OTRO_CASO, levels = c("No", "Yes"));
## summary(swabsraw$OTRO_CASO)

##
## <DATES>: {FECHA_INGRESO, FECHA_SINTOMAS, FTECHA_DEF};
##

tmp <- c("FECHA_INGRESO", "FECHA_SINTOMAS", "FECHA_DEF");
swabsraw[, tmp[1]] <- as.Date(as.character(swabsraw[, tmp[1]]), format = "%Y-%m-%d")
swabsraw[, tmp[2]] <- as.Date(as.character(swabsraw[, tmp[2]]), format = "%Y-%m-%d")
swabsraw[, tmp[3]] <- as.Date(na_if(as.character(swabsraw[, tmp[3]]), "9999-99-99"), format = "%Y-%m-%d")

# [NEW COLUMN]: DECEASED = Yes (is.na(FECHA_DEF) == FALSE), No (is.na(FECHA_DEF) == TRUE);
swabsraw$DECEASED <- NA;
swabsraw$DECEASED[which(is.na(swabsraw[, tmp[3]]) == TRUE)] <- "No"; swabsraw$DECEASED[which(is.na(swabsraw[, tmp[3]]) == FALSE)] <- "Yes"; 
swabsraw$DECEASED <- factor(swabsraw$DECEASED, levels = c("No", "Yes"));
rm(entidas, mex, i, ind, pkg, tmp);