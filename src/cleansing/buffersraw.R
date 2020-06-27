temp <- tempfile(fileext = ".zip")
gd <- drive_download(as_id("1-Bw5ECd5B0D-bhfcXbuyePrKFbiNfg_F"), path = temp, overwrite = TRUE)
out <- unzip(temp, exdir = tempdir())
buffers <- read.csv(out)
rm(gd, out, temp)

# select()
buffersraw <- buffers[, c("SEXO", "EDAD", "RESULTADO", "FECHA_SINTOMAS", "FECHA_DEF", "ENTIDAD_UM")]
# {Date} = as.Date(%Y-%m-%d)
buffersraw$FECHA_SINTOMAS <- as.Date(as.character(buffersraw$FECHA_SINTOMAS), "%Y-%m-%d"); buffersraw$FECHA_DEF <- as.Date(as.character(buffersraw$FECHA_DEF), "%Y-%m-%d");
# {RESULTATO} = as.factor(Positive, Negative, Pending)
buffersraw$RESULTADO <- as.factor(buffersraw$RESULTADO); levels(buffersraw$RESULTADO) <- c("Positive", "Negative", "Pending");
# {SEXO} = as.factor(Female, Male)
buffersraw$SEXO <- as.factor(buffersraw$SEXO); levels(buffersraw$SEXO) <- c("Female", "Male");
# {ENTIDAD_UM}
buffersraw$ENTIDAD_UM <- as.factor(buffersraw$ENTIDAD_UM)
entidas <- read.csv("~/TABASCO-MEXCOV-19/data/gobierno/Catalogo_de_Entidas.csv", header = TRUE); entidas <- entidas[-c(33:36),];
buffersraw <- merge(x = buffersraw, y = entidas, by.x = "ENTIDAD_UM", by.y = "CLAVE_ENTIDAD", all.x = TRUE)
buffersraw$ENTIDAD_FEDERATIVA <- droplevels(buffersraw$ENTIDAD_FEDERATIVA); buffersraw <- buffersraw[, -1];
# {ENTIDAD_UM = State, SEXO = Gender, EDAD = Age, RESULTADO = Result, FECHA_SINTOMAS = Date_Confirmed, FECHA_DEF = Date_Death}
colnames(buffersraw) <- c("Gender", "Age", "Result", "Date_Confirmed", "Date_Death", "State");
rm(buffers, entidas)
