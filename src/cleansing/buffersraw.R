temp <- tempfile(fileext = ".zip")
gd <- drive_download(as_id("1-Bw5ECd5B0D-bhfcXbuyePrKFbiNfg_F"), path = temp, overwrite = TRUE)
out <- unzip(temp, exdir = tempdir())
buffers <- read.csv(out)
rm(gd, out, temp)

# select()
buffersraw <- buffers[, c("SEXO", "EDAD", "RESULTADO", "FECHA_SINTOMAS", "FECHA_DEF", "ENTIDAD_UM")]
# {FECHA_SINTOMAS} = as.Date(%Y-%m-%d)
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
# {ENTIDAD_FEDERATIVA} = {North, Center, South}
states <- levels(buffersraw$ENTIDAD_FEDERATIVA)
north  <- states[c(2, 3, 26, 6, 8, 25, 10)];
south  <- states[c(30, 20, 27, 5, 4, 23, 31)];
center <- states[-c(2, 3, 26, 6, 8, 25, 10, 30, 20, 27, 5, 4, 23, 31)];
buffersraw$State.Labels <- NA;
buffersraw$State.Labels[which(buffersraw$ENTIDAD_FEDERATIVA %in% north)]  <- "North";
buffersraw$State.Labels[which(buffersraw$ENTIDAD_FEDERATIVA %in% south)]  <- "South";
buffersraw$State.Labels[which(buffersraw$ENTIDAD_FEDERATIVA %in% center)] <- "Center";
buffersraw$State.Labels <- as.factor(buffersraw$State.Labels);
# {ENTIDAD_UM = State, SEXO = Gender, EDAD = Age, RESULTADO = Result, FECHA_SINTOMAS = Date_Confirmed, FECHA_DEF = Date_Death}
colnames(buffersraw) <- c("Gender", "Age", "Result", "Date_Confirmed", "Date_Death", "State", "State.Labels");
rm(buffers, entidas, center, north, south, states);
