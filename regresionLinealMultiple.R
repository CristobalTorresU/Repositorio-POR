# Librerias
library()
library()
library()

# Lectura de datos
datos <- read.csv(file = "~/Repositorio-POR/EP09 Datos.csv", stringsAsFactors = TRUE)

datos <- datos %>% filter(Gender == 1)
datos[["Gender"]] <- NULL
datos <- sample_n(datos, 50, replace = FALSE)
