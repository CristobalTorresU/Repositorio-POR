# Librerías
library(dplyr)
library(ggpubr)
library(caret)
library(lmtest)
library(car)

# Lectura de datos
datos <- read.csv2(file = "~/Repositorio-POR/EP09 Datos.csv")

# 1. Definimos una semilla
set.seed(1624)

# 2. Seleccionar los datos pertinentes
datosMujeres <- datos %>% filter(Gender == 1)
muestra <- datosMujeres[sample(nrow(datosMujeres),50), ]

# 3. Seleccionar aleatoriamente 8 posibles variables predictoras
predictoras <- sample(24,8)

# 4. Elegir otra variable que no este en la seleccionada
# La variable de interés es el peso (weight)
correlaciones <- cor(muestra)
max_cor <- max(correlaciones[23,-23])

indice_max_cor <- which.max(correlacion[23, -23])
indice_max_cor

# 5. Realizar una regresión lineal simple con la variable del paso 4.
muestraP <- muestra[predictoras]
muestraP$Weight <- muestra$Weight
muestraP$Hip.Girth <- muestra$Hip.Girth

modelo <- lm(Weight ~ Hip.Girth, data = muestra)
print(summary(modelo))

