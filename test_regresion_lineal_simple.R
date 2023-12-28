library(dplyr)
library(ggpubr)
library(caret)
library(lmtest)
library(car)
set.seed(1624) #Se Define la semilla 
datos <- read.csv2("EP09 Datos.csv", stringsAsFactors = TRUE)
mujeres <- filter(datos, Gender == 0)
mujeres <- mujeres[sample(nrow(mujeres),50), ]
#shoulder, bitrochanteric, anke minimum girth, ankle diameter, wrists diameter, Knees diameter
#thigh girth, waist girth.

columnas_restantes <- mujeres %>%select(-c(Shoulder.Girth,
                                           Bitrochanteric.diameter,
                                           Ankle.Minimum.Girth,
                                           Ankles.diameter,
                                           Wrists.diameter,
                                           Knees.diameter,
                                           Thigh.Girth,
                                           Waist.Girth))
#Se busca la mejor relacion lineal.
correlaciones <- cor(columnas_restantes)
max_cor <- which.max(correlaciones[15, -15])
print(max_cor)
#Hip.Girth
columnas_seleccionadas <- mujeres %>% select(c(Shoulder.Girth,
                                               Hip.Girth,
                                               Bitrochanteric.diameter,
                                               Ankle.Minimum.Girth,
                                               Ankles.diameter,
                                               Wrists.diameter,
                                               Knees.diameter,
                                               Thigh.Girth,
                                               Waist.Girth,
                                               Weight))
#Se crea el modelo con validacion cruzada
modelo <- train(Weight ~ Hip.Girth, data = columnas_seleccionadas, method = "lm",
                trControl = trainControl(method = "cv", number = 10))
modelo <- modelo[["finalModel"]]


# Se procede a obtener los residuos y estadísticas de influencia
# de los casos
evaluacion_modelo <- data.frame(predicted.probabilities = 
                                  fitted(modelo))

evaluacion_modelo[["residuos_estandarizados"]] <- rstandard(modelo)
evaluacion_modelo[["residuos_estudiantizados"]] <- rstudent(modelo)
evaluacion_modelo[["distancia_cook"]] <- cooks.distance(modelo)
evaluacion_modelo[["dfbeta"]] <- dfbeta(modelo)
evaluacion_modelo[["dffit"]] <- dffits(modelo)
evaluacion_modelo[["apalancamiento"]] <- hatvalues(modelo)
evaluacion_modelo[["covratio"]] <- covratio(modelo)

cat("Influencia de los casos: \n")

# 5% de los residuos estandarizados
# Deben estar bajo el 95%
sospechosos1 <- which(abs(evaluacion_modelo[["residuos_estandarizados"]]) > 1.96)
cat(" # Residuos estandarizados fuera del 95% esperado # \n", sospechosos1)

# Observaciones con distancia de Cook donde son mayor a 1
sospechosos2 <- which(evaluacion_modelo[["distancia_cook"]] > 1)
cat(" # Residuos con distancia de Cook mayor a 1 # \n", sospechosos2)
cat(" No hay residuos con distancia de Cook mayor a 1 \n")

# Observaciones con apalancamiento superior al doble del apalancamiento
# promedio: (k + 1)/n
# La cantidad no debe ser mayor que el promedio
apalancamiento_prom <- 2 / nrow(columnas_seleccionadas)
sospechosos3 <- which(evaluacion_modelo[["apalancamiento"]] > 2*apalancamiento_prom)
cat(" # Residuos con apalancamiento fuera del rango promedio, teniendo como promedio
        igual a ", apalancamiento_prom, " ", sospechosos3)

# Observaciones con dfbeta mayor o igual a 1
sospechosos4 <- which(apply(evaluacion_modelo[["dfbeta"]] >= 1, 1, any))
cat(" # Residuos con dfbeta mayor o igual a 1 #\n", sospechosos4)

# Observaciones con covarianzas fuera del rango
cvri_inferior <- 1 - 3*apalancamiento_prom
cvri_superior <- 1 + 3*apalancamiento_prom

sospechosos5 <- which(evaluacion_modelo[["covratio"]] < cvri_inferior
                      | evaluacion_modelo[["covratio"]] > cvri_superior)

cat(" # Residuos con razón de covarianza fuera de rango #", sospechosos5)

# Se juntan todos los sospechosos para poder evaluar cuántos hay en total
sospechosos <- c(sospechosos1, sospechosos2, sospechosos3,
                 sospechosos4, sospechosos5)

# Se ordenan y se eliminan los sospechosos duplicados
sospechosos <- sort(unique(sospechosos))

cat(" # Se tienen, en total, los siguientes datos observados sospechosos #\n") 
print(round(evaluacion_modelo[sospechosos, c("distancia_cook", "apalancamiento", "covratio")],
            3))

