# librarias
library(ez)
library(tidyverse)
library(ggpubr)

# Lectura de datos
datos = read.csv2("/home/dilget/Repositorio-POR/EP05 Datos.csv", sep = ",");

# Hipotesis a contrastar:
# H0: Los tiempos en que se demoran los usuarios para formular una consulta de una pregunta
# de dificultad en leyes, musica y matematicas son iguales.
# Ha: Lo contrario

# Se filtran los datos para tener solo los requeridos
muestraMedia <- datos %>% filter(dificultad == "Media");
muestraMedia <- muestraMedia %>% filter(area == "Leyes" |
                                          area == "Música" |
                                          area == "Matemáticas");

# Se ponen los datos a lo ancho
muestra_ancha <- muestraMedia %>% pivot_wider(names_from = "area",
                                         values_from = "tiempo");

# Se separan muestras para cada una de las distintas variables buscadas
leyes <- muestraMedia %>% filter(area == "Leyes");
musica <- muestraMedia %>% filter(area == "Música");
matematicas <- muestraMedia %>% filter(area == "Matemáticas");

# Se comprueba la normalidad de las variables
shapiro.test(leyes$tiempo)
shapiro.test(musica$tiempo)
shapiro.test(matematicas$tiempo)

# Se realiza ANOVA
# TODO: Entender como usar la funcion factor
muestraMedia[["area"]] <- factor(muestraMedia[["area"]])
muestraMedia[["id"]] <- factor(muestraMedia[["id"]])
resultado = ezANOVA(muestraMedia, dv = tiempo, wid = id, between = area, return_aov = TRUE)
resultado$`Levene's Test for Homogeneity of Variance`

summary(resultado$aov)

# Post-Hoc