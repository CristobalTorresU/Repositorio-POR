# Librerías
library(ez)
library(tidyr)
library(dplyr)
library(ggpubr)

# Se leen los datos
datos = read.csv2("/home/dilget/Repositorio-POR/EP05 Datos.csv", sep = ",");

# Se plantean las hipótesis
# H0: No existen diferencias significativas en los tiempos de formulación de 
#     consultas con niveles de dificultad distintos en el área de biología.
# Ha: El complemento.

# Se filtran los datos requeridos
muestra = datos %>% filter(area == "Biología");

baja <- muestra %>% filter(dificultad == "Baja");
media <- muestra %>% filter(dificultad == "Media");
alta <- muestra %>% filter(dificultad == "Alta");

# Condiciones:
# Normalidad
shapiro.test(baja$tiempo)
shapiro.test(media$tiempo)
shapiro.test(alta$tiempo)

# Esfericidad


# Factores
# TODO: Entender el por que
muestra[["dificultad"]] <- factor(muestra[["dificultad"]])
muestra[["id"]] <- factor(muestra[["id"]])

# Realizar anova para muestras correlacionadas
prueba <- ezANOVA(data = muestra,
                  dv = tiempo,
                  wid = id,
                  within = dificultad,
                  return_aov = TRUE)

summary(prueba$aov)


# Test de esfericidad de Mauchly
prueba[["Mauchly's Test for Sphericity"]]

#