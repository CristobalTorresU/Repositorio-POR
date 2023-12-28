# Librerías
library(boot)
library(tidyverse)
library(ez)
library(ggpubr)
library(multcomp)

# Lectura de datos
datos = read.csv2("/home/dilget/Repositorio-POR/EP08 Datos CASEN 2017.csv", sep = ";", fileEncoding = "latin1")

# Pregunta de investigación
# 1- La cantidad de hogares de la Región Metropolitana (region) en que el/la 
# entrevistado/a tiene un trabajo permanente (o12) ¿es similar entre hombres y 
# mujeres (sexo) casados/as (ecivil)?

# Se filtran los datos a los que se requieren
muestra <- datos %>% filter(region == "Región Metropolitana de Santiago")
muestra <- muestra %>% filter(ecivil == "Casado(a)")
muestra <- muestra %>% filter(o12 == "Permanente?")

# Seteamos una semilla
set.seed(111)

# Simulación de Monte Carlo






# Bootstrapping




