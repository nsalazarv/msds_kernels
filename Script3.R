### Script P3 Tarea 2 Kernels

rm(list = ls())
set.seed(23)
library("rgl")

path = "C:/Users/Mateo Rojas/OneDrive - Ventum Group/Escritorio/datos.txt"
data = read.delim(path, header = TRUE, sep = ",", dec = ".")

data = data[sample(nrow(data)),]

##Ejercicio 2

#Pregunta A
