install.packages("gtools")
library(gtools)


install.packages("arrangements")
library(arrangements)

library(arrangements)

install.packages("tidyverse")
library(tidyverse)

install.packages("tidyr")   
library(tidyr)

install.packages("combinat") 
library(combinat)

# Apartados 6 y 7 del Laboratorio Yatzy - Generación de casos con permutaciones y verificación

##Ahora bien, usando paquetes con funciones de generación de permutaciones, variaciones y combinaciones:

##6. Replique el proceso para generar los casos posibles de cada uno de los sucesos de los apartados (2) y (3).

##Obtenga el espacio muestral.

##El espacio muestral al lanzar 5 dados se obtiene utilizando el concepto de permutaciones con repetición:
###Cada dado es independiente
###Cada dado tiene 6 resultados posibles: Las caras del 1 al 6
###
##Para poder resolver este problema, usaremos los paquetes gtools y arrangements,estos sirven para realizar operaciones relacionadas con análisis combinatorio, es decir, para generar y trabajar con permutaciones, combinaciones y variaciones de forma eficiente
## gtools: Este paquete contiene una variedad de funciones útiles, entre las que se incluyen funciones para generar permutaciones y combinaciones. Es un paquete más general que ofrece diversas utilidades.
## arrangements: Este paquete está específicamente optimizado para la generación rápida de permutaciones y combinaciones, especialmente cuando se trabaja con conjuntos grandes. Ofrece funciones de alto rendimiento para estas tareas combinatorias.
###El orden sí importa

# Generar espacio muestral de 5 dados y su matriz 

# Datos base: posibles valores de un dado
dados <- 1:6
num_dados <- 5

# 1. Espacio Muestral
total_posibles <- 6^5
cat("Total de combinaciones posibles al lanzar 5 dados:", total_posibles, "\n")

# 2. Generar Combinaciones con Repetición
combinaciones_con_repeticion <- combinations(n=length(dados), r=num_dados, repeats.allowed=TRUE, v=dados)


##El espacio muestral Ω está compuesto por 7,776 secuencias posibles de resultados de los 5 dados. Por ejemplo, (1, 2, 3, 4, 5), (6, 6, 6, 6, 6), (1, 1, 2, 3, 4), etc.).

##Obtenga los casos favorables a los sucesos: obtener una tercia, un full, una escalera y, no obtener ninguno de los resultados anteriores.

###Obtener una "Triple" (Tres dados iguales y dos diferentes) 

###Como se esta trabajando con combinacioens es necesario definir funciones para clasificar las tiradas 

dados <- 1:6
espacio_muestral <- expand.grid(rep(list(dados), 5))

# Función para detectar tercia (triple): debe haber exactamente 3 valores iguales, y 2 valores diferentes (cada uno apareciendo 1 vez)
es_triple <- function(x) {
  freqs <- table(x)
  length(freqs) == 3 && any(freqs == 3) && all(freqs <= 3)
}

sum(triples)    # Casos de triples

View(data.frame(Total_casos_triple = sum(triples)))


# Aplicar función a cada fila del espacio muestral para saber si es tercia o no
triples <- apply(espacio_muestral, 1, es_triple)

# Extraer sólo las filas que cumplen con ser tercia
matriz_triples <- espacio_muestral[triples, ]

# Mostrar número total de casos de tercia
cat("Número total de casos de tercia:", nrow(matriz_triples), "\n")

# Guardar o visualizar matriz final con casos de tercia
print(head(matriz_triples, 10))  # Mostrar las primeras 10 filas como ejemplo



###Obtener un "Full" (Tres dados iguales y dos dados iguales) 

# Crear espacio muestral: todos los resultados posibles al lanzar 5 dados
dados <- 1:6
espacio_muestral <- expand.grid(rep(list(dados), 5))

# Función corregida para detectar full house: exactamente dos valores diferentes:
# uno se repite 3 veces y otro se repite 2 veces
es_full <- function(x) {
  freqs <- table(x)           # Cuenta cuántas veces se repite cada número
  length(freqs) == 2 && all(sort(freqs) == c(2, 3))  # 2 valores, con cuentas 2 y 3 respectivamente
}

sum(fulls)      # Casos de full

View(data.frame(Total_casos_full = sum(fulls)))

# Aplicar función a cada fila del espacio muestral para detectar full house
fulls <- apply(espacio_muestral, 1, es_full)

# Extraer sólo las filas que cumplen con ser full house
matriz_fulls <- espacio_muestral[fulls, ]

# Mostrar número total de casos de full house
cat("Número total de casos de full house:", nrow(matriz_fulls), "\n")


###Obtener una "Escalera" (Menor o Mayor) 



# Crear espacio muestral: todos los resultados posibles al lanzar 5 dados
dados <- 1:6
espacio_muestral <- expand.grid(rep(list(dados), 5))

# Función para detectar escalera: debe ser 1-2-3-4-5 o 2-3-4-5-6
es_escalera <- function(x) {
  sorted <- sort(x)
  all(sorted == 1:5) || all(sorted == 2:6)
}

# Aplicar función a cada fila del espacio muestral para saber si es escalera o no
escaleras <- apply(espacio_muestral, 1, es_escalera)

# Extraer sólo las filas que cumplen con ser escalera
matriz_escaleras <- espacio_muestral[escaleras, ]

# Mostrar número total de casos de escalera
cat("Número total de casos de escalera:", nrow(matriz_escaleras), "\n")

# Mostrar las primeras 10 filas como ejemplo
print(head(matriz_escaleras, 10))

# Para ver toda la tabla en RStudio usa View(matriz_escaleras)

  

###No obtener ninguno de los resultados anteriores

# Código R para generar matriz con casos de "nada" en Yatzy

# Crear espacio muestral: todos los resultados posibles al lanzar 5 dados
dados <- 1:6
espacio_muestral <- expand.grid(rep(list(dados), 5))

# Funciones para detectar combinaciones
es_triple <- function(x) {
  freqs <- table(x)
  length(freqs) == 3 && any(freqs == 3) && all(freqs <= 3)
}

es_full <- function(x) {
  freqs <- table(x)
  length(freqs) == 2 && all(sort(freqs) == c(2, 3))
}

es_escalera <- function(x) {
  sorted <- sort(x)
  all(sorted == 1:5) || all(sorted == 2:6)
}

# Función para detectar "nada": no es triple, full ni escalera
es_nada <- function(x) {
  !(es_triple(x) || es_full(x) || es_escalera(x))
}

# Aplicar funciones al espacio muestral
triples <- apply(espacio_muestral, 1, es_triple)
fulls <- apply(espacio_muestral, 1, es_full)
escaleras <- apply(espacio_muestral, 1, es_escalera)

# Determinar casos de "nada"
nadas <- apply(espacio_muestral, 1, es_nada)

# Extraer matriz con filas que son casos de "nada"
matriz_nadas <- espacio_muestral[nadas, ]

# Mostrar número total de casos "nada"
cat("Número total de casos sin combinaciones:", nrow(matriz_nadas), "\n")

# Mostrar las primeras 10 filas como ejemplo
print(head(matriz_nadas, 10))

# Para ver toda la matriz en RStudio usa View(matriz_nadas)


###Vamos a Crear un data frame que tenga un resumen de todas las posibles combinaciones de cada uno de los sucesos 

resumen <- data.frame(
  Tipo = c("Triple", "Full", "Escalera", "Nada"),
  Casos = c(sum(triples), sum(fulls), sum(escaleras), sum(nadas))
)

View(resumen)



##7. Verifique que los resultados obtenidos en el apartado (6) se corresponde con lo obtenido en (2) y (3).

all.equal(sum(fulls), total_resultados_full)
all.equal(sum(triples), total_resultados_triple)
all.equal(sum(escaleras), total_resultados_escalera)
all.equal(sum(nadas), total_ninguno)

###Para validar que los resultados obtenidos con paquetes de generación de permutaciones, variaciones y combinaciones corresponden  con los obtenidos mediante métodos con R base, se hara uso de la funcion all.equal(). Ya que la funcion verifica si los valores son numéricamente equivalentes, permitiendo pequeñas tolerancias en casos de redondeo o representación interna. 

comparacion_resultados <- data.frame(
  Tipo = c("Full", "Triple", "Escalera", "Nadas"),
  Valor = c(sum(fulls), sum(triples), sum(escaleras), sum(nadas)),
  Total = c(total_resultados_full, total_resultados_triple, total_resultados_escalera, total_ninguno),
  Coinciden = c(
    all.equal(sum(fulls), total_resultados_full) == TRUE,
    all.equal(sum(triples), total_resultados_triple) == TRUE,
    all.equal(sum(escaleras), total_resultados_escalera) == TRUE,
    all.equal(sum(nadas), total_ninguno) == TRUE
  )
)

# Mostrar la tabla
print(comparacion_resultados)

