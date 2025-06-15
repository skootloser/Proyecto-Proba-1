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


install.packages("gtools")
library(gtools)


install.packages("arrangements")
library(arrangements)

library(arrangements)


# Generar espacio muestral de 5 dados
espacio_muestral <- permutations(x = 1:6, k = 5, replace = TRUE)


nrow(espacio_muestral)

##El espacio muestral Ω está compuesto por 7,776 secuencias posibles de resultados de los 5 dados. Por ejemplo, (1, 2, 3, 4, 5), (6, 6, 6, 6, 6), (1, 1, 2, 3, 4), etc.).

##Obtenga los casos favorables a los sucesos: obtener una tercia, un full, una escalera y, no obtener ninguno de los resultados anteriores.

###Obtener una "Triple" (Tres dados iguales y dos diferentes) 

###Como se esta trabajando con combinacioens es necesario definir funciones para clasificar las tiradas 

es_triple <- function(x) {
  freqs <- table(x)
  length(freqs) == 3 && any(freqs == 3) && all(freqs <= 3)
}

triples <- apply(espacio_muestral, 1, es_triple)

sum(triples)    # Casos de triples

###Obtener un "Full" (Tres dados iguales y dos dados iguales) 

es_full <- function(x) {
freqs <- table(x)           # Cuenta cuántas veces se repite cada número
length(freqs) == 2 && all(sort(freqs) == c(2, 3))
}

fulls <- apply(espacio_muestral, 1, es_full)

sum(fulls)      # Casos de full


###Obtener una "Escalera" (Menor o Mayor) 



es_escalera <- function(x) {
  sorted <- sort(x)
  all(sorted == 1:5) || all(sorted == 2:6)
}

escaleras <- apply(espacio_muestral, 1, es_escalera)

sum(escaleras)  # Casos de escalera

  

###No obtener ninguno de los resultados anteriores


es_nada <- function(x) {
  !(es_triple(x) || es_full(x) || es_escalera(x))
}
nadas <- !(triples | fulls | escaleras)

sum(nadas)      # Casos sin combinaciones



7. Verifique que los resultados obtenidos en el apartado (6) se corresponde con lo obtenido en
(2) y (3).
