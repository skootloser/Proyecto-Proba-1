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



#### SCRIP CODIGOS UTILIZADOS: ####

## 1. Variables principales

caras_dado <- 6
num_dados <- 5

# 1. 1. Espacio Muestral para 1 Lanzamiento

espacio_muestral <- caras_dado^num_dados

## 2. Obtener Casos Favorables en un Lanzamiento

# 2. 1. Obtener Casos Triple

opciones_valor_triple_para_Triple <- choose(6, 1) # Obtener combinaciones para la triada

opciones_valores_diferentes_Triple <- choose(5, 2) # Obtener combinaciones de 2 dados diferentes entre ellos y ambos distintos de la cara del dado apareciente de la triada

distribucion_posiciones_triple <- factorial(5) / (factorial(3) * factorial(1) * factorial(1)) # Distribucion de los ordenes de los 5 dados

total_resultados_triple <- opciones_valor_triple_para_Triple* opciones_valores_diferentes_Triple * distribucion_posiciones_triple # Producto resultante de lo antes calculado para obtener el total de casos favorables del suceso

total_resultados_triple # Total de casos Triples en un lanzamiento

## 2. 2. Obtener Casos Full

opciones_valor_triple_para_Full <- choose(6, 1)  # Obtener combinaciones para la triada

opciones_valor_par_Full<- choose(5, 1) # Obtener combinaciones de los dos dados iguales entre si pero distintos a los de la triada

distribucion_posiciones_full <- choose(5, 3) # Distribucion de los ordenes de los 5 dados

total_resultados_full <- opciones_valor_triple_para_Full * opciones_valor_par_Full * distribucion_posiciones_full # Producto resultante de lo antes calculado para obtener el total de casos favorables del suceso

total_resultados_full # Total de casos Full en un lanzamiento

## 2. 3. Obtener Casos Escalera

numero_tipos_escalera <- 2 # Conteo directo de total tipos escaleras

permutaciones_escalera <- factorial(5) # Permutacion orden de los dados

total_resultados_escalera <- numero_tipos_escalera * permutaciones_escalera # Producto para obtener el total de casos favorables del suceso

total_resultados_escalera # Total de casos Escalera en un lanzamiento

## 2. 4. Obtener Casos Ninguno

total_no_obtener_puntos <- espacio_muestral - (total_resultados_escalera + total_resultados_full + total_resultados_triple) # Propiedad de Complemento

total_no_obtener_puntos # Total de casos Ninguno en un lanzamiento

## 3. Hallar Probabilidades de cada Suceso en un Lanzamiento

prob_full <- total_resultados_full / espacio_muestral
prob_triple <- total_resultados_triple / espacio_muestral
prob_escalera <- total_resultados_escalera / espacio_muestral

prob_ninguno <- 1 - (prob_escalera + prob_full + prob_triple) # Propiedad de Complemento

## 4. Hallar Probabilidad de Ganar

# 4. 1. Espacio muestral total o real (conformado por 2 lanzamientos)

espacio_muestral_total <- espacio_muestral * espacio_muestral # Equivalente a elevar al cuadrado

# 4. 2. Calculo de vectores y matrices para hallar la probabilidad

total_resultados_obtenibles <- c("Escaleras" = total_resultados_escalera, "Triple" = total_resultados_triple, "Full" = total_resultados_full, "Ninguno" = total_no_obtener_puntos)
# Vector del total de casos por suceso obtenible en un lanzamiento

puntos <- c("Escalera" = 10,"Triple" = 5,  "Full" = 8,"Ninguno" = 0) # Vector de cantidad de puntos por suceso

prob_combinada <- outer(probabilidad_un_solo_lanzamiento, probabilidad_un_solo_lanzamiento, FUN = "*")
colnames(prob_combinada) <- names(probabilidad_un_solo_lanzamiento)
rownames(prob_combinada) <- names(probabilidad_un_solo_lanzamiento)
# Matriz de probabilidades de sucesos combinados

puntos_totales_combinados <- outer(puntos, puntos, FUN = "+")
colnames(puntos_totales_combinados) <- names(puntos)
rownames(puntos_totales_combinados) <- names(puntos)
# Matriz de puntos por sucesos combinados

Puntaje_Ganador <- puntos_totales_combinados >= 12 # Condición ganadora de los posibles sucesos obtenibles

probabilidad_ganar <- sum(prob_combinada[Puntaje_Ganador]) # Sumatoria de todas las probabilidades de sucesos combinados que sean mayor o igual a 12 que permitan al usuario ganar


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

# 2. vamos a generar su matriz
combinaciones_con_repeticion <- combinations(n=length(dados), r=num_dados, repeats.allowed=TRUE, v=dados)

# Mostrar solo las primeras 10 combinaciones
cat("\nPrimeras 10 combinaciones con repetición:\n")
print(combinaciones_con_repeticion[1:10, ])


##El espacio muestral Ω está compuesto por 7,776 secuencias posibles de resultados de los 5 dados. Por ejemplo, (1, 2, 3, 4, 5), (6, 6, 6, 6, 6), (1, 1, 2, 3, 4), etc.).

##Obtenga los casos favorables a los sucesos: obtener una tercia, un full, una escalera y, no obtener ninguno de los resultados anteriores.

###Obtener una "Triple" (Tres dados iguales y dos diferentes) 

###Como se esta trabajando con combinacioens es necesario definir funciones para clasificar las tiradas 

dados <- 1:6
espacio_muestral <- expand.grid(rep(list(dados), 5))

# Función para detectar tercia (triple): debe haber exactamente 3 valores iguales y 2 valores diferentes (cada uno apareciendo 1 vez)
es_triple <- function(x) {
  freqs <- table(x)
  length(freqs) == 3 && any(freqs == 3) && all(freqs <= 3)
}

# Aplicar función a cada fila del espacio muestral para saber si es tercia o no
triples <- apply(espacio_muestral, 1, es_triple)

# Ahora puedes sumar y ver
sum(triples)    

View(data.frame(Total_casos_triple = sum(triples)))

# Extraer sólo las filas que cumplen con ser tercia
matriz_triples <- espacio_muestral[triples, ]

# Mostrar número total de casos de tercia
cat("Número total de casos de tercia:", nrow(matriz_triples), "\n")

print(head(matriz_triples, 10)) 


###Obtener un "Full" (Tres dados iguales y dos dados iguales) 

# Crear espacio muestral: todos los resultados posibles al lanzar 5 dados
dados <- 1:6
espacio_muestral <- expand.grid(rep(list(dados), 5))

# Función corregida para detectar full house: 
# uno se repite 3 veces y otro se repite 2 veces
es_full <- function(x) {
  freqs <- table(x)           
  length(freqs) == 2 && all(sort(freqs) == c(2, 3)) 
}

sum(fulls)      

View(data.frame(Total_casos_full = sum(fulls)))


# Aplicar función a cada fila del espacio muestral para detectar full 

fulls <- apply(espacio_muestral, 1, es_full)

# Extraer sólo las filas que cumplen con ser full house
matriz_fulls <- espacio_muestral[fulls, ]

# Mostrar número total de casos de full house
cat("Número total de casos de full house:", nrow(matriz_fulls), "\n")

print(head(matriz_fulls, 10)) 


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
print(head(matriz_escaleras, 10))


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




#7. Verifique que los resultados obtenidos en el apartado (6) se corresponde con lo obtenido en (2) y (3).


all.equal(sum(fulls), total_resultados_full)
all.equal(sum(triples), total_resultados_triple)
all.equal(sum(escaleras), total_resultados_escalera)
all.equal(sum(nadas), total_no_obtener_puntos)

###Para validar que los resultados obtenidos con paquetes de generación de permutaciones, variaciones y combinaciones corresponden  con los obtenidos mediante métodos con R base, se hara uso de la funcion all.equal(). Ya que la funcion verifica si los valores son numéricamente equivalentes, permitiendo pequeñas tolerancias en casos de redondeo o representación interna. 

comparacion_resultados <- data.frame(
  Tipo = c("Full", "Triple", "Escalera", "Nadas"),
  Valor = c(sum(fulls), sum(triples), sum(escaleras), sum(nadas)),
  Total = c(total_resultados_full, total_resultados_triple, total_resultados_escalera, total_no_obtener_puntos),
  Coinciden = c(
    all.equal(sum(fulls), total_resultados_full) == TRUE,
    all.equal(sum(triples), total_resultados_triple) == TRUE,
    all.equal(sum(escaleras), total_resultados_escalera) == TRUE,
    all.equal(sum(nadas), total_no_obtener_puntos) == TRUE
  )
)

# Mostrar la tabla
print(comparacion_resultados)


