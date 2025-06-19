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




