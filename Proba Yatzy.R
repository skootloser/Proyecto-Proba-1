####### Comprender el enunciado y los items que se nos proporcionan #######

## Una persona realiza dos lanzamientos independientes de 5 dados, el puntaje es en base a las caras o números obtenidos de cada dado

caras_dado <- 6
num_dados <- 5
espacio_muestral <- 6^5

## Primero evaluaremos los casos favorables donde el usario obtiene puntos a favor y sus propiedades

## Triple: Para el caso de Triple "(tres iguales y dos diferentes)" estamos hablando de permutaciones de un multiconjunto, o más simplemente, arreglos de objetos donde algunos son idénticos.
## Cuando lanzas 5 dados y quieres un "Triple" (por ejemplo: 4,4,4,1,6) lo que interesa es la disposición específica de los valores en los 5 dados. Cada dado es distinguible por su posición (Dado 1, Dado 2,..; Dado 5)
## Calculamos las diferentes maneras en que se puede obtener un Triple

## Tenemos n = 5 (dados),  n1 = 3 (dados con el mismo valor) , n2 = 1 (dado con un valor diferente) , n3 = 1 (dado con otro valor diferente)

# 1. Elegir el valor para el trio
# Concepto de Combinación C(6, 1)
opciones_valor_triple_para_Triple <- choose(6, 1)

# 2. Elegir los posibles valores para el par diferente del numero escogido en el trio y entre ellos.
# Concepto de Combinación C(5, 1)
opciones_valores_diferentes_Triple <- choose(5, 2)

# 3. Distribuir los 5 dados en las 5 posiciones, el orden seria (X,X,X,Y,Z) por ejemplo (1,1,1,2,3)
# Concepto de Coeficiente Multinomial (5! / (3! * 1! * 1!))
distribucion_posiciones_triple <- factorial(5) / (factorial(3) * factorial(1) * factorial(1))

# 4. Obtener los posibles valores de obtener un Triple
total_resultados_triple <- opciones_valor_triple_para_Triple* opciones_valores_diferentes_Triple * distribucion_posiciones_triple
print(paste("Resultados para Triple:", total_resultados_triple))

## Obteniendo así los posibles resultados de un triple como el producto de una combinación con el coeficiente multinomial ###
## Explicacion: Primero se elige un valor de 6 lo cual es el concepto de combinacion, luego se seleccionan dos valores que deben ser diferentes entre sí y diferentes del primer valor
## en esta seleccion el orden no importa para definir los valores "unicos" que aparecen en el lanzamiento de los dados.
## Para el arreglo final se debe evaluar los posibles resultados de ordenar 3 dados iguales y 2 diferentes entre sí, siendo esto un coeficiente binomial ###

### Casos para obtener Full ###

## Un Full consiste en tres dados de un número y dos dados de un mismo número, los denotamos como (X,X,X,Y,Y) por ejemplo (1,1,1,2,2)
## Para este caso contamos n = 5 dados, n1 = 3 dados iguales, n2 = dado de un numero diferente a n1 , n3 = dado igual a n2
## Observamos que para este caso se trata de un problema de permutación con repetición o, más precisamente, de coeficiente multinomial. Es el número de formas de organizar 5 objetos, donde 3 son idénticos (el primer valor) y 2 son idénticos (el segundo valor).

# 1. Elegimos los posibles valores para el trio de Full
# Concepto de combinación  C(6, 1)
opciones_valor_triple_para_Full <- choose(6, 1) 

# 2. Elegir los posibles valores para el par diferente del valor del trio
# Concepto de Combinación C(5, 1)
opciones_valor_par_Full<- choose(5, 1)

# 3. Distribuir los valores en las 5 posiciones de los dados
# Concepto de Coeficiente Multinomial (5! / (3! * 2!)) esto es equivalente a elegir 3 posiciones para el triplete de 5, C(5, 3)
distribucion_posiciones_full <- choose(5, 3)

# Por ultimo hayamos el total de posibles resultados de obtener un Full (8 puntos)

total_resultados_full <- opciones_valor_triple_para_Full * opciones_valor_par_Full * distribucion_posiciones_full
print(paste("Resultados para Full:", total_resultados_full))

### Obteniendo asi los posibles resultados de obtener un Full como el productos de una combinacion y el coeficiente multinomial #####
## Explicacion: estas son combinaciones porque el orden en que elegimos los valores X y Y no importan para definir la "clase" de valores que formarán el Full
## Una vez que tienes los valores específicos este coeficiente cuenta las formas de organizar los 5 números en los 5 dados distinguibles. Es una forma de "permutación con repetición" de un multiconjunto.

##### Casos de obtener una Escalera (Menor y Mayor) ####

## Existen 2 casos de escalera: menor (1,2,3,4,5) y mayor (2,3,4,5,6) por lo cual podemos observar que se trata de una simple permutacion en ambos casos ##

# 1. Número de tipos de escaleras (menor y mayor)
# Conteo directo
numero_tipos_escalera <- 2

# 2. Obtener los valores para cada tipo de escalera
# Concepto de Permutación 5!
permutaciones_escalera <- factorial(5)

# 3. Obtener el total de resultados posibles de conseguir una escalera
total_resultados_escalera <- numero_tipos_escalera * permutaciones_escalera
print(paste("Resultados para Escalera:", total_resultados_escalera))

### De esta forma obtenemos el total de posibles resultados de obtener una escalera menor o mayor ##
## Explicacion: En este caso estamos tomando todos los resultados y el orden si importa,  ya que una asignación como (Dado 1=1, Dado 2=2, ..., Dado 5=5) es un resultado distinto entre ellos
## Ambas son escaleras, pero son secuencias únicas de números en los dados y como son dos tipos simplemente multiplicamos por 2.


## Hallar la probabilidad de que una persona gane el juego ##

## Para esto debemos conocer las probabilidades de cada suceso interpretados como puntos obtenidos, estos son obtener un triple, full o escalera, se dividen por el total de resultados posibles (Espacio muestral) ####

## Estos son los resultados de cada suceso en una sola partida ##

prob_full <- total_resultados_full / espacio_muestral
prob_triple <- total_resultados_triple / espacio_muestral
prob_escalera <- total_resultados_escalera / espacio_muestralç

# Tambien podemos obtener la probabilidad contrario, que la persona no obtenga ningun puntaje

prob_ninguno <- 1 - (prob_escalera + prob_full + prob_triple)
# Nota: Esta suma de probabilidades no cuenta con interseccion puesto que los sucesos son mutuamente excluyentes

# Ahora organizaremos los posibles eventos de ganar y sus probabilidades en vectores para facilitar los calculos #

puntos_posibles <- c(10, 8, 5, 0)
probabilidades_por_jugada <- c(prob_escalera, prob_full, prob_triple, prob_ninguno)

## Calcular la probabilidad de ganar en base a su segunda jugada ##
# La persona gana si acumula 12 puntos o mas al final de las dos partidas, estas son independientes entre si 

# Calcular la suma de puntos para cada combinación posible de dos partidas trabajando con matrices ##

# Creamos una matriz donde cada celda [i,j] es puntos_posibles[i] + puntos_posibles[j] y obtenemos los puntamos resultantes:
suma_puntos_matriz <- outer(puntos_posibles, puntos_posibles, FUN = "+")
# Nota: Esto solo nos genera los posibles puntos obtenidos por una sola partida

# Calculamos ahora la probabilidad combinada para cada combinación posible en las dos partidas
# Esto genera una matriz donde cada celda [i,j] es prob_partida_1[i] * prob_partida_2[j]

probabilidad_combinada_matriz <- outer(probabilidades_por_jugada, probabilidades_por_jugada, FUN = "*")

# Identificamos las combinaciones donde la suma de puntos es >= 12.

condicion_ganadora <- suma_puntos_matriz >= 12

# Finalmente sumamos las probabilidades solo para las combinaciones donde la condición se cumpla
probabilidad_ganar <- sum(probabilidad_combinada_matriz[condicion_ganadora])

## Obteniendo asi la probabilidad de ganar ##

## Porbabildiad de perder ##

probabilidad_perder <- 1 - probabilidad_ganar 

#### Conseguimos mediante una matriz la probabildiad de que alguien gane el juego en dos lanzamientos y con el uso de principios probabilisticos encontramos la probabilidad de perder ###

