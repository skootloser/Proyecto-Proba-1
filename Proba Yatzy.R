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

## Total de Ningunos ##
total_ninguno <- espacio_muestral - (total_resultados_escalera + total_resultados_full + total_resultados_triple)
total_ninguno

## Hallar la probabilidad de que una persona gane el juego ##

## Para esto debemos conocer las probabilidades de cada suceso interpretados como puntos obtenidos, estos son obtener un triple, full o escalera, se dividen por el total de resultados posibles (Espacio muestral) ####

## Estos son los resultados de cada suceso en una sola partida ##

prob_full <- total_resultados_full / espacio_muestral
prob_triple <- total_resultados_triple / espacio_muestral
prob_escalera <- total_resultados_escalera / espacio_muestral

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

# 1. Creamos una matriz de probabilidades para la primera partida (como una columna)
probabilidades_ganar_columna <- as.matrix(probabilidades_por_jugada)

# 2. Creamos una segunda matriz de probabilidades para la segunda partida (como una fila)
probabilidades_ganar_fila <- t(probabilidades_ganar_columna) # t() calcula la transpuesta

# 3. Multiplicamos las matrices columna por la matriz fila para obtener la matriz de productos
# Esto es una multiplicación de matrices.
probabilidad_ganar_combinada <- probabilidades_ganar_columna %*% probabilidades_ganar_fila

print(probabilidad_ganar_combinada) # Observamos una matriz de las probabilidades de ganar en base a cada suceso

## Esta matriz representa las probabilidades de cada posible combinación de resultados entre la Partida 1 y la Partida 2
## Como las partidas son independientes, la probabilidad de que ambos eventos ocurran es simplemente el producto de sus probabilidades individuales

# Identificamos las combinaciones donde la suma de puntos es >= 12.

Puntaje_Ganador <- suma_puntos_matriz >= 12

# Finalmente sumamos las probabilidades solo para las combinaciones donde la suma de puntos sea mayor igual a 12
# Usamos la nueva matriz generada probabilidad_ganar_combinada
probabilidad_ganar <- sum(probabilidad_ganar_combinada[Puntaje_Ganador])

## Obteniendo asi la probabilidad de ganar ##

## Porbabildiad de perder ##

probabilidad_perder <- 1 - probabilidad_ganar 

#### Conseguimos mediante una matriz la probabildiad de que alguien gane el juego en dos lanzamientos y con el uso de principios probabilisticos encontramos la probabilidad de perder ###


######### Obtener casos favorables y ninguno #######

# Casos Favorables para escalera: 5!=120
# Donde: 2×120=240

# Casos Favorables para Full: C(6, 1) = 6 , C(5, 1) = 5 , orden posiciones 5!/3!*2! = 10
# Donde: 6*5*10 = 300

# Casos favorables para Triple: C(6, 1) = 6 , C(5, 2) = 10 orden posiciones 5!/3!*1!*1! = 20
# Donde: 6*5*10 = 1200

# Casos obtener Ninguno: Utilizamos propiedad de complemento
# Suma de obtener alguno de los 3 sucesos: 120+300+1200 = 1740
# Donde: 7776 (Probabilidad de todos los posibles resultados) - 1740(Suma de resultados a favor) = 6036

## Obteniendo asi todos los posibles resultados para cada enunciado ###############


### Porbabilidad de cada suceso ###

# Como ya antes lo habiamos calculado:

# Probabilidad de obtener un Full (total resultados Full/ Total de resultados posibles)
round(prob_full * 100, 4)

# Probabildiad de obtener un Triple (total resultados triple/ total de resultados posibles)
round(prob_triple * 100, 4)

# Probabildiad de obtener una escalera (total resultados escaleras / total de resultados posibles)
round(prob_escalera * 100, 4)

# Probabilidad de no obtener ninguno de los anteriores (probabilidad de ninguno / total de resultados posibles)
round(prob_ninguno * 100, 4)

# Para verificar que todas nuestras probabilidades estan bien definidas su suma debe ser igual a 1, tomando en cuenta que son sucesos mutuamente excluyentes ##
prob_escalera + prob_full + prob_triple + prob_ninguno

####### Observamos que se cumple la propiedad #########


