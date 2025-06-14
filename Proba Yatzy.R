####### Comprender el enunciado y los items que se nos proporcionan #######

## Una persona realiza dos lanzamientos independientes de 5 dados, el puntaje es en base a las caras o números obtenidos de cada dado

## Primero evaluaremos los casos favorables donde el usario obtiene puntos a favor y sus propiedades

## Triple: Para el caso de Triple "(tres iguales y dos diferentes)" estamos hablando de permutaciones de un multiconjunto, o más simplemente, arreglos de objetos donde algunos son idénticos.
## Cuando lanzas 5 dados y quieres un "Triple" (por ejemplo: 4,4,4,1,6) lo que interesa es la disposición específica de los valores en los 5 dados. Cada dado es distinguible por su posición (Dado 1, Dado 2,..; Dado 5)
## Calculamos las diferentes maneras en que se puede obtener un Triple

## Tenemos n = 5 (dados),  n1 = 3 (dados con el mismo valor) , n2 = 1 (dado con un valor diferente) , n3 = 1 (dado con otro valor diferente)
## Denotamos que el numero de casos de distribuir estos valores es una permutacion con repeticion

caras_dado <- 6
num_dados <- 5

# 1. Determinamos cuantos posibles resultados de par ordenado se puede obtener de los dados diferentes

opciones_dos_diferentes_para_triple <- choose(caras_dado - 1, 2) #funcion base para calcular el coeficiente binomial, leído como "n sobre k"

# 2. Número de formas de organizar los 5 dados en 5 posiciones: 5! / (3! * 1! * 1!) 
# Se organizan de la forma: (X,X,X,Y,Z) siendo X, Y, Z numero de cada dado. Por ejemplos: (1,1,1,2,3)

permutaciones_triple <- factorial(num_dados) / (factorial(3) * factorial(1) * factorial(1))

# 3. Obtener el número de casos favorables de obtener un Triple, asignando el puntaje obtenido

casos_triple_5Puntos <- caras_dado * opciones_dos_diferentes_para_triple * permutaciones_triple

####### Obteniendo así la cantidad de resultados posibles para que el usuario obtenga 5 puntos o un triple #########

