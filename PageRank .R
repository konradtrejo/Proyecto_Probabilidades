
# PageRank Algorithm  # 

#Usaremos un caso particularp
#Definimos nuestra matriz de adyacencia con los equipos sudamericanos
#que participaron para las eliminatorias del Mundial Rusia 2018

#Orden para cada País que participo en las eliminatorias

#1. Argentina	
#2. Bolivia	
#3. Brasil	
#4. Chile	
#5. Colombia	
#6. Ecuador	
#7. Paraguay	
#8. Perú	
#9. Uruguay	
#10.Venezuela

#Definimos nuestra matriz de adyacencia
#A[i,j] = 3 si el país 'i' le gano al país 'j'
#A[i,j] = 1 si el país 'i' empato con país 'j'
#A[i,j] = 0 si el país 'i' perdió con el país 'j'

num_equipos <- 10
#Se implementa la matriz
A <- matrix(c(0, 3, 1 ,3, 3 ,0, 0, 1, 3, 1, 
              3, 0, 1, 3, 0, 1, 3, 0, 0, 3,
              3, 3, 0, 3, 3, 3, 3, 3, 1, 3, 
              0, 0, 0, 0, 1, 3, 0, 3, 3, 3, 
              0, 3, 1, 1, 0, 3, 0, 3, 1, 3, 
              0, 3, 0, 3, 0, 0, 1, 0, 3, 3,
              1, 3, 1, 3, 0, 3, 0, 0, 0, 0,
              1, 3, 0, 0, 1, 3, 3, 0, 3, 1, 
              1, 3, 0, 3, 3, 3, 3, 3, 0, 3, 
              1, 3, 0, 0, 1, 0, 0, 1, 1, 0),
            nrow = num_equipos,ncol = num_equipos,byrow = TRUE)

library(igraph)
plot(graph.adjacency(A))

#Primero tenemos que crear una nueva matrix A1 que resultara de
#dividir cada columna de la matriz A entre la suma de cada columna
A1 <- matrix(nrow = num_equipos,ncol= num_equipos)
for( i in 1:num_equipos){
  sum_columna <- sum(A[,i])
  A1[,i] = A[,i]/sum_columna
}

#El siguiente paso es encontra un vector pagerank_equipos de tamaño 10 que represente el PageRank
#de cada equipo

#Y como es ese vector pues es el vector propio del maximo valor propio
#Imprimimos el maximo valor proximo
vector_ValoresPropios <- c(eigen(A1)$values)
vector_VectoresPropios <- matrix(eigen(A1)$vectors,nrow = 10, ncol = 10)

#Ahora calculamos cual es el maximo valor propio.
maxVP <- -Inf
for(i in 1:10){
  if(maxVP < Mod(vector_ValoresPropios[i])){
    maxVP <- Mod(vector_ValoresPropios[i])
    index <- i
  } 
}

#Imprimimos el maximo valor propio
print(maxVP)
#Imprimimos el vector propio relacionado a ese vector
pagerank_equipos <- Mod(vector_VectoresPropios[,index])
print(pagerank_equipos)
#Ahora ya tenemos el pagerank de cada uno



