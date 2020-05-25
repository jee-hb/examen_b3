#
# Universidad Austral 
# Facultad de Posgrado de Ingenieria
# Maestria en Ciencia de datos
# Materia: Algoritmos y Estructuras de Datos
# Prof. Andres Scoccimarro
# Prof. Gustavo Dejean
#
# Veronica Farach
#
# Preguntas del Examen
# 

#######################
## 1
## Cual/es  funcion/es utilizaria para obtener los datos estadisticos basico de un DF?

v = 1:100
df <- data.frame(datos = v)

summary(df)

library(dplyr)
df %>% summarise( Min_Pob = min(datos),
                  Q1er_pob = quantile(datos,0.25),
                  Mediana_Pob = median(datos),
                  Media_Pob = mean(datos),
                  Q3er_pob = quantile(datos,0.75),
                  Max_Pob = max(datos))


######################
## 2 
# Asociar cada problema con la estructura de datos que mejor lo represente
# ?rbol, grafo, cola, pila, hash

# Se requiere administrar los accesos a la informacion 
# de un diccionario de datos, donde existe una palabra clave 
# que identifica al dato, la cual tiene asociada su descripci?n. 
# Dada la cantidad de elementos de este diccionario, 
# se requiere que el acceso a cada descripci?n, el cual se realiza 
# a trav?s de la clave sea lo m?s eficiente posible.
####### Hash

##
# Un supermercado requiere administrar la fila de cada caja y optimizar 
# las mismas para mejorar su funcionamiento.
####### Cola

##
# La empresa X maneja sus costos con sistema LIFO, por lo que debe 
# organizar de alguna forma el ingreso de la mercader?a para poder 
# obtener el costo correcto
####### Pila

##
# Se necesita administrar una lista de elementos en forma ordenada, 
# de manera que el agregado de nuevos elementos y su b?squeda se realice 
# de la forma m?s eficiente posible.
####### ?rbol Binario


##
# La empresa X posee un grupo de dep?sitos y necesita optimizar las rutas 
# de distribuci?n entre estos.
####### Grafo


#######################
## 3
## Necesito realizar un ciclo for desde la fila 2 hasta la ante 
# ultima fila de un data frame llamado "datos". ¿ como hara el ciclo 
# para crear una nueva variable, llamada logV1,  que  sea el logaritmo 
# de la  columna llamada V1

datos = data.frame(V1 = 1:100, logV1 = 0)

for (i in 2:nrow(datos) - 1) { 
 datos$logV1[i] = log(datos$V1[i])
}


#######################
## 5
# ¿Cual de las siguientes afirmaciones es correcta?
# b. La O de un algoritmo define la relacion entre la cantidad de elementos a procesar 
# y el tiempo de ejecucion del algoritmo.


#######################
## 4
# Unir cada funcion con su respectiva O

# Respuesta 1: O(n)
# El ciclo que se ejecuta itera un numero 
# constante de veces = n = longitud del vector v
contains <- function (v, c) {
  if (length(v) == 0) return(FALSE);
  for( i in v) {
    if (c == i) return(TRUE);
  }
  return(FALSE);
}
contains(c("a","n","a","n","a"), "h")
         
# Respuesta 2: O(n*n)
# Se ejecutan dos ciclos anidados * n -1
# por cada n-1 itera n-1
sort <- function(v) {
  if (length(v) == 0 || length(v) == 1) return(v);
  swapped <- TRUE;
  last <- (length(v) - 1);
  while(swapped && last > 0) {
    swapped <- FALSE;
    for (i in c(1:last)) {
      if (v[i] > v[(i+1)]) {
        x <- v[i];
        v[i] <- v[(i+1)];
        v[(i+1)] <- x;
        swapped <- TRUE;
      }
    }
    last <- last - 1;
  }
  return(v);
}
sort(5000:4)


# Respuesta 3: O(log(n))
# Se ejecutan ciclos anidados al llamar nuevamente la funci?n
# en un orden 
sortContains <- function(v, x) {
  if (length(v) == 1) return(v[1] == x);
  p <- floor(length(v) / 2);
  if (x == v[p]) return(TRUE);
  if (x < v[p] && p > 1) return(sortContains(v[1:(p-1)], x));
  if (x > v[p] && p < length(v)) return(sortContains(v[(p+1):length(v)], x));
  return(FALSE);
}


#######################
## 6
# Necesito unir  un DF llamados "df1"  de n1 filas por m2 columnas con otro DF llamado 
# "df2" de n2 filas por m2 columnas.
# ?Que condicion suficiente  se debe cumplir para realizar un rbind() entre ambos DF? 
  
# Opcion -c- n1 = n2 no se cumple
n1 = 100
m2 = 3
n2 = 50

# Opcion -a- nombre y tipo de una o mas variables no se cumple
# Opcion -d- variable en común no se cumple
# Opcion -e- nombre y tipo de todas las variables no se cumple
df1 <- 1:300
df2 <- replicate(50, c("a","b","c"))

dim(df1) <- c(n1,m2)
dim(df2) <- c(n2,m2)

df1 <- as.data.frame(df1)
df2 <- as.data.frame(df2)

colnames(df1) <- c("Nro1", "Nro2", "Nro3")
colnames(df2) <- c("Letra1", "Letra2", "Letra3")

# Opcion -g- rbind no requiere comando merge()
df_bind <- rbind(df1,df2)

# Respuesta correcta = -b- m2 = m2
# Si m2 son diferentes obtengo:
# Error in rbind(deparse.level, ...) : 
# numbers of columns of arguments do not match


