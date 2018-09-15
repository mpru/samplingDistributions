# -----------------------------------------------------------
# ESTUDIO DE DISTRIBUCIONES MUESTRALES MEDIANTE SIMULACIÓN
# -----------------------------------------------------------

# PASO 1: CÓMO SIMULAR MUESTRAS, CALCULAR ESTADÍSTICAS Y GRAFICAR
# ---------------------------------------------------------------

# Simular una muestra de tamaño n = 50 de una población N(80, 5)
n <- 50
datos <- rnorm(n, mean = 80, sd = 5)

# Calcular media, variancia y desvío estandar de la muestra
mean(datos)
var(datos)
sd(datos)

# Hacer un histograma de los 50 valores de la muestra
hist(datos, breaks = 10)

# Simular 100 muestras de tamaño n = 5 de una población N(80, 5)
n <- 5
nsim <- 100
datos <- matrix(rnorm(n * nsim, mean = 80, sd = 5), nrow = nsim)
head(datos)
dim(datos) # cada fila es una muestra

# Calcular media, variancia y desvío  de cada muestra
medias <- apply(datos, 1, mean)
head(medias)
length(medias)

variancias <- apply(datos, 1, var)
head(variancias)
length(variancias)

desvios <- apply(datos, 1, sd)
head(desvios)
length(desvios)

# Hacer un histograma con las medias de cada una de las 100 muestras
hist(medias, breaks = 10)

# PASO 2: A SIMULAR Y JUGAR!
# ---------------------------------------------------------------

# La siguiente función utiliza los conceptos anteriores pero de manera más elaborada
# para probar con distintas distribuciones de X, tamaños muestrales, cantidad de
# muestras simuladas, etc.

# Necesitamos tener instalados los paquetes: dplyr, ggplot2, manipulate, ggExtra, gridExtra

# El código que implementa la simulación se encuentra en el archivo simularDistrMuestral.R
# y primero debemos cargarlo a R:
source("simularDistrMuestral.R")

# Ahora lo usamos así e interactuamos con el gráfico:
simularDistrMedia()

# PASO 3: Seguir con variancia y proporción
# Faltaría hacer la intro pero las funciones se usan así:

simularDistrS2()

simularDistrProp()
