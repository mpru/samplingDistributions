**Esta carpeta contiene la primera versión de este proyecto, que implementaba lo mismo pero usando el paquete manipulate y RStudio.**

# samplingDistributions
**Interactive visualization of sampling distributions with ggplot2 and manipulate**


Uno de los conceptos más desafiantes para enseñar y aprender en cualquier curso básico de estadística es el de distribución muestral. Los estudiantes suelen experimentar dificultades para incorporar la noción de que las estadísticas son variables aleatorias y poseen una distribución de probabilidad propia, al mismo tiempo que los docentes buscan maneras prácticas para enfrentar esta abstrac-ción. 


Con este material se emplea R para simular reiteradas muestras y analizar interactivamente la distribución de las estadísticas obtenidas, haciendo uso de controladores para modificar parámetros distribucionales y de la simulación. Se comienza detallando cómo simular una muestra para luego simular múltiples muestras y calcular y graficar mediante funciones básicas de R base las respectivas distribu-ciones muestrales. 


Una vez que los estudiantes experimentan con el proceso de generación de muestras, se procede a la exploración interactiva, haciendo uso de tres funciones desarrolladas para este fin. La función simularDistrMedia() permite estudiar la distribución muestral de la media aritmética, desplegando en el panel de gráficos de RStudio una interfaz con controles para modificar el número de simulaciones, el tamaño muestral y la distribución de la variable estudiada (normal o no), entre otros. Ante cada nueva selección de opciones, el gráfico se actualiza mostrando un histograma de las medias muestrales simuladas, un box-plot y un gráfico de probabilidad normal, que permite apreciar cómo se ve afectada la distribución de la media muestral. 


Se presentan, además, los parámetros estadísticos teóricos de la distribución de la media junto con el promedio y variancia empíricos y una curva de densidad normal, para que los estudiantes pue-dan comparar y verificar con sus conocimientos teóricos. La función simularDis-trS2() permite verificar la distribución chi-cuadrado de la variancia muestral de una población normal y la función simularDistrProp() permite estudiar la distribución en el muestreo de la proporción muestral. Estas funciones constituyen una herramienta útil en el proceso de enseñanza-aprendizaje del concepto de distribución muestral.

Esto podría encerrarse en un paquetito o en una Shinny-App.
