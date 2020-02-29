# Simulación de distribuciones muestrales


**Visualización interactiva de distribuciones muestrales con ggplot2 y Shiny-apps**

Aplicación web disponible en [https://mpru.shinyapps.io/samplingDistributions/](https://mpru.shinyapps.io/samplingDistributions/).

Uno de los conceptos más desafiantes para enseñar y aprender en cualquier curso básico de estadística es el de distribución muestral. Los estudiantes suelen experimentar dificultades para incorporar la noción de que las estadísticas son variables aleatorias y poseen una distribución de probabilidad propia, al mismo tiempo que los docentes buscan maneras prácticas para enfrentar esta abstracción. Con aplicación web se emplea R para simular reiteradas muestras y analizar interactivamente la distribución de las estadísticas obtenidas, haciendo uso de controladores para modificar parámetros distribucionales y de la simulación. 


En el caso del estudio de la distribución muestral de la media, se puede modificar el número de simulaciones, el tamaño muestral y la distribución de la variable estudiada (normal o no), entre otros. Ante cada nueva selección de opciones, el gráfico se actualiza mostrando un histograma de las medias muestrales simuladas, un box-plot y un gráfico de probabilidad normal, que permite apreciar cómo se ve afectada la distribución de la media muestral. Se presentan, además, los parámetros estadísticos teóricos de la distribución de la media junto con el promedio y variancia empíricos y una curva de densidad normal, para poder comparar y verificar los conocimientos teóricos con los resultados de la simulación. 


Las restantes pestañas de la aplicación permiten estudiar de manera similar la distribución chi-cuadrado de la variancia muestral de una población normal y la distribución en el muestreo de la proporción muestral. Estas funciones constituyen una herramienta útil en el proceso de enseñanza-aprendizaje del concepto de distribución muestral.
