library(dplyr)
library(ggplot2)
library(manipulate)
library(ggExtra)
library(gridExtra)

distrMedia <- function(distr, n, nsim, mu, sigma) {

    # Simular muestras, cada fila es una muestra, con distribución normal o con Beta(2, 8)
    if (distr == "normal") {
        datos <- matrix(rnorm(n * nsim, mu, sigma), nrow = nsim)
    } else {
        # Generamos datos de una Gamma(3/2, 1/2)
        mu <- 1 # esto es E(X) = mu
        sigma <- round(sqrt(2 * mu), 1) # esto es el desvio de la vble X
        datos <- matrix(rgamma(n * nsim, shape = mu/2, rate = 1/2), nrow = nsim)
    }
    
    # Calcular media de cada muestra
    dataMedias <- data_frame(medias = rowMeans(datos))
    
    # Calcular la media y desvío de todas las medias
    promMedias <- round(mean(dataMedias$medias), 2)
    desvMedias <- round(sd(dataMedias$medias), 2)
    
    # Calcular parámetros teóricos de la distribución de la media
    esp <- mu
    desvest <- round(sigma / sqrt(n), 2)
    desvest_max <- round(sigma / sqrt(5), 2) #para fijar ejes
    
    # Para las anotaciones
    leyenda <- list(bquote(N ~ "(" ~ mu ==.(mu) ~ "," ~ frac(sigma, sqrt(n)) == .(desvest) ~ ")"))
    texto <- ifelse(distr == "normal", "X~normal", "X~no~normal")
    texto <- paste0(
        texto,
        "\n n == ", n,
        "\n mu == ", mu,
        "\n sigma == ", sigma,
        "\n sigma / sqrt(n) == ", desvest,
        "\n Promedio~de~bar(X) == ", promMedias,
        "\n Desvío~de~bar(X) == ", desvMedias
    )
    
    # Hacer histograma
    options(warn = -1)
    p <- ggplot(dataMedias, aes(x = medias)) +
        geom_histogram(aes(y=..density..), fill = "orange", color = "black", bins = 60) +
        theme_bw() +
        stat_function(fun = function(x) dnorm(x, mu, desvest), size = 1.2, aes(color = "black")) +
        scale_color_manual("", values = "darkred", labels = leyenda) +
        theme(legend.position=c(-0.01, 0.82),
              legend.justification=c(0, 0),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.text = element_text(size = 11))
    
    # Ajustar limite superior eje vertical si es necesario
    ls <- max(0.4, ggplot_build(p)$layout$panel_ranges[[1]]$y.range[2])
    p <- p + scale_y_continuous("Densidad", limits = c(0, ls))
    
    # Ajustar limites del eje horizontal y agregar anotaciones
    if (distr == "normal") {
        p <- p + 
            scale_x_continuous(expression(bar("X")), limits = mu + c(-4, 4) * desvest_max, breaks = seq(mu - 4 * desvest_max, mu + 4 * desvest_max, desvest_max)) +
            annotate("text", x = mu + 3.999 * desvest_max, y = ls * seq(0.999, by = -0.1, length.out = 7), hjust = 1, label = texto, parse = TRUE)
    } else {
        p <- p + 
            scale_x_continuous(expression(bar("X")), limits = c(0, 4), breaks = 0:4) +
            annotate("text", x = 4, y = ls * seq(0.999, by = -0.1, length.out = 7), hjust = 1, label = texto, parse = TRUE)
    }
    
    # Grafico de probabilidad normal
    # Escribir una funcion especial para poder agregarle la linea xq por default no sale
    qqplot.data <- function (vec) {
        y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
        x <- qnorm(c(0.25, 0.75))
        slope <- diff(y)/diff(x)
        int <- y[1L] - slope * x[1L]
        d <- data.frame(resids = vec)
        ggplot(d, aes(sample = resids)) + stat_qq(size = 1) + geom_abline(slope = slope, intercept = int)
    }
    p2 <- qqplot.data(dataMedias$medias) + 
        coord_flip() +
        theme_bw() 
    if (distr == "normal") {
        p2 <- p2 + 
            scale_y_continuous(expression(bar("X")), limits = mu + c(-4, 4) * desvest_max, breaks = seq(mu - 4 * desvest_max, mu + 4 * desvest_max, desvest_max)) +
            scale_x_continuous(expression(z[p]), limits = c(-4, 4))
    } else {
        p2 <- p2 + 
            scale_y_continuous(expression(bar("X")), limits = c(0, 4)) +
            scale_x_continuous(expression(z[p]), limits =  c(-4, 4))
    }

    # Agregarle boxplot arriba
    p2 <- ggMarginal(p2, type = "boxplot", fill="orange", margins = "x", size = 6)
    
    # Juntar ambos graficos
    grid.arrange(p, p2, ncol=1)
    options(warn = 0)
}

simularDistrMedia <- function() {
    manipulate(
        distrMedia(distr, n, nsim, mu, sigma), 
        n = slider(5, 100, label = "Tamaño Muestral"),
        distr = picker("normal", "no normal", label = "Distribución de X"),
        nsim = slider(100, 2000, label = "Nº de muestras a simular", step = 100, initial = 1000),
        mu = slider(-500, 500, label = "E(X) - Usar sólo con X normal", initial = 100, step = 10),
        sigma = slider(1, 100, label = "Desvío(X) - Usar sólo con X normal", initial = 10, step = 5)
    )   
}


distrVariancia <- function(n, nsim, mu, sigma) {
    
    # Simular muestras, cada fila es una muestra, con distribución normal o con Beta(2, 8)
    datos <- matrix(rnorm(n * nsim, mu, sigma), nrow = nsim)

    # Calcular variancia de cada muestra
    S2 <- apply(datos, 1, var)
    
    # Transformación de la variable Y = (n-1) * S2 / sigma2 ~ Chi2(n-1) si X ~ N(mu, sigma)
    y <- S2 * (n - 1) / sigma^2
    
    # Calcular la media y variancia de todas las variancias
    promS2 <- round(mean(y), 2)
    varS2 <- round(var(y), 2)
    
    # Parámetros (valores teóricos)
    esp <- n - 1
    variancia <- 2 * (n - 1)

    # Para las anotaciones
    texto <- paste0(
        "X~es~Normal",
        "\n n == ", n,
        "\n mu == ", mu,
        "\n sigma == ", sigma,
        "\n Y == (n-1) * S^2 / sigma^2 ~es~ chi[n-1]^2",
        "\n E(Y) ==", n-1,
        "\n Var(Y) ==", 2*(n-1),
        "\n Promedio~de~Y == ", promS2,
        "\n Variancia~de~Y == ", varS2
    )
    
    # Hacer histograma
    options(warn = -1)
    dataY <- data_frame(y = y)
    p <- ggplot(dataY, aes(x = y)) +
        geom_histogram(aes(y=..density..), fill = "orange", color = "black", bins = 40) +
        theme_bw() +
        stat_function(fun = function(x) dchisq(x, n - 1), size = 1.2, aes(color = "black")) +
        scale_color_manual("", values = "darkred", labels = list(bquote(chi[.(n-1)]^2))) +
        scale_x_continuous(expression(Y == "(n-1)" ~ s^2 / sigma^2), limits = c(0, 80), breaks = seq(0, 80, 10)) +
        theme(legend.position=c(-0.01, 0.85),
              legend.justification=c(0, 0),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.text = element_text(size = 11))
    
    # Ajustar limite superior eje vertical si es necesario
    ls <- max(0.2, ggplot_build(p)$layout$panel_ranges[[1]]$y.range[2])
    p <- p + scale_y_continuous("Densidad", limits = c(0, ls))
    
    # Agregar anotaciones
    p <- p + annotate("text", x = 80, y = ls * seq(0.999, by = -0.1, length.out = 9), hjust = 1, label = texto, parse = TRUE)

    # Grafico cuantil cuantil para la distribución chi cuadrado
    dataqq <- data_frame(
        yOrdenado = sort(y),
        cuantilTeorico = qchisq((1:nsim) / (nsim + 1), df = n - 1)
    )
    coefs <- summary(lm(cuantilTeorico ~ yOrdenado, dataqq))$coef[, 1]
    p2 <- ggplot(dataqq, aes(y = cuantilTeorico, x = yOrdenado)) + 
        geom_abline(slope = coefs[2], intercept = coefs[1]) +
        # geom_smooth(method='lm', color = "black") +
        geom_point() + 
        scale_x_continuous("Cuantiles observados", limits = c(0, 80), breaks = seq(0, 80, 10)) + 
        scale_y_continuous("Cuantiles teóricos", limits = c(0, 80), breaks = seq(0, 80, 10)) +
        theme_bw() + 
        annotate("text", x = 80, y = 0, hjust = 1, vjust = 0, label = "QQ~Plot~para~Distribución~chi^2", parse = T)
    p2 <- ggMarginal(p2, type = "boxplot", fill="orange", margins = "x", size = 6)
    
    # Juntar ambos graficos
    grid.arrange(p, p2, ncol=1)
    options(warn = 0)
}

simularDistrS2 <- function() {
    manipulate(
        distrVariancia(n, nsim, mu, sigma), 
        n = slider(5, 40, label = "Tamaño Muestral"),
        nsim = slider(100, 2000, label = "Nº de muestras a simular", step = 100, initial = 1000),
        mu = slider(-500, 500, label = "E(X)", initial = 100, step = 10),
        sigma = slider(1, 100, label = "Desvío(X)", initial = 10, step = 5)
    )   
}


distrProporcion <- function(n, nsim, p) {
    
    # Simular muestras, cada fila es una muestra, con distribución normal o con Beta(2, 8)
    datos <- matrix(rbinom(n * nsim, 1, p), nrow = nsim)
    
    # Calcular proporcion muestral de cada muestra
    pSombrero <- rowMeans(datos)
    
    # Calcular la media y variancia de todas las proporciones
    prompSomb <- round(mean(pSombrero), 2)
    varpSomb <- round(var(pSombrero), 4)
    
    # Parámetros (valores teóricos)
    esp <- p
    variancia <- round(p * (1 - p) / n, 4)
    
    # Para las anotaciones
    leyenda <- list(bquote(N ~ "(" ~ p ==.(p) ~ "," ~ frac(pq, n) == .(variancia) ~ ")"))
    texto <- paste0(
        "\n n == ", n,
        "\n p ==", p,
        "\n pq/n ==", variancia,
        "\n sqrt(pq/n) ==", round(sqrt(p * (1 - p) / n), 4),
        "\n Promedio~de~los~hat(p) == ", prompSomb,
        "\n Variancia~de~los~hat(p) == ", varpSomb
    )
    
    # Hacer histograma
    options(warn = -1)
    dataProp <- data_frame(pSombrero = pSombrero)
    p1 <- ggplot(dataProp, aes(x = pSombrero)) +
        geom_histogram(aes(y=..density..), fill = "orange", color = "black", bins = 40) +
        theme_bw() +
        stat_function(fun = function(x) dnorm(x, esp, sqrt(varpSomb)), size = 1.2, aes(color = "black")) +
        scale_color_manual("", values = "darkred", labels = leyenda) +
        scale_x_continuous(expression(hat(p)), limits = c(0, 1), breaks = seq(0, 1, .2)) +
        theme(legend.position=c(-0.01, 0.85),
              legend.justification=c(0, 0),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.text = element_text(size = 11))
    
    # Ajustar limite superior eje vertical si es necesario
    ls <- max(0.2, ggplot_build(p1)$layout$panel_ranges[[1]]$y.range[2])
    p1 <- p1 + scale_y_continuous("Densidad", limits = c(0, ls))
    
    # Agregar anotaciones
    p1 <- p1 + annotate("text", x = 1, y = ls * seq(0.999, by = -0.1, length.out = 6), hjust = 1, label = texto, parse = TRUE)
    
    # Grafico de probabilidad normal
    # Escribir una funcion especial para poder agregarle la linea xq por default no sale
    qqplot.data <- function (vec) {
        y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
        x <- qnorm(c(0.25, 0.75))
        slope <- diff(y)/diff(x)
        int <- y[1L] - slope * x[1L]
        d <- data.frame(resids = vec)
        ggplot(d, aes(sample = resids)) + stat_qq(size = 1) + geom_abline(slope = slope, intercept = int)
    }
    p2 <- qqplot.data(dataProp$pSombrero) + 
        coord_flip() +
        theme_bw() +
        scale_y_continuous(expression(hat("p")), limits = 0:1, breaks = seq(0, 1, 0.2)) +
        scale_x_continuous(expression(z[p]), limits = c(-4, 4))
    p2 <- ggMarginal(p2, type = "boxplot", fill="orange", margins = "x", size = 6)
    
    # Juntar ambos graficos
    grid.arrange(p1, p2, ncol=1)
    options(warn = 0)
}

simularDistrProp <- function() {
    manipulate(
        distrProporcion(n, nsim, p), 
        n = slider(5, 100, label = "Tamaño Muestral"),
        nsim = slider(100, 2000, label = "Nº de muestras a simular", step = 100, initial = 1000),
        p = slider(0.01, 0.99, label = "Verdadera proporción p", initial = 0.5)
    )   
}
