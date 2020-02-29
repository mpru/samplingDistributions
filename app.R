#--------------------------------------
# Paquetes
#--------------------------------------

library(shinythemes)
library(shiny)
library(rmarkdown)

#--------------------------------------
# Cargar funciones auxiliares
#--------------------------------------

source("helpers.R")

#--------------------------------------
# UI (user interface)
#--------------------------------------

ui <- fluidPage(
  
  # Theme
  theme = shinytheme("cosmo"),
  
  # Application title
  titlePanel("Estudio de distribuciones muestrales por simulación"),
  
  navbarPage(
    "Opciones",
    
    tabPanel(
      "Media",
      # Sidebar with a slider input for number of bins 
      sidebarPanel(
        sliderInput("n_media", "Tamaño muestral", value = 5, min = 5, max = 100, step = 1),
        sliderInput("nsim_media", "Nº de muestras a simular", value = 1000, min = 100, max = 2000, step = 1),
        selectInput("distr_x", "Distribución de X:", c("Normal", "No normal (Gamma(1.5, 0.5))")),
        # Esto solo se muestra si distr_media se elige en Normal:
        conditionalPanel(condition = 'input.distr_x == "Normal"',
                         sliderInput("mu_media", "E(X)", value = 100, min = -500, max = 500, step = 10),
                         sliderInput("sigma_media", "Desvío(X)", value = 10, min = 1, max = 100, step = 5)
                         )
      ),
      # Show a plot of the generated distribution
      mainPanel(
        h1("Distribución muestral de la media"),
        plotOutput("mediaplot", height = 800, width = 600)
      )
    ),
    
    tabPanel(
      "Variancia",
      # Sidebar with a slider input for number of bins 
      sidebarPanel(
        sliderInput("n_var", "Tamaño muestral", value = 5, min = 5, max = 40, step = 1),
        sliderInput("nsim_var", "Nº de muestras a simular", value = 1000, min = 100, max = 1000, step = 1),
        h4("X tiene distribución Normal."), 
        sliderInput("mu_var", "E(X)", value = 100, min = -500, max = 500, step = 10),
        sliderInput("sigma_var", "Desvío(X)", value = 10, min = 1, max = 100, step = 5)
      ),
      # Show a plot of the generated distribution
      mainPanel(
        h1("Distribución muestral de la variancia"),
        plotOutput("varplot", height = 800, width = 600)
      )
    ),
    
    tabPanel(
      "Proporción",
      # Sidebar with a slider input for number of bins 
      sidebarPanel(
        sliderInput("n_prop", "Tamaño muestral", value = 5, min = 5, max = 100, step = 1),
        sliderInput("nsim_prop", "Nº de muestras a simular", value = 1000, min = 100, max = 2000, step = 1),
        sliderInput("p_prop", "Verdadera proporción p", value = 0.5, min = 0.01, max = 0.99, step = 0.01)
      ),
      # Show a plot of the generated distribution
      mainPanel(
        h1("Distribución muestral de la proporción"),
        plotOutput("propplot", height = 800, width = 600)
      )
    ),
    
    tabPanel(
      "Acerca de la aplicación",
      includeMarkdown("README.md"),
      hr()
    )
  )

  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$varplot <- renderPlot({
    distrVariancia(input$n_var, input$nsim_var, input$mu_var, input$sigma_var, 18)
  })
  
  output$propplot <- renderPlot({
    distrProporcion(input$n_prop, input$nsim_prop, input$p_prop, 18)
  })
  
  output$mediaplot <- renderPlot({
    distrMedia(input$distr_x, input$n_media, input$nsim_media, input$mu_media, input$sigma_media, 18)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
