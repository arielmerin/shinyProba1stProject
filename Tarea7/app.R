#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- pageWithSidebar(
    headerPanel("Tarea 7", windowTitle = "El mejor equipo de proba"),
    sidebarPanel(
      h4("Aguilar Ayala Julio, Aquino Chapa Armando Abraham, Merino Peña Kevin Ariel"),
        h3("Paridad"),
        helpText("a) Realice una función que dado un número entero determine si
                 es par o impar."),
        numericInput("paridad", label = "Ingresa un número", value = 1 ),
        h3("Caminante"),
        helpText("b) Realice un trace los pasos de un caminante"),
        sliderInput("pasos", 
                    label = "Pasos que dará el caminante:",
                    min = 0, max = 10000, value = c(0, 10000)),
        numericInput("probaDerecha", 
                     label = "Ingrese la probabilidad de ir a la derecha", 
                     min = 0, max = 1, 
                     step = 0.01,
                     value = 0.25),
        numericInput("probaAbajo", 
                     label = "Ingrese la probabilidad de ir abajo",
                     min = 0, max = 1, 
                     step = 0.01,
                     value = 0.25),
        numericInput("probaArriba", 
                     label = "Ingrese la probabilidad de ir arriba", 
                     min = 0, max = 1, 
                     step = 0.01,
                     value = 0.25),
        numericInput("probaIzquierda", 
                     label = "Ingrese la probabilidad de ir a la izquierda", 
                     min = 0, max = 1, 
                     step = 0.01,
                     value = 0.25),
        numericInput("xOrigen", 
                     label = "Inicio en x", 
                     min = 0, max = 1, 
                     step = 1,
                     value = 0),
        numericInput("yOrigen", 
                     label = "Inicio en y", 
                     min = 0, max = 1, 
                     step = 1,
                     value = 0),
        selectInput("color", "Ingresa el color de nuestra gráfica", 
                    c("Azulito" = "blue",
                      "Verdecito" = "green",
                      "Rojito" = "red"), 
                    selected = "blue"),
        h3("Dado"),
        helpText("c) Grafique los resultados de tirar m veces un dado con n caras"),
        numericInput("caras", 
                     label = "Número de caras del dado", 
                     min = 0, max = 1000, 
                     step = 1,
                     value = 6),
        sliderInput("dado", 
                    label = "Cuantas veces se va a tirar el dado:",
                    min = 0, max = 10000, value = c(0, 10000))
        
    ),
    mainPanel(imageOutput("logo", inline = TRUE ),
              textOutput("text1"),
              textOutput("text2"),
              htmlOutput("text3"),
              h2( "Elegiste un número: ", span(textOutput("answer"), style = "color:blue")),
              plotOutput("plot1"),
              plotOutput("histograma")
    )
)
server <- function(input, output) {

  
    paridades <- function(numerito){
      if (is.na(numerito)) {
        return("  ")
      }
      numero <- as.integer(numerito)
      if((numero %% 2) == 0 ){
        return("par")
      }else{
        return("impar")
      }
    }
    
    definirDireccion <- function(probaIzquierda, probaDerecha, probaAbajo, probaArriba){
      unity <- runif(1,0,1)
      step <- c(0,0)
      suma = probaAbajo + probaArriba + probaDerecha + probaIzquierda
      if( unity < probaIzquierda){
        #Se va izquierda
        return(c(-1,0))
      }
      if(unity < probaIzquierda + probaDerecha){
        #Se va derecha
        return(c(1,0))
      }
      if(unity < probaAbajo + probaIzquierda + probaDerecha){
        #Se va abajo
        return(c(0,-1))
      }
      #Se va arriba
      return(c(0,1))
    }
    
    camino <- function(pasos, probaIzquierda, probaDerecha, probaAbajo, probaArriba,x,y){
      inicio <-c(x,y)
      xs <- c(x)
      ys <- c(y)
      for (i in 1:pasos) {
        nuevoPaso <- definirDireccion(probaIzquierda, probaDerecha, probaAbajo, probaArriba)
        inicio <- inicio + nuevoPaso
        xs <-append(xs, inicio[1])
        ys <- append(ys, inicio[2])
      }
      return(list(xs, ys))
    }
    
    output$plot1 <- renderPlot({
      trayecto1 <- camino(input$pasos[2], 
                          input$probaIzquierda, 
                          input$probaDerecha,
                          input$probaAbajo, 
                          input$probaArriba, 
                          input$xOrigen, 
                          input$yOrigen)
      plot(trayecto1[[1]], trayecto1[[2]], main="Recorrido del caminante", ylab = " ", xlab = " ",
           type = "l", col= input$color)
    })
    
    output$answer <- renderText(paridades(input$paridad))
    
    output$histograma <- renderPlot({
      dice <- sample(1:input$caras, input$dado[2], replace=TRUE)
      
      # calculate the sum of simluated dice rolls
      dice_sum <- sum(dice)
      
      # calculate the average outcome 
      result <- dice_sum/input$dado[2]
      
      # draw a histogram with a density plot
      hist(dice, breaks=seq(0,input$caras, 1), 
           probability = TRUE,
           main = "Tirar un dado",
           xlab = "Número de caras",
           col = rainbow(6))
      lines(density(dice))
    })
    
    output$logo <- renderImage({
      list(src = "ciencias.png", height = 120, width = 100)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
