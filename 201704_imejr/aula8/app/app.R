library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
   titlePanel("Old Faithful Geyser Data"),
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      mainPanel(
         plotlyOutput("distPlot")
      )
   )
)

server <- function(input, output) {
  
   output$distPlot <- renderPlotly({
     
      p <- ggplot(faithful) +
        geom_histogram(aes(x = waiting),
                       bins = input$bins,
                       fill = 'white', 
                       colour = 'black')
      ggplotly(p)
      
   })
}

shinyApp(ui = ui, server = server)

