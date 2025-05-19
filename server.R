library(shiny)
library(DT)


music <- read.csv("music.csv", header = TRUE, sep = ",")

server <- function(input, output) {
  
  output$testText <- renderText({
    paste("Boredom level ",
          input$testRange[1], " - ",
          input$testRange[2], "%")
  })
  
  output$data <- renderDataTable({
    datatable(music, options = list(scrollX = TRUE))
  })

}
