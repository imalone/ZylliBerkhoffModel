library(shiny)

library(ggplot2)
library(rCharts)

require(reshape2)
options(RCHART_WIDTH=600)

source("support.R")
ngctL <- lapply(paste0("ngct",1:4,".rds"), readRDS)

lapply(2:4, function(x){setkeyv(ngctL[[x]],paste0("V",(x-1):1))})

shinyServer(
  function(input,output, clientData, session) {
    tokens <- reactive({inputToTokens(input$inputText)})
    predict <- reactive({backoffPredict(tokens(), ngctL)})
    output$res1 <- renderPrint({unlist(predict())})
    #output$res1 <- renderPrint(tokens())
  }
)
