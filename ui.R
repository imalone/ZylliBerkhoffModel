library(shiny)
library(shinyBS)
require(rCharts)

bsModalHelp <-  bsModal(
  "helpmodal", "Help", "helptoggle", size = "large",
  h3("Word prediction help")
)

shinyUI(fluidPage(

  headerPanel("Word prediction"),
  sidebarPanel(
    h3('A sidebar!')
  ),
  mainPanel(
    fluidRow(column(width=4, "It's a row!"),
             column(width=2, align="right",
                    bsButton('helptoggle','Help',icon=icon('question'),
                             style='info',type='toggle'))
    ),
    fluidRow(column(width=2, "Input"),
             column(width=6,
                    textInput('inputText','Type here')
             ),
             fluidRow(textOutput('res1'))
    )   
  ),
  bsModalHelp
))
