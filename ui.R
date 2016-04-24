library(shiny)
library(shinyBS)
require(rCharts)

bsModalHelp <-  bsModal(
  "helpmodal", "Help", "helptoggle", size = "large",
  h3("Word prediction help"),
  p("This application predicts the most likely next words in a phrase entered ",
    "in the 'Type here' box and displays them in a number of ways: a ranked list",
    " (on the right), a graph of probabilities (below) and a word cloud (bottom ",
    "right)."),
  p("There are a number of controls"),
  tags$ul(tags$li("Model used, Kneser-Ney with interpolation, or 'stupid backoff'."),
          tags$li("Live update, if interactivity is lagging it can be disabled, and",
                  " you can use the 'submit' button to request a new prediction"),
          tags$li("Profanity filter, the ",
                  a(href="https://github.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words",
                    "Shutterstock obscene words filter"), " is used to filter out ",
                  "potentially offensive suggestions by default."),
          tags$li("Number of matches, the number of matches to display can be adjusted",
                  " up to 10 matches")),
  h3("Background"),
  p("This application was completed for the Coursera Data Science Specialisation",
    " capstone, reports on the ",
    a("dataset used", href="http://rpubs.com/imalone/corpusreport"),
    " and the ",
    a("preparation of this application",
      href="http://rpubs.com/imalone/NextWord"),
    " can be found on RPubs. Chapter 4 of the Stanford NLP group book ",
    a("Foundations of Statistical Natural Language Processing",
      href="http://nlp.stanford.edu/fsnlp"), " was invaluable in writing it.")
)


shinyUI(fluidPage(

  headerPanel("ZylliBerkhoff"),
  sidebarPanel(
    bsButton('helptoggle','Help',icon=icon('question'),
             style='info',type='toggle'),
    radioButtons("model", "Model", list("Kneser-Ney"="KN","Backoff"="backoff"),
                 selected = "KN"),
    checkboxInput("dynamic", "Live update", TRUE),
    checkboxInput("proffilter", "Profanity filter", TRUE),
    sliderInput("nres","Number of matches", min=1, max=10, value=3)
  ),
  mainPanel(
    fluidRow(
      conditionalPanel(" output.ready === 'ready'", class="btn-info","Please wait"),
      column(textInput('inputText','Type here'),
             actionButton('doUpdate', 'Submit'),
             width=8),
      column(tableOutput('res1'), width=4)
    ),
    fluidRow(
      column(width=8, plotOutput('graph')),
      column(width=4, plotOutput('cloud'))
    )
  ),
  bsModalHelp
))
