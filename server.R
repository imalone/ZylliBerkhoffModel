library(shiny)

library(ggplot2)
require(wordcloud)
options(RCHART_WIDTH=500)

source("support.R")
source("debounce.R")

internalmax <- 40
maxwc <- 10    
pal <- brewer.pal(8,"Dark2")

ngctLi <- readRDS("./Data/ngctLbig-prepped-no-double-digit-cut.rds")
disclist <- readRDS("./Data/ngctLbig-no-double-digit-disclist.rds")

profWords <- readLines(file.path("Data","profanities","en"))
profLinesWordCount <- countWords(profWords)
profWords <- data.table(profWords[profLinesWordCount==1])[,key=V1]
noprofWords <- data.table(V1=character())

knpthresh <- 0.001 # Magic number established as threshold to avoid NA results
predictKN <- function(x) {KNpredict(x, ngctLi, disclist, pthresh=knpthresh)}
predictBO <- function(x) {backoffPredict(x, ngctLi, maxwc)}

shinyServer(
  function(input,output, clientData, session) {

    predictfn <- predictKN    
    predict <- reactive({
      if (input$proffilter) {
        filter = profWords
      } else {
        filter = noprofWords
      }
      predlist <- predictfn()(tokens()) 
      nupper <- min(nrow(predlist), internalmax)
      predlist[1:nupper ][!filter, on=c("w"="V1")]
    })


    debouncedInput <- debounce(input$inputText, 800)

    tkinput <- reactiveValues(text="")    

    observe({
      if(input$dynamic) {
        tkinput$text <- debouncedInput() 
      }
    })
    observe({
      if (input$doUpdate || ! input$dynamic) {
        tkinput$text <- isolate(input$inputText)
      }
    })
#     
    predictfn <- reactive({
      if (input$model == "KN") {
        predictKN
      }
      else {
        predictBO
      }
    })

    

    tokens <- reactive({inputToTokens(tkinput$text)})

    predictedN <- reactive({head(predict(), input$nres)})
    
    output$res1 <- renderTable({predictedN()[, .("Next Word"=w, "Rank" = .I)]},
                               include.rownames = FALSE)

    wordgraph <- reactive({
      if(input$model == "KN") {
        eq = aes(reorder(w, prob), prob)
        label <- "Probability"
      }
      else {
        eq = aes(reorder(w, score), score)
        label <- "Score"
      }
      ggplot( predictedN(), mapping = eq ) +
        geom_bar(stat="identity") +
        ylab(label) + xlab("Next word") + coord_flip() +
        theme(text = element_text(size=20))
    })
    output$graph <- renderPlot(wordgraph())

    outcloud <- reactive({
      yval = "score"
      if(input$model == "KN") {
        yval = "prob"
      }
      else {
        yval = "score"
      }
      wordcloud(predict()$w, predict()[[yval]],min.freq=0,
                random.order=TRUE, color=pal, max.words=maxwc)})
    
    output$cloud <- renderPlot(outcloud())

})
