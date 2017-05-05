library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Stock return comparison"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
          sidebarPanel(
                  sliderInput("zoomStart", "start of the zoom range:", min = 2010, max = 2016, value = 2010),
                  sliderInput("zoomEnd", "end of the zoom range:", min = 2016, max = 2020, value = 2016),
                  textInput("stockSymbol", "Stock Symbol", "AAPL"),
                  verbatimTextOutput("value"),
                  actionButton("do", "Click Me")),
    mainPanel(
         h3("Instructions:")
         ,h5("    - Use sliders to adjust start and end of the chart interval in years")
         ,h5("    - to start press click next to the symbol; you will get two stocks symbols IBM and Google plus the stock you selected")
         ,h5("      (it may require extra click for the very first slider use, due to difference in app behavior in local browser and on shinyapps.io web site)")
         ,h5("    ")
         ,span(textOutput("readingData"), style="color:blue")
         ,h5("    ")
         ,span(textOutput("symbolError"), style="color:red")
         ,h5("    ")
        ,span(textOutput("symbolOk"), style="color:blue")
        ,h5("    ")
        ,span(textOutput("stockTimeInterval"), style="color:purple")
        ,h5("    ")
        ,span(textOutput("IBMReturn"), style="color:purple")
        ,span(textOutput("GOOGReturn"), style="color:purple")
        ,span(textOutput("stockReturn"), style="color:purple")
        ,h5("    ")
        ,plotOutput("distPlot")
    )
  )
))
