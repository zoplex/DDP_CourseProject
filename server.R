

library(shiny)
library(ggplot2)
library(xts)
library(dygraphs)

shinyServer(function(input, output, session) {
        initflag        <<- 1
        symbolError     <<- ""
        symbolOk        <<- ""
        
        # Get fixed list of stock data from Yahoo Finance
        sp500_url       <<- "http://real-chart.finance.yahoo.com/table.csv?s=^GSPC&a=07&b=24&c=2010&d=05&e=1&f=2017&g=d&ignore=.csv"
        djia_url        <<- "http://real-chart.finance.yahoo.com/table.csv?s=DJIA&a=07&b=24&c=2010&d=05&e=1&f=2017&g=d&ignore=.csv"
        djix_url        <<- "http://real-chart.finance.yahoo.com/table.csv?s=DJIX&a=07&b=24&c=2010&d=05&e=1&f=2017&g=d&ignore=.csv"
        ibm_url         <<- "http://real-chart.finance.yahoo.com/table.csv?s=IBM&a=07&b=24&c=2010&d=05&e=1&f=2017&g=d&ignore=.csv"
        goog_url        <<- "http://real-chart.finance.yahoo.com/table.csv?s=GOOG&a=07&b=24&c=2010&d=05&e=1&f=2017&g=d&ignore=.csv"
        
        
        yahoo.read <- function(url, messageon, newsym ){
                dat <- try( read.table(url,header=TRUE,sep=",") )
                if (class(dat)=="try-error") {
                        #cat("try-error caught \n")
                        symbolError     <<- paste0("symbol ", newsym, " is not valid - please enter new symbol")
                        #cat(paste0("Trapped error - message: ", symbolError, "\n") )
                        output$symbolError <- renderText({ symbolError })
                        symbolOK  <<- ""
                        output$symbolOk <- renderText({ symbolOk })
                        #cat(paste0("Trapped error - symbolError: [", symbolError, "], symbolOK: [", symbolOK, "] \n") )
                        return (NA)
                } else {
                        symbolError             <<- ""
                        output$symbolError      <- renderText({ symbolError })
                        if ( messageon ) {
                                symbolOK                <<- paste0("symbol ", newsym, " added to the plot")
                                output$symbolOk         <- renderText({ symbolOK })                                
                        }

                        #cat(paste0("no error - symbolError: [", symbolError, "], symbolOK: [", symbolOK, "] \n") )
                        
                        df <- dat[,c(1:7)]
                        names(df)  <- c("Date", "Open", "High", "Low", "Close", "Volme", "AdjClose")
                        df$Date <- as.Date(as.character(df$Date))
                        return(df)
                }
        }
        output$readingData      <- renderText({ "... reading data ..." })
        ibm     <<- yahoo.read(ibm_url, FALSE, "")
        goog    <<- yahoo.read(goog_url, FALSE, "")
        spxst   <<- yahoo.read(sp500_url, FALSE, "")
        djiast   <<- yahoo.read(djia_url, FALSE, "")
        output$readingData      <- renderText({ " " })
        
        
        output$spacetext <- renderUI({
                HTML(paste(" ", " ", sep="<br/>"))
        })
        observeEvent(input$do, {
                #cat("testing CHANGE click value of ", toString(initflag), "\n" )
                initflag <<- input$do
                #cat("testing REACTIVE click value of ", toString(initflag), "\n" )
        })
        observeEvent(input$zoomStart, {
                plotZoomStart         <- input$zoomStart
                #cat("testing CHANGE click value of ", toString(initflag), "\n" )
                initflag <<- 1
                #cat("testing REACTIVE click value of ", toString(initflag), "\n" )
        })
        observeEvent(input$zoomEnd, {
                plotZoomEnd         <- input$zoomEnd
                #cat("testing CHANGE click value of ", toString(initflag), "\n" )
                initflag <<- 1
                #cat("testing REACTIVE click value of ", toString(initflag), "\n" )
        })
        observeEvent(input$stockSymbol, {
                newSymbol                       <- input$stockSymbol
                output$stockTimeInterval        <- renderText({ " " })
                output$symbolOk                 <- renderText({ " " })
                output$stockReturn              <- renderText({ " " })
                #cat("testing CHANGE click value of ", toString(initflag), "\n" )
                #initflag <<- 1
                #cat("testing REACTIVE click value of ", toString(initflag), "\n" )
        })
        calcReturn      <- function( df ) { (df[1,"AdjClose"] - df[nrow(df),"AdjClose"]) / df[nrow(df),"AdjClose"] * 100.0 }
        output$distPlot <- renderPlot({
                clickcount              <- input$do
                plotZoomStart           <- input$zoomStart
                plotZoomEnd             <- input$zoomEnd
                newSymbol               <- input$stockSymbol
                tmsmsg                  <- paste0("stock returns for "
                                                  , toString(plotZoomStart), " - ", toString(plotZoomEnd), ":" ) 
                cat(tmsmsg)
                output$stockTimeInterval  <- renderText({ tmsmsg } )
                if ( initflag >=1 ) {
                        initflag    <<- 0
                        cat("testing UPDATED/RESET click value of ", toString(initflag), "\n" )

                        any_url         <- paste0("http://real-chart.finance.yahoo.com/table.csv?s="
                                                , newSymbol, "&a=07&b=24&c=2010&d=05&e=1&f=2017&g=d&ignore=.csv")
                        output$readingData      <- renderText({ "... reading data ..." })
                        anyst                   <- yahoo.read(any_url, TRUE, newSymbol )
                        output$readingData      <- renderText({ " " })
                        spxstplot                       <- spxst[ spxst$Date >= as.Date(paste0(plotZoomStart,"-01-01")) & 
                                                                    spxst$Date <= as.Date(paste0(plotZoomEnd,"-12-31")),]
                        djiaplot                        <- djiast[ djiast$Date >= as.Date(paste0(plotZoomStart,"-01-01")) & 
                                                                    djiast$Date <= as.Date(paste0(plotZoomEnd,"-12-31")),]
                        ibmplot                         <- ibm[ ibm$Date >= as.Date(paste0(plotZoomStart,"-01-01")) & 
                                                                ibm$Date <= as.Date(paste0(plotZoomEnd,"-12-31")),]
                        googplot                        <- goog[ goog$Date >= as.Date(paste0(plotZoomStart,"-01-01")) & 
                                                                  goog$Date <= as.Date(paste0(plotZoomEnd,"-12-31")),]
                        
                        ibm_return      <- calcReturn( ibmplot )
                        goog_return     <- calcReturn( googplot )

                        if (class(anyst)=="data.frame")   {
                                anyplot                 <- anyst[ anyst$Date >= as.Date(paste0(plotZoomStart,"-01-01")) & 
                                                                  anyst$Date <= as.Date(paste0(plotZoomEnd,"-12-31")),]
                                anyst_return            <- calcReturn( anyplot)
                                output$stockReturn      <- renderText({ paste0(newSymbol, "   ", toString(round(anyst_return,digits=1)), " %") }) 
                                output$IBMReturn        <- renderText({ paste0("IBM     ", toString(round(ibm_return,digits=1)), " %") }) 
                                output$GOOGReturn       <- renderText({ paste0("GOOG    ", toString(round(goog_return,digits=1)), " %") }) 
                                
                                ggplot(ibmplot,aes(Date,AdjClose)) +
                                        geom_line(aes(color="IBM")) +
                                        geom_line(data=googplot,aes(color="GOOG")) +
                                        geom_line(data=anyplot,aes(color=newSymbol)) +
                                        labs(color="Legend") +
                                        scale_colour_manual("", breaks = c("IBM", "GOOG", newSymbol),
                                                            values = c("blue", "brown", "purple")) +
                                        ggtitle(paste0("Closing Stock Prices: IBM, Google & ", newSymbol)) +
                                        theme(plot.title = element_text(lineheight=.7, face="bold"))
                        } else {
                                ggplot(ibmplot,aes(Date,AdjClose)) + 
                                        geom_line(aes(color="IBM")) +
                                        geom_line(data=googplot,aes(color="GOOG")) +
                                        labs(color="Legend") +
                                        scale_colour_manual("", breaks = c("IBM", "GOOG", newSymbol),
                                                            values = c("blue", "brown", "purple")) +
                                        ggtitle("Closing Stock Prices: IBM &  Google") + 
                                        theme(plot.title = element_text(lineheight=.7, face="bold"))
                        }
                }
   })
})
