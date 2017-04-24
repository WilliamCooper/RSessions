#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

print (getwd())
options (stringsAsFactors=FALSE)
library(shiny)
library(shiny)
suppressMessages (suppressWarnings (
  library(Ranadu, quietly=TRUE, warn.conflicts=FALSE))
)
formatTime <- function (time) {
  t <- as.POSIXlt (time, tz='UTC')
  tt <- sprintf ("%d:%02d:%02d", t$hour, t$min, t$sec)
  return (tt)
}




# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("R Sessions"),
   tabsetPanel (id='whichTab', type='pills',
                tabPanel ('TOC',
                          includeHTML('TOC/TOC.html')),
                tabPanel ('Session1',
                          tabsetPanel (id='S1tab', type='pills',
                                       tabPanel ('Getting Started',
                                                 includeHTML('./Session1/Session1a.html')),
                                       tabPanel ('RStudio Tour',
                                                 includeHTML('./Session1/Session1b.html')),
                                       tabPanel ('Some Examples',
                                                 includeHTML('./Session1/Session1c.html'),
                                                 tabsetPanel (id='S1ex', type='pills',
                                                              tabPanel ('simple plot',
                                                                        includeHTML ('./Session1/E1Code.html'),
                                                                        sidebarLayout(
                                                                          sidebarPanel(
                                                                            selectInput (inputId='S1Var', label='variable to plot', 
                                                                                         choices=c('Temperature'='ATX',
                                                                                                   'Wind Speed'='WSC',
                                                                                                   'Pressure'='PSXC'))
                                                                          ),
                                                                          mainPanel(
                                                                            plotOutput ('S1E1Plot')
                                                                          )
                                                                      )),
                                                              tabPanel ('sounding',
                                                                        includeHTML ('Session1/E2Code.html'),
                                                                        plotOutput ('S1E2Plot', width="50%")),
                                                              tabPanel ('stats',
                                                                        includeHTML ('Session1/E3Code.html'),
                                                                        dataTableOutput ('S1Stats')),
                                                              tabPanel ('recovery factor',
                                                                        includeHTML ('Session1/E4Code.html'))
                                                )
                                       ),
                                       tabPanel ('Text-with-Code',
                                                 includeHTML ('Session1/Session1d.html')),
                                       tabPanel ('Getting Ranadu',
                                                 includeHTML ('Session1/Session1e.html')))),
                tabPanel ('Session2'),
                tabPanel ('Session3'),
                tabPanel ('Session4'),
                tabPanel ('Session5'),
                tabPanel ('Session6'),
                tabPanel ('Session7'),
                tabPanel ('Session8')
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$S1E1Plot <- renderPlot ({
    V <- input$S1Var
    nm <- c('Temperature [deg. C]', 'Wind Speed [m/s]', 'Pressure [hPa]')
    names (nm) <- c('ATX', 'WSC', 'PSXC')    ## these are the actual variables in the data file
    # Data <- getNetCDF('/Data/DEEPWAVE/DEEPWAVErf20.nc', c('ATX', 'WSC', 'PSXC'))
    load ('Session1/Data.Rdata')
    plot (Data$Time, Data[, V], type='l', col='blue', lwd=2, xlab='Time [UTC]', ylab=nm[V])
    title ("DEEPWAVE flight 20")
    # with (Data, plotWAC (data.frame (Time, Data[, V]), ylab=nm[V]))
  })
  
  output$S1E2Plot <- renderPlot ({
    # Directory <- DataDirectory ()    # for portability; sets the local data directory
    # Flight <- "rf20"                 # select a flight
    # Project = "DEEPWAVE"             # select a project
    # fname = sprintf("%s%s/%s%s.nc", Directory,Project,Project,Flight)
    # # XXX set variables needed, here a standard list including DPX and EWX
    # # preliminary look shows that final descent was from 84400 to 91100
    # Data <- getNetCDF (fname, c("Time", "DPXC", "ATX", "PALT"), 84400, 91100)
    saveDataFile <- 'Session1/Data2.RData'
    # save (Data, file = saveDataFile) 
    # for future runs, it will be much faster to use:
    load(saveDataFile)
    plot (Data$DPXC, Data$PALT, type='l', lwd=1.5, # type='l': line plot
          xlab='Temperature or Dew Point [deg C]', ylab='pressure altitude [m]')   
    lines (Data$ATX, Data$PALT, col='forestgreen', lwd=2) # add temperature
    s <- Data$DPXC > Data$ATX
    lines (Data$DPXC[s], Data$PALT[s], col='red', lwd=3)
    # will show how to add legends, titles, axis labels, etc, later
    
  })
  
  output$S1Stats <- renderDataTable ({
    Dstats <- data.frame()
    VarList <- c('WIC', 'ATX', 'DPXC', 'PSXC', 'GGALT', 'PALT')
    Ds <- getNetCDF ('/Data/DEEPWAVE/DEEPWAVErf20.nc', VarList)
    ## FL400 means pressure altitude of 40000 ft
    Ds <- Ds[Ds$PALT/0.3048 > 40000, ]  ## select only points above 40000 ft
    save (Ds, file='Session1/Data3.Rdata')
    load ('Session1/Data3.Rdata')
    Dstats['Time', 1] <- 'Time'
    Dstats['Time', 2] <- NA
    Dstats['Time', 3] <- NA
    Dstats['Time', 4] <- formatTime (Ds$Time[1])
    Dstats['Time', 5] <- formatTime (Ds$Time[nrow(Ds)])
    for (nm in names(Ds)) {
      if (nm == 'Time') {next}
      Dstats[nm, 1] <- nm
      Dstats[nm, 2] <- mean (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 3]   <- sd   (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 4]  <- min  (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 5]  <- max  (Ds[, nm], na.rm=TRUE)
    }
    names(Dstats) <- c('variable', 'mean', 'sd', 'min', 'max')
    row.names (Dstats) <- names(Ds)
    for (k in 2:5) {
      Dstats[2:nrow(Dstats), k] <- sprintf('%.3f', as.numeric(Dstats[2:nrow(Dstats), k]))
    }
    Dstats
  })
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

