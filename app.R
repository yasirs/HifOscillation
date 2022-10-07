#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(deSolve)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("HIF Oscillations"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "HIF0", label = "HIF[0]:",
                         min = .00001, max = 5, value = .1),
            numericInput(inputId = "Lactate0", label = "Lactate[0]",
                         min = .00001, max = 2, value = .2),
            numericInput(inputId = "GFP0", label = "GFP[0]", 
                         min = 0, max = 20, value = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("outPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  parameters = c(ksxs = .02, ksx = 1, kpx = 1, kdx = 0.2, kdsx = 1,
                 ksy = .01, kpy = 1, kdy = .01,
                 Vg = 1, kg = 0.05, dg = 0.1)
  
  
  
  
  
  times = seq(0, 1000, by=1)
  
  output$outPlot <- renderPlot({
  
  
  
  
  
  state = c(HIF = input$HIF0, Lactate=input$Lactate0, GFP=input$GFP0)
  
  model = function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dHIF = ksxs + ksx*(HIF^2)/(kpx + HIF^2) - kdx*HIF - 
        kdsx*HIF*Lactate
      
      dLactate = ksy*HIF^2/(kpy + HIF^2) - kdy*Lactate
      
      dGFP = Vg*HIF^3/(kg+HIF^3) - dg*GFP
      return(list(c(dHIF, dLactate, dGFP)))
    })
  }
  
  
  out = ode(y = state, times=times, func=model, parms = parameters)
  
  plot(out)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
