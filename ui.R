library(shiny)

shinyUI(fluidPage(
    titlePanel("NACA 4-digit Airfoil"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("m", "Max camber (1st digit):",
                        min=0, max=9, step=1, value=2),
            sliderInput("p", "Max camber location (2st digit):",
                        min=0, max=9, step=1, value=4),
            sliderInput("t", "Thickness (3rd & 4th digit):",
                        min=1, max=40, step=1, value=12),
            sliderInput("num", "Number of panels:",
                        min=12, max=200, step=2, value=48),
            sliderInput("alpha", "Angle of attack:",
                        min=-20, max=20, step=1, value=8)
    ),

    mainPanel(
        textOutput("title"),
        plotlyOutput("plot")
    )
  )
))
