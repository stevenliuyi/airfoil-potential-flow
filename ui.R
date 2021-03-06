library(shiny)
library(plotly)

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
                        min=-20, max=40, step=1, value=8)
        ),

        mainPanel(
            plotlyOutput("plot"),
            tags$div(
                style='margin-top:20px',
                fluidRow(
                    column(8,
                           tableOutput("table")),
                    column(4,
                           selectInput("quantity", label = NULL,
                                          choices = c("pressure coefficient",
                                                      "velocity",
                                                      "local circulation")),
                           checkboxInput("annotation", "Show annotations")
                    )
                )
            )
        )
    ),
    
    tags$div(
        style='text-align: center; color: grey',
        HTML("Steven Liu&nbsp;&nbsp;2017&nbsp;&nbsp;"),
        tags$a(href="https://github.com/stevenliuyi/",
               title="my GitHub",
               icon("github")),
        tags$a(href="https://github.com/stevenliuyi/airfoil-potential-flow",
               title="source code for this app",
               icon("code-fork"))
    )
))
