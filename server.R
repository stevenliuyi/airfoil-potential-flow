library(shiny)
library(plotly)
library(stringr) # for str_pad

shinyServer(function(input, output) {
    source("./panel.method.R")
    
    panel.method.outputs <- reactive({
        four.digits <- paste0(toString(input$m),
                              toString(input$p),
                              str_pad(input$t,2,pad="0"))
        results <- panel.method(four.digits, input$num, input$alpha)
        list("results" = results, "four.digits" = four.digits)
    })
    
    output$title <- renderText({
        
    })

    output$plot <- renderPlotly({
        pm.outputs <- panel.method.outputs()
        results <- pm.outputs$results
        four.digits <- pm.outputs$four.digits

        title.font <- list(family = 'Arial, sans-serif',
                           size = 18,
                           color = 'lightgrey')
        
        x <- results$x
        cp <- results$cp
        
        # deplicate the first elements to the last
        x <- c(x, x[1])
        cp <- c(cp, cp[1])
        
        plot_ly(x = results$x.boundary, 
                y = results$y.boundary,
                type = 'scatter',
                mode = 'lines',
                name = 'airfoil', 
                fill = 'tozeroy') %>%
            add_trace(x = x,
                      y = cp,
                      mode = 'lines+markers',
                      name = 'Cp',
                      yaxis = 'y2',
                      fill = FALSE) %>%
            layout(title = paste0('NASA ', four.digits),
                   titlefont = list(color = 'darkgrey'),
                   xaxis = list(range = c(0,1),
                                title = 'x/c',
                                titlefont = title.font),
                   yaxis = list(range = c(-0.2,0.8),
                                title = 'y/c',
                                titlefont = title.font),
                   yaxis2 = list(overlaying = 'y',
                                 range = c(1,-4),
                                 title = 'Cp',
                                 titlefont = title.font,
                                 side = 'right'),
                   showlegend = FALSE,
                   margin = list(r=80))
    })
})
