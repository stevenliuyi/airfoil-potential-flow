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
    
    output$plot <- renderPlotly({
        pm.outputs <- panel.method.outputs()
        results <- pm.outputs$results
        four.digits <- pm.outputs$four.digits
        alpha <- input$alpha * pi / 180 # angle of attack in radian

        title.font <- list(family = 'Arial, sans-serif',
                           size = 18,
                           color = 'lightgrey')
        
        x <- results$x
        
        if (input$quantity == 'pressure coefficient')
        {
            y <- results$cp
            name <- 'Cp'
            yrange <- c(1,-4)
        } else if (input$quantity == 'velocity') {
            y <- results$vel
            name <- 'V'
            yrange <- c(-1,4)
        }
        
        # deplicate the first elements to the last
        x <- c(x, x[1])
        y <- c(y, y[1])
        
        # for annotations of upper and lower surface
        lower.sample <- as.integer(input$num/4)
        upper.sample <- as.integer(input$num/4*3)
        
        plotly <- plot_ly(x = round(results$x.boundary, 3), 
                y = round(results$y.boundary, 3),
                type = 'scatter',
                mode = 'lines',
                name = 'airfoil', 
                fill = 'tozeroy') %>%
            add_trace(x = round(x,3),
                      y = round(y,3),
                      mode = 'lines+markers',
                      name = name,
                      yaxis = 'y2',
                      fill = FALSE,
                      text = paste0(name, ' = ', round(y,3)),
                      hoverinfo = 'text') %>%
            layout(title = paste0('NASA ', four.digits),
                   titlefont = list(color = 'darkgrey'),
                   xaxis = list(range = c(0,1),
                                title = 'x/c',
                                titlefont = title.font),
                   yaxis = list(range = c(-0.2,0.8),
                                title = 'y/c',
                                titlefont = title.font),
                   yaxis2 = list(overlaying = 'y',
                                 range = yrange,
                                 title = name,
                                 titlefont = title.font,
                                 side = 'right'),
                   showlegend = FALSE,
                   margin = list(r=80))
        
        if (input$annotation == TRUE) {
            plotly <- plotly %>%
                add_annotations( # leading edge annotation
                    x=0, y=0, text='leading edge', xref='x', yref='y',
                    showarrow = TRUE, arrowhead = 4, ax=-20, ay=40, opacity=.5) %>%
                add_annotations( # trailing edge annotation
                    x=1, y=0, text='trailing edge', xref='x', yref='y',
                    showarrow = TRUE, arrowhead = 4, ax=20, ay=-40, opacity=.5) %>%
                add_annotations( # lower surface annotation
                    x=x[lower.sample], y=y[lower.sample],
                    text=paste0(name, ' on lower surface'), xref='x', yref='y2',
                    showarrow = TRUE, arrowhead = 4, ax=-20, ay=40, opacity=.5) %>%
                add_annotations( # upper surface annotation
                    x=x[upper.sample], y=y[upper.sample],
                    text=paste0(name, ' on upper surface'), xref='x', yref='y2',
                    showarrow = TRUE, arrowhead = 4, ax=20, ay=-40, opacity=.5) %>%
                add_annotations( # chord annotation
                    x=0.7, y=0, text='chord line', xref='x', yref='y',
                    showarrow = TRUE, arrowhead = 4, ax=-20, ay=40, opacity=.5) %>%
                add_annotations( # angle of attack annotation
                    x=0.1*cos(alpha), y=0.1*sin(alpha),
                    text='angle of attack', xref='x', yref='y',
                    showarrow = TRUE, arrowhead = 4, ax=20, ay=-40, opacity=.5) %>%
                layout( # angle of attack line
                    shapes = list(type='line', x0=0, y0=0,
                                  x1=0.2*cos(alpha), y1=0.2*sin(alpha),
                                  xref='x', yref='y', opacity=.5))
        }
        
        plotly
    })
})
