##shiny app to select points

#enter your rds file for ordered points dataframe of your image outline
#file comes from get_edge_points.R
giraffe <- readRDS("rds_files/bear.rds")


library(ggplot2)
shinyApp(
  ui = basicPage(
    fluidRow(
      column(width = 6,
             plotOutput("plot", height = 500, width = 3500, #change this to adjust view of plot in app
                        click = "plot_click"  
             ),
             h4("Clicked points"),
             DT::DTOutput("plot_clickedpoints")
      ),
      column(width = 6,
             plotOutput("selected_plot", height = 500, width = 3500)) #change this to adjust view of plot in app
    )
  ),
  server = function(input, output, session) {
    data <- giraffe
    
    # vals <- reactiveValues(
    #    keeprows = rep(TRUE, nrow(data)
    # ))
    var1 <- c()
    var2 <- c()
    val <- reactiveValues( clickx = NULL, clicky = NULL, data = cbind (var1, var2))
    
    observe({
      input$plot_click
      isolate({
        # save new points added
        val$clickx = c(val$clickx, input$plot_click$x)
        val$clicky = c(val$clicky, input$plot_click$y)
        # add new points to data
        val$data <- rbind(val$data, cbind(input$plot_click$x, input$plot_click$y))
      })
    })
    
    
    output$plot1 <- renderPlot({
      input$updateplot
      plot(val$data[,1], val$data[,2])
    })
    
    
    output$plot <- renderPlot({
      ggplot(data, aes(x,y)) +
        geom_point(size = 0.2) +
        xlim(0, 2000) +
        ylim(0, 2500) +
        theme_void() +
        scale_y_reverse() 
    })
    output$selected_plot <- renderPlot({
      val_dat <- as.data.frame(val$data)
      ggplot(val_dat, aes(x = val_dat[,1], y = val_dat[,2])) +
        geom_path() +
        xlim(0, 2000) +
        ylim(0, 2500) +
        theme_void() +
        scale_y_reverse() 
    })
    # observeEvent(input$plot_click, {
    #   res <- nearPoints(data, input$plot_click, maxpoints = 1, allRows = TRUE)
    #   vals$keeprows <- xor(vals$keeprows, res$selected_)
    # })
    output$plot_clickedpoints <- DT::renderDT({
      as.data.frame(val$data)},
      extensions = c('Buttons'), 
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv'),
        pageLength = nrow(data)
    ))
  }
)
    