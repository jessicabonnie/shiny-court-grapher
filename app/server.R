library(shiny)

source("prep.R")

shinyServer(function(input, output) {
        
        output$map <- renderPlot({
                
                ggplot(csb.df) + 
                        aes(long,lat,group=group) + 
                        geom_polygon() +
                        geom_path(color="white") +
                        theme_nothing(legend=TRUE) 
        })
        
        output$summary <- renderText({
                txt <- "Lorem ipsum dolor sit amet, eu eros atqui mea, nec stet persius singulis in. Ceteros delectus volutpat et sed, nam in saepe tantas nostrum. Errem voluptatum ut mei, has nisl habemus dissentiunt ad, his ludus meliore ut. Ea vix graece oportere. Dicit option sanctus pri eu, id sit cibo antiopam laboramus."
                
                txt
        })
})