library(shiny)

shinyUI(navbarPage("ILPPP",
                   tabPanel("Home",
                            sidebarPanel(
                                    selectInput(inputId = "outcome", label = "Outcome", 
                                                choices = c("Percent Mandatory", "Percent Voluntary",
                                                            "Percent ...")),
                                    sliderInput("year", "Year:", 
                                                min=0, max=2010, value=2015),
                            ),
                            mainPanel(
                                    plotOutput("map")
                            )
                   ),
                   tabPanel("About",
                            htmlOutput("summary")
                   )
)
)