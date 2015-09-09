library(shiny)

shinyUI(navbarPage("ILPPP",
                   tabPanel("Home",
                            sidebarPanel(
                                    selectInput(inputId = "outcome", label = "Outcome", choices = c("Percent Mandatory", "Percent Voluntary", "Percent ..."))
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