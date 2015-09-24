library(shiny)

shinyUI(navbarPage("ILPPP",
                   tabPanel("Home",
                            sidebarPanel(
                              sliderInput("FYear",
                                          "Fiscal Year:",
                                          min = 2010,
                                          max = 2015,
                                          step= 1,
                                          value = 2012),
                              radioButtons("disposition", "Percent Disposition:",
                                           c("Involuntary" = "I",
                                             "Voluntary" = "V",
                                             "Mandatory Outpatient Treatment" = "MO",
                                             "Dismissed" = "D"),
                                           selected="I"),
                              selectInput(inputId = "outcome",
                                          label = "Outcome",
                                          choices = c("Percent Mandatory", "Percent Voluntary",
                                                      "Percent ..."))
                            
                              
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