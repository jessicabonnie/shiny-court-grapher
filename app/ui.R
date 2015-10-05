library(shiny)

shinyUI(fluidPage(verticalLayout(fluid=TRUE,navbarPage("ILPPP",
                   tabPanel("Home",
                            mainPanel(
                              htmlOutput("description"),
                              sliderInput("FYear",
                                          "Fiscal Year:",
                                          min = 2010,
                                          max = 2015,
                                          step= 1,
                                          value = 2012, sep=""),
                              radioButtons("disposition", "Percent Disposition:",
                                           c("Involuntary" = "I",
                                             "Voluntary" = "V",
                                             "Mandatory Outpatient Treatment" = "MO",
                                             "Dismissed" = "D"),
                                           selected="I")

                            
                              
                              ),
                            
                            
                            mainPanel(
                                    plotOutput("map")
                            ),
                            mainPanel(dataTableOutput(outputId="table"))
                   ),
                   tabPanel("About",
                            htmlOutput("summary")
                   )
)
)
)
)