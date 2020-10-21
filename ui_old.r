########################################################
# Remove warnings
########################################################
options(warn=-1)

########################################################
# Recall inputs to be used in the app
########################################################
source("utils.r")

########################################################
# Shiny UI
########################################################
navbarPage("sMCDA for DGE", id="nav",
           
           tabPanel("Data Exploration",
                    
                    fluidRow(
                      
                      column(width = 9,
                             
                             box(width = NULL, solidHeader = TRUE,
                                 leafletOutput("map", height=500)
                             ),
                             box(width = NULL,
                                 plotOutput("IndicatorHistogram")
                             )
                      ),
                      column(width = 3,
                             
                             box(width = NULL, status = "warning",
                                 uiOutput("indicatorSelect"),
                                 selectInput("indicators", "Show Layer",
                                             choices = layers,
                                             selectize = FALSE
                                 )
                             ),
                             checkboxInput('projects', ' Show Projects', FALSE)
                      ) 
                    )
           ),
           tabPanel("Perform sMCDA",
                    
                    fluidRow(
                      column(width = 2,
                             box(width = NULL, title = "Objectives Weight:",
                                 sliderInput("environment",
                                             "Environment:",
                                             min = 0, max = 100, value = 33),
                                 sliderInput("economy",
                                             "Economy:",
                                             min = 0, max = 100, value = 33),
                                 sliderInput("social",
                                             "Social:",
                                             min = 0, max = 100, value = 34)
                             ),
                             actionButton("sMCDA","Perform sMCDA"),
                             checkboxInput('projectsres', ' Show Projects', FALSE)
                      ),
                      column(width = 2,
                             box(width = NULL, title = "Environmental Indicators Weight:",
                                 sliderInput("climatech",
                                             "Climate Change:",
                                             min = 0, max = 100, value = 0),
                                 sliderInput("humtox",
                                             "Human Toxicity:",
                                             min = 0, max = 100, value = 0),
                                 sliderInput("partmatfor",
                                             "Particulate Matter Formation:",
                                             min = 0, max = 100, value = 0),
                                 sliderInput("watdep",
                                             "Water Depletion:",
                                             min = 0, max = 100, value = 0),
                                 sliderInput("metdep",
                                             "Metal Depletion:",
                                             min = 0, max = 100, value = 0)
                             ),
                      
                             box(width = NULL, title = "Economic Indicators Weight:",
                                 sliderInput("avegencos",
                                             "Average Generation Costs:",
                                             min = 0, max = 100, value = 0)
                             ),
                      
                             box(width = NULL, title = "Social Indicators Weight:",
                                 sliderInput("accrisk",
                                             "Accident Risk:",
                                             min = 0, max = 100, value = 0),
                                 sliderInput("natseirisk",
                                             "Natural Seismic Risk:",
                                             min = 0, max = 100, value = 0)
                             )
                    ),
                    column(width = 8,
                           
                           box(width = NULL, solidHeader = TRUE,
                               leafletOutput("map_res", height=500)
                           ),
                           box(width = NULL,
                               plotOutput("IndicatorHistogram_res")
                           )
                    )
                    
           )
)
)
