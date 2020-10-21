library(shiny)
library(shinydashboard)
library(tools)
library(vroom)
library(tidyverse)

# Dashboard global header for the sMCDA tool
dbHeader <- dashboardHeader(title = "sMCDA Tool",
                            tags$li(a(href = 'http://www.psi.ch',
                                    img(src = 'psi.png',
                                    title = "Paul Scherrer Institute", 
                                    height = "18px")),
                                    class = "dropdown"),
                            tags$li(a(href="mailto:matteo.spada@psi.ch",
                                      icon("envelope"),
                                      title = "Contact Us"),
                                    class = "dropdown"),
                            tags$li(a(href = 'http://www.psi.ch',
                                    icon("power-off"),
                                    title = "Logout"),
                                    class = "dropdown"))


# Graphical user interface using shinydashboard
ui <- dashboardPage(
  
  # Generate the header
  dbHeader,
  
  # Generate the Sidebar
  dashboardSidebar(
    
    # Generate the sidebar items
    sidebarMenu(
      menuItem("Data Input", tabName = "input", icon = icon("database")),
      menuItem("Criteria Selection", tabName = "criteria", icon=icon("brain")),
      menuItem("Method", tabName = "method", icon = icon("whmcs"),
               menuSubItem("Weighted Sum", tabName = "ws", icon = icon("layer-group")),
               menuSubItem("Outranking Approach", tabName = "outranking", icon = icon("layer-group"))),
      menuItem("About", tabName = "about", icon = icon("question-circle")) 
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("input", 
              fluidPage(
                titlePanel("Input Data"),
                p("Welcome to spatial Multi-Criteria Decison Analysis Tool!"),
                p("Data format: First column for the alternatives and the 
                  remaining columns for the performance of the alternatives on 
                  the criteria. The performances of the alternatives should be 
                  numerical values. Download a sample file. "),

                fileInput("file", NULL, 
                          buttonLabel = "Upload...", 
                          multiple = FALSE),
                dataTableOutput("datatab")
              )
      ),
      tabItem("criteria",
              fluidPage(
                h1("Criteria Selection"),
                selectInput(
                  "variables",
                  multiple = TRUE, 
                  label = h3("Select Variable"),
                  ""
                ),
                dataTableOutput("datatab2")
                )
      
      ),
      tabItem("ws",
              fluidPage(
                h1("Weighted Sum")
              )
      ),
      tabItem("outranking",
              fluidPage(
                h1("Outranking Approach")
              )
      ),
      tabItem("about",
              fluidPage(
                h1("About")
              )
      )
    )
  )
)

server <- function(input, output, session){
  
  data <- reactive({
    
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)
    read.csv(inFile$datapath)
    # 
    # ext <- file_ext(input$file$name)
    # switch(ext,
    #        csv = vroom(input$file$datapath, delim = ","),
    #        tsv = vroom(input$file$datapath, delim = "\t"),
    #        validate("Invalid file; Please upload a .csv or .tsv file")
    # )
  }) 
  
  output$datatab <- renderDataTable({
    data()
  })
  
  observe({
    updateSelectInput(
      session,
      "variables",
      choices=names(data()))
  })
  
  output$datatab2 <- renderDataTable({
    data() %>% select(!!!input$variables)
    })
  
}

shinyApp(ui, server)