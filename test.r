require(shiny)

ui <- fluidPage(
  fileInput("file", NULL, buttonLabel = "Upload...", multiple = TRUE),
  tableOutput("head"),
  selectInput("n", "Layer", NULL)
)

server <- function(input,output,session) {
  data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = vroom::vroom(input$file$datapath, delim = ","),
           tsv = vroom::vroom(input$file$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  output$head <- renderTable({
    head(data())
  })
  observe({
    updateSelectInput(session, "n",
                      choices = data()
    )})
  
}

shinyApp(ui,server)