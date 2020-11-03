# Objects in this file are shared across all sessions in the same R process
source("global.r")

# Graphical user interface using Shinydashboard
ui <- dashboardPage(
  
  # Generate the header
  dbHeader,
  
  # Generate the Sidebar
  dashboardSidebar(
    
    # Generate the sidebar items
    sidebarMenu(
      menuItem("Data Input", tabName = "input", icon = icon("database")),
      menuItem("Criteria Preparation", tabName = "criteria", icon=icon("whmcs")),
      menuItem("Run sMCDA", tabName = "mcda", icon = icon("chart-bar"),
               menuSubItem("Weighted Sum", tabName = "ws", icon = icon("layer-group")),
               menuSubItem("Outranking Approach", tabName = "outranking", icon = icon("layer-group"))),
      menuItem("About", tabName = "about", icon = icon("question-circle")) 
    )
  ),
  dashboardBody(
    tabItems(
      # Generate the Data Input Tab
      tabItem("input", 
              fluidPage(
                
                fluidRow(
                  
                  column(width = 4,
                         titlePanel("Data Input"),
                         p("Welcome to spatial Multi-Criteria Decison Analysis Tool!"),
                         p("Data format: First column for the alternatives and the 
                  remaining columns for the performance of the alternatives on 
                  the criteria. The performances of the alternatives should be 
                  numerical values. Download a sample file. "),
                         
                         fileInput("file", NULL, 
                                   buttonLabel = "Upload...", 
                                   accept=c(".shp",".dbf",".sbn",".sbx",".shx",".prj"),
                                   multiple = TRUE),
                         wellPanel(
                           radioButtons("visuBtn", NULL, choices = c(Table = "Table", Map = "Map"))
                         )),
                  column(width = 8,
                         conditionalPanel(
                           condition = "input.visuBtn == 'Table'",
                           dataTableOutput("datatab")
                         ),
                         conditionalPanel(
                           condition = "input.visuBtn == 'Map'",
                           selectInput("layers",
                                       label=h3("Show Layer"),
                                       "",
                                       selected = NULL
                                       
                           ),
                           checkboxInput('hist', "Show Histogram", FALSE),
                           mapviewOutput("mapplot", height = "600px"),
                           plotOutput("histogram")
                         )
                  )
                )
              )
      ),
      # Generate the Criteria Selection and Elaboration Tab
      tabItem("criteria",
              fluidPage(
                h1("Criteria Selection"),
                selectInput(
                  "variables",
                  multiple = TRUE,
                  label = h3("Select Variable"),
                  ""
                )
                ),
              ),
      # Generate the Weighted Sum Tab
      tabItem("ws",
              fluidPage(
                h1("Weighted Sum"),
                box(width = 4,
                    uiOutput("wssli"),
                    actionButton("updateWSSli", "Update Weigths")
                    ),
                box(width = 3,
                    numericInput("MCrunsws", "Input Monte-Carlo runs", 1),
                    actionButton("sMCDAws","Perform sMCDA")
                ),
                box(width = 5,
                    plotOutput("IndicatorHistogram")
                )
                )
                
      ),
      #Generate the Outranking Method Tab
      tabItem("outranking",
              navbarPage("Outranking Approach", id = "nav",
                tabPanel("Input Data Preparation",
                  p("Input the classe thresholds for each criteria")
                  
                ),
                tabPanel("Thresholds & Weights",
                         uiOutput("outrankingsli"),
                         actionButton("updateOutSli", "Update Weigths")
                         
                ),
                tabPanel("Simulation & Results",
                         numericInput("MCrunsout", "Input Monte-Carlo runs", 1),
                         actionButton("sMCDAout","Perform sMCDA")
                         
                )
              )
      ),
      # Generate the About page
      tabItem("about",
              fluidPage(
                h1("About")
              )
      )
    )
  )
)


server <- function(input, output, session){
  
  ##########################################
  ############ Input Data Page #############
  ##########################################
  
  # Read input file from external source to do this upload all possible files in the 
  data <- reactive({
    
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile) 
    
    dir <- dirname(inFile[1,4])
    
    for ( i in 1:nrow(inFile)) {
      file.rename(inFile[i,4], paste0(dir,"/",inFile[i,1]))}
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    st_read(getshp)
  })
  
  # Generate the Criteria dataset
  layers <- reactive({
    data() %>% select(-1) %>% st_drop_geometry() # Only for CCS test
    # data() %>% select(-c(1,2)) %>% st_drop_geometry() # Corrected one
  })
  
  # Collect Alternative names
  alternatives <- reactive({
    data() %>% select(1) %>% st_drop_geometry() # Only for CCS test
    # data() %>% select(2) %>% st_drop_geometry() # Corrected One 
  })
  
  # Select Criteria from the dataset to be used as input file 
  observe({
    updateSelectInput(
      session,
      "layers",
      choices=names(layers()),
      selected = NULL)
  })
  
  output$histogram <- renderPlot({
    
    if (input$hist > 0) {
      inphist <- data() %>% st_drop_geometry() 
      ggplot() +
        geom_bar(aes(x=inphist[,1],y=inphist[,input$layers]), # In the corrected version use inphist[,1] for x
                 stat="identity", width=0.5,color="black",fill="black") +
        xlab("Alternative") + ylab(input$layers) +
        theme_set(theme_bw(base_size = 20)) +
        theme(panel.border=element_rect(size=1.2, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title=element_text(size=22,face="bold"),
              axis.ticks=element_line(size = 1.2,colour = "black"),
              legend.position="none")
    }
    
  }, bg="transparent")
  
  
  observeEvent(input$layers,{
    
    indt <- data() %>% select(input$layers)
    indt_nogeom <- indt %>% st_drop_geometry() 
    output$mapplot <- renderMapview(mapview(indt,
                                            layer.name = input$layers))  
  })
  
  # Show Data file
  output$datatab <- renderDataTable({
    data()
  })
  
  ##########################################
  ######## Criteria Selection Page #########
  ##########################################
  
  # Select Criteria from the dataset to be used as input file 
  observe({
    updateSelectInput(
      session,
      "variables",
      choices=names(data()))
  })
  
  # Generate the Criteria dataset
  crit <- reactive({
    data() %>% select(!!!input$variables)
  })
  
  # Collect Alternative names
  alt <- reactive({
    data() %>% select(1)
  })
  
  # Collect criteria names 
  critnames <- reactive(names(crit()))
  
  # Collect total number of criteria
  numSliders <- reactive(length(critnames()))
  
  output$unccrit <- renderUI({
    lapply(1:numSliders(), function(i) {
      list(radioButtons(critnames()[i],
                        paste0('Select nature of ', critnames()[i]),
                        choices = c("Exact" = "exact", 
                                    "Uncertain" = "pdf"), selected = "exact"))
    })
  })
  
  # output$uncdist <- renderUI({
  #   lapply(1:numSliders(), function(i) {
  #     if (unccrit == "pdf"){
  #       list(radioButtons(critnames()[i],
  #                         paste0('Select distribution for ', critnames()[i]),
  #                         choices = c("" = "exact", 
  #                                     "Uncertain" = "pdf"), selected = "exact")) 
  #     } else {
  #       NULL
  #       }
  #   })
  # })
  
  ##########################################
  ############## Weighted Sum ##############
  ##########################################
  
 
  
  # Generate sliders for weighted sum dynamically based on the number of criteria
  output$wssli <- renderUI({
    lapply(1:numSliders(), function(i) {
      sliderInput(
        critnames()[i],
        paste0('Select the Weight (%) for ', critnames()[i]),
        min = 0,
        max = 100,
        value = 100/numSliders(),
        step = 1,
        post = "%")
    })

  })
  
  # Update the slider based on the input from the user
  observeEvent(input$updateWSSli, {
    
    totslider <- numeric(0)
    for (i in 1:numSliders()){
      totslider <- sum(totslider, input[[critnames()[i]]])
    }
    
    lapply(1:numSliders(), function(i) {
      updateSliderInput(session,
                        critnames()[i],
                        value = (input[[critnames()[i]]]/totslider)*100
                        )})
  })

  # Select number of Monte-Carlo Runs, default is 1, i.e. no MC.
  observe(
    updateNumericInput(session, "MCrunsws", value = input$value)
  )
  
  # Calculate the Weighted Sum MCDA
  observeEvent(input$sMCDAws, {
    
    critnames <- names(crit())
    tmp1 <- lapply(1:length(alt), function (i) calc_ws(crit()[i]))
    tmp2 <- lapply(1:length(alt), function(i) rep(alt,length(critnames)))
    tmp3 <- lapply(1:length(alt), function(i) rep(critnames))
    
    df.forhistplot <- as.data.frame(cbind(as.numeric(unlist(tmp2)),unlist(tmp1),unlist(tmp3)))
    colnames(df.forhistplot) <- c("id","normind","indname")
    
  })
  
  # Plot the MCDA results
  output$IndicatorHistogram_res <- renderPlot({
    
    critnames <- names(data() %>% select(!!!input$variables))
    cbbPalette <- colorBin(
      palette = brewer.pal(length(critnames),"Spectral"),
      bins=length(critnames),
      na.color = c("white")
    )
    
    ggplot(df.forhistplot) +
      geom_bar(aes(x=as.numeric(levels(id))[id],y=as.numeric(levels(normind))[normind],fill=indname),position="stack",
               stat="identity", width=0.5) +
      scale_x_continuous(breaks=c(seq(0,31,1))) +
      scale_y_continuous(breaks=c(seq(0,1,0.1))) +
      coord_cartesian(ylim=c(0,1),xlim=c(0,31)) +
      scale_fill_manual(values=cbbPalette) +
      xlab("Area") + ylab("sMCDA") +
      theme_set(theme_bw(base_size = 20)) +
      theme(panel.border=element_rect(size=1.2, colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title=element_text(size=22,face="bold"),
            axis.ticks=element_line(size = 1.2,colour = "black"),
            legend.title=element_blank(),
            legend.justification=c(1,1),
            legend.position="bottom")
    
  })
  
  ##########################################
  ########## Outranking Approach ###########
  ##########################################
  
  # Collect criteria names 
  critnamesOut <- reactive(names(crit()))
  
  # Collect total number of criteria
  numSlidersOut <- reactive(length(critnamesOut()))
  
  # Generate sliders for weighted sum dynamically based on the number of criteria
  output$outrankingsli <- renderUI({
    lapply(1:numSlidersOut(), function(i) {
      sliderInput(
        critnamesOut()[i],
        paste0('Select the Weight (%) for ', critnamesOut()[i]),
        min = 0,
        max = 100,
        value = 100/numSlidersOut(),
        step = 1,
        post = "%")
    })
    
  })
  
  # Update the slider based on the input from the user
  observeEvent(input$updateOutSli, {
    
    totsliderOut <- numeric(0)
    for (i in 1:numSlidersOut()){
      totsliderOut <- sum(totsliderOut, input[[critnamesOut()[i]]])
    }
    print(input[[critnamesOut()[1]]])
    lapply(1:numSlidersOut(), function(i) {
      updateSliderInput(session,
                        critnamesOut()[i],
                        value = (input[[critnamesOut()[i]]]/totsliderOut)*100
      )})
  })
  
  # Select number of Monte-Carlo Runs, default is 1, i.e. no MC.
  observe(
    updateNumericInput(session, "MCrunsOut", value = input$value)
  )
  
  # Calculate the Weighted Sum MCDA
  observeEvent(input$sMCDAout, {
    
    
  })
  
}

shinyApp(ui, server)