source("global.r")

###########################################

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
               menuSubItem("Outranking Approach", tabName = "outranking", icon = icon("layer-group"))
      ),
      menuItem("About", tabName = "about", icon = icon("question-circle")) 
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("input", 
              fluidPage(
                fluidRow(
                  
                  column(width = 4,
                         titlePanel("Input Data"),
                         h4("Welcome to spatial Multi-Criteria Decison Analysis Tool!"),
                         h4("Data format: First column for the alternatives and the 
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
                           condition = "input.visuBtn == 'Map'",
                           selectInput("layers",
                                       label=h3("Show Layer"),
                                       ""
                                       
                           ),
                           checkboxInput('hist', "Show Histogram", FALSE),
                           leafletOutput("mapplot", height = "600px"),
                           uiOutput("downloadInMap"),
                           plotlyOutput("histogram"),
                         )
                  )
                ),
                fluidRow(
                  conditionalPanel(
                    condition = "input.visuBtn == 'Table'",
                    DTOutput('datatab')
                  )
                  
                )
              )
      ),
      # Generate the Criteria Selection and Elaboration Tab
      tabItem("criteria",
              fluidPage(
                h1("Criteria Selection"),
                wellPanel(
                  radioButtons("SelBtnCrit", "Select Criteria Input Information:", choices = c("Input From File" = "File", "Manual Input" = "Manual")),  
                ),
                conditionalPanel(
                  condition = "input.SelBtnCrit == 'Manual'", 
                  selectInput(
                    "variables",
                    multiple = TRUE,
                    label = h3("Select Variable"),
                    ""
                  ),
                  uiOutput("unccrit")
                ),
                conditionalPanel(
                  condition = "input.SelBtnCrit == 'File'",
                  fileInput("fileCritInfo", NULL, 
                            buttonLabel = "Upload...", 
                            accept=c(".csv", ".xlsx"),
                            multiple = FALSE),
                  DTOutput('critinfo')
                )
                
              )
      ),
      # Generate the Weighted Sum Tab
      tabItem("ws",
              fluidPage(
                fluidRow(
                  column(3,
                         h1("Weighted Sum"),
                         wellPanel( 
                           radioButtons("wSelBtnWS", "Select Weight Profile:", choices = c("Sample Weights" = "SWws", "Input Weights" = "IWws"), selected = "SWws"), 
                         ),
                         conditionalPanel(
                           condition = "input.wSelBtnWS == 'IWws'", # Input weight profiles manually only when "Input Weights" is selected
                           uiOutput("wssli"),
                           actionButton("updateWSSli", "Update Weigths")  
                         )
                  ),
                  column(2,
                         p(""),
                         numericInput("MCrunsws", "Input Monte-Carlo runs", 1),
                         actionButton("sMCDAws","Perform sMCDA")
                  ),
                  column(7,
                         conditionalPanel(
                           condition = "input.sMCDAws == TRUE", # Generate the result panel only once the "Perform sMCDA" button is clicked
                           checkboxInput('MCDAhistws', "Show Histogram", FALSE),
                           leafletOutput("resmapws", height = "600px"),
                           uiOutput("downloadWSMap"),
                           plotlyOutput("reshistws"),
                         )
                  )
                  
                )
                
              )
              
      ),
      #Generate the Outranking Method Tab
      tabItem("outranking",
              navbarPage("Outranking Approach", id = "nav",
                         tabPanel("Input Data Preparation", 
                                  fluidPage(
                                    fluidRow(
                                      radioButtons("SelBtnClass", "Select Class Input Information:", choices = c("Input from File" = "FileClass", "Manual Input" = "ManualClass"), selected = "FileClass")
                                    ),
                                    fluidRow(
                                      uiOutput("selectedClassPanel")
                                    )
                                  )
                         ),
                         tabPanel("Thresholds",
                                  fluidPage(
                                    fluidRow(
                                      radioButtons("SelBtnThr", "Select Threshold Input Information:", choices = c("Input From File" = "FileThr", "Manual Input" = "ManualThr"), selected = "FileThr")
                                    ),
                                    fluidRow(
                                      uiOutput("selectedThrPanel")
                                    )
                                  )
                         ),
                         tabPanel("Simulation & Results",
                                  fluidPage(
                                    fluidRow(
                                      column(3,
                                             fluidPage(
                                               fluidRow(
                                                 radioButtons("outSelBtn", "Select Weight Profile:", choices = c("Sample Weights" = "SWout", "Input Weights" = "IWout")),  
                                               ),
                                               fluidRow(
                                                 uiOutput("outrankingsli")
                                               ),
                                               fluidRow(
                                                 uiOutput("outrankingActBtn")
                                               )
                                             )
                                      ),
                                      column(2,
                                             p(""),
                                             sliderInput("lambda", label = h3("Input Decision Maker strongness:"), min = 0, 
                                                         max = 100, value = c(51, 85)),
                                             numericInput("MCrunsout", "Input Monte-Carlo runs", 1),
                                             actionButton("sMCDAout","Perform sMCDA")
                                      ),
                                      column(7,
                                             conditionalPanel(
                                               condition = "input.sMCDAout == TRUE", # Generate the result panel only once the "Perform sMCDA" button is clicked
                                               checkboxInput('sMCDAhistOut', "Show Histogram", FALSE),
                                               leafletOutput("resmapout", height = "600px"),
                                               uiOutput("downloadOutMap"),
                                               plotlyOutput("reshistout")
                                             )
                                      )
                                    )
                                  )
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
  
  #
  # Browse and upload input file
  #
  # Read input file from external source to do this upload all possible files in the 
  in.data.file <- reactive({
    inFile <- input$file # rename input for code simplicity 
    
    req(inFile) # This is used instead of # if(is.null(inFile))
    
    show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
    
    # Need to do this, since *.shp files could not be read alone, also the other files need to be read in at the same time
    dir <- dirname(inFile[1,4])
    
    for ( i in 1:nrow(inFile)) {
      file.rename(inFile[i,4], paste0(dir,"/",inFile[i,1]))
    }
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    
    shpfile <- st_read(getshp)
    remove_modal_spinner()
    shpfile
  })
  
  #
  # Plot and Table section
  #
  # # Show input data when the radio button "Table" is selected (this is the default)
  # output$datatab <- renderDataTable({
  #   data() %>% st_drop_geometry() # data() is the input file in the original version, now in.data.file()
  # })
  
  # Collect the dataset without spatial geometry
  data <- reactive({
    in.data.file() %>% st_drop_geometry()
  })
  
  # Collect spatial geometry for plotting purposes
  geom <- reactive({
    in.data.file() %>% st_geometry()
  })
  
  # Collect Alternative names
  alternatives <- reactive({
    data() %>% select(1)  # Only for CCS test
    # data() %>% select(2) %>% st_drop_geometry() # Corrected One 
  })
  
  
  # Show input data when the radio button "Table" is selected (this is the default)
  output$datatab <- renderDT(
    datatable(data(),
              editable = FALSE,
              rownames = FALSE,
              selection = 'none',
              options = list(
                dom = 'Bfrtip',
                autoWidth=TRUE,
                scrollX = TRUE
              )
    )
  )
  
  # 
  # Generate input matrix and vectors for further analysis
  #
  # Generate the Criteria data.frame
  layers <- reactive({
    data() %>% select(-1)
  }) # Only for CCS test
  
  # Select Criteria from the data.frame to be used for plot purposes 
  observe({
    updateSelectInput(
      session,
      "layers",
      choices=c("",names(layers())))
  })
  
  # Render the histogram of the selected Criteria if the "Show Histogram" is checked
  histexp <- reactiveValues(dat = 0)
  output$histogram <- renderPlotly({
    if (input$hist > 0) {
      
      inphist <- data()
      fig <- plot_ly(x = inphist[,1],y = inphist[,input$layers],type = "bar", marker = list(color = 'rgb(49,130,189)')) %>% 
        layout(xaxis = list(title = "", tickangle = -45), yaxis = list(title = paste0("",input$layers,""))) %>% 
        config(plot_ly(),toImageButtonOptions= list(format = "png", filename = paste0("hist_",input$layers,"_",Sys.Date()),width = 1000,height =  350))
      fig
    }
  }) 
  
  # Plot the layer under interest when the radioButton "Map" is selected
  mapexp <- reactiveValues(dat = 0)
  
  observeEvent(input$layers,{
    show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
    indt <- data() %>% select(all_of(input$layers)) %>% st_set_geometry(geom())
    output$mapplot <- renderLeaflet(mapexp$dat <- mapview:::removeZoomControl(mapview(indt,layer.name = input$layers)@map))
    remove_modal_spinner()
    
    # Download button for map
    output$downloadInMap <- renderUI(
      
      if(input$layers > 0) {
        downloadButton("download_map", "Download Image", icon = icon("download")) 
      } 
    )
    
    output$download_map <- downloadHandler(
      filename = function(){
        paste0("map_",input$layers,"_",Sys.Date(),".png")
      },
      contentType = c("image/png"),
      content = function(file){
        show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
        mapshot(mapexp$dat,
                file = file,
                remove_controls = c("layersControl")
        )
        remove_modal_spinner()
      }
    )
  }, ignoreInit=TRUE)
  
  ##########################################
  ######## Criteria Selection Page #########
  ##########################################
  
  # If Input from file, open the csv or xlsx file containing the information
  in.crit.file <- reactive({
    
    inFile <- input$fileCritInfo # rename input for code simplicity 
    
    req(inFile) # This is used instead of # if(is.null(inFile))
    show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
    if(regexpr("\\.xlsx",inFile$datapath) != -1){
      
      critfile <- read.xlsx(inFile$datapath)
      
    } else {
      
      critfile <- read.csv(inFile$datapath)
    }
    remove_modal_spinner()
    critfile
  })
  
  # Show input data when the file input is selected
  output$critinfo <- renderDT(
    datatable(in.crit.file(), 
              editable = FALSE,
              rownames = FALSE,
              selection = 'none', 
              options = list(
                dom = 'Bfrtip',
                scrollX = TRUE
              )
    )
  )
  
  # If manual input selected, select Criteria from the dataset to be used as input file 
  observe({
    updateSelectInput(
      session,
      "variables",
      choices=c("",names(layers())))
  })
  
  # Generate the Criteria dataset
  crit <- reactive({
    if(input$SelBtnCrit == 'Manual'){ # if manual input is selected
      layers() %>% select(!!!input$variables)
    } else { # if file input is selected
      tmp <- in.crit.file() %>% select(Names) %>% filter_all(any_vars(!is.na(.))) %>% filter(Names != "")
      tmp$Names
    }
  })
  
  # Collect criteria names 
  critnames <- reactive({ 
    if(input$SelBtnCrit == 'Manual'){ # if manual input is selected
      names(crit())
    } else { # if file input is selected
      crit()
    }
  })
  
  # Collect total number of criteria
  numCriteria <- reactive(
    length(critnames())
  )
  
  # Select inputs for further analysis, i.e. criteria information like polarity, behavior (exact or type of distribution), etc.
  # Automatically rendering the page based on the selection of criteria
  output$unccrit <- renderUI({
    if(numCriteria() > 0){ # Start rendering if and only if at least 1 cretaria has been selected
      
      fluidPage(
        fluidRow(
          
          # First Column, based on the number of selected criteria create a drop-down menu to select the polarity of each criteria
          column(width = 3,
                 lapply(1:numCriteria(), function(i) {
                   list(
                     selectInput(paste0('polarity_',i),
                                 paste0('Select Polarity of ', critnames()[i]),
                                 choices = c("",
                                             "Positive" = "+", 
                                             "Negative" = "-")
                     )
                   )
                 })
          ),
          
          # Second Column, based on the number of selected criteria create a drop-down menu to select the nature (exact or a distribution type) of each criteria
          column(width = 3,
                 lapply(1:numCriteria(), function(i) {
                   
                   list(
                     selectInput(paste0('nature_',i),
                                 paste0('Select nature of ', critnames()[i]),
                                 choices = c("",
                                             "Exact" = "exact",
                                             "Uniform" = "unif",
                                             "Normal" = "norm",
                                             "Lognormal" = "logno",
                                             "Poisson" = "pois"
                                 )
                     )  
                   )
                 })
          ),
          
          # Third Column, based on the number of selected criteria create a set of drop-down menu to select the criteria in the input data.frame. 
          # If exact or Poisson, only one parameter is needed (i.e. only one drop-down menu is created), 
          # while for uniform, normal and log-normal distributions two drop-down menu are generated one close to the other to select the parameters
          # of interest from the input data.frame.
          column(width = 6,
                 lapply(1:numCriteria(), function(i) {
                   list(
                     
                     # Exact Criteria Input
                     conditionalPanel(
                       condition = paste0("input.nature_",i, "== 'exact'"),
                       selectInput(paste0('exact_',i),
                                   label = "Select Layer",
                                   choices = c("",names(layers()))
                       )
                     ),
                     
                     # Criteria Input distributed uniformly
                     conditionalPanel(
                       condition = paste0("input.nature_",i, "== 'unif'"),
                       splitLayout(cellWidths = c("50%", "50%"), # Split the column to generate two drop-down menu
                                   selectInput(paste0('unif_min_',i),
                                               label = "Select Layer for Min Unif",
                                               choices = c("",names(layers()))
                                   ), 
                                   selectInput(paste0('unif_max_',i),
                                               label = "Select Layer for Max Unif",
                                               choices = c("",names(layers()))
                                   )
                       )
                       
                     ),
                     
                     # Criteria Input distributed normally
                     conditionalPanel(
                       condition = paste0("input.nature_",i, "== 'norm'"),
                       splitLayout(cellWidths = c("50%", "50%"), # Split the column to generate two drop-down menu  
                                   selectInput(paste0('norm_mean_',i),
                                               label = "Select Layer for Normal Mean",
                                               choices = c("",names(layers()))
                                   ), 
                                   selectInput(paste0('norm_sd_',i),
                                               label = "Select Layer for Normal SD",
                                               choices = c("",names(layers()))
                                   )
                       )
                       
                     ),
                     
                     # Criteria Input distributed log-normally
                     conditionalPanel(
                       condition = paste0("input.nature_",i, "== 'logno'"),
                       splitLayout(cellWidths = c("50%", "50%"), # Split the column to generate two drop-down menu
                                   selectInput(paste0('logno_mean_',i),
                                               label = "Select Layer for LogNormal Mean",
                                               choices = c("",names(layers()))
                                   ), 
                                   selectInput(paste0('logno_sd_',i),
                                               label = "Select Layer for LogNormal SD",
                                               choices = c("",names(layers()))
                                   )
                       )
                       
                     ),
                     
                     # Criteria Input distributed as a Poisson model
                     conditionalPanel(
                       condition = paste0("input.nature_",i, "== 'pois'"),
                       selectInput(paste0('pois_',i),
                                   label = "Select Layer for Poisson parameter",
                                   choices = c("",names(layers()))
                       )
                       
                       
                     )
                   )
                 })
                 
          )
        )
      )      
    }
    
  })
  
  # Save all the inputs of this page in the different input vectors for the sMCDA,
  # i.e., polarity, nature and matrix input vectors. 
  
  # Save polarity vector
  polarity <- reactive({
    if(input$SelBtnCrit == 'Manual'){ # If manual input is selected
      unlist(
        lapply(1:numCriteria(), function(i) {
          input[[paste0("polarity_",i)]]
        }))
    } else { # if file input is selected
      tmp <- in.crit.file() %>% select(Polarity) %>% filter_all(any_vars(!is.na(.))) %>% filter(Polarity != "")
      tmp$Polarity
    }
  })
  
  # Save type of data (exact or distribution)
  nature <- reactive({
    if(input$SelBtnCrit == 'Manual'){ # If manual input is selected
      unlist(
        lapply(1:numCriteria(), function(i) {
          input[[paste0("nature_",i)]]
        }
        ))
    } else { # If file input is selected
      tmp <- in.crit.file() %>% select(Nature) %>% filter_all(any_vars(!is.na(.))) %>% filter(Nature != "")
      tmp$Nature
    }
  })
  
  # Save variables input
  matinput <- reactive({
    if(input$SelBtnCrit == 'Manual'){ # If manual input is selected
      unlist(
        lapply(1:numCriteria(), function(i) {
          if (nature()[i] == 'exact'){
            input[[paste0("exact_",i)]]
          } else if (nature()[i] == 'unif'){
            c(input[[paste0("unif_min_",i)]],input[[paste0("unif_max_",i)]])
          } else if (nature()[i] == 'norm'){
            c(input[[paste0("norm_mean_",i)]],input[[paste0("norm_sd_",i)]])
          } else if (nature()[i] == 'logno'){
            c(input[[paste0("logno_mean_",i)]],input[[paste0("logno_sd_",i)]])
          } else if (nature()[i] == 'pois'){
            input[[paste0("pois_",i)]]
          }
          
        })
      )} else { # If file input is selected
        tmp <-in.crit.file() %>% select(Layers)
        tmp$Layers
      }
  })
  
  # Generating MCDA input matrix
  inMCDAmat <- reactive({
    layers() %>% select(all_of(matinput()))
  })
  
  ##########################################
  ############## Weighted Sum ##############
  ##########################################
  
  # Generate sliders for weighted sum dynamically based on the number of criteria
  output$wssli <- renderUI({
    lapply(1:numCriteria(), function(i) {
      sliderInput(
        paste0("tradeoff_",i),
        paste0('Select the Weight (%) for ', critnames()[i]),
        min = 0,
        max = 100,
        value = 100/numCriteria(),
        step = 1,
        post = "%")
    })
  })
  
  # Update the slider based on the input from the user
  observeEvent(input$updateWSSli, {
    
    totslider <- numeric(0)
    for (i in 1:numCriteria()){
      totslider <- sum(totslider, input[[paste0("tradeoff_",i)]])
    }
    
    lapply(1:numCriteria(), function(i) {
      updateSliderInput(session,
                        paste0("tradeoff_",i),
                        value = (input[[paste0("tradeoff_",i)]]/totslider)*100
      )})
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Select number of Monte-Carlo Runs, default is 1, i.e. no MC.
  observe(
    updateNumericInput(session, "MCrunsws", value = input$value)
  )
  
  # reactiveValues for map and histogram download
  mapWSexp <- reactiveValues(dat = 0)
  histWSexp <- reactiveValues(dat = 0)
  
  # Calculate the Weighted Sum MCDA
  observeEvent(input$sMCDAws, {
    
    # Weight profile based on the selection between sampling or manual input
    if(input$wSelBtnWS == 'IWws'){
      
      ws.weights <- unlist(
        lapply(1:numCriteria(), function(i) {
          input[[paste0("tradeoff_",i)]]/100
        }
        )
      )
    } else {
      
      ws.weights <- NULL
    }
    
    # Check number of Runs
    if(input$MCrunsws != 1){
      
      show_modal_spinner(spin = "atom", color = "#112446",text = HTML("Calculating...It might take some time!<br> Please Wait..."))
      # Calculate the sMCDA results using a min-max normalization and a weighted-sum aggregation
      sMCDAresws <- sMCDAunccritWS(input$MCrunsws,nature(),alternatives(),geom(),inMCDAmat(),polarity(),ws.weights,session)
      
      # Plot the resulting sMCDA map score
      output$resmapws <-renderLeaflet(mapWSexp$dat <- mapview:::removeZoomControl(mapview(sMCDAresws[,2],layer.name = c("Mean sMCDA Score"), col.regions = mapcolpal, at = at_10)@map))
      remove_modal_spinner()
      
      # Render the histogram of the resulting sMCDA score if the "Show Histogram" is checked
      output$reshistws <- renderPlotly({
        if (input$MCDAhistws > 0) {
          
          inphist <- sMCDAresws %>% st_drop_geometry()
          fig <- plot_ly(x = inphist[,1],y = inphist[,2],type = "bar", marker = list(color = 'rgb(49,130,189)'), error_y = ~list(array = inphist[,3], color = '#000000')) %>% 
            layout(xaxis = list(title = "", tickangle = -45), yaxis = list(title = "sMCDA score")) %>% 
            config(plot_ly(),toImageButtonOptions= list(format = "png",filename = paste0("hist_weightedsum_MC_",Sys.Date()) ,width = 1000,height =  350))
          fig
        }
      }) 
      
    } else {
      
      show_modal_spinner(spin = "atom", color = "#112446",text = HTML("Calculating...It might take some time!<br> Please Wait..."))
      # Calculate the sMCDA results using a min-max normalization and a weighted-sum aggregation
      sMCDAresws <- sMCDAallexactcritWS(alternatives(),geom(),inMCDAmat(),polarity(),ws.weights)
      
      # Plot the resulting sMCDA map score
      output$resmapws <- renderLeaflet(mapWSexp$dat <- mapview:::removeZoomControl(mapview(sMCDAresws[,2],layer.name = c("sMCDA Score"), col.regions = mapcolpal, at = at_10)@map))
      remove_modal_spinner()
      
      # Render the histogram of the resulting sMCDA score if the "Show Histogram" is checked
      output$reshistws <- renderPlotly({
        if (input$MCDAhistws > 0) {
          
          inphist <- sMCDAresws %>% st_drop_geometry()
          fig <- plot_ly(x = inphist[,1],y = inphist[,2],type = "bar", marker = list(color = 'rgb(49,130,189)')) %>% 
            layout(xaxis = list(title = "", tickangle = -45), yaxis = list(title = "sMCDA score")) %>% 
            config(plot_ly(),toImageButtonOptions= list(format = "png",filename = paste0("hist_weightedsum_",Sys.Date()),width = 1000,height =  350))
          fig
        }
      }) # Transparency added to avoid a white square below the map when the "Show Histogram" is not checked
    }
    
    # Download  map
    output$downloadWSMap <- renderUI(
      actionButton("download_WSmap", "Download Map", icon = icon("download"))
    )
    
    # Open Modal on button click
    observeEvent(input$download_WSmap,
                 ignoreInit = TRUE,
                 ignoreNULL = TRUE,   # Show modal on start up
                 showModal(myDownloadImagesWSRes())
    )
    
    output$download_ws_png <- downloadHandler(
      filename = function(){
        paste0("map_result_weightedsum_",Sys.Date(),".png")
      },
      content = function(file){
        show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
        mapshot(mapWSexp$dat,
                file = file,
                remove_controls = c("layersControl")
        )
        removeModal()
        remove_modal_spinner()
      }
    )
    output$download_ws_jpg <- downloadHandler(
      filename = function(){
        paste0("map_result_weightedsum_",Sys.Date(),".jpg")
      },
      content = function(file){
        show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
        mapshot(mapWSexp$dat,
                file = file,
                remove_controls = c("layersControl")
        )
        removeModal()
        remove_modal_spinner()
      }
    )
    output$download_ws_csv <- downloadHandler(
      filename = function(){
        paste0("map_result_weightedsum_",Sys.Date(),".csv")
      },
      content = function(file){
        show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
        write.csv(sMCDAresws %>% st_drop_geometry(),file,row.names = FALSE)
        removeModal()
        remove_modal_spinner()
      }
    )
    output$download_ws_xlsx <- downloadHandler(
      filename = function(){
        paste0("map_result_weightedsum_",Sys.Date(),".xlsx")
      },
      content = function(file){
        show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
        write.xlsx(sMCDAresws %>% st_drop_geometry(),file)
        removeModal()
        remove_modal_spinner()
      }
    )
    output$download_ws_shp <- downloadHandler(
      filename = function(){
        paste0("map_result_weightedsum.zip")
      },
      content = function(file){
        show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
        dataplot <- sMCDAresws
        
        # create a temp folder for shp files
        temp_shp <- tempdir()
        
        # write shp files
        st_write(dataplot, paste0(temp_shp,"/map_result_weightedsum.shp"), append=FALSE)
        
        # zip all the shp files
        zip_file <- file.path(temp_shp, paste0("/map_result_weightedsum.zip"))
        shp_files <- list.files(temp_shp,
                                paste0("map_result_weightedsum"),
                                full.names = TRUE)
        # the following zip method works for me in linux but substitute with whatever method working in your OS 
        zip_command <- paste("zip -j", 
                             zip_file, 
                             paste(shp_files, collapse = " "))
        system(zip_command)
        # copy the zip file to the file argument
        file.copy(zip_file, file)
        # remove all the files created
        file.remove(zip_file, shp_files)
        removeModal()
        remove_modal_spinner()
        
      }
    )
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  ##########################################
  ########## Outranking Approach ###########
  ##########################################
  
  #
  # Data Preparation Page
  #
  
  observe({
    if (input$SelBtnClass == 'ManualClass'){
      output$selectedClassPanel <- renderUI(
        fluidRow(
          # Insert number of classes manually
          numericInput("nmbclassout", "Insert the number of classes of interest:", NULL),
          # Call UI rendering for the manual input of the profiles values
          uiOutput("classout")
        )
      )
    } else if(input$SelBtnClass == 'FileClass'){
      output$selectedClassPanel <- renderUI(
        fluidRow(
          # Input file
          fileInput("fileClassInfo", NULL, 
                    buttonLabel = "Upload...", 
                    accept=c(".csv", ".xlsx"),
                    multiple = FALSE),
          
          # Show input data when the file input is selected
          renderDT(
            datatable(in.class.file(), 
                      editable = FALSE,
                      rownames = FALSE,
                      selection = 'none',
                      options = list(
                        dom = 'Bfrtip',
                        scrollX = TRUE
                      )
            )
          )
        )
        
      )
    }
  })
  
  # If Input from file, open the csv or xlsx file containing the information
  in.class.file <- reactive({
    
    inFile <- input$fileClassInfo # rename input for code simplicity 
    
    req(inFile) # This is used instead of # if(is.null(inFile))
    if(regexpr("\\.xlsx",inFile$datapath) != -1){
      
      read.xlsx(inFile$datapath)
      
    } else {
      
      read.csv(inFile$datapath)
    }
    
  })
  
  
  # Select number classes for the analysis
  observeEvent(input$nmbclassout,{
    
    if(!is.na(input$nmbclassout)) {
      
      if(input$nmbclassout == 2){
        output$classout <- renderUI({
          lapply(1:numCriteria(), function(i) {
            numericInput(
              paste0("class_",i),
              paste0('Select the Class Threshold for ', critnames()[i]),
              value = input$value
            )
          })
        })
        
      } else if (input$nmbclassout > 2){
        
        nmbclass <- input$nmbclassout - 1
        output$classout <- renderUI({
          lapply(1:numCriteria(), function(i) {
            listcrit <- lapply(1:nmbclass, function(j) {
              numericInput(
                paste0("crit",i,"class_",j),
                paste0('Select the Class Threshold number ',j,' for ', critnames()[i]),
                value = input$value
              )
            })
            do.call(splitLayout, list <- listcrit) # Split the numeric inputs horizontally for each criteria
          })
        })
        
      }
    }
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE # Added to avoid the app crashing if no inputs given after deleting a previous value
  
  )
  
  # Save profile classes
  profMat <- reactive({
    if(input$SelBtnClass == 'ManualClass'){ # If manual input is selected
      if(input$nmbclassout == 2){
        unlist(
          lapply(1:numCriteria(), function(i) {
            input[[paste0("class_",i)]]
          }
          ))
      } else if(input$nmbclassout > 2) {
        nmbclass <- input$nmbclassout - 1
        listcrit <- lapply(1:numCriteria(), function(i) {
          lapply(1:nmbclass, function(j) {
            input[[paste0("crit",i,"class_",j)]]
          })
        })
        do.call(rbind,listcrit)
      }
    } else {
      t(as.matrix(in.class.file()))
    }
    
  })
  
  #
  # Thresholds Page
  #
  
  observe({
    if (input$SelBtnThr == 'ManualThr'){ # If Manual Input render the manual entries
      output$selectedThrPanel <- renderUI(
        fluidPage(
          column(4,
                 h4("Select Indifference Thresholds"),
                 uiOutput("indthrout")
          ),
          column(4,
                 h4("Select Preference Thresholds"),
                 uiOutput("prefthrout")
          ),
          column(4,
                 h4("Select Veto Thresholds"),
                 uiOutput("vetothrout")
          )
        )
      )
    } else if(input$SelBtnThr == 'FileThr'){ # If input file browse and select the file containing the thresholds
      output$selectedThrPanel <- renderUI(
        fluidRow(
          # Input file
          fileInput("fileThrInfo", NULL, 
                    buttonLabel = "Upload...", 
                    accept=c(".csv", ".xlsx"),
                    multiple = FALSE),
          
          # Show input data when the file input is selected
          renderDT(
            datatable(in.thr.file(), 
                      editable = FALSE,
                      rownames = FALSE,
                      selection = 'none',
                      options = list(
                        dom = 'Bfrtip',
                        scrollX = TRUE
                      )
            )
          )
        )
        
      )
    }
  })
  
  # If Input from file, open the csv or xlsx file containing the information
  in.thr.file <- reactive({
    
    inFile <- input$fileThrInfo # rename input for code simplicity 
    
    req(inFile) # This is used instead of # if(is.null(inFile))
    if(regexpr("\\.xlsx",inFile$datapath) != -1){
      
      read.xlsx(inFile$datapath)
      
    } else {
      
      read.csv(inFile$datapath)
    }
    
  })
  
  # Generate the dynamic Indifference Thresholds input for each selected criteria
  output$indthrout <- renderUI({
    lapply(1:numCriteria(), function(i) {
      numericInput(
        paste0("indif_",i),
        paste0('Select the Indifference Threshold for ', critnames()[i]),
        value = input$value
      )
    })
  })
  
  # Save indifference thresholds
  indifthr <- reactive({
    if(input$SelBtnThr == 'ManualThr'){ # If manual input is selected
      unlist(
        lapply(1:numCriteria(), function(i) {
          input[[paste0("indif_",i)]]
        }
        ))
    } else {
      tmp <- in.thr.file() %>% select(Indifference)
      tmp$Indifference
    }
  })
  
  # Generate the dynamic Preference Thresholds input for each selected criteria
  output$prefthrout <- renderUI({
    lapply(1:numCriteria(), function(i) {
      numericInput(
        paste0("pref_",i),
        paste0('Select the Preference Threshold for ', critnames()[i]),
        value = input$value
      )
    })
  })
  
  # Save preference thresholds
  prefthr <- reactive({
    if(input$SelBtnThr == 'ManualThr'){ # If manual input is selected
      unlist(
        lapply(1:numCriteria(), function(i) {
          input[[paste0("pref_",i)]]
        }
        ))
    } else {
      tmp <- in.thr.file() %>% select(Preference)
      tmp$Preference
    }
  })
  
  # Generate the dynamic Preference Thresholds input for each selected criteria
  output$vetothrout <- renderUI({
    lapply(1:numCriteria(), function(i) {
      numericInput(
        paste0("veto_",i),
        paste0('Select the Veto Threshold for ', critnames()[i]),
        value = input$value
      )
    })
  })
  
  # Save veto thresholds
  vetothr <- reactive({
    if(input$SelBtnThr == 'ManualThr'){ # If manual input is selected
      unlist(
        lapply(1:numCriteria(), function(i) {
          input[[paste0("veto_",i)]]
        }
        ))
    } else {
      tmp <- in.thr.file() %>% select(Veto)
      tmp$Veto
    }
  })
  
  #
  # Simulation & Results Page
  #
  
  observe({
    # Generate sliders for weighted sum dynamically based on the number of criteria
    if (input$outSelBtn == 'IWout'){ # If input weight profile
      output$outrankingsli <- renderUI({
        lapply(1:numCriteria(), function(i) {
          sliderInput(
            paste0("weight_",i),
            paste0('Select the Weight (%) for ', critnames()[i]),
            min = 0,
            max = 100,
            value = 100/numCriteria(),
            step = 1,
            post = "%")
        })
      })
      output$outrankingActBtn <- renderUI({
        actionButton("updateOutSli", "Update Weigths")
      })
    } 
  })
  
  # Update the slider based on the input from the user
  observeEvent(input$updateOutSli, {
    
    totsliderOut <- numeric(0)
    for (i in 1:numCriteria()){
      totsliderOut <- sum(totsliderOut, input[[paste0("weight_",i)]])
    }
    lapply(1:numCriteria(), function(i) {
      updateSliderInput(session,
                        paste0("weight_",i),
                        value = (input[[paste0("weight_",i)]]/totsliderOut)*100
      )})
  })
  
  # Select number of Monte-Carlo Runs, default is 1, i.e. no MC.
  observe(
    updateNumericInput(session, "MCrunsOut", value = input$value)
  )
  
  # reactiveValues for map and histogram download
  mapOutexp <- reactiveValues(dat = 0)
  histOutexp <- reactiveValues(dat = 0)
  
  # Calculate the Electre-Tri sMCDA
  observeEvent(input$sMCDAout, {
    
    # Weight profile based on the selection between sampling or manual input
    if(input$outSelBtn == 'IWout'){
      
      out.weights <- unlist(
        lapply(1:numCriteria(), function(i) {
          input[[paste0("weight_",i)]]/100
        }
        )
      )
    } else {
      
      out.weights <- NULL
    }
    
    
    # Check number of Runs
    if(input$MCrunsout != 1){
      
      show_modal_spinner(spin = "atom", color = "#112446",text = HTML("Calculating...It might take some time!<br> Please Wait..."))
      
      # Calculate the sMCDA results using a min-max normalization and a weighted-sum aggregation
      sMCDAresout <- sMCDAunccritOut(input$MCrunsout,nature(),alternatives(),geom(),polarity(),inMCDAmat(), t(profMat()),indifthr(),prefthr(),vetothr(),input$lambda/100,out.weights,session)
      
      # Plot the resulting sMCDA map score
      output$resmapout <- renderLeaflet(mapOutexp$dat <- mapview:::removeZoomControl(mapview(sMCDAresout[,2],layer.name = c("Mean sMCDA Score"), col.regions = mapcolpal, at = at_10)@map))
      remove_modal_spinner()
      
      # Render the histogram of the resulting sMCDA score if the "Show Histogram" is checked
      output$reshistout <- renderPlotly({
        if (input$sMCDAhistOut > 0) {
          
          inphist <- sMCDAresout %>% st_drop_geometry()
          fig <- plot_ly(x = inphist[,1],y = inphist[,2],type = "bar", marker = list(color = 'rgb(49,130,189)'), error_y = ~list(array = inphist[,3], color = '#000000')) %>% 
            layout(xaxis = list(title = "", tickangle = -45), yaxis = list(title = "sMCDA score")) %>% 
            config(plot_ly(),toImageButtonOptions= list(format = "png",filename = paste0("hist_outranking_MC_",Sys.Date()),width = 1000,height =  350))
          fig
        }
      }) 
      
    } else {
      
      show_modal_spinner(spin = "atom", color = "#112446",text = HTML("Calculating...It might take some time!<br> Please Wait..."))
      
      # Calculate the sMCDA results using an Electre-TRI method
      sMCDAresout <- sMCDAallexactcritOut(alternatives(),geom(),polarity(), inMCDAmat(), t(profMat()),indifthr(), prefthr(), vetothr(), input$lambda/100,out.weights)
      
      # Plot the resulting sMCDA map score
      output$resmapout <- renderLeaflet(mapOutexp$dat <- mapview:::removeZoomControl(mapview(sMCDAresout[,2],layer.name = c("sMCDA Score"), col.regions = mapcolpal, at = at_10)@map))
      remove_modal_spinner()
      
      # Render the histogram of the resulting sMCDA score if the "Show Histogram" is checked
      output$reshistout <- renderPlotly({
        if (input$sMCDAhistOut > 0) {
          
          inphist <- sMCDAresout %>% st_drop_geometry()
          fig <- plot_ly(x = inphist[,1],y = inphist[,2],type = "bar", marker = list(color = 'rgb(49,130,189)')) %>% 
            layout(xaxis = list(title = "", tickangle = -45), yaxis = list(title = "sMCDA score")) %>% 
            config(plot_ly(),toImageButtonOptions= list(format = "png",filename = paste0("hist_outranking_",Sys.Date()),width = 1000,height =  350))
          fig
        }
      }) 
    }
    
    # Download map
    output$downloadOutMap <- renderUI(
      actionButton("download_Outmap", "Download Map", icon = icon("download"))
    )
    
    # Open Modal on button click
    observeEvent(input$download_Outmap,
                 ignoreInit = TRUE,
                 ignoreNULL = TRUE,   # Show modal on start up
                 showModal(myDownloadImagesOutRes())
    )
    
    output$download_out_png <- downloadHandler(
      filename = function(){
        paste0("map_result_outranking_",Sys.Date(),".png")
      },
      content = function(file){
        show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
        mapshot(mapOutexp$dat,
                file = file,
                remove_controls = c("layersControl")
        )
        removeModal()
        remove_modal_spinner()
      }
    )
    output$download_out_jpg <- downloadHandler(
      filename = function(){
        paste0("map_result_outranking_",Sys.Date(),".jpg")
      },
      content = function(file){
        show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
        mapshot(mapOutexp$dat,
                file = file,
                remove_controls = c("layersControl")
        )
        removeModal()
        remove_modal_spinner()
      }
    )
    output$download_out_csv <- downloadHandler(
      filename = function(){
        paste0("map_result_outranking_",Sys.Date(),".csv")
      },
      content = function(file){
        show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
        write.csv(sMCDAresout %>% st_drop_geometry(),file,row.names = FALSE)
        removeModal()
        remove_modal_spinner()
      }
    )
    output$download_out_xlsx <- downloadHandler(
      filename = function(){
        paste0("map_result_outranking_",Sys.Date(),".xlsx")
      },
      content = function(file){
        show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
        write.xlsx(sMCDAresout %>% st_drop_geometry(),file)
        removeModal()
        remove_modal_spinner()
      }
    )
    output$download_out_shp <- downloadHandler(
      filename = function(){
        paste0("map_result_outranking.zip")
      },
      content = function(file){
        show_modal_spinner(spin = "atom", color = "#112446",text = "Please Wait...")
        dataplot <- sMCDAresout
        
        # create a temp folder for shp files
        temp_shp <- tempdir()
        
        # write shp files
        st_write(dataplot, paste0(temp_shp,"/map_result_outranking.shp"), append=FALSE)
        
        # zip all the shp files
        zip_file <- file.path(temp_shp, paste0("/map_result_outranking.zip"))
        shp_files <- list.files(temp_shp,
                                paste0("map_result_outranking"),
                                full.names = TRUE)
        # the following zip method works for me in linux but substitute with whatever method working in your OS 
        zip_command <- paste("zip -j", 
                             zip_file, 
                             paste(shp_files, collapse = " "))
        system(zip_command)
        # copy the zip file to the file argument
        file.copy(zip_file, file)
        # remove all the files created
        file.remove(zip_file, shp_files)
        removeModal()
        remove_modal_spinner()
        
      }
    )
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

shinyApp(ui, server)