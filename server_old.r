#####
# TODO
# - clean up utils.r and server.r 
# - optimize utils.r for more general functions (as for example the polygonized part)
# - HOWTO automatically change the weighting for indicators in a way that everything stand at 100% (At the second level only)?
#####

########################################################
# Remove warnings
########################################################
options(warn=-1)

########################################################
# Recall inputs to be used in the app
########################################################
source("utils.r")

########################################################
# Shiny Server 
########################################################
# Define server logic required for the application
function(input, output, session) {
  
  observe({
    
    # Create the base map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        fitBounds(6.8, 46, 9.2, 47.8)
    })
  })
  
  observe({
    
    # Create the base map
    output$map_res <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        fitBounds(6.8, 46, 9.2, 47.8)
    })
  })
  
  observe({
    
    colorData <- df.poly_new[[input$indicators]]
    nameInd <- names(layers[which(layers == input$indicators)])
    
    if (input$indicators != "" & input$indicators != "HeatFlux" & input$indicators != "DoubEqu"
        & input$indicators != "DoubEnv" & input$indicators != "DoubEco" & input$indicators != "DoubSoc"
        & input$indicators != "TripEqu" & input$indicators != "TripEnv" & input$indicators != "TripEco"
        & input$indicators != "TripSoc" & input$indicators != "noW" & input$indicators != "EqualW"
        & input$indicators != "EnvW" & input$indicators != "EcoW" & input$indicators != "SocW") {
      
        mp <- leafletProxy("map") %>% # needed to append a new geometry on the current map
          clearShapes() %>% # needed to remove the previous geometries on the current map
          clearControls() %>% # needed to remove the previous legend on the current map
          clearMarkers() %>%
          addPolygons(data=df.poly_new,stroke = TRUE, color="white", weight=0.7, 
                      fillOpacity = 0.5, smoothFactor = 0.5, 
                      fillColor=~pal(colorData)) %>% 
          addLegend("bottomright", pal = pal, values = 0:1,
                    title = nameInd,
                    opacity = 1) %>%
          addLabelOnlyMarkers(poly_centers$long, poly_centers$lat,label=poly_centers$id, 
                              labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
  
    } else if (input$indicators != "" & input$indicators == "HeatFlux" & input$indicators != "DoubEqu"
               & input$indicators != "DoubEnv" & input$indicators != "DoubEco" & input$indicators != "DoubSoc"
               & input$indicators != "TripEqu" & input$indicators != "TripEnv" & input$indicators != "TripEco"
               & input$indicators != "TripSoc" & input$indicators != "noW" & input$indicators != "EqualW"
               & input$indicators != "EnvW" & input$indicators != "EcoW" & input$indicators != "SocW") {
      
      # Plot heat flux on the current map
      mp <- leafletProxy("map") %>% # needed to append a new geometry on the current map
        clearShapes() %>% # needed to remove the previous geometries on the current map
        clearControls() %>% # needed to remove the previous legend on the current map
        clearMarkers() %>%
        addPolygons(data=df.poly_new,stroke = TRUE, color="white", weight=0.7, 
                    fillOpacity = 0.5, smoothFactor = 0.5, 
                    fillColor=~pal_hf(colorData)) %>% 
        addLegend("bottomright", pal = pal_hf, values = 50:140,
                  title = nameInd,
                  opacity = 1)
      
    } else if (input$indicators != "" & input$indicators != "HeatFlux" & input$indicators == "DoubEqu"
               | input$indicators == "DoubEnv" | input$indicators == "DoubEco" | input$indicators == "DoubSoc"
               | input$indicators == "TripEqu" | input$indicators == "TripEnv" | input$indicators == "TripEco"
               | input$indicators == "TripSoc" | input$indicators == "noW" | input$indicators == "EqualW"
               | input$indicators == "EnvW" | input$indicators == "EcoW" | input$indicators == "SocW") {
      
      # Plot heat flux on the current map
      mp <- leafletProxy("map") %>% # needed to append a new geometry on the current map
        clearShapes() %>% # needed to remove the previous geometries on the current map
        clearControls() %>% # needed to remove the previous legend on the current map
        clearMarkers() %>%
        addPolygons(data=df.poly_new,stroke = TRUE, color="white", weight=0.7, 
                    fillOpacity = 0.5, smoothFactor = 0.5, 
                    fillColor=~pal_sM(colorData)) %>% 
        addLegend("bottomright", 
                  colors = c("#D7191C", "#FDAE61", "#FFFFBF", "#A6D96A", "#1A9641"),
                  labels = c("Low", "Medium-Low", "Medium", "Medium-High", "High"),
                  title = "Sustainability",
                  opacity = 1)
      
      } else if (input$indicators == "") {
      
      # Empty the current map
      mp <- leafletProxy("map") %>% # needed to append a new geometry on the current map
        clearShapes() %>% # needed to remove the previous geometries on the current map
        clearControls() %>%
        clearMarkers() 
    }
    
    if (input$projects > 0) {
      
      leafIcons <- iconList(
        New = makeIcon("data/Map-Pins/sMCDAforDGE/Green.png",iconWidth = 20,iconHeight = 30, iconAnchorX = 15,iconAnchorY = 25),
        Planned = makeIcon("data/Map-Pins/sMCDAforDGE/Blue.png",iconWidth = 20,iconHeight = 30, iconAnchorX = 15,iconAnchorY = 25),
        Stop = makeIcon("data/Map-Pins/sMCDAforDGE/Red.png",iconWidth = 20,iconHeight = 30, iconAnchorX = 15,iconAnchorY = 25))
      
      html_legend <- "<img border='0' src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACkAAAA2CAYAAABa4LC4AAAACXBIWXMAAAsTAAALEwEAmpwYAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAABCFJREFUeNrUmU1ME1EQx19LQREEUqIcEKiJHowGqwc5eKB6N/amN2tMvKl49ESJJhwteibCTW8leNZy1EQtqOEgCYXAAYnY8lXlwzr/Zou7b9/b3e5ud+MkL5vubnd+O29m3rzZQKlUYk6ku/tEhA4YURptqkt5GlkaucXFpZwTHQE7kAQWpwNGjEaPhb8s0MjQSBNwuqaQBJegQ9IimBFwkmDHXIUkOFhszCGcCDZBsBnHkASYosMDo3uORkKs5WQ9azxed3Cu+H2frc/vso3cnhnDCIEO2IIkOAQB/KdfdL3j0iHWebWRhc81sPqmoFTB7tYftvZlhy2/KbKV979lt03Bxwk2bxlSAcQ0nBfBnbnTorGaVYF1Z0fXZbDTCEQRqAwSFryuPhc6EmC991tZR99hx8648u4Xm3lWYHvbOt0TBBk3hSRARO+g+lzjsTp28VFb2e/cEvjrx+E8K67u85eGCDQphSRAJORPPODlp+2GfmdX4K+Zu6sii14g0GzlB685xU8xLFgLQAie2/ckXNZjxBHkcqEmkk/fbHZ1ikWC50MPJ/0Kj86SGj8In21gkWtNzAuBHujjJKmBVIoEnRW9FIk1I2pLxvlgQZL2UqAPejmJSyGxkvghAr0ayKj6SrvHVjTQG1VDtvKm90MEestcIXWoV/zRT4F+9SoEPl2WtlM4uAop0B9k/4EElQ2TppzyUwT68yEs5DTv/25a9RmS0w++ynQX+DLKDxHoLah9Mqu+sjG/5wukQG9WDanZsf2gPYkfItCbkVpyzSdIbCukkHxXAc7rNSgAuQq9UNmTq/PkhPqOJdqCegup20GmRck8zb8Z9iBe5cblt0VzSKU3c5CKYPrc5LYnkIJZW1C7oOFGLDe5VXNr4vnQw8mY0dqtueiFNedebuoCRrpbVKY8R4dxr6wJX8y91hkhxbdaRFVQkrfm7OhGTSDRahEsgynTUk2x5pD6HCLP7byJ7LH2dcfUikYNqzZlFeqpRbtF0l5BREdk9SQTWBNvM8CvQnByN+SzuKOWMCp6mQQ0za9CcHLB+lqVIBAF/ckRo7a02dwl+FoTzm63eke9+E0/Gwt8sFYFqUx7go/2D8M/q05LuB/9SNE0y9rQljdiyrSPaIrT3F7VacmgYZox+6/l7zgU8Yh2TQ/91I1mS40tuIiggJgiwJjV3aJVifH+Ofdqs/xVwSxQBIDTfP/JFUsq1owq1bKmLdN7r1XY5MILzDwXrioxdbvZVUgFFIH0gj/f9zis6eVIACFXrPihI0gZKPre6H+jvWwAeLuab4qOIBVQ5LZBHhTfeQQ+aBvQEaQCCqW3LNz6kABTdvUEXPgobwY6ToAJJzocQyqgsi+5jizoKqQkmGz7oE4A6dbo6upM0Cjh6OZzXbNkLeWvAAMAvAKnvEMcmNQAAAAASUVORK5CYII=' 
      style='width:20px;height:25px;'>Active Project<br/>
      <img src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACkAAAA2CAYAAABa4LC4AAAACXBIWXMAAAsTAAALEwEAmpwYAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAABBFJREFUeNrUmc9PE1EQx18LSOVHIEZDgkDxokfqxSOtF29qT97UevAs/gWU/6DEP0C46qXqzVPhphdbbxoPQAhKMKSVgC2/6nzrA/fNvre77e524yQvm3Rfdz47b2bevNlYs9kUfmRqamKaLhgpGqOWW1UaZRprGxuba350xDqBJLAsXTAyNJIe/rJOo0SjSMDFUCEJLkeXvEcwJ+A8wS4FCklwsNiSTzgdbI5gS74hCbBAl2dOc3oSY6Knf0zE+/655OlRVZw0tsVJfduNYZFA5zqCJDhohP+kdff7hq+LCyMzoncgKWI9CaOC5kldHB+si8NaRRztfTVNW4GPE2zVM6QExDLM6OAujt1RrOZVYN3f2+9NsBUEog7UBAkL3lcmxvvFwPg9grzh2xmP9r6Ig623onna4LfeEGTWFZIAEb3z1t/ifSNicOJBy/eCEvjq/uYrsm6N31og0LwRkgCRkD9xwOFrTx39rlOBv/769kJn0ZsEWj5nYDcLfIlhwTAAW8+n5w4lH7X0OHHEWS5UIjlxJR3oEpvSF/QwSUsemyUVP0Bq6b90S3RDoAf6mOQVSFkkMCvOim6KRl9acp1bMsuDRfNmoQr0QS+TrBESO0kUotGrQKaUtxpMRgKp0ZuyQo5w00cCadfb4uq1hvqZP0Yp0G/dhcAXt08ajRjSrj8u/gOJywOTUk5FKRr91V5s5LTulkm1iCFV/eA7W+4aL6OiEI3emtUny8rkxo9oIO16y1ZI5cR2vL8eCaRGb8loSRycohAcK4yQvKsA5+02KABZhV47O5Nb8+Qb64zDaiVqKxZ1ybxoezM6g3QrNx7WPhsh+UGsai02Epdnu1L81ndWRf3nqtKCoaWe9nQQa+x+CN2aeD70MFly2ruVm3Dkxu7HcK1IFuQBYzwtyihfo8tyt6wJX9QYocBbLboqKM+tif5NGHKw9U63DRZcSzVpzQUlHVHkBZ03kT00zyy007AalbtQMox2i6G9okS0a9Er32aO70IsTXS+zN+1HbWcU9ErDKBFvgvByTU7Q1vy9xm2/uSiU1va7fiQ47Um+oqdVu+oF+s7K/zndR6sbUHKZc/xaN/ffN12WsJ89CN1y2xqQ3s+iMllX+QWaTct4cUMDdOS2389f8ehiEe0K30Qr3s7XERTQKwQYMbradGrZLh/ItrxVcEtUDSAFd5/CsSS0popWS0rbY6B8bvaJhdewLCrZKzt5kAhJSgC6SX/fSj5UOnlGAAht734oS9IEyj63uh/o73sAPiknW+KviAlKHLbPAfFdx6ND3YM6AtSgkLpYw9TnxNgoVM9sQA+yruBLhNgzo8O35AS1PQl15cFA4U0BFPHPmjfUwkyqDE5eTVHo4lrkM8NzJJhyh8BBgBraqka7loZJwAAAABJRU5ErkJggg=='
      style='width:20px;height:25px;'>Project Planned<br/>
      <img src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACkAAAA2CAYAAABa4LC4AAAACXBIWXMAAAsTAAALEwEAmpwYAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAABANJREFUeNrMmUtPFEEQx3uGGAlCIEZjjLw8qEdQPgD7AYzs1ZPDQa9i8O7u2Ud2j+JBOBg8rpEPsHwAFI6iB0ACIRKym1UCMcxYf9OLPT01D+axYyWdyc70dv2muqq7q8ZwHEckkeHhwVG6oI1TG1AeNaitUtvY2treSKLDiANJYEW6oBWojUT4yya1OrUaAdcyhSQ4iy6liGBBwCWCnU8VkuBgsfmEcBysRbD1xJAEWKHL46A+N7oNceu8EFfP/bu3+1uIL8dCfD0KNUKVQGdiQRIcggD+M8k9n+wzxN1+Q0z0GKKvy19B60SIlUNHLDUdsdzyBV6GjxNsIzKkBMQ0jHFws1dMl9WiCqz7cs/2g11DIHKgps948zpgL/V8PmiKF4PxACH4H/6PcXq9msekXhEKSVZE9E7pg78eMUWBrJiGYByMx7zslNTvP93UAQvyZx3w3fWuQL+LK/DXe99OxE/b8+g2TfuqnyUr+hRjerIAhGBcWJSZ+go73XItdEXyo8umuNltiCwF40OPHp+Sx2NJly/coaXl/sVsAdsCPdCnSckFKQ8JmhU7Axigb1JynVqyqAfLRE9nIaGPifaiLyR2kjyE0euCHHe91YV8IBm94ypkv276XCC9ev9ymWqot/0xT9H1g8/0djJyhjTC9+7/UUyZMCnHKSdXIEZ/w1Q38vaZL19I92/wtae7qT5YP8rHmozepuqTLmuuH+djRUbvqgrpythWfuVjSUZv3deSSJzykHorAFKvKsB5Ow0KQO2E3mzn5Oo6+UHtsdToLCSTQda4xbymvxlykE4tO8jLQyFlbeZ0KYLpFw/sjkB+bHj0bKouGJiILR5kb02MDz1M3u+7d7sedsKac/u2J2B8s0U55Rt0WeiUNeGL771WrOilFu4UVNKt+WovG2uWdmxuG6yEHtWkNcuu5YgiL+11E6vHp8NwK3rKLMppeEDuQiNZlFt8yiuI6NFIBStpTbzNjO4/cPI0pLxrc/UfK+jQK3xAa/ouBCevt5JNOwKR2V2qQWXpsPTB0s+aZXL2uAdjnBfnfngXbj1YzwQpp93So3122z7zsoT+T7f5afYrQ0dOxOS0V9V7KNafdVkCIDMD5VS+PigRj2h3lagfXmLLdl4SchHmALFMgIWo2WJUKej++Wbf4ZR7AoXps6bXn1KBlH5T4ALJDxT3GbdoRvHD2MUBmf7OcNOp70gALO+wflvU0+hUIZVz57QnML7bpylpAOB0lECJHThMIGFte6beQ4Eenz98pn/6LB89U4GUoFD6IELXJwRYiavHSOGjfBjoAgFaSQtWiUQCVAMsaCXVkdiSikUB8zYNH/QIINNqQ0PXLGoOrmmOm5ols5Q/AgwAEfp4WwZ3594AAAAASUVORK5CYII='
      style='width:20px;height:25px;'>Project Stopped"
      
      mp %>%
        addMarkers(projects$long,projects$lat,icon=leafIcons[projects$project]) %>%
        addControl(html=html_legend,position = "bottomleft")
      
    }
    
  })
  
  output$IndicatorHistogram <- renderPlot({
    
    if (input$indicators != "" & input$indicators != "HeatFlux") {
    
      nameInd <- names(layers[which(layers == input$indicators)])  
      ggplot(sel_df) +
        geom_bar(aes(x=as.numeric(id),y=sel_df[,input$indicators]),
                 stat="identity", width=0.5,color="black",fill="black") +
        scale_x_continuous(breaks=c(seq(0,max(as.numeric(sel_df$id)),1))) +
        scale_y_continuous(breaks=c(seq(0,1,0.1))) +
        coord_cartesian(ylim=c(0,1),xlim=c(0,max(as.numeric(sel_df$id)))) +
        xlab("Area") + ylab(nameInd) +
        theme_set(theme_bw(base_size = 20)) +
        theme(panel.border=element_rect(size=1.2, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title=element_text(size=22,face="bold"),
              axis.ticks=element_line(size = 1.2,colour = "black"),
              legend.position="none")
      
    }
    
  })
  
  observe({
    
    updateSliderInput(session, "climatech",
                      value = input$environment/5)
    updateSliderInput(session, "humtox",
                      value = input$environment/5)
    updateSliderInput(session, "partmatfor",
                      value = input$environment/5)
    updateSliderInput(session, "watdep",
                      value = input$environment/5)
    updateSliderInput(session, "metdep",
                      value = input$environment/5)
    updateSliderInput(session, "avegencos",
                      value = input$economy)
    updateSliderInput(session, "accrisk",
                      value = input$social/2)
    updateSliderInput(session, "natseirisk",
                      value = input$social/2)
    
  })
  
  # Need to be reduced for computational time as well in order to get 
  # a more optimize view
  observeEvent(input$sMCDA, {

      # Weighting Scheme
      climch <- (input$environment/100)*(input$climatech/100)
      humt <- (input$environment/100)*(input$humtox/100)
      pmf <- (input$environment/100)*(input$partmatfor/100)
      wdep <- (input$environment/100)*(input$watdep/100)
      mdep <- (input$environment/100)*(input$metdep/100)
      avgec <- (input$economy/100)*(input$avegencos/100)
      accr <- (input$social/100)*(input$accrisk/100)
      nsr <- (input$social/100)*(input$natseirisk/100)
      
      sMCDA_res_tmp <- lapply(1:length(sel_df$id), function (i) calc_ws(climch*sel_df$ClimateCh[i], humt*sel_df$HumanTox[i], 
                                                                        pmf*sel_df$PartMatFor[i], wdep*sel_df$WaterDep[i], 
                                                                        mdep*sel_df$MetalDep[i], avgec*sel_df$AveGenCost[i], 
                                                                        accr*sel_df$AccRisk[i],nsr*sel_df$NatSeiRisk[i]))
      tmp1 <- lapply(1:length(sel_df$id), function (i) c(climch*sel_df$ClimateCh[i], 
                                                       humt*sel_df$HumanTox[i], 
                                                       pmf*sel_df$PartMatFor[i], 
                                                       wdep*sel_df$WaterDep[i],
                                                       mdep*sel_df$MetalDep[i],
                                                       avgec*sel_df$AveGenCost[i], 
                                                       accr*sel_df$AccRisk[i],
                                                       nsr*sel_df$NatSeiRisk[i]))
      tmp2 <- lapply(1:length(sel_df$id), function(i) rep(sel_df$id[i],8))
      tmp3 <- lapply(1:length(sel_df$id), function(i) rep(c("Climate Change",
                                                          "Human Toxicity",
                                                          "Particulate Matter Formation",
                                                          "Water Depletion",
                                                          "Metal Depletion",
                                                          "Average Generation Costs",
                                                          "Accident Risk",
                                                          "Natural Seismic Risk")))
      df.forhistplot <- as.data.frame(cbind(as.numeric(unlist(tmp2)),unlist(tmp1),unlist(tmp3)))
      colnames(df.forhistplot) <- c("id","normind","indname")
      
      sMCDA_res_tmp.df <- as.data.frame(unlist(sMCDA_res_tmp))
      colnames(sMCDA_res_tmp.df) <- "Result"
      sMCDA_res_tmp.df$id = sel_df$id
      sMCDA_res <- join(tmp.points, sMCDA_res_tmp.df, by="id")
      #
      # Polygonize the data.frame of the inputs for the sMCDA 
      # in order to plot them using leaflet
      #
      # Select attributes of interest for plotting and analisys 
      # purposes
      sel_df_res <- distinct(sMCDA_res, id, hole,
                             Result)
      sel_df_name_res <- sel_df_res$id # extract id only for polygonize purpose
      # For each id extract the polygon to be spatialized for plotting
      # purposes
      poly_res <-lapply(sel_df_name_res, function(x) polyfunc(x, dat=sMCDA_res))
      # Spatialized the polygons
      sp.poly_res <- SpatialPolygons(poly_res)
      # Create a new spatialized polygon for plotting purposes 
      df.poly_res <- SpatialPolygonsDataFrame(sp.poly_res, 
                                              data=data.frame(row.names=sel_df_name_res, sel_df_res))
      colorData <- df.poly_res$Result
      
      mpr <- leafletProxy("map_res") %>% # needed to append a new geometry on the current map
        clearShapes() %>% # needed to remove the previous geometries on the current map
        clearControls() %>% # needed to remove the previous legend on the current map
        clearMarkers() %>% # needed to remove previous markers on the current map
        addPolygons(data=df.poly_res,stroke = TRUE, color="white", weight=0.7, 
                    fillOpacity = 0.5, smoothFactor = 0.5, 
                    fillColor=~pal(colorData)) %>% 
        addLegend("bottomright", pal = pal, values = 0:1,
                  title = "sMCDA",
                  opacity = 1) %>%
        addLabelOnlyMarkers(poly_centers$long, poly_centers$lat,label=poly_centers$id, 
                            labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
        
      
      if (input$projectsres > 0) {

        leafIcons <- iconList(
          New = makeIcon("data/Map-Pins/sMCDAforDGE/Green.png",iconWidth = 20,iconHeight = 30, iconAnchorX = 15,iconAnchorY = 25),
          Planned = makeIcon("data/Map-Pins/sMCDAforDGE/Blue.png",iconWidth = 20,iconHeight = 30, iconAnchorX = 15,iconAnchorY = 25),
         Stop = makeIcon("data/Map-Pins/sMCDAforDGE/Red.png",iconWidth = 20,iconHeight = 30, iconAnchorX = 15,iconAnchorY = 25))

        html_legend <- "<img border='0' src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACkAAAA2CAYAAABa4LC4AAAACXBIWXMAAAsTAAALEwEAmpwYAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAABCFJREFUeNrUmU1ME1EQx19LQREEUqIcEKiJHowGqwc5eKB6N/amN2tMvKl49ESJJhwteibCTW8leNZy1EQtqOEgCYXAAYnY8lXlwzr/Zou7b9/b3e5ud+MkL5vubnd+O29m3rzZQKlUYk6ku/tEhA4YURptqkt5GlkaucXFpZwTHQE7kAQWpwNGjEaPhb8s0MjQSBNwuqaQBJegQ9IimBFwkmDHXIUkOFhszCGcCDZBsBnHkASYosMDo3uORkKs5WQ9azxed3Cu+H2frc/vso3cnhnDCIEO2IIkOAQB/KdfdL3j0iHWebWRhc81sPqmoFTB7tYftvZlhy2/KbKV979lt03Bxwk2bxlSAcQ0nBfBnbnTorGaVYF1Z0fXZbDTCEQRqAwSFryuPhc6EmC991tZR99hx8648u4Xm3lWYHvbOt0TBBk3hSRARO+g+lzjsTp28VFb2e/cEvjrx+E8K67u85eGCDQphSRAJORPPODlp+2GfmdX4K+Zu6sii14g0GzlB685xU8xLFgLQAie2/ckXNZjxBHkcqEmkk/fbHZ1ikWC50MPJ/0Kj86SGj8In21gkWtNzAuBHujjJKmBVIoEnRW9FIk1I2pLxvlgQZL2UqAPejmJSyGxkvghAr0ayKj6SrvHVjTQG1VDtvKm90MEestcIXWoV/zRT4F+9SoEPl2WtlM4uAop0B9k/4EElQ2TppzyUwT68yEs5DTv/25a9RmS0w++ynQX+DLKDxHoLah9Mqu+sjG/5wukQG9WDanZsf2gPYkfItCbkVpyzSdIbCukkHxXAc7rNSgAuQq9UNmTq/PkhPqOJdqCegup20GmRck8zb8Z9iBe5cblt0VzSKU3c5CKYPrc5LYnkIJZW1C7oOFGLDe5VXNr4vnQw8mY0dqtueiFNedebuoCRrpbVKY8R4dxr6wJX8y91hkhxbdaRFVQkrfm7OhGTSDRahEsgynTUk2x5pD6HCLP7byJ7LH2dcfUikYNqzZlFeqpRbtF0l5BREdk9SQTWBNvM8CvQnByN+SzuKOWMCp6mQQ0za9CcHLB+lqVIBAF/ckRo7a02dwl+FoTzm63eke9+E0/Gwt8sFYFqUx7go/2D8M/q05LuB/9SNE0y9rQljdiyrSPaIrT3F7VacmgYZox+6/l7zgU8Yh2TQ/91I1mS40tuIiggJgiwJjV3aJVifH+Ofdqs/xVwSxQBIDTfP/JFUsq1owq1bKmLdN7r1XY5MILzDwXrioxdbvZVUgFFIH0gj/f9zis6eVIACFXrPihI0gZKPre6H+jvWwAeLuab4qOIBVQ5LZBHhTfeQQ+aBvQEaQCCqW3LNz6kABTdvUEXPgobwY6ToAJJzocQyqgsi+5jizoKqQkmGz7oE4A6dbo6upM0Cjh6OZzXbNkLeWvAAMAvAKnvEMcmNQAAAAASUVORK5CYII=' 
         style='width:20px;height:25px;'>Active Project<br/>
         <img src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACkAAAA2CAYAAABa4LC4AAAACXBIWXMAAAsTAAALEwEAmpwYAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAABBFJREFUeNrUmc9PE1EQx18LSOVHIEZDgkDxokfqxSOtF29qT97UevAs/gWU/6DEP0C46qXqzVPhphdbbxoPQAhKMKSVgC2/6nzrA/fNvre77e524yQvm3Rfdz47b2bevNlYs9kUfmRqamKaLhgpGqOWW1UaZRprGxuba350xDqBJLAsXTAyNJIe/rJOo0SjSMDFUCEJLkeXvEcwJ+A8wS4FCklwsNiSTzgdbI5gS74hCbBAl2dOc3oSY6Knf0zE+/655OlRVZw0tsVJfduNYZFA5zqCJDhohP+kdff7hq+LCyMzoncgKWI9CaOC5kldHB+si8NaRRztfTVNW4GPE2zVM6QExDLM6OAujt1RrOZVYN3f2+9NsBUEog7UBAkL3lcmxvvFwPg9grzh2xmP9r6Ig623onna4LfeEGTWFZIAEb3z1t/ifSNicOJBy/eCEvjq/uYrsm6N31og0LwRkgCRkD9xwOFrTx39rlOBv/769kJn0ZsEWj5nYDcLfIlhwTAAW8+n5w4lH7X0OHHEWS5UIjlxJR3oEpvSF/QwSUsemyUVP0Bq6b90S3RDoAf6mOQVSFkkMCvOim6KRl9acp1bMsuDRfNmoQr0QS+TrBESO0kUotGrQKaUtxpMRgKp0ZuyQo5w00cCadfb4uq1hvqZP0Yp0G/dhcAXt08ajRjSrj8u/gOJywOTUk5FKRr91V5s5LTulkm1iCFV/eA7W+4aL6OiEI3emtUny8rkxo9oIO16y1ZI5cR2vL8eCaRGb8loSRycohAcK4yQvKsA5+02KABZhV47O5Nb8+Qb64zDaiVqKxZ1ybxoezM6g3QrNx7WPhsh+UGsai02Epdnu1L81ndWRf3nqtKCoaWe9nQQa+x+CN2aeD70MFly2ruVm3Dkxu7HcK1IFuQBYzwtyihfo8tyt6wJX9QYocBbLboqKM+tif5NGHKw9U63DRZcSzVpzQUlHVHkBZ03kT00zyy007AalbtQMox2i6G9okS0a9Er32aO70IsTXS+zN+1HbWcU9ErDKBFvgvByTU7Q1vy9xm2/uSiU1va7fiQ47Um+oqdVu+oF+s7K/zndR6sbUHKZc/xaN/ffN12WsJ89CN1y2xqQ3s+iMllX+QWaTct4cUMDdOS2389f8ehiEe0K30Qr3s7XERTQKwQYMbradGrZLh/ItrxVcEtUDSAFd5/CsSS0popWS0rbY6B8bvaJhdewLCrZKzt5kAhJSgC6SX/fSj5UOnlGAAht734oS9IEyj63uh/o73sAPiknW+KviAlKHLbPAfFdx6ND3YM6AtSgkLpYw9TnxNgoVM9sQA+yruBLhNgzo8O35AS1PQl15cFA4U0BFPHPmjfUwkyqDE5eTVHo4lrkM8NzJJhyh8BBgBraqka7loZJwAAAABJRU5ErkJggg=='
         style='width:20px;height:25px;'>Project Planned<br/>
         <img src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACkAAAA2CAYAAABa4LC4AAAACXBIWXMAAAsTAAALEwEAmpwYAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAABANJREFUeNrMmUtPFEEQx3uGGAlCIEZjjLw8qEdQPgD7AYzs1ZPDQa9i8O7u2Ud2j+JBOBg8rpEPsHwAFI6iB0ACIRKym1UCMcxYf9OLPT01D+axYyWdyc70dv2muqq7q8ZwHEckkeHhwVG6oI1TG1AeNaitUtvY2treSKLDiANJYEW6oBWojUT4yya1OrUaAdcyhSQ4iy6liGBBwCWCnU8VkuBgsfmEcBysRbD1xJAEWKHL46A+N7oNceu8EFfP/bu3+1uIL8dCfD0KNUKVQGdiQRIcggD+M8k9n+wzxN1+Q0z0GKKvy19B60SIlUNHLDUdsdzyBV6GjxNsIzKkBMQ0jHFws1dMl9WiCqz7cs/2g11DIHKgps948zpgL/V8PmiKF4PxACH4H/6PcXq9msekXhEKSVZE9E7pg78eMUWBrJiGYByMx7zslNTvP93UAQvyZx3w3fWuQL+LK/DXe99OxE/b8+g2TfuqnyUr+hRjerIAhGBcWJSZ+go73XItdEXyo8umuNltiCwF40OPHp+Sx2NJly/coaXl/sVsAdsCPdCnSckFKQ8JmhU7Axigb1JynVqyqAfLRE9nIaGPifaiLyR2kjyE0euCHHe91YV8IBm94ypkv276XCC9ev9ymWqot/0xT9H1g8/0djJyhjTC9+7/UUyZMCnHKSdXIEZ/w1Q38vaZL19I92/wtae7qT5YP8rHmozepuqTLmuuH+djRUbvqgrpythWfuVjSUZv3deSSJzykHorAFKvKsB5Ow0KQO2E3mzn5Oo6+UHtsdToLCSTQda4xbymvxlykE4tO8jLQyFlbeZ0KYLpFw/sjkB+bHj0bKouGJiILR5kb02MDz1M3u+7d7sedsKac/u2J2B8s0U55Rt0WeiUNeGL771WrOilFu4UVNKt+WovG2uWdmxuG6yEHtWkNcuu5YgiL+11E6vHp8NwK3rKLMppeEDuQiNZlFt8yiuI6NFIBStpTbzNjO4/cPI0pLxrc/UfK+jQK3xAa/ouBCevt5JNOwKR2V2qQWXpsPTB0s+aZXL2uAdjnBfnfngXbj1YzwQpp93So3122z7zsoT+T7f5afYrQ0dOxOS0V9V7KNafdVkCIDMD5VS+PigRj2h3lagfXmLLdl4SchHmALFMgIWo2WJUKej++Wbf4ZR7AoXps6bXn1KBlH5T4ALJDxT3GbdoRvHD2MUBmf7OcNOp70gALO+wflvU0+hUIZVz57QnML7bpylpAOB0lECJHThMIGFte6beQ4Eenz98pn/6LB89U4GUoFD6IELXJwRYiavHSOGjfBjoAgFaSQtWiUQCVAMsaCXVkdiSikUB8zYNH/QIINNqQ0PXLGoOrmmOm5ols5Q/AgwAEfp4WwZ3594AAAAASUVORK5CYII='
        style='width:20px;height:25px;'>Project Stopped"
        
        mpr %>%
          addMarkers(projects$long,projects$lat,icon=leafIcons[projects$project]) %>%
          addControl(html=html_legend,position = "bottomleft")
      }
      
      output$IndicatorHistogram_res <- renderPlot({
        
          cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
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
   
  })
  
}
