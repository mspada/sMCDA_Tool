########################################################
# Libraries required by the application
########################################################
library(plyr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(maptools)
library(RColorBrewer)
library(rgeos)
library(shiny)
library(shinydashboard)

########################################################
# Function used for the calculation
########################################################
# Polygonize each polygon of the input data in order
# to be usable by leaflet for plotting purposes
polyfunc<-function(groupname, dat){
  poly<-filter(dat, id==groupname) %>% select(long, lat)
  return(Polygons(list(Polygon(poly)), groupname))
}

# To better plot the ID number on the map
calc.labpt = function(pols) {
  # Prepopulate the label point matrix with the centroid values
  coords=coordinates(pols)
  
  # For each polygon in pols, calculate the appropriate label point
  for(i in seq_len(length(pols))) {
    
    # First fetch the polygon to process
    p=pols[i,]
    
    init=0                     # Initial amount to shrink
    estep=sqrt(gArea(p)/pi)/10 # Additional amount to shrink for each step
    
    # Try repeatedly shrinking the polygon until we’re left
    # with a polygon whose convex hulls fits inside 
    repeat {
      repeat {
        r = init + estep               # Amount to shrink
        p2 = gBuffer(p, width = -r)    # Shrink the polygon
        if( gArea(p2) <= 0 )           # If the shrunken polygon is empty ...
          estep = estep/2 else break   # ... try again with a smaller value
      }
      
      # If we’re left with more than one polygon, choose the largest one
      areas=sapply(p2 @ polygons[[1]]@Polygons, function(x) x @ area)
      if(length(areas) > 1) {
        # Note that we create a *new* SpatialPolygon containing the largest polygon.
        # I guess in theory we *could* have just replaced the @Polygons slot of p2,
        # but then gArea seems to crash R ... :(
        ind.max = which.max(areas)
        p2 = SpatialPolygons(list(Polygons(list(p2 @ polygons[[1]]@Polygons[ind.max][[1]]),
                                           ID="middle")), proj4string=CRS(proj4string(p2)))
      }
      
      # Calculate the convex hull of the inner polygon.
      # If this is wholly contained in the original polygon,
      # break out of the loop and set the label point to
      # the centroid of the inner polygon.
      if( gContains(p, gConvexHull(p2)) ) break else init=init+estep
    }
    coords[i,] = coordinates(p2)
  }
  coords
}

calc_ws <- function(ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8){
  
  weightsum <- sum(ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8)
}

# Create color palette for the indicators and sMCDA results mapping
colfunc <- colorRampPalette(c("green", "red"))

########################################################
# Input and preparation of data for plot and analysis
########################################################
# Define the coordinate system for the input shp files
crs=CRS("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 
        +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.4,15.1,405.3,0,0,0,0 
        +units=m +no_defs")

# Input data from shp file
input <- readShapeSpatial("../Outranking Methods/Results/V3.0/Results.shp",proj4string=crs,verbose=TRUE)

# Add lat/lon coordinates to the input data 
input_latlon <- spTransform(input, CRS("+init=epsg:4326"))
input_latlon@data$id = rownames(input_latlon@data)
tmp.points <- fortify(input_latlon, region="id")
sMCDA_in_tmp <- join(tmp.points, input_latlon@data, by="id")

# Remove unuseful column
sMCDA_in <- subset(sMCDA_in_tmp, select = -c(ID))

# Rename the Heat Flux CONTOUR attribute
names(sMCDA_in)[names(sMCDA_in)=="CONTOUR"] <- "HeatFlux"

#
# Polygonize the data.frame of the inputs for the sMCDA 
# in order to plot them using leaflet
#
# Select attributes of interest for plotting and analisys 
# purposes
sel_df <- distinct(sMCDA_in, id, hole, 
                   HeatFlux, ClimateCh, HumanTox, 
                   PartMatFor, WaterDep, MetalDep, AveGenCost,
                   AccRisk, NatSeiRisk, DoubEqu, DoubEnv, DoubEco, DoubSoc,
                   TripEqu, TripEnv, TripEco, TripSoc, noW, EqualW, EnvW, 
                   EcoW, SocW)
sel_df_name <- sel_df$id # extract id only for polygonize purpose
# For each id extract the polygon to be spatialized for plotting
# purposes
poly_new <-lapply(sel_df_name, function(x) polyfunc(x, dat=sMCDA_in))
# Spatialized the polygons
sp.poly_new <- SpatialPolygons(poly_new)
# Create a new spatialized polygon for plotting purposes 
df.poly_new <- SpatialPolygonsDataFrame(sp.poly_new, 
                                        data=data.frame(row.names=sel_df_name, sel_df))

# Extract polygon centers for plotting purposes
poly_centers <- as.data.frame(calc.labpt(df.poly_new))#data.frame(gCentroid(df.poly_new, byid=TRUE))
colnames(poly_centers) <- c("long", "lat")
poly_centers$id <- rownames(poly_centers)

# Create color palette for the indicator and results plot
pal <- colorBin(
  palette = colfunc(10),
  domain = df.poly_new$ClimateCh,
  bins=10,
  na.color = c("white")
)

# Create color palette for the heat flux plot
pal_hf <- colorBin(
  palette = brewer.pal(7,"Spectral"),
  domain = df.poly_new$HeatFlux,
  bins=7,
  na.color = c("white")
)

# Create color palette for the sMCDA plot
pal_sM <- colorBin(
  palette = brewer.pal(5,"RdYlGn"),
  domain = df.poly_new$DoubEqu,
  bins=4,
  na.color = c("white")
)

projects <- read.csv("data/Projects/geothermal_ch_projects.csv")

MCDAalg <- c("Choose an algorithm" = "", "Weighted Sum" = "ws")

layers <- c("No Layer" = "","Heat Flux" = "HeatFlux", "Climate Change" = "ClimateCh","Human Toxicity" = "HumanTox",
            "Particulate Matter Formation" = "PartMatFor", "Water Depletion" = "WaterDep",
            "Metal Depletion" = "MetalDep","Average Generation Costs" = "AveGenCost", 
            "Accident Risk" = "AccRisk","Natural Seismic Risk" = "NatSeiRisk", "Doublet Equal" = "DoubEqu",
            "Doublet Env" = "DoubEnv", "Doublet Eco" = "DoubEco", "Doublet Soc" = "DoubSoc", 
            "Triplet Equal" = "TripEqu", "Triplet Env" = "TripEnv", "Triplet Eco" = "TripEco", 
            "Triplet Soc" = "TripSoc", "No Weights" = "noW", "Equal Weights" = "EqualW", "Env Driven" = "EnvW",
            "Eco Driven" = "EcoW", "Social Driven" = "SocW")
