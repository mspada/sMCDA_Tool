library(doParallel)
library(foreach)
source("Test Electre-Tri/electre-tri.R")

numCores <- 4
registerDoParallel(numCores)

# perfMat <- read.csv("Test Electre-Tri/Test_10Ind.csv",sep=";")
# perfMat <- perfMat[,-1]
# perfMat <- cbind(shp$dbf$ClimChD,shp$dbf$HumToxD,shp$dbf$PartMatFD,shp$dbf$WatDepD,shp$dbf$MetDepD,shp$dbf$AvgGenCD,
#             shp$dbf$Tax,shp$dbf$SevAccD,shp$dbf$SeismicRis,shp$dbf$IndSeiD,shp$dbf$EINWOHNERZ,shp$dbf$ParkInd)

minmaxcriteria <- replicate(12,"min")
profiles <- cbind(c(1.5E-1,3E-2),c(1E-1,2E-2),c(3E-4,1E-4),c(3E-3,1E-3),c(3E-2,1E-2),c(50,25),
                  c(21,16),c(6E-2,1E-2),c(2.01,1.01),c(70,50),c(100000,50000),c(2.01,1.01))
IndifferenceThresholds <- c(5.00E-03,5.00E-03,0,0,0,1,0.1,5.00E-03,0,2,250,0)
PreferenceThresholds <- c(1.00E-02,1.00E-02,5E-5,5E-4,5.00E-03,5,0.5,1.00E-02,0,5,500,0)
VetoThresholds <- c(1e6,1e6,1e6,1e6,1e6,1e6,1e6,1e6,1e6,1e6,1E6,1e6) # default
criteriaWeights <- NULL # No Weights
criteriaWeights <- c(0.067,0.067,0.067,0.067,0.067,0.17,0.17,0.067,0.067,0.067,0.067,0.067) # Equal Weight
criteriaWeights <- c(0.16,0.16,0.16,0.16,0.16,0.05,0.05,0.02,0.02,0.02,0.02,0.02) # Env Centered
criteriaWeights <- c(0.02,0.02,0.02,0.02,0.02,0.4,0.4,0.02,0.02,0.02,0.02,0.02) # Eco Centered
criteriaWeights <- c(0.02,0.02,0.02,0.02,0.02,0.05,0.05,0.16,0.16,0.16,0.16,0.16) # Social Centered

lambda <- seq(0.51,0.85,0.01)

res <- numeric(0)
N = 1000

shp <- shapefiles::read.dbf("Test Electre-Tri/Data/InputData.dbf")
res <- foreach (icount(N), .combine=rbind) %dopar% {
  
  ClimCh <- runif(length(shp$dbf$ClimChD),shp$dbf$ClimChT,shp$dbf$ClimChD)
  HumTox <- runif(length(shp$dbf$HumToxD),shp$dbf$HumToxT,shp$dbf$HumToxD)
  PartMatF <- runif(length(shp$dbf$PartMatFD),shp$dbf$PartMatFT,shp$dbf$PartMatFD)
  WatDep <- runif(length(shp$dbf$WatDepD),shp$dbf$WatDepT,shp$dbf$WatDepD)
  MetDep <- runif(length(shp$dbf$MetDepD),shp$dbf$MetDepT,shp$dbf$MetDepD)
  AvgGenC <- runif(length(shp$dbf$AvgGenCD),shp$dbf$AvgGenCT,shp$dbf$AvgGenCD)
  SevAcc <- runif(length(shp$dbf$SevAccD),shp$dbf$SevAccT,shp$dbf$SevAccD)
  IndSei <- runif(length(shp$dbf$IndSeiD),shp$dbf$IndSeiD,shp$dbf$IndSeiT)
  perfMat <- cbind(ClimCh,HumTox,PartMatF,WatDep,MetDep,AvgGenC,shp$dbf$Tax,
                   SevAcc,shp$dbf$SeismicRis,IndSei,shp$dbf$EINWOHNERZ,shp$dbf$ParkInd)
  electretri(perfMat,profiles,minmaxcriteria,criteriaWeights,IndifferenceThresholds,PreferenceThresholds,VetoThresholds, lambda = sample(lambda,1))
}

shpres <- shapefiles::read.dbf("Test Electre-Tri/Results/Shapefile/Results_MarginalDist.dbf")
tmp <- rowsum(res, rep(1:length(shp$dbf$ClimChD), times=N))/(N/100)
shpres$dbf$Result_SocW_H <- as.numeric(tmp[,1]*0+tmp[,2]*0.5+tmp[,3]*1)
shapefiles::write.dbf(shpres,"Test Electre-Tri/Results/Shapefile/Results_MarginalDist")

##############################################################
######## Create Shapefile containing all input data #########
##############################################################
library(shapefiles)
shp <- shapefiles::read.dbf("Test Electre-Tri/InputData2.dbf")

ParkInd <- ifelse(shp$dbf$Kategorie=="RN",1,0)
ParkInd <- ifelse(ParkInd==0,2,1)
ParkInd[is.na(ParkInd)] <- 0
shp$dbf$ParkInd <- ParkInd

library(openxlsx)
my_data <- read.xlsx("Test Electre-Tri/Tax.xlsx")
tax <- numeric(0)
for (i in 1: length(shp$dbf$NAME_3)){
  for (j in 1: length(my_data$Kanton)){
    if(as.character(shp$dbf$NAME_3)[i] == my_data$Kanton[j]){
      tax[i] <- my_data$Tax[j]
    }else{}
  }
}
shp$dbf$Tax <- tax

my_data <- read.csv("Test Electre-Tri/Doublet.csv",header=FALSE)
ClimCh <- numeric(0)
HumTox <- numeric(0)
PartMatForm <- numeric(0)
WatDep <- numeric(0)
MetDep <-numeric(0)
AvgGenCost <- numeric(0)
SevAcc <- numeric(0)
for (i in 1:length(shp$dbf$CONTOUR)){ 
  for (j in 2:length(my_data[1,])){
    if(shp$dbf$CONTOUR[i]==my_data[1,j]){
      ClimCh[i] <- my_data[2,j]
      HumTox[i] <- my_data[3,j]
      PartMatForm[i] <- my_data[4,j]
      WatDep[i] <- my_data[5,j]
      MetDep[i] <- my_data[6,j]
      AvgGenCost[i] <- my_data[7,j]
      SevAcc[i] <- my_data[8,j]}
    else{}
  }
}
shp$dbf$ClimChD <- ClimCh
shp$dbf$HumToxD <- HumTox
shp$dbf$PartMatFD <- PartMatForm
shp$dbf$WatDepD <- WatDep
shp$dbf$MetDepD <- MetDep
shp$dbf$AvgGenCD <- AvgGenCost
shp$dbf$SevAccD <- SevAcc
shp$dbf$IndSeiD <- rep(3,length(shp$dbf$NAME_3))

# For Triplet use the loop above, but considering the followings
# instead of the Doublet
# my_data <- read.csv("Test Electre-Tri/Triplet.csv",header=FALSE)
# shp$dbf$ClimChT <- ClimCh
# shp$dbf$HumToxT <- HumTox
# shp$dbf$PartMatFT <- PartMatForm
# shp$dbf$WatDepT <- WatDep
# shp$dbf$MetDepT <- MetDep
# shp$dbf$AvgGenCT <- AvgGenCost
# shp$dbf$SevAccT <- SevAcc
# shp$dbf$IndSeiT <- rep(1,length(shp$dbf$NAME_3))

shapefiles::write.dbf(shp,"Test Electre-Tri/InputData2")