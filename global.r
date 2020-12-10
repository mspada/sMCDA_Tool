########################################################
# Libraries required by the application
########################################################
library(shiny)
library(shinydashboard)
library(shinybusy)
library(openxlsx)
library(tidyverse)
library(DT)
library(sf)
library(mapview)
library(ggplot2)
library(doParallel)
library(foreach)

###################################################
################ Global Options ###################
###################################################

# Allow shiny to open large files, now set to 100 MB
options(shiny.maxRequestSize=100*1024^2)
# Remember: ID at 1st column and Alternatives Names at the second column always!!!!

# Color palette for mapview
#colors <- reactive({brewer.pal(10, "RdYlGn")}) # Put in the global.r script

# Set common options for mapview
mapviewOptions(legend.pos = "bottomright",
               layers.control.pos = "topright",
               basemaps = c("CartoDB.Positron", "OpenStreetMap", "OpenTopoMap"),
               homebutton = FALSE,
               fgb = FALSE)

# Dashboard global header for the sMCDA tool
dbHeader <- dashboardHeader(title = "sMCDA Tool",
                            tags$li(a(href = 'http://www.psi.ch/ta',
                                      target="_blank",
                                      img(src = 'psi.png',
                                          title = "Paul Scherrer Institute",
                                          height = "20px")),
                                    style = "padding-top:0px; padding-bottom:10px;",
                                    class = "dropdown"),
                            tags$li(a(href="mailto:matteo.spada@psi.ch",
                                      target="_blank",
                                      icon("envelope"),
                                      title = "Contact Us"),
                                    class = "dropdown"),
                            tags$li(a(href = 'http://www.psi.ch',
                                      icon("power-off"),
                                      title = "Logout"),
                                    class = "dropdown"))

###################################################
######## Marginal Distributions Generator #########
###################################################

# This function sample values from 
# different type of marginal distributions
margdist <- function(input, margdist){
  
  # - input, array containing the inputs for the 
  #   samplings. These need to be assessed a priori
  # - margdist, name of the marginal distribution
  #   under interest; by now 3 marginal distributions
  #   are implemented:
  #     1) Uniform, required min and max value of the distribution 
  #     2) Normal, required the mean and the variance of the distribution
  #     3) LogNormal, required the log of both mean and sd of the distribution
  #     4) Poisson, required the lambda (mean) of the distribution
  #     5) Negative Binomial, required the size and probability parameters. The size is the target number of successfull trials, or dispersion parametet, 
  #                           while the probability is the one of success in each trial. 
  #     6) Beta, required the a and b parameters of the distribution, also sometime called shape 1 and shape 2, respectively
  
  if (margdist == "unif"){
    res <- runif(length(input[,1]),input[,1],input[,2])
  } else if (margdist == "norm"){
    res <- rnorm(length(input[,1]),input[,1],input[,2])
  } else if (margdist == "lognorm") {
    res <- rlnorm(length(input[,1]),input[,1],input[,2])  
  } else if (margdist == "pois" | margdist == "poisson") {
    res <- rpois(length(input[,1]),input[,1])  
  } else if (margdist == "ng") {
    res <- rnbinom(length(input[,1]),input[,1],input[,2])  
  } else if (margdist == "beta") {
    res <- rbeta(length(input[,1]),input[,1],input[,2])  
  }
  return(res)
}

###################################################
######## sMCDA Weighted Sum calculations ##########
###################################################

# This function calculate the sMCDA result for a 
# set of exact criteria
sMCDAallexactcritWS <- function(alt, g, inMat, pol, wgt) {
  
  # - alt, vector of alternative names
  # - g, vector of the spatial geometry of each alternative
  # - inMat, input sMCDA matrix
  # - pol, vector of the polarity of each alternative
  # - wgt, weighted scheme
  
  ###################
  # Normalization
  ###################
  
  # min-max
  norm.cri.minmax <- vector()
  for (j in 1:length(inMat[1,])){
    norm.cri.minmax <- cbind(norm.cri.minmax, normminmax(inMat[,j], pol[j]))
  }
  
  ###################
  # Weights
  ###################
  w <- weightsdist(wgt,length(inMat[1,]))
  
  ###################
  # Aggregations
  ###################
  
  # Weighted Sum
  res.minmax.ws <- weightedsum(norm.cri.minmax,w)
  
  ###################
  # Result
  ###################
  
  # Resulting data.frame.
  tmpres <-  cbind(alt,res.minmax.ws)
  # Generate the column names of the resulting matrix
  colnames(tmpres) <- c("Alternatives", "sMCDA Score")
  tmpres <- st_set_geometry(tmpres, g)
  return(tmpres)
}


# This function calculate the sMCDA result for a 
# set of uncertain criteria
sMCDAunccritWS <- function(N, nat, alt, g, inMat, pol, wgt, session) {
  
  # - N, number of Monte-Carlo runs
  # - nat, nature of the criteria, i.e. exact or defined as a distribution
  # - alt, vector of alternative names
  # - g, vector of the spatial geometry of each alternative
  # - inMat, input sMCDA matrix
  # - pol, vector of the polarity of each alternative
  # - wgt, weighted scheme
  
  # Register the number of cores to be used for parallelization purposes
  registerDoParallel(4) 
  
  # Force input variables to avoid issues in the parallel process (see https://github.com/rstudio/shiny/issues/2163)
  force(nat)
  force(alt)
  force(pol)
  force(g)
  force(inMat)
  
  # Run Monte-Carlo in parallel over numCores cores
  res <- foreach (icount(N), .combine=rbind, .multicombine=TRUE, 
                  .packages = c("tidyverse", "shiny"), 
                  .export = c("margdist","normminmax","weightsdist","weightedsum")) %dopar% { 
                    
                    #get matrix with values to be normalized (exact and random values for non-exact criteria)
                    i=1 # for input matrix
                    k=1 # for criteria and pol vectors
                    orgmat <- inMat[,1]
                    
                    while(i <= length(inMat[1,]))
                    { # Read each column of the input matrix
                      
                      if (nat[k] == "exact" | nat[k] == "pois"){ # Check the type of criteria (exact, poisson distributed or other distribution)
                        tmp <- inMat %>% select(i) # Read the values of the criteria (column) for all alternatives or to be used for marginal poisson distribution sample
                        i = i+1
                      }
                      
                      else if (nat[k] != "exact"){ # Check the type of criteria (exact, poisson distributed or other distribution)
                        tmpmd <- inMat %>% select(i, i+1) # Read the values of the criteria to be used for marginal distribution sample
                        tratmp <- margdist(tmpmd,nat[k]) # Extract value of the criteria (column) for all alternatives from the selected marginal distribution
                        tmp <- t(t(tratmp))
                        i = i+2
                      }
                      
                      orgmat <- cbind(orgmat,tmp) #matrix with initial values
                      k=k+1
                      
                    } #close while loop
                    
                    ###################
                    # normalized matrix
                    ###################
                    
                    orgmat <- orgmat[,-1]
                    
                    # min-max
                    norm.cri.minmax <- vector()
                    for (j in 1:length(orgmat[1,])){
                      norm.cri.minmax <- cbind(norm.cri.minmax, normminmax(as.numeric(as.character(orgmat[,j])), pol[j]))
                    }
                    
                    ###################
                    # Weights
                    ###################
                    w <- weightsdist(wgt,(k-1))
                    
                    ###################
                    # Aggregations
                    ###################
                    
                    # Weighted Sum
                    res.minmax.ws <- weightedsum(norm.cri.minmax,w)
                  }
  
  ###################
  # Result
  ###################
  # Estimate the mean and standard deviation results for each alternative score based on a Monte-Carlo sampling
  mean.minmax.ws <- vector()
  sd.minmax.ws <- vector()
  
  for(i in 1:dim(inMat)[1]){
    
    mean.minmax.ws[i] <- mean(res[,i])
    sd.minmax.ws[i] <- sd(res[,i])
    
  }
  
  # Resulting data.frame. Important: For some reasons the results are inverted with respect to the Alternative names that's why I had to reverse the vector names indt[,1]
  tmpres <-  cbind(alt,mean.minmax.ws,sd.minmax.ws)
  
  # Generate the column names of the resulting matrix
  colnames(tmpres) <- c("Alternatives", "Mean sMCDA Score", "SD sMCDA Score")
  tmpres <- st_set_geometry(tmpres, g)
  
  return(tmpres)
  
}

###################################################
######### sMCDA Outranking calculations ###########
###################################################

# This function calculate the sMCDA result for a 
# set of uncertain criteria
sMCDAallexactcritOut <- function(alt,g,pol, inMat, profMat,indif, pref, veto, l, wgt){
  
  # - alt, vector of alternative names
  # - g, vector of the spatial geometry of each alternative
  # - pol, vector of the polarity of each alternative
  # - inMat, input sMCDA matrix
  # - profMat, input classification profiles
  # - indif, input indifference thresholds
  # - pref, input preference thresholds
  # - veto, input veto thresholds
  # - l, input lambda
  # - wgt, weighted scheme
  
  force(alt)
  force(pol)
  force(g)
  force(inMat)
  force(profMat)
  force(indif)
  force(pref)
  force(veto)
  force(l)
  
  res.electre <- electretri(inMat,profMat,pol,wgt,indif,pref,veto, l)
  
  if (length(profMat[,1]) == 2){
    res.final <- as.numeric(res.electre[,1]*0+res.electre[,2]*0.5+res.electre[,3]*1)  
  } else if (length(profMat[,1]) == 3) {
    res.final <- as.numeric(res.electre[,1]*0+res.electre[,2]*0.33+res.electre[,3]*0.66+res.electre[,4]*1)  
  } else if (length(profMat[,1]) == 4) {
    res.final <- as.numeric(res.electre[,1]*0+res.electre[,2]*0.25+res.electre[,3]*0.5+res.electre[,4]*0.75+res.electre[,5]*1)
  } else if (length(profMat[,1]) == 5) {
    res.final <- as.numeric(res.electre[,1]*0+res.electre[,2]*0.2+res.electre[,3]*0.4+res.electre[,4]*0.6+res.electre[,5]*0.8+res.electre[,6]*1)
  } else {
    res.final <- as.numeric(res.electre)
  }
  
  ###################
  # Result
  ###################
  
  # Resulting data.frame.
  tmpres <-  cbind(alt,res.final)
  
  # Generate the column names of the resulting matrix
  colnames(tmpres) <- c("Alternatives", "sMCDA Score")
  tmpres <- st_set_geometry(tmpres, g)
  return(tmpres)
  
}

sMCDAunccritOut <- function(N,nat,alt,g,pol,inMat,profMat,indif,pref,veto,l,wgt,session){
  
  # - N, number of Monte-Carlo runs
  # - nat, nature of the criteria, i.e. exact or defined as a distribution
  # - alt, vector of alternative names
  # - g, vector of the spatial geometry of each alternative
  # - pol, vector of the polarity of each alternative
  # - inMat, input sMCDA matrix
  # - profMat, input classification profiles
  # - indif, input indifference thresholds
  # - pref, input preference thresholds
  # - veto, input veto thresholds
  # - l, input lambda
  # - wgt, weighted scheme
  
  # Register the number of cores to be used for parallelization purposes
  registerDoParallel(4) 
  
  # Force input variables to avoid issues in the parallel process (see https://github.com/rstudio/shiny/issues/2163)
  force(nat)
  force(alt)
  force(pol)
  force(g)
  force(inMat)
  force(profMat)
  force(indif)
  force(pref)
  force(veto)
  force(l)
  
  # Run Monte-Carlo in parallel over numCores cores
  res.electre <- foreach (icount(N), .combine=rbind, .multicombine=TRUE, 
                          .packages = c("tidyverse", "shiny"), 
                          .export = c("margdist","weightsdist","electretri")) %dopar% { 
                            
                            # Get matrix with values to be normalized (exact and random values for non-exact criteria)
                            i=1 # for input matrix
                            k=1 # for criteria and pol vectors
                            orgmat <- inMat[,1]
                            
                            while(i <= length(inMat[1,]))
                            { # Read each column of the input matrix
                              
                              if (nat[k] == "exact" | nat[k] == "pois"){ # Check the type of criteria (exact, poisson distributed or other distribution)
                                tmp <- inMat %>% select(i) # Read the values of the criteria (column) for all alternatives or to be used for marginal poisson distribution sample
                                i = i+1
                              }
                              
                              else if (nat[k] != "exact"){ # Check the type of criteria (exact, poisson distributed or other distribution)
                                tmpmd <- inMat %>% select(i, i+1) # Read the values of the criteria to be used for marginal distribution sample
                                tratmp <- margdist(tmpmd,nat[k]) # Extract value of the criteria (column) for all alternatives from the selected marginal distribution
                                tmp <- t(t(tratmp))
                                i = i+2
                              }
                              
                              orgmat <- cbind(orgmat,tmp) #matrix with initial values
                              k=k+1
                              
                            } #close while loop
                            
                            ###################
                            # normalized matrix
                            ###################
                            
                            orgmat <- orgmat[,-1]
                            
                            ###################
                            # Weights
                            ###################
                            w <- weightsdist(wgt,(k-1))
                            
                            ###################
                            # Results
                            ###################
                            res.electre.tri <- electretri(orgmat,profMat,pol,w,indif,pref,veto,l)
                            
                          }
  
  # Estimate Mean and Standard Deviation
  idx <- rep(1:length(inMat[,1]))
  res.electre <- cbind(idx, as.data.frame(res.electre))  
  res.electre.mean <- as.matrix(res.electre %>% group_by(idx) %>% summarise_all(mean) %>% select(-1))
  res.electre.sd <- as.matrix(res.electre %>% group_by(idx) %>% summarise_all(sd) %>% select(-1))
  
  # Calculate final score
  if (length(profMat[,1]) == 2){
    res.mean.final <- as.numeric(res.electre.mean[,1]*0+res.electre.mean[,2]*0.5+res.electre.mean[,3]*1)  
    res.sd.final <- as.numeric(res.electre.sd[,1]*0+res.electre.sd[,2]*0.5+res.electre.sd[,3]*1)
  } else if (length(profMat[,1]) == 3) {
    res.mean.final <- as.numeric(res.electre.mean[,1]*0+res.electre.mean[,2]*0.33+res.electre.mean[,3]*0.66+res.electre.mean[,4]*1) 
    res.sd.final <- as.numeric(res.electre.sd[,1]*0+res.electre.sd[,2]*0.33+res.electre.sd[,3]*0.66+res.electre.sd[,4]*1)
  } else if (length(profMat[,1]) == 4) {
    res.mean.final <- as.numeric(res.electre.mean[,1]*0+res.electre.mean[,2]*0.25+res.electre.mean[,3]*0.5+res.electre.mean[,4]*0.75+res.electre.mean[,5]*1)
    res.sd.final <- as.numeric(res.electre.sd[,1]*0+res.electre.sd[,2]*0.25+res.electre.sd[,3]*0.5+res.electre.sd[,4]*0.75+res.electre.sd[,5]*1)
  } else if (length(profMat[,1]) == 5) {
    res.mean.final <- as.numeric(res.electre.mean[,1]*0+res.electre.mean[,2]*0.2+res.electre.mean[,3]*0.4+res.electre.mean[,4]*0.6+res.electre.mean[,5]*0.8+res.electre.mean[,6]*1)
    res.sd.final <- as.numeric(res.electre.sd[,1]*0+res.electre.sd[,2]*0.2+res.electre.sd[,3]*0.4+res.electre.sd[,4]*0.6+res.electre.sd[,5]*0.8+res.electre.sd[,6]*1)
  } else {
    res.mean.final <- as.numeric(res.electre.mean)
    res.sd.final <- as.numeric(res.electre.sd)
  }
  
  ###################
  # Result
  ###################
  
  # Resulting data.frame.
  tmpres <-  cbind(alt,res.mean.final,res.sd.final)
  
  # Generate the column names of the resulting matrix
  colnames(tmpres) <- c("Alternatives", "Mean sMCDA Score", "SD sMCDA Score")
  tmpres <- st_set_geometry(tmpres, g)
  return(tmpres)
  
}

###################################################
###################  Weights ######################
###################################################

# This function check for the presence of the
# indicator weights vector. If no input are given,
# then the weights are sample between 0 and 1 at
# each Monte-Carlo run in the main script.
# In addition, the script checks if the sum of the 
# weights do not overcome 1, if so it will correct this

weightsdist <- function(criteriaWeights,n){
  
  # - criteriaWeights, array containing the input
  # weights, if not given criteriaWeights=NULL
  # - n, number of indicators
  if (is.null(criteriaWeights)){
    vp <- sample(seq(0,1,0.01),n) # Sampling the weights-->random numbers
  } else {
    vp <- criteriaWeights # Using the weights given as input
  }
  if (sum(vp) > 1 | sum(vp) < 1) { # Check if the sum of the weights = 1, if not correct
    vp <- vp/sum(vp) 
  }
  return(vp)
  
}

###################################################
#################  Normalization ##################
###################################################

# This function normalize each indicator/criteria using
# a min-max method
normminmax <- function(input, polarity){
  
  # - input, array containing the inputs for the 
  #   normalization
  # - polarity, direction of the indicator, i.e. 
  #   "+" means the higher the indicator value is the better, 
  #   "-" the lower is, the better
  
  if (polarity == "+"){
    res <- (input-min(input))/(max(input)-min(input))
  } else if (polarity == "-") {
    res <- (max(input)-input)/(max(input)-min(input))
  }
  return(res)
}

###################################################
#################  Aggregation ####################
###################################################

# This function aggregate the weighted indicators
# by using a weighted sum
weightedsum <- function(inmat, weights){
  # - inmat, array containing the normalized
  #   indicator matrix
  # - weights, vector containing the weights
  
  res <- lapply(1:length(inmat[,1]), function (i) sum(weights[]*inmat[i,]))
  return(unlist(res))
  
}

###################################################
#################  Electre-Tri ####################
###################################################

electretri <- function (performanceMatrix, profiles,minmaxcriteria, criteriaWeights, IndifferenceThresholds, 
                        PreferenceThresholds, VetoThresholds, lambda_range) 
{
  if (lambda_range[1] == lambda_range[2]) {
    lambda <- lambda_range[1]
  } else {
    lambda <- sample(seq(lambda_range[1],lambda_range[2],0.01),1)
  }
  
  pm = performanceMatrix
  nrow_pm = nrow(pm)
  pr <- profiles
  nrow_pr = nrow(pr)
  length_cr = ncol(pm)
  if (is.null(criteriaWeights)){
    vp <- sample(seq(0,1,0.01),length_cr)
  } else {
    vp <- criteriaWeights
  }
  if (sum(vp) > 1) {
    vp <- vp/sum(vp)
  }
  mmv <- minmaxcriteria
  p_t = PreferenceThresholds
  q_t = IndifferenceThresholds
  v_t = VetoThresholds
  partialConcordance_al_pr_gj <- matrix(rep(0, nrow_pm * length_cr), 
                                        nrow_pm, length_cr)
  partialConcordance_pr_al_gj <- matrix(rep(0, nrow_pm * length_cr), 
                                        nrow_pm, length_cr)
  partialDiscordance_al_pr_gj <- matrix(rep(0, nrow_pm * length_cr), 
                                        nrow_pm, length_cr)
  partialDiscordance_pr_al_gj <- matrix(rep(0, nrow_pm * length_cr), 
                                        nrow_pm, length_cr)
  globalconcordance <- matrix(rep(0, nrow_pm * nrow_pr * 2), 
                              nrow_pm, nrow_pr * 2)
  credibility <- matrix(rep(0, nrow_pm * nrow_pr * 2), nrow_pm, 
                        nrow_pr * 2)
  Optimistic <- matrix(rep(0, nrow_pm * (nrow_pr + 1)), nrow_pm,
                       nrow_pr + 1)
  credibility_al_pr <- matrix(rep(0, nrow_pm %*% nrow_pr), 
                              nrow_pm, nrow_pr)
  credibility_pr_al <- matrix(rep(0, nrow_pr * nrow_pm), nrow_pr, 
                              nrow_pm)
  h = 0
  k = 1
  while (k <= nrow_pr) {
    for (i in 1:nrow_pm) {
      for (j in 1:length_cr) {
        if (mmv[j] == "+") {
          
          c_a_b = (pm[i, j] - as.numeric(pr[k, j]) + p_t[j])/(p_t[j] - q_t[j])
        }
        if (mmv[j] == "-") {
          c_a_b = (as.numeric(pr[k, j]) - pm[i, j] + p_t[j])/(p_t[j] - q_t[j])
        }
        if (c_a_b < 0) {
          c_a_b <- 0
        }
        if (c_a_b < 0) {
          partialConcordance_al_pr_gj[i, j] = 0
        }
        if (c_a_b >= 1) {
          partialConcordance_al_pr_gj[i, j] = 1
        }
        else {
          partialConcordance_al_pr_gj[i, j] = c_a_b
        }
      }
    }
    h = h + 1
    globalconcordance[, h] = partialConcordance_al_pr_gj %*% vp
    for (i in 1:nrow_pm) {
      for (j in 1:length_cr) {
        if (mmv[j] == "+") {
          c_a_b = (as.numeric(pr[k, j]) - pm[i, j] + p_t[j])/(p_t[j] - q_t[j])
        }
        if (mmv[j] == "-") {
          c_a_b = (pm[i, j] - as.numeric(pr[k, j]) + p_t[j])/(p_t[j] - q_t[j])
        }
        if (c_a_b < 0) {
          c_a_b <- 0
        }
        if (c_a_b < 0) {
          partialConcordance_pr_al_gj[i, j] = 0
        }
        if (c_a_b >= 1) {
          partialConcordance_pr_al_gj[i, j] = 1
        }
        else {
          partialConcordance_pr_al_gj[i, j] = c_a_b
        }
      }
    }
    h = h + 1
    globalconcordance[, h] = partialConcordance_pr_al_gj %*% vp
    k = k + 1
  }
  
  l = 0
  k = 1
  while (k <= nrow_pr) {
    for (i in 1:nrow_pm) {
      for (j in 1:length_cr) {
        if ((mmv[j] == "+")) {
          d_a_b = (as.numeric(pr[k, j]) - pm[i, j] - p_t[j])/(v_t[j] - p_t[j])
          
        }
        if ((mmv[j] == "-")) {
          d_a_b = (pm[i, j] - as.numeric(pr[k, j]) - p_t[j])/(v_t[j] - p_t[j])
        }
        if (d_a_b < 0) {
          d_a_b <- 0
        }
        if (d_a_b < 0) {
          partialDiscordance_al_pr_gj[i, j] = 0
        }
        if (d_a_b >= 1) {
          partialDiscordance_al_pr_gj[i, j] = 1
        }
        else {
          partialDiscordance_al_pr_gj[i, j] = d_a_b
        }
      }
    }
    l = l + 1
    for (h in 1:nrow_pm) {
      if (max(partialDiscordance_al_pr_gj[h, ] == 1)) {
        credibility[h, l] = 0
      }
      else if (max(partialDiscordance_al_pr_gj[h, ]) < 
               globalconcordance[h, l]) {
        credibility[h, l] = globalconcordance[h, l]
      }
      else if (max(partialDiscordance_al_pr_gj[h, ]) > 
               globalconcordance[h, l]) {
        credibility[h, l] = ((1 - max(partialDiscordance_al_pr_gj[h,]))/(1 - globalconcordance[h, l])) * globalconcordance[h,l]
      }
    }
    
    for (i in 1:nrow_pm) {
      for (j in 1:length_cr) {
        if ((mmv[j] == "+")) {
          d_b_a = (pm[i, j] - as.numeric(pr[k, j]) - p_t[j])/(v_t[j] - p_t[j])
        }
        if ((mmv[j] == "-")) {
          d_b_a = (as.numeric(pr[k, j]) - pm[i, j] - p_t[j])/(v_t[j] - p_t[j])
        }
        
        if (d_b_a < 0) {
          d_b_a <- 0
        }
        if (d_b_a < 0) {
          partialDiscordance_pr_al_gj[i, j] = 0
        }
        if (d_b_a >= 1) {
          partialDiscordance_pr_al_gj[i, j] = 1
        }
        else {
          partialDiscordance_pr_al_gj[i, j] = d_b_a
        }
      }
    }
    l = l + 1
    for (h in 1:nrow_pm) {
      if (max(partialDiscordance_pr_al_gj[h, ] == 1)) {
        credibility[h, l] = 0
      }
      else if (max(partialDiscordance_pr_al_gj[h, ]) < globalconcordance[h, l]) {
        credibility[h, l] = globalconcordance[h, l]
      }
      else if (max(partialDiscordance_pr_al_gj[h, ]) > globalconcordance[h, l]) {
        credibility[h, l] = ((1 - max(partialDiscordance_pr_al_gj[h, ]))/(1 - globalconcordance[h, l])) * globalconcordance[h,l]
      }
    }
    
    k = k + 1
  }
  
  for (i in 1:nrow_pm) {
    ok = 0
    for (j in 1:nrow_pr) {
      if ((credibility[i, 2 * j - 1] < lambda) && (credibility[i, 2 * j] >= lambda)) {
        if (ok == 0) {
          Optimistic[i, j] = 1
          ok = 1
        }
      }
    }
    if (ok == 0) {
      Optimistic[i, j + 1] = 1
    }
  }
  
  return(Optimistic)
}

#####################################
## Modal Download Button Functions ##
#####################################

# Function to download WS resulting map
myDownloadImagesWSRes <- function() {
  div(id = "downloadImageWSRes",
      modalDialog(downloadButton("download_ws_png","Download png"),
                  br(),
                  br(),
                  downloadButton("download_ws_jpg","Download jpg"),
                  br(),
                  br(),
                  downloadButton("download_ws_csv","Download csv"),
                  br(),
                  br(),
                  downloadButton("download_ws_xlsx","Download xlsx"),
                  br(),
                  br(),
                  downloadButton("download_ws_shp","Download shapefile"),
                  easyClose = TRUE, title = "Download Image")
  )
}

# Function to download Out resulting map
myDownloadImagesOutRes <- function() {
  div(id = "downloadImageOutRes",
      modalDialog(downloadButton("download_out_png","Download png"),
                  br(),
                  br(),
                  downloadButton("download_out_jpg","Download jpg"),
                  br(),
                  br(),
                  downloadButton("download_out_csv","Download csv"),
                  br(),
                  br(),
                  downloadButton("download_out_xlsx","Download xlsx"),
                  br(),
                  br(),
                  downloadButton("download_out_shp","Download shapefile"),
                  easyClose = TRUE, title = "Download Image")
  )
}