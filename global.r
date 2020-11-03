########################################################
# Libraries required by the application
########################################################
library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(leaflet)
library(mapview)
library(ggplot2)

########################################################
# Global Options
########################################################

# Shiny option to increase the maximum uploadable file size, now 100 MB
options(shiny.maxRequestSize=100*1024^2) 

# Mapview options 
mapviewOptions(legend.pos = "bottomright",
               layers.control.pos = "topleft",
               basemaps = c("CartoDB.Positron", "OpenStreetMap", "OpenTopoMap"),
               homebutton = FALSE)

########################################################
# Libraries required by the application
########################################################


calc_ws <- function(ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8){
  
  weightsum <- sum(ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8)
}

# Create color palette for the indicators and sMCDA results mapping
colfunc <- colorRampPalette(c("green", "red"))

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


electretri <- function (performanceMatrix, profiles,minmaxcriteria, criteriaWeights, IndifferenceThresholds, 
                        PreferenceThresholds, VetoThresholds, lambda) 
{
  
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
        if (mmv[j] == "max") {
          c_a_b = (pm[i, j] - pr[k, j] + p_t[j])/(p_t[j] - 
                                                    q_t[j])
        }
        if (mmv[j] == "min") {
          c_a_b = (pr[k, j] - pm[i, j] + p_t[j])/(p_t[j] - 
                                                    q_t[j])
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
    globalconcordance[, h] = partialConcordance_al_pr_gj %*% 
      vp
    for (i in 1:nrow_pm) {
      for (j in 1:length_cr) {
        if (mmv[j] == "max") {
          c_a_b = (pr[k, j] - pm[i, j] + p_t[j])/(p_t[j] - 
                                                    q_t[j])
        }
        if (mmv[j] == "min") {
          c_a_b = (pm[i, j] - pr[k, j] + p_t[j])/(p_t[j] - 
                                                    q_t[j])
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
    globalconcordance[, h] = partialConcordance_pr_al_gj %*% 
      vp
    k = k + 1
  }
  
  l = 0
  k = 1
  while (k <= nrow_pr) {
    for (i in 1:nrow_pm) {
      for (j in 1:length_cr) {
        if ((mmv[j] == "max")) {
          d_a_b = (pr[k, j] - pm[i, j] - p_t[j])/(v_t[j] -
                                                    p_t[j])
          
        }
        if ((mmv[j] == "min")) {
          d_a_b = (pm[i, j] - pr[k, j] - p_t[j])/(v_t[j] -
                                                    p_t[j])
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
        credibility[h, l] = ((1 - max(partialDiscordance_al_pr_gj[h, 
        ]))/(1 - globalconcordance[h, l])) * globalconcordance[h, 
                                                               l]
      }
    }
    
    for (i in 1:nrow_pm) {
      for (j in 1:length_cr) {
        if ((mmv[j] == "max")) {
          d_b_a = (pm[i, j] - pr[k, j] - p_t[j])/(v_t[j] -
                                                    p_t[j])
        }
        if ((mmv[j] == "min")) {
          d_b_a = (pr[k, j] - pm[i, j] - p_t[j])/(v_t[j] -
                                                    p_t[j])
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
      else if (max(partialDiscordance_pr_al_gj[h, ]) < 
               globalconcordance[h, l]) {
        credibility[h, l] = globalconcordance[h, l]
      }
      else if (max(partialDiscordance_pr_al_gj[h, ]) > 
               globalconcordance[h, l]) {
        credibility[h, l] = ((1 - max(partialDiscordance_pr_al_gj[h, 
        ]))/(1 - globalconcordance[h, l])) * globalconcordance[h, 
                                                               l]
      }
    }
    
    k = k + 1
  }
  
  for (i in 1:nrow_pm) {
    ok = 0
    for (j in 1:nrow_pr) {
      if ((credibility[i, 2 * j - 1] < lambda) && (credibility[i, 
                                                               2 * j] >= lambda)) {
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
