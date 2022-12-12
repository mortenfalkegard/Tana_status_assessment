# Complete stock status assessment script

library(triangle)
#library(openxlsx)
library(tidyverse)
library(rio)

estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

#==============================
# global variable declarations
#==============================

k <- 10000

MakeAdjustment <- FALSE
IncludeUnreported <- FALSE

Spawn_W_med <- vector()
Spawn_W_min <- vector()
Spawn_W_max <- vector()
Spawn_W_mod <- vector()

GBM_maal <- vector()
GBM_prob <- vector()
GBM_4yr_maal <- vector()
GBM_4yr_prob <- vector()
Spawn4yrMin <- vector()
Spawn4yrMed <- vector()
Spawn4yrMax <- vector()
Expl_L <- vector()
Expl_M <- vector()
Expl_H <- vector()
ExplAdj <- vector()
CatchForFile <- vector()
OrigCatchForFile <- vector()
FemPropForFile <- vector()
DiffPercent <- vector()
UnrepForFile <- vector()

RiverNumber <- vector()
RiverList <- character()
RiverYear <- vector()

l <- 0 # this is a line counter for the results data frame

#=====================
# initiate river list
#=====================

river_list <- import("data/gbm-eval-riverlist.csv", encoding = "UTF-8")

NumberOfRivers <- nrow(river_list)

#=====================
# loop through rivers
#=====================

for(r in 1:NumberOfRivers) {

  StdizedTrib <- vector()
  StdizedTot <- vector()
  StdizedPfa <- vector()
  ExplStart <- vector()
  ExplMedEst <- vector()
  FemPropMedEst <- vector()
  VideoKgTot <- vector()
  CatchTrib <- vector()
  PfaTrib <- vector()
  UnrepEst <- vector()

  GBM_lo <- river_list[r, "GBM_low"]
  GBM_hi <- river_list[r, "GBM_high"]
  GBM_m <- river_list[r, "GBM_med"]
  GBM_dist <- rtriangle(k, GBM_lo, GBM_hi, GBM_m)
  
  #====================================
  # Select status assessment procedure
  #====================================
  if(river_list[r, "AssessProc"] == 0) { 
    
    RiverFileName <- paste("data/rivers/gbm-nosizeclass-", river_list[r, "RiverName"], ".csv", sep = "")
    river_data <- import(RiverFileName, encoding = "UTF-8")
  
    NumberOfYears <- nrow(river_data)

    if("VideoCount1SW" %in% colnames(river_data)) { #trib with video monitoring, i.e. Utsjoki
      
      for(i in 1:NumberOfYears) {

        VideoKgTot[i] <- river_data[i, "VideoCount1SW"] * river_data[i, "AvgSize1SW"] +
          river_data[i, "VideoCountMSW"] * river_data[i, "AvgSizeMSW"]
        FemPropMedEst[i] <- (river_data[i, "VideoCount1SW"] * river_data[i, "AvgSize1SW"] * river_data[i, "FemProp1SW"] +
                               river_data[i, "VideoCountMSW"] * river_data[i, "AvgSizeMSW"] * river_data[i, "FemPropMSW"]) / VideoKgTot[i]
        CatchTrib[i] <- river_data[i, "Catch"]
        ExplStart[i] <- (river_data[i, "Catch"] / VideoKgTot[i]) #* 1.5 # multiply here to temporarily compensate for underreporting in Utsjoki
        FemPropForFile[i + l] <- FemPropMedEst[i]
        
        if(river_data[i, "AssessMethod"] == 1) { # 1 means normal year of fishing, evaluate with catch and expl rate
          
          PfaTrib[i] <- CatchTrib[i] / ExplStart[i]
          
        } else {
          
          PfaTrib[i] <- VideoKgTot[i]
          
        }
          
      }
      
    } else { # area without video, e.g. Tana main stem and Vetsi

      for(i in 1:NumberOfYears) {
        
        CatchTrib[i] <- river_data[i, "Catch"]
        ExplStart[i] <- river_data[i, "ExplStart"]
                
        if(river_data[i, "AssessMethod"] == 1) { # 1 means normal year of fishing, evaluate with catch and expl rate
          
          FemPropMedEst[i] <- river_data[i, "FemProp"]
          PfaTrib[i] <- CatchTrib[i] / ExplStart[i]
          
        } else {

          FemPropMedEst[i] <- (river_data[i, "FemProp_G"] * river_data[i, "Count_G"] * river_data[i, "Size_G"] +
              river_data[i, "FemProp_M"] * river_data[i, "Count_M"] * river_data[i, "Size_M"] +
              river_data[i, "FemProp_S"] * river_data[i, "Count_S"] * river_data[i, "Size_S"]) / 
            (river_data[i, "Count_G"] * river_data[i, "Size_G"] + river_data[i, "Count_M"] * river_data[i, "Size_M"] +
              river_data[i, "Count_S"] * river_data[i, "Size_S"])
          
          PfaTrib[i] <- river_data[i, "Count_G"] * river_data[i, "Size_G"] + 
            river_data[i, "Count_M"] * river_data[i, "Size_M"] +
            river_data[i, "Count_S"] * river_data[i, "Size_S"]
          
        }
        
        FemPropForFile[i + l] <- FemPropMedEst[i]
      
      }  
    }

    # if TRUE, make exploitation adjustment based on correlation between trib and Tana total
    if(isTRUE(MakeAdjustment)) { 
      
      AvgTrib <- mean(river_data[1:NumberOfYears, "Catch"])
      AvgTot <- mean(river_data[1:NumberOfYears, "TanaTotal"])
      StdTrib <- sd(river_data[1:NumberOfYears, "Catch"])
      StdTot <- sd(river_data[1:NumberOfYears, "TanaTotal"])
      
      AvgPfa <- mean(PfaTrib)
      StdPfa <- sd(PfaTrib)
      
      for(i in 1:NumberOfYears) {
        
        StdizedTrib[i] <- (CatchTrib[i] - AvgTrib) / StdTrib
        StdizedTot[i] <- (river_data[i, "TanaTotal"] - AvgTot) / StdTot
        StdizedPfa[i] <- (PfaTrib[i] - AvgPfa) / StdPfa
        
      }
      
      StdizedDiff <- StdizedTot - StdizedPfa
      DiffKG <- StdizedDiff * StdPfa
      
      for(i in 1:NumberOfYears) {
        
        a <- (river_data[i, "UnrepMax"] - river_data[i, "UnrepMin"]) / (max(StdizedDiff) - min(StdizedDiff))
        b <- river_data[i, "UnrepMax"] - (a * max(StdizedDiff))
        UnrepEst[i] <- river_data[i, "UnrepMax"] - (a * StdizedDiff[i] + b)
        
      }
      
      PfaTribCorrected <- PfaTrib + DiffKG
      ExplEstimate <- CatchTrib / PfaTribCorrected
      ExplCorr <- (ExplEstimate - ExplStart) * river_data[i, "ExplCorrSoft"]
      ExplMedEst <- ExplStart + ExplCorr
      
    } else { # no adjustment, use static numbers directly from Excel-files
      
      ExplMedEst <- ExplStart
      UnrepEst <- river_data[ , "UnrepMed"]
      ExplCorr <- ExplMedEst - ExplStart
      
    }
    
    for(i in 1:NumberOfYears) {

      fempropL <- FemPropMedEst[i] * river_data[i, "FemPropMin"]
      fempropM <- FemPropMedEst[i]
      fempropH <- FemPropMedEst[i] * river_data[i, "FemPropMax"]
      FemProp <- rtriangle(k, fempropL, fempropH, fempropM)

      CatchStat <- river_data[i, "Catch"]
      
      if(river_data[i, "AssessMethod"] == 1) { # 1 means normal year of fishing, evaluate with catch and expl rate
        
        fangstL <- ExplMedEst[i] * river_data[i, "ExplMin"]
        fangstM <- ExplMedEst[i]
        fangstH <- ExplMedEst[i] * river_data[i, "ExplMax"]
        ExplRate <- rtriangle(k, fangstL, fangstH, fangstM)
        
        if(identical(IncludeUnreported, FALSE)) { UnrepEst[i] <- 0 }
        
        UnreportedL <- UnrepEst[i] * 0.9 #river_data[i, "UnrepMin"]
        UnreportedM <- UnrepEst[i]  #river_data[i, "UnrepMed"]
        UnreportedH <- UnrepEst[i] * 1.1 #river_data[i, "UnrepMax"]
        Unreported <- rtriangle(k, UnreportedL, UnreportedH, UnreportedM)
        UnrepForFile[i + l] <- UnreportedM
        
        CatchCorrected <- CatchStat / (1 - Unreported)
        CatchForFile[i + l] <- estimate_mode(CatchCorrected)
        OrigCatchForFile[i + l] <- CatchStat
        
        PFA <- CatchStat / ExplRate
        SpawnEst <- PFA - CatchCorrected
        
        Expl_L[i + l] <- min(ExplRate)
        Expl_H[i + l] <- max(ExplRate)
        Expl_M[i + l] <- estimate_mode(ExplRate)
        ExplAdj[i + l] <- ExplCorr[i]
        
      } else {
        
        CatchCorrected <- CatchStat
        CatchForFile[i + l] <- CatchStat
        OrigCatchForFile[i + l] <- CatchStat
        
        if(identical(IncludeUnreported, FALSE)) { UnrepEst[i] <- 0 }
        
        UnreportedL <- UnrepEst[i] * 0.9 #river_data[i, "UnrepMin"]
        UnreportedM <- UnrepEst[i]  #river_data[i, "UnrepMed"]
        UnreportedH <- UnrepEst[i] * 1.1 #river_data[i, "UnrepMax"]
        Unreported <- rtriangle(k, UnreportedL, UnreportedH, UnreportedM)
        UnrepForFile[i + l] <- UnreportedM
        
        CountUncL <- PfaTrib[i] * river_data[i, "CountUncMin"]
        CountUncM <- PfaTrib[i] * river_data[i, "CountUncMed"]
        CountUncH <- PfaTrib[i] * river_data[i, "CountUncMax"]
        PFA <- rtriangle(k, CountUncL, CountUncH, CountUncM)
        
        SpawnEst <- PFA - CatchCorrected
        
        Expl_L[i + l] <- 0
        Expl_H[i + l] <- 0
        Expl_M[i + l] <- 0
        ExplAdj[i + l] <- 0

      }
      
      SpawnFem <- SpawnEst * FemProp
      
      Spawn_W_mod[i + l] <- estimate_mode(SpawnFem)
      Spawn_W_min[i + l] <- min(SpawnFem)
      Spawn_W_max[i + l] <- max(SpawnFem)
      Spawn_W_med[i + l] <- median(SpawnFem)
    
      SpawnFemClean <- rtriangle(k, Spawn_W_min[i + l], Spawn_W_max[i + l], Spawn_W_mod[i + l])
      GBM_attain <- which(SpawnFemClean >= GBM_dist)
      GBM_maal[i + l] <- mean(SpawnFemClean / GBM_dist)
      GBM_prob[i + l] <- length(GBM_attain) / k

      RiverNumber[i + l] <- river_list[r, "RiverNumber"]
      RiverList[i + l] <- paste(river_list[r, "RiverFullName"])
      RiverYear[i + l] <- river_data[i, "Year"]
      
    }
      
  } # end of nosizeclass-evaluation
  
  else if(river_list[r, "AssessProc"] == 1) { # sizeclass-evaluation here, i.e. the Norwegian tributaries with size-class catch statistics 

    RiverFileName <- paste("data/rivers/gbm-sizeclass-", river_list[r, "RiverName"], ".csv", sep = "")
    river_data <- import(RiverFileName, encoding = "UTF-8")
    
    NumberOfYears <- nrow(river_data)

    CatchTrib <- river_data[1:NumberOfYears, "Catch_G"] + river_data[1:NumberOfYears, "Catch_M"] + river_data[1:NumberOfYears, "Catch_S"]

    for(i in 1:NumberOfYears) {
      
      if(river_data[i, "AssessMethod"] == 1) { # 1 means normal year of fishing, evaluate with catch and expl rate
        
        ExplStart[i] <- CatchTrib[i] / (river_data[i, "Catch_G"] / river_data[i, "ExplStart_G"] +
          river_data[i, "Catch_M"] / river_data[i, "ExplStart_M"] +
          river_data[i, "Catch_S"] / river_data[i, "ExplStart_S"])
        PfaTrib[i] <- CatchTrib[i] / ExplStart[i]
        FemPropMedEst[i] <- (river_data[i, "FemProp_G"] * river_data[i, "Catch_G"] +
          river_data[i, "FemProp_M"] * river_data[i, "Catch_M"] +
          river_data[i, "FemProp_S"] * river_data[i, "Catch_S"]) / CatchTrib[i]
        
      } else { # currently only one alternative assessment option: 2021 with no fishing and just counts
        
        ExplStart[i] <- 0
        PfaTrib[i] <- river_data[i, "Count_G"] + river_data[i, "Count_M"] + river_data[i, "Count_S"]
        FemPropMedEst[i] <- (river_data[i, "FemProp_G"] * river_data[i, "Count_G"] +
          river_data[i, "FemProp_M"] * river_data[i, "Count_M"] +
          river_data[i, "FemProp_S"] * river_data[i, "Count_S"]) / 
          (river_data[i, "Count_G"] + river_data[i, "Count_M"] + river_data[i, "Count_S"])
        
      }
      
      FemPropForFile[i + l] <- FemPropMedEst[i]
      
    }

    # if TRUE, make exploitation adjustment based on correlation between trib and Tana total
    if(isTRUE(MakeAdjustment)) {
      
      AvgTrib <- mean(CatchTrib)
      AvgTot <- mean(river_data[1:NumberOfYears, "TanaTotal"])
      StdTrib <- sd(CatchTrib)
      StdTot <- sd(river_data[1:NumberOfYears, "TanaTotal"])
      
      AvgPfa <- mean(PfaTrib)
      StdPfa <- sd(PfaTrib)
      
      for(i in 1:NumberOfYears) {
        
        StdizedTrib[i] <- (CatchTrib[i] - AvgTrib) / StdTrib
        StdizedTot[i] <- (river_data[i, "TanaTotal"] - AvgTot) / StdTot
        StdizedPfa[i] <- (PfaTrib[i] - AvgPfa) / StdPfa
        
      }
      
      StdizedDiff <- StdizedTot - StdizedPfa
      DiffKG <- StdizedDiff * StdPfa
      
      PfaTribCorrected <- PfaTrib + DiffKG
      ExplEstimate <- CatchTrib / PfaTribCorrected
      ExplCorr <- (ExplEstimate - ExplStart) * river_data[i, "ExplCorrSoft"]
      ExplMedEst <- ExplStart + ExplCorr
      
    } else { # no adjustment, use static numbers directly from Excel-files
      
      ExplMedEst <- ExplStart
      ExplCorr <- ExplMedEst - ExplStart
      
    }
    
    for(i in 1:NumberOfYears) {
      
      fangst_G_med <- river_data[i, "ExplStart_G"] + ExplCorr[i]
      fangst_G_min <- fangst_G_med * river_data[i, "ExplMin"]
      fangst_G_max <- fangst_G_med * river_data[i, "ExplMax"]
      ExplRate_G <- rtriangle(k, fangst_G_min, fangst_G_max, fangst_G_med)
      
      fangst_M_med <- river_data[i, "ExplStart_M"] + ExplCorr[i]
      fangst_M_min <- fangst_M_med * river_data[i, "ExplMin"]
      fangst_M_max <- fangst_M_med * river_data[i, "ExplMax"]
      ExplRate_M <- rtriangle(k, fangst_M_min, fangst_M_max, fangst_M_med)
      
      fangst_S_med <- river_data[i, "ExplStart_S"] + ExplCorr[i]
      fangst_S_min <- fangst_S_med * river_data[i, "ExplMin"]
      fangst_S_max <- fangst_S_med * river_data[i, "ExplMax"]
      ExplRate_S <- rtriangle(k, fangst_S_min, fangst_S_max, fangst_S_med)
      
      femprop_G_med <- river_data[i, "FemProp_G"]
      femprop_G_min <- femprop_G_med * river_data[i, "FemPropMin"]
      femprop_G_max <- femprop_G_med * river_data[i, "FemPropMax"]
      sexratio_G <- rtriangle(k, femprop_G_min, femprop_G_max, femprop_G_med)
      
      femprop_M_med <- river_data[i, "FemProp_M"]
      femprop_M_min <- femprop_M_med * river_data[i, "FemPropMin"]
      femprop_M_max <- femprop_M_med * river_data[i, "FemPropMax"]
      sexratio_M <- rtriangle(k, femprop_M_min, femprop_M_max, femprop_M_med)
      
      femprop_S_med <- river_data[i, "FemProp_S"]
      femprop_S_min <- femprop_S_med * river_data[i, "FemPropMin"]
      femprop_S_max <- femprop_S_med * river_data[i, "FemPropMax"]
      sexratio_S <- rtriangle(k, femprop_S_min, femprop_S_max, femprop_S_med)

      if(isTRUE(IncludeUnreported)) {
        
        UnreportedL <- river_data[i, "UnrepMin"]
        UnreportedM <- river_data[i, "UnrepMed"]
        UnreportedH <- river_data[i, "UnrepMax"]
        
      } else {
        
        UnreportedL <- 0
        UnreportedM <- 0
        UnreportedH <- 0
        
      }
      
      Unreported <- rtriangle(k, UnreportedL, UnreportedH, UnreportedM)
      UnrepForFile[i + l] <- UnreportedM
      
      CatchCorrected_G <- river_data[i, "Catch_G"] / (1 - Unreported)
      CatchCorrected_M <- river_data[i, "Catch_M"] / (1 - Unreported)
      CatchCorrected_S <- river_data[i, "Catch_S"] / (1 - Unreported)
      CatchForFile[i + l] <- estimate_mode(CatchCorrected_G) + estimate_mode(CatchCorrected_M) + estimate_mode(CatchCorrected_S)
      OrigCatchForFile[i + l] <- river_data[i, "Catch_G"] + river_data[i, "Catch_M"] + river_data[i, "Catch_S"]
      
      if(river_data[i, "AssessMethod"] == 1) { # 1 means normal year of fishing, evaluate with catch and expl rate
        
        PFA_G <- river_data[i, "Catch_G"] / ExplRate_G
        PFA_M <- river_data[i, "Catch_M"] / ExplRate_M
        PFA_S <- river_data[i, "Catch_S"] / ExplRate_S
        
      } else { # alternatively, year with no fishing, PFA is given from counts
        
        CountUncertainty <- rtriangle(k, river_data[i, "CountUncMin"], river_data[i, "CountUncMax"], river_data[i, "CountUncMed"])
        PFA_G <- river_data[i, "Count_G"] * CountUncertainty
        PFA_M <- river_data[i, "Count_M"] * CountUncertainty
        PFA_S <- river_data[i, "Count_S"] * CountUncertainty
        
      }
      
      SpawnEst_G <- PFA_G - CatchCorrected_G
      SpawnEst_M <- PFA_M - CatchCorrected_M
      SpawnEst_S <- PFA_S - CatchCorrected_S
      
      SpawnFem <- (SpawnEst_G * sexratio_G) + (SpawnEst_M * sexratio_M) + (SpawnEst_S * sexratio_S)

      Spawn_W_mod[i + l] <- estimate_mode(SpawnFem)
      Spawn_W_min[i + l] <- min(SpawnFem)
      Spawn_W_max[i + l] <- max(SpawnFem)
      Spawn_W_med[i + l] <- median(SpawnFem)
      
      SpawnFemClean <- rtriangle(k, Spawn_W_min[i + l], Spawn_W_max[i + l], Spawn_W_mod[i + l])
      GBM_attain <- which(SpawnFemClean >= GBM_dist)
      GBM_maal[i + l] <- mean(SpawnFemClean / GBM_dist)
      GBM_prob[i + l] <- length(GBM_attain) / k
      
      Expl_L[i + l] <- ExplMedEst[i] * river_data[i, "ExplMin"]
      Expl_H[i + l] <- ExplMedEst[i] * river_data[i, "ExplMax"]
      Expl_M[i + l] <- ExplMedEst[i]
      ExplAdj[i + l] <- ExplCorr[i]
      
      RiverNumber[i + l] <- river_list[r, "RiverNumber"]
      RiverList[i + l] <- paste(river_list[r, "RiverFullName"])
      RiverYear[i + l] <- river_data[i, "Year"]
      
    }
    
  } else if(river_list[r, "AssessProc"] == 2) { # Polmak special assessment procedure here
    
    CatchCount <- vector()
    SnorkCount <- vector()
    
    river_data <- import("data/rivers/gbm-Polmak.csv", encoding = "UTF-8")
    NumberOfYears <- nrow(river_data)

    for(i in 1:NumberOfYears) {

      if(river_data[i, "AssessMethod"] == 1) { # 1 means normal year of fishing, evaluate with catch and expl rate
        
        CatchTrib[i] <- river_data[i, "Catch_kg"]
   
        CatchCount[i] <- river_data[i, "Catch_nr"]
        SnorkCount[i] <- river_data[i, "Snorkeling"]
        
        DetectEff_M <- river_data[i, "DetEff"]
        DetectEff_L <- DetectEff_M * river_data[i, "DetEffMin"]
        DetectEff_H <- DetectEff_M * river_data[i, "DetEffMax"]
        DetectEff <- rtriangle(k, DetectEff_L, DetectEff_H, DetectEff_M)
        
        Coverage_M <- river_data[i, "SnorkCoverage"]
        Coverage_L <- Coverage_M * river_data[i, "CoverMin"]
        Coverage_H <- Coverage_M * river_data[i, "CoverMax"]
        Coverage <- rtriangle(k, Coverage_L, Coverage_H, Coverage_M)
        
        SnorkEst <- SnorkCount[i] / (DetectEff * Coverage)
        
        ExplStart[i] <- median(CatchCount[i] / (CatchCount[i] + SnorkEst))
        
        FemPropMedEst[i] <- river_data[i, "FemProp"]
        FemPropForFile[i + l] <- FemPropMedEst[i]        
        
        PfaTrib[i] <- CatchTrib[i] / ExplStart[i]
        
      } else { # year without fishing, only snorkeling counts
        
        diveuncG_M <- river_data[i, "diveuncG_M"] # median diving uncertainty, grilse
        diveuncG_L <- diveuncG_M * river_data[i, "diveuncMin"]
        diveuncG_H <- diveuncG_M * river_data[i, "diveuncMax"]
        diveunc_G <- rtriangle(k, diveuncG_L, diveuncG_H, diveuncG_M)
        
        diveuncM_M <- river_data[i, "diveuncM_M"] # median diving uncertainty, MSW
        diveuncM_L <- diveuncM_M * river_data[i, "diveuncMin"]
        diveuncM_H <- diveuncM_M * river_data[i, "diveuncMax"]
        diveunc_M <- rtriangle(k, diveuncM_L, diveuncM_H, diveuncM_M)
        
        femprop_G_med <- river_data[i, "FemProp_G"]
        femprop_G_min <- femprop_G_med * river_data[i, "FemPropMin"]
        femprop_G_max <- femprop_G_med * river_data[i, "FemPropMax"]
        sexratio_G <- rtriangle(k, femprop_G_min, femprop_G_max, femprop_G_med)
        femprop_M_med <- river_data[i, "FemProp_M"]
        femprop_M_min <- femprop_M_med * river_data[i, "FemPropMin"]
        femprop_M_max <- femprop_M_med * river_data[i, "FemPropMax"]
        sexratio_M <- rtriangle(k, femprop_M_min, femprop_M_max, femprop_M_med)
        
        sizeG_med <- river_data[i, "sizeG"] # average weight, 1SW female
        sizeG_min <- sizeG_med * river_data[i, "sizeMin"]
        sizeG_max <- sizeG_med * river_data[i, "sizeMax"]
        sizeG <- rtriangle(k, sizeG_min, sizeG_max, sizeG_med)
        sizeM_med <- river_data[i, "sizeM"] # average weight, MSW female
        sizeM_min <- sizeM_med * river_data[i, "sizeMin"]
        sizeM_max <- sizeM_med * river_data[i, "sizeMax"]
        sizeM <- rtriangle(k, sizeM_min, sizeM_max, sizeM_med)
        
        divecount_G <- river_data[i, "Count_G"] / river_data[i, "AreaCover"] # number of grilse counted
        divecount_M <- river_data[i, "Count_M"] / river_data[i, "AreaCover"] # number of MSW/PS counted
        
        gyteest_G <- divecount_G * diveunc_G
        gyteest_M <- divecount_M * diveunc_M
        
        gytehunn_G <- gyteest_G * sexratio_G
        gytehunn_M <- gyteest_M * sexratio_M
        
        ExplStart[i] <- 0

      }
      
    }

    if(isTRUE(MakeAdjustment)) {
      
      AvgTrib <- mean(river_data[1:NumberOfYears, "Catch_kg"])
      AvgTot <- mean(river_data[1:NumberOfYears, "TanaTotal"])
      StdTrib <- sd(river_data[1:NumberOfYears, "Catch_kg"])
      StdTot <- sd(river_data[1:NumberOfYears, "TanaTotal"])
      
      AvgPfa <- mean(PfaTrib)
      StdPfa <- sd(PfaTrib)
      
      for(i in 1:NumberOfYears) {
        
        StdizedTrib[i] <- (CatchTrib[i] - AvgTrib) / StdTrib
        StdizedTot[i] <- (river_data[i, "TanaTotal"] - AvgTot) / StdTot
        StdizedPfa[i] <- (PfaTrib[i] - AvgPfa) / StdPfa
        
      }
      
      StdizedDiff <- StdizedTot - StdizedPfa
      DiffKG <- StdizedDiff * StdPfa
      
      PfaTribCorrected <- PfaTrib + DiffKG
      ExplEstimate <- CatchTrib / PfaTribCorrected
      ExplCorr <- (ExplEstimate - ExplStart) * river_data[i, "ExplCorrSoft"]
      ExplMedEst <- ExplStart + ExplCorr
      
    } else { # no adjustment, use static numbers directly from Excel-files
      
      ExplMedEst <- ExplStart
      ExplCorr <- ExplMedEst - ExplStart
      
    }
    
    for(i in 1:NumberOfYears) {

      if(river_data[i, "AssessMethod"] == 1) { # 1 means normal year of fishing, evaluate with catch and expl rate
        
        fangstL <- ExplMedEst[i] * river_data[i, "ExplMin"]
        fangstM <- ExplMedEst[i]
        fangstH <- ExplMedEst[i] * river_data[i, "ExplMax"]
        ExplRate <- rtriangle(k, fangstL, fangstH, fangstM)
        
        fempropL <- FemPropMedEst[i] * river_data[i, "FemPropMin"]
        fempropM <- FemPropMedEst[i]
        fempropH <- FemPropMedEst[i] * river_data[i, "FemPropMax"]
        FemProp <- rtriangle(k, fempropL, fempropH, fempropM)
        
        if(isTRUE(IncludeUnreported)) {
          
          UnreportedL <- river_data[i, "UnrepMin"]
          UnreportedM <- river_data[i, "UnrepMed"]
          UnreportedH <- river_data[i, "UnrepMax"]
          
        } else {
          
          UnreportedL <- 0
          UnreportedM <- 0
          UnreportedH <- 0
          
        }
        
        Unreported <- rtriangle(k, UnreportedL, UnreportedH, UnreportedM)
        UnrepForFile[i + l] <- UnreportedM
        
        CatchCorrected <- CatchTrib[i] / (1 - Unreported)
        CatchForFile[i + l] <- estimate_mode(CatchCorrected)
        OrigCatchForFile[i + l] <- CatchTrib[i]
        
        PFA <- CatchTrib[i] / ExplRate
        SpawnEst <- PFA - CatchCorrected
        SpawnFem <- SpawnEst * FemProp
        
        Spawn_W_mod[i + l] <- estimate_mode(SpawnFem)
        Spawn_W_min[i + l] <- min(SpawnFem)
        Spawn_W_max[i + l] <- max(SpawnFem)
        Spawn_W_med[i + l] <- median(SpawnFem)
        
        SpawnFemClean <- rtriangle(k, Spawn_W_min[i + l], Spawn_W_max[i + l], Spawn_W_mod[i + l])
        GBM_attain <- which(SpawnFemClean >= GBM_dist)
        GBM_maal[i + l] <- mean(SpawnFemClean / GBM_dist)
        GBM_prob[i + l] <- length(GBM_attain) / k
        
        Expl_L[i + l] <- min(ExplRate)
        Expl_H[i + l] <- max(ExplRate)
        Expl_M[i + l] <- estimate_mode(ExplRate)
        ExplAdj[i + l] <- ExplCorr[i]
        
        RiverNumber[i + l] <- river_list[r, "RiverNumber"]
        RiverList[i + l] <- paste(river_list[r, "RiverFullName"])
        RiverYear[i + l] <- river_data[i, "Year"]
        
      } else {
      
        FemPropForFile[i + l] <- estimate_mode((gytehunn_G + gytehunn_M) / (gyteest_G + gyteest_M))
        
        SpawnFem <- (gytehunn_G * sizeG) + (gytehunn_M * sizeM)
        
        Spawn_W_mod[i + l] <- estimate_mode(SpawnFem)
        Spawn_W_min[i + l] <- min(SpawnFem)
        Spawn_W_max[i + l] <- max(SpawnFem)
        Spawn_W_med[i + l] <- median(SpawnFem)
        
        SpawnFemClean <- rtriangle(k, Spawn_W_min[i + l], Spawn_W_max[i + l], Spawn_W_mod[i + l])
        GBM_attain <- which(SpawnFemClean >= GBM_dist)
        GBM_maal[i + l] <- mean(SpawnFemClean/GBM_dist)
        GBM_prob[i + l] <- length(GBM_attain) / k
        
        Expl_L[i + l] <- 0
        Expl_H[i + l] <- 0
        Expl_M[i + l] <- 0
        ExplAdj[i + l] <- 0
        
        UnrepForFile[i + l] <- 0
        
        RiverNumber[i + l] <- river_list[r, "RiverNumber"]
        RiverList[i + l] <- paste(river_list[r, "RiverFullName"])
        RiverYear[i + l] <- river_data[i, "Year"]
        
        CatchForFile[i + l] <- 0
        OrigCatchForFile[i + l] <- 0
        
      }
      
    }
    
  } else if(river_list[r, "AssessProc"] == 3) { # Akujoki (and other snorkeling tribs) here
    
    RiverFileName <- paste("data/rivers/gbm-snork-", river_list[r, "RiverName"], ".csv", sep="")
    river_data <- import(RiverFileName, encoding = "UTF-8")
    
    NumberOfYears <- nrow(river_data)
    
    for(i in 1:NumberOfYears) {
      
      if(river_data[i, "EstMethod"] == 1) { # year with snorkeling data
        
        diveuncG_M <- river_data[i, "diveuncG_M"] # median diving uncertainty, grilse
        diveuncG_L <- diveuncG_M * river_data[i, "diveuncMin"]
        diveuncG_H <- diveuncG_M * river_data[i, "diveuncMax"]
        diveunc_G <- rtriangle(k, diveuncG_L, diveuncG_H, diveuncG_M)
        
        diveuncM_M <- river_data[i, "diveuncM_M"] # median diving uncertainty, MSW
        diveuncM_L <- diveuncM_M * river_data[i, "diveuncMin"]
        diveuncM_H <- diveuncM_M * river_data[i, "diveuncMax"]
        diveunc_M <- rtriangle(k, diveuncM_L, diveuncM_H, diveuncM_M)
        
        femprop_G_med <- river_data[i, "FemProp_G"]
        femprop_G_min <- femprop_G_med * river_data[i, "FemPropMin"]
        femprop_G_max <- femprop_G_med * river_data[i, "FemPropMax"]
        sexratio_G <- rtriangle(k, femprop_G_min, femprop_G_max, femprop_G_med)
        
        femprop_M_med <- river_data[i, "FemProp_M"]
        femprop_M_min <- femprop_M_med * river_data[i, "FemPropMin"]
        femprop_M_max <- femprop_M_med * river_data[i, "FemPropMax"]
        sexratio_M <- rtriangle(k, femprop_M_min, femprop_M_max, femprop_M_med)
  
        sizeG_med <- river_data[i, "sizeG"] # average weight, 1SW female
        sizeG_min <- sizeG_med * river_data[i, "sizeMin"]
        sizeG_max <- sizeG_med * river_data[i, "sizeMax"]
        sizeG <- rtriangle(k, sizeG_min, sizeG_max, sizeG_med)
        
        sizeM_med <- river_data[i, "sizeM"] # average weight, MSW female
        sizeM_min <- sizeM_med * river_data[i, "sizeMin"]
        sizeM_max <- sizeM_med * river_data[i, "sizeMax"]
        sizeM <- rtriangle(k, sizeM_min, sizeM_max, sizeM_med)
  
        divecount_G <- river_data[i, "Count_G"] / river_data[i, "AreaCover"] # number of grilse counted
        divecount_M <- river_data[i, "Count_M"] / river_data[i, "AreaCover"] # number of MSW/PS counted
        
        gyteest_G <- divecount_G * diveunc_G
        gyteest_M <- divecount_M * diveunc_M
        
        gytehunn_G <- gyteest_G * sexratio_G
        gytehunn_M <- gyteest_M * sexratio_M
        
        FemPropForFile[i + l] <- estimate_mode((gytehunn_G + gytehunn_M) / (gyteest_G + gyteest_M))
        
        SpawnFem <- (gytehunn_G * sizeG) + (gytehunn_M * sizeM)
        
        Spawn_W_mod[i + l] <- estimate_mode(SpawnFem)
        Spawn_W_min[i + l] <- min(SpawnFem)
        Spawn_W_max[i + l] <- max(SpawnFem)
        Spawn_W_med[i + l] <- median(SpawnFem)
        
        SpawnFemClean <- rtriangle(k, Spawn_W_min[i + l], Spawn_W_max[i + l], Spawn_W_mod[i + l])
        GBM_attain <- which(SpawnFemClean >= GBM_dist)
        GBM_maal[i + l] <- mean(SpawnFemClean/GBM_dist)
        GBM_prob[i + l] <- length(GBM_attain) / k
  
        Expl_L[i + l] <- 0
        Expl_H[i + l] <- 0
        Expl_M[i + l] <- 0
        ExplAdj[i + l] <- 0
        
        UnrepForFile[i + l] <- 0
        
        RiverNumber[i + l] <- river_list[r, "RiverNumber"]
        RiverList[i + l] <- paste(river_list[r, "RiverFullName"])
        RiverYear[i + l] <- river_data[i, "Year"]
        
        CatchForFile[i + l] <- 0
        OrigCatchForFile[i + l] <- 0
        
      } else { # have to use main stem catch data as basis
        
        ExplMedEst[i] <- river_data[i, "ExplStart"]
        fangstL <- ExplMedEst[i] * river_data[i, "ExplMin"]
        fangstM <- ExplMedEst[i]
        fangstH <- ExplMedEst[i] * river_data[i, "ExplMax"]
        ExplRate <- rtriangle(k, fangstL, fangstH, fangstM)
        
        FemPropMedEst[i] <- river_data[i, "FemProp"]
        fempropL <- FemPropMedEst[i] * river_data[i, "FemPropMin"]
        fempropM <- FemPropMedEst[i]
        fempropH <- FemPropMedEst[i] * river_data[i, "FemPropMax"]
        FemProp <- rtriangle(k, fempropL, fempropH, fempropM)
        FemPropForFile[i + l] <- estimate_mode(FemProp)

        CatchStat <- river_data[i, "Catch"]
        
        if(isTRUE(IncludeUnreported)) {
          
          UnreportedL <- river_data[i, "UnrepMin"]
          UnreportedM <- river_data[i, "UnrepMed"]
          UnreportedH <- river_data[i, "UnrepMax"]
          
        } else {
          
          UnreportedL <- 0
          UnreportedM <- 0
          UnreportedH <- 0
          
        }
        
        Unreported <- rtriangle(k, UnreportedL, UnreportedH, UnreportedM)
        UnrepForFile[i + l] <- UnreportedM
        
        CatchCorrected <- CatchStat / (1 - Unreported)
        CatchForFile[i + l] <- estimate_mode(CatchCorrected)
        OrigCatchForFile[i + l] <- CatchStat
        
        PFA <- CatchStat / ExplRate
        SpawnEst <- PFA - CatchCorrected
        SpawnFem <- SpawnEst * FemProp
        
        Spawn_W_mod[i + l] <- estimate_mode(SpawnFem)
        Spawn_W_min[i + l] <- min(SpawnFem)
        Spawn_W_max[i + l] <- max(SpawnFem)
        Spawn_W_med[i + l] <- median(SpawnFem)
        
        SpawnFemClean <- rtriangle(k, Spawn_W_min[i + l], Spawn_W_max[i + l], Spawn_W_mod[i + l])
        GBM_attain <- which(SpawnFemClean >= GBM_dist)
        GBM_maal[i + l] <- mean(SpawnFemClean / GBM_dist)
        GBM_prob[i + l] <- length(GBM_attain) / k
        
        Expl_L[i + l] <- min(ExplRate)
        Expl_H[i + l] <- max(ExplRate)
        Expl_M[i + l] <- estimate_mode(ExplRate)
        ExplAdj[i + l] <- 0
        
        RiverNumber[i + l] <- river_list[r, "RiverNumber"]
        RiverList[i + l] <- paste(river_list[r, "RiverFullName"])
        RiverYear[i + l] <- river_data[i, "Year"]
        
      }
      
    }
    
  }
  
  # evaluate 4-year management targets throughout the evaluation period
  for (i in 1:NumberOfYears) {
    
    if(i < 4) {
      
      GBM_4yr_maal[i + l] <- 0
      GBM_4yr_prob[i + l] <- 0
      Spawn4yrMin[i + l] <- 0
      Spawn4yrMed[i + l] <- 0
      Spawn4yrMax[i + l] <- 0
      
    } else {
      
      j <- i - 3
      Spawn4yrMin[i + l] <- mean(Spawn_W_min[(j + l):(i + l)])
      Spawn4yrMed[i + l] <- mean(Spawn_W_mod[(j + l):(i + l)])
      Spawn4yrMax[i + l] <- mean(Spawn_W_max[(j + l):(i + l)])
      gytehunn <- rtriangle(k, Spawn4yrMin[i + l], Spawn4yrMax[i + l], Spawn4yrMed[i + l])
      GBM_4yr_attain <- which(gytehunn >= GBM_dist)
      GBM_4yr_maal[i + l] <- mean(gytehunn / GBM_dist)
      GBM_4yr_prob[i + l] <- length(GBM_4yr_attain) / k
      
    }
    
  }

  l <- l + NumberOfYears #increase results counter

} # end river-loop

# write results to status assessment results file
GBM_results <- data.frame(RiverNumber, RiverList, RiverYear, CatchForFile, OrigCatchForFile, FemPropForFile,
                          Spawn_W_mod, Spawn_W_min, Spawn_W_max, Spawn_W_med, 
                          GBM_maal, GBM_prob, Spawn4yrMin, Spawn4yrMed, Spawn4yrMax, 
                          GBM_4yr_maal, GBM_4yr_prob, Expl_L, Expl_M, Expl_H, ExplAdj, UnrepForFile)

export(GBM_results, "results/GBM_results.csv", ";", dec = ".", bom = TRUE)
