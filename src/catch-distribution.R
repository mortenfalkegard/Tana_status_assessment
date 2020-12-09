# coastal+river catch distribution script

#library(triangle)
library(openxlsx)

setwd("C:/Users/morten.falkegard/OneDrive - NINA/Tana monitoring group/Status assessment files")

# variables defining starting year and number of years used in estimation
StartingYear <- "2018"
NumberOfYears <- 1

# obtain list of rivers with spawning targets, 1.5 kg proportions and PFA method
river_list <- read.xlsx("catch-dist-riverlist.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
NumberOfRivers <- nrow(river_list)-2 # -2 because last items of list are Other tribs and Tana total, currently not supported

# obtain coastal catch distribution numbers (proportion of salmon from Tana in different coastal regions)
coastal_dist <- read.xlsx("coastal-dist-table.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

# obtain annual list of salmon catch (biomass) from coastal regions and Tana main stem 
coastal_MS_catch <- read.xlsx("coastal-MS-catch.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
j <- which(coastal_MS_catch$Year == StartingYear)
YearList <- coastal_MS_catch[j:(j+NumberOfYears-1), "Year"]

# obtain annual list of main stem catch proportions of the different Tana stocks
MS_catch_proportions <- read.xlsx("catch-dist-MS-props.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

# obtain status assessment results
GBM_assess <- read.xlsx("GBM_results.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

# variable declarations
TributaryCatch <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
TributaryFemProp <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
TributarySpawn <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
TributaryExpl <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
TributaryGBMAttain <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
MainStemCatch <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
PFA_MS <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
PFA_MS_15 <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
PFA_trib <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
PFA15_prop <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
PFA_coast <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
PFA_coast_fem <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
TanaCoastalCatch <- vector()
StockCoastalCatch <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
StockTotalCatch <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
OverExploitation <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)
MaxTotalExploitation <- matrix(nrow=NumberOfYears, ncol=NumberOfRivers)

# distribute the Tana main stem catch on the different stocks
j <- which(coastal_MS_catch$Year == StartingYear)
for (i in 1:NumberOfRivers) {
  MainStemCatch[1:NumberOfYears, i] <- coastal_MS_catch[j:(j+NumberOfYears-1), "TanaMS"] *
    MS_catch_proportions[j:(j+NumberOfYears-1), i+1]
}

for (i in 1:NumberOfRivers) { 
  CurrentRiver <- as.character(river_list[i, "RiverFullName"])
  if(river_list[i, "PFA_method"]==1) { # Tana MS and tribs with very limited fisheries, treat as special cases
    j <- which(GBM_assess$RiverYear == StartingYear & GBM_assess$RiverList == CurrentRiver)
    TributaryCatch[1:NumberOfYears, i] <- 0
    TributaryFemProp[1:NumberOfYears, i] <- GBM_assess[j:(j+NumberOfYears-1), "FemPropForFile"]
    TributarySpawn[1:NumberOfYears, i] <- GBM_assess[j:(j+NumberOfYears-1), "Spawn_W_mod"]
    TributaryExpl[1:NumberOfYears, i] <- 0
    TributaryGBMAttain[1:NumberOfYears, i] <- GBM_assess[j:(j+NumberOfYears-1), "GBM_maal"]
  } else if(CurrentRiver=="Other tribs") { # Other tribs, special case
    TributaryCatch[1:NumberOfYears, i] <- 0
    TributaryFemProp[1:NumberOfYears, i] <- 0.5
    TributaryExpl[1:NumberOfYears, i] <- 0
    TributarySpawn[1:NumberOfYears, i] <- (MainStemCatch[1:NumberOfYears, i]*TributaryFemProp[1:NumberOfYears,i])/0.5 # assume 0.5 main stem exploitation rate for now
    TributaryGBMAttain[1:NumberOfYears, i] <- TributarySpawn[1:NumberOfYears, i]/river_list[i, "GBM_med"]
  } else {
    j <- which(GBM_assess$RiverYear == StartingYear & GBM_assess$RiverList == CurrentRiver)
    TributaryCatch[1:NumberOfYears, i] <- GBM_assess[j:(j+NumberOfYears-1), "CatchForFile"]
    TributaryFemProp[1:NumberOfYears, i] <- GBM_assess[j:(j+NumberOfYears-1), "FemPropForFile"]
    TributarySpawn[1:NumberOfYears, i] <- GBM_assess[j:(j+NumberOfYears-1), "Spawn_W_mod"]
    TributaryExpl[1:NumberOfYears, i] <- GBM_assess[j:(j+NumberOfYears-1), "Expl_M"]
    TributaryGBMAttain[1:NumberOfYears, i] <- GBM_assess[j:(j+NumberOfYears-1), "GBM_maal"]
  }
}

for (i in 1:NumberOfRivers) {
  CurrentRiver <- as.character(river_list[i, "RiverFullName"])
  if(river_list[i, "PFA_method"]==1) { # Tana MS and tribs with Genmix assessment
    PFA_trib[1:NumberOfYears, i] <- TributarySpawn[1:NumberOfYears, i]/TributaryFemProp[1:NumberOfYears, i]+
      MainStemCatch[1:NumberOfYears, i]
    PFA_MS[1:NumberOfYears, i] <- PFA_trib[1:NumberOfYears, i]
  } else {
    PFA_trib[1:NumberOfYears, i] <- TributarySpawn[1:NumberOfYears, i]/TributaryFemProp[1:NumberOfYears, i]
    PFA_MS[1:NumberOfYears, i] <- PFA_trib[1:NumberOfYears, i] + MainStemCatch[1:NumberOfYears, i]
  }
  PFA_MS_15[1:NumberOfYears, i] <- PFA_MS[1:NumberOfYears, i] * (1-river_list[i, "PropSize_1.5"])
}

PFA15_prop <- prop.table(PFA_MS_15, 1)
j <- which(coastal_MS_catch$Year == StartingYear)
TanaCoastalCatch <- rowSums(coastal_MS_catch[j:(j+NumberOfYears-1), 2:10]) # columns 2 to 10 equals coastal regions
StockCoastalCatch <- TanaCoastalCatch * PFA15_prop
PFA_coast <- PFA_MS + StockCoastalCatch
PFA_coast_fem <- PFA_coast * TributaryFemProp
StockTotalCatch <- StockCoastalCatch + MainStemCatch + TributaryCatch

for (i in 1:NumberOfRivers) {
  for (j in 1:NumberOfYears) {
    if(TributarySpawn[j, i]>river_list[i, "GBM_med"]) {
      OverExploitation[j, i] <- 0
    } else if(PFA_coast_fem[j, i]>river_list[i, "GBM_med"]) {
      OverExploitation[j, i] <- (river_list[i, "GBM_med"]-TributarySpawn[j, i])/river_list[i, "GBM_med"]
    } else {
      OverExploitation[j, i] <- StockTotalCatch[j, i] * TributaryFemProp[j, i] / river_list[i, "GBM_med"]
    }
    if(PFA_coast_fem[j, i]>river_list[i, "GBM_med"]) {
      MaxTotalExploitation[j, i] <- (PFA_coast_fem[j, i]-river_list[i, "GBM_med"])/PFA_coast_fem[j, i]
    } else {
      MaxTotalExploitation[j, i] <- 0
    }
  }
}

# make summary table
SumRiverList <- character()
SumTribCatch <- vector()
SumMSCatch <- vector()
SumCoastCatch <- vector()
SumSpawning <- vector()
AvgOverExpl <- vector()
AvgMaxTotalExpl <- vector()
#SumPFATrib <- vector()
#SumPFAMS <- vector()
#SumPFACoast <- vector()

SumRiverList <- paste(river_list[1:NumberOfRivers, "RiverFullName"])
SumTribCatch <- colSums(TributaryCatch)/NumberOfYears
SumMSCatch <- colSums(MainStemCatch)/NumberOfYears
SumCoastCatch <- colSums(StockCoastalCatch)/NumberOfYears
SumSpawning <- colSums(TributarySpawn/TributaryFemProp)/NumberOfYears
#SumPFATrib <- colSums(PFA_trib)/NumberOfYears
#SumPFAMS <- colSums(PFA_MS)/NumberOfYears
#SumPFACoast <- colSums(PFA_coast)/NumberOfYears

AvgOverExpl <- apply(OverExploitation, 2, mean)
AvgMaxTotalExpl <- apply(MaxTotalExploitation, 2, mean)

if(NumberOfYears>3) {
  OverExpl1 <- OverExploitation[1, ]
  OverExpl2 <- OverExploitation[2, ]
  OverExpl3 <- OverExploitation[3, ]
  OverExpl4 <- OverExploitation[4, ]
  MaxExpl1 <- MaxTotalExploitation[1, ]
  MaxExpl2 <- MaxTotalExploitation[2, ]
  MaxExpl3 <- MaxTotalExploitation[3, ]
  MaxExpl4 <- MaxTotalExploitation[4, ]
  
  CatchDistResults <- data.frame(SumRiverList, SumTribCatch, SumMSCatch, SumCoastCatch,
                                 SumSpawning, AvgOverExpl, AvgMaxTotalExpl, OverExpl1,
                                 OverExpl2, OverExpl3, OverExpl4, MaxExpl1, MaxExpl2,
                                 MaxExpl3, MaxExpl4)
} else {
#  CatchDistResults <- data.frame(SumRiverList, SumTribCatch, SumMSCatch, SumCoastCatch,
#                                 SumSpawning, AvgOverExpl, AvgMaxTotalExpl, SumPFATrib, SumPFAMS, SumPFACoast )
  CatchDistResults <- data.frame(SumRiverList, SumTribCatch, SumMSCatch, SumCoastCatch,
                                 SumSpawning, AvgOverExpl, AvgMaxTotalExpl )
  
}

write.xlsx(CatchDistResults, "Catch_Dist_results.xlsx")
