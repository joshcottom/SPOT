##################################################################################
##
## Script name:   0-MasterScript.R
##
## Version:       1.3.1-C-0126
## 
## Author:        Dr Josh Cottom (J.Cottom@Imperial.ac.uk)
##
## Organisation:  Imperial College London - Dr Costas Velis (C.Velis@Imperial.ac.uk) Research Group.
##
## Last updated:  January 2026
##
##################################################################################
##
## Description:   Main script that runs the probabilistic MFA as part of SPOT.
##                This occurs after the machine learning outputs have been generated. 
##
## Inputs:        1) Excel workbook of MFA input values
##
##
## Outputs:       1) Excel workbook of probabilistic MFA resutls at municipal level
##                2) Excel workbook of probabilistic MFA results at provincial level 
##                3) Excel workbook of probabilistic MFA results at national level 
##
##################################################################################
##
##  Packages:     Uncomment the below if not previously installed
##
##                install.packages("triangle")
##                install.packages("truncnorm")
##                install.packages("readxl")
##                install.packages("ranger")
##                install.packages("openxlsx")
##                install.packages("mc2d")
##                install.packages("tictoc")
##                install.packages("moments")
##                install.packaged("sensitivity")
##
##################################################################################

##################################################################################
##  Load libraries and log start time                                                                      
##################################################################################

##  Log start time
start.time <- Sys.time()

##  Load necessary libraries
library(triangle)
library(truncnorm)
library(readxl)
library(openxlsx)
library(mc2d)
library(tictoc)
library(moments)
library(sensitivity)
library(tidyr)

##################################################################################
##  Set run specific inputs                                                                     
##################################################################################

##  Set the working directory as that of the main folder location
wd <- ""

##  Name of the Excel workbook with inputs
workbook <- ""

##  Version number to add to output files if required
version <- "V1.3.1-C-0126"

##  Assign the  functions you want to use to summarize the raw Monte Carlo results,
##  giving each a name. The name is used in the output results file.
##  Note: statistics are reported for each output, therefore a large number of statistics
##  significantly increases the output file size.
summary.stats <- list("Mean" = function(x) {if(all(is.na(x))){NA}else{mean(x,na.rm=T)}},
                      #"Standard_deviation" = function(x){if(all(is.na(x))){NA}else{sd(x,na.rm=T)}},
                      #"Skewness" = function(x){if(all(is.na(x))){NA}else{if(length(unique(x))>1){skewness(x,na.rm=T)}else{0}}},
                      #"Kurtosis" = function(x){if(all(is.na(x))){NA}else{if(length(unique(x))>1){kurtosis(x,na.rm=T)}else{NA}}},
                      # For quantiles, keep the "Quantiles" name the same but add the 
                      # name you want it to be reported under in the nested list followed
                      # by the quantile. This ensures all quantiles are calculated simultaneously
                      # thereby saving time. Quantiles should always come last in the list.
                      "Quantiles" = list("5th percentile" = 0.05,
                                         "Lower quartile" = 0.25,
                                         "Median" = 0.5,
                                         "Upper quartile" = 0.75,
                                         "95th percentile" = 0.95)
)

##  Select whether you wish to save the full output PDFs for select municipalities.
##  The  Unique IDs of the municipalities for which you wish to save the full output PDFs
##  should be specified in the inputs workbook, 'Retain_full_munic' sheet,
retain_full_munic <- FALSE

## Select whether you wish to save the full output PDFs for provincial level aggregation
retain_full_province <- FALSE

## Select whether you wish to save the full output PDFs for country level aggregation
retain_full_country <- FALSE

##  Select whether you wish to run sensitivity analysis
run_sensitivity_analysis <- FALSE

##  Select which metric you want to use for the sensitivity analysis:
##    1 = Absolute emissions (debris + openly burned)
##    2 = Absolute emissions (debris)
##    3 = Absolute emissions (openly burned)
##    4 = Per capita emissions (debris + openly burned)
##    5 = Per capita emissions (debris)
##    6 = Per capita emissions (openly burned)
##    7 = SDG11.6.1
##    8 = Mismanaged waste
##    9 = Recycling rate
metric <- 1

##  If you wish to save the above three full PDF outputs to a different folder, add the directory here,
##  otherwise, leave as NA.
Large_dir <- NA

##  Set the number of iterations you want to perform in the Monte Carlo simulation
iterations <- 1000

##  Set the seed for the random number generation (RNG)
set.seed(1810)

##################################################################################
##  Load the excel inputs                                                                     
##################################################################################

tic("Load inputs")
setwd(paste0(wd,"/script"))
source("1-LoadInputs.R")
toc()

##################################################################################
##  Create template files to populate with inputs / outputs
##################################################################################

tic("Create template files")
setwd(paste0(wd,"/script"))
source("2-Templates.R")
toc()

##################################################################################
##  Sample RF Predictions and make necessary adjustments based on if Kernal 
##  density estimation is required, removal of outliers and rurality.
##################################################################################

tic("Predicting SWM data")
setwd(paste0(wd,"/script"))
source("3-Predictions.R")
toc()

##################################################################################
##  Define required functions
##################################################################################

 tic("Define Core Functions")
 setwd(paste0(wd,"/script"))
 source("4a-DefineMunicipalMFA.R")
 source("4b-SystemofEquations.R")
 source("4c-DefineCoeffsOutputs.R")
 source("4d-DefineSummaryFunction.R")
 source("4e-DefineSensitivityAnalysis.R")
 toc()

##################################################################################
##  Run MFA for municipalities in each country
##################################################################################

 tic("Run Probabilistic MFA by municipality")
 setwd(paste0(wd,"/script"))
 source("5-RunMFA.R")
 toc()
 
 ##################################################################################
 ##  Aggregate MFA to higher spatial scales
 ##################################################################################
 
 tic("Aggregating MFA")
 setwd(paste0(wd,"/script"))
 source("6-AggregateMFA.R")
 toc()
 
 ##################################################################################
 ##  Save outputs                                                     
 ##################################################################################
 tic("Save results")
 hs <- createStyle(textDecoration = "BOLD")
 setwd(paste0(wd,"/outputs"))
 write.xlsx(Results[[1]], file = paste0("SPOT_MFA_Municipal_results_",version,"_",iterations,"i.xlsx"), headerStyle = hs, withFilter = T)
 write.xlsx(Results[[2]], file = paste0("SPOT_MFA_Provincial_results_",version,"_",iterations,"i.xlsx"), headerStyle = hs, withFilter = T)
 write.xlsx(Results[[3]], file = paste0("SPOT_MFA_National_results_",version,"_",iterations,"i.xlsx"), headerStyle = hs, withFilter = T)
 
  if(run_sensitivity_analysis==T){
   saveRDS(sensitivity.results,file=paste0("SPOT_Sensitivity_analysis_results_",version,"_",iterations,"i.rds"))
  }
 
 if(length(raw.save)>0){
   if(!is.na(Large_dir)){
     setwd(Large_dir)
   }
   saveRDS(raw.save, file = paste0("SPOT_MFA_raw_results_",version,"_",iterations,"i.rds"))
 }
 toc()

 
 #log end time
 end.time<-Sys.time()
 #Calculate and report time taken
 print(end.time - start.time)
 
 
 