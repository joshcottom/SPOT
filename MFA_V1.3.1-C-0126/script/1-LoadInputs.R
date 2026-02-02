##################################################################################
##
## Script name:   1-LoadInputs.R
##
## Version:       1.3.1-C-0126
## 
## Author:        Dr Josh Cottom (J.Cottom@Imperial.ac.uk)
##
## Organisation:  Imperial College London - Dr Costas Velis (C.Velis@Imperial.ac.uk) Research Group.
##
## Last updated:  January 2026
##
## Parent script  0-MasterScript.R
##
## Description:   Loads inputs for the probabilistic MFA
##
##################################################################################

##################################################################################
##  Load the excel based input files                                                               
##################################################################################

##  Set the working directory as the inputs location
setwd("../inputs")

##  Load the excel PDF sheet
PDFs <- read_excel(workbook,sheet="PDFs")


##  Load the excel inputs sheet specifying the first 7 columns (names) as text and
##  the remaining columns (population & parameters) to be numeric
inputs <- read_excel(workbook,
                     sheet="MFA_inputs",
                     skip=2,
                     col_types = c(rep("text",8),
                                   rep("numeric",14),
                                   rep("numeric",length(PDFs$Input_ID)*4)))

##  Load the processes sheet
processes <- read_excel(workbook,sheet = "Processes")

##  Load the excel outputs sheet
coeffs <- read_excel(workbook,sheet = "Coefficients")

##  Load the excel outputs sheet
outputs <- read_excel(workbook,sheet="Outputs")

##  Load the excel street sweeping sheet
Street_sweep <- read_excel(workbook,sheet="Street_sweep")

##  Extract the unique IDs of all the municipalities
uniqueID <- inputs$Unique_ID

##  Load the list of municipalities for which the full raw results should be saved
munic.raw <- read_excel(workbook,sheet="Retain_full_munic")$Unique_ID

##  Create an empty warning list
warnings <- list()

##  Check for empty inputs to population, rurality, or socio-economic data and stop if any are empty
if(sum(is.na(inputs[,9:22])==TRUE)>0){
        warnings <- c(warnings,paste0("There are some blank inputs  relating to population, rurality, or socioeconomic data. Model halted"))
        warning("There are some blank inputs relating to population, rurality, or socioeconomic data. Model halted")
        stop()
}  



## list the parameters sequentially
params <- c(rbind(PDFs$Parameter_1,PDFs$Parameter_2,PDFs$Parameter_3,PDFs$Parameter_4))

##  Check for empty inputs to SWM data (where a parameter input is required)
for(i in 1:length(params)){
  if(any(is.na(inputs[,22+i])) & params[i]!="NA"){
    
    print(i)
    
    warnings <- c(warnings,paste0("There are some missing input parameters. Model halted"))
    warning("There are some missing input parameters. Model halted")
    stop()
  }  

}

  
  
  





  


