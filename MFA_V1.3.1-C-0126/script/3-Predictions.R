##################################################################################
##
## Script name:   3-AdjustPredictions.R
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
## Description:   Adjusts random forest quantile regression predictions based on 
##                if Kernal density estimation is required, removal of outliers 
##                and rurality. Any predictions that have actual measured data are
##                also replaced with actual values.
##
##################################################################################

##################################################################################
##  Create a function to sample input PDFs
##################################################################################

sample.pdf<- function(Input_ID){
  
  ## Get the distribution type
  distribution <- PDFs[which(PDFs$Input_ID==Input_ID),"PDF"][[1]]
  
  Param1 <- inputs[m,paste(Input_ID,"1",sep = "_")][[1]]
  Param2 <- inputs[m,paste(Input_ID,"2",sep = "_")][[1]]
  if(as.vector(PDFs[which(PDFs$Input_ID==Input_ID),"No_Parameters"])>2){Param3 <- inputs[m,paste(Input_ID,"3",sep = "_")][[1]]}
  if(as.vector(PDFs[which(PDFs$Input_ID==Input_ID),"No_Parameters"])>3){Param4 <- inputs[m,paste(Input_ID,"4",sep = "_")][[1]]}

  if(distribution=="Beta-PERT"){
    sample.pdf <- rpert(iterations,min = Param1, mode = Param2,max = Param3, shape=Param4)
  }
  if(distribution=="Triangular"){
    sample.pdf <- rtriang(iterations,min = Param1, mode = Param2,max = Param3)
  }
  if(distribution=="Normal"){
    sample.pdf <- rtruncnorm(iterations, a=Param3 , b=Param4, mean = Param1, sd = Param2)
  }
  if(distribution=="Uniform"){
    sample.pdf <- runif(iterations,min = Param1, max = Param2)
  }
  return(sample.pdf)
}

##################################################################################
##  Estimate total litter as global variable                                         
##################################################################################

## Estimate the total litter for the European data (global not by municipality)
LT <- rtruncnorm(iterations, a=0 , b=100, mean = 0.81, sd = 0.15)/
  (rpert(iterations,min = 70, mode = 95,max = 97.5, shape=4)/100)

