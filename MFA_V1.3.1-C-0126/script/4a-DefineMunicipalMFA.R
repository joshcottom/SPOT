##################################################################################
##
## Script name:   5a-DefineMunicipalMFA.R
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
## Description:   Create a function to run the MFA for each municipality
##
##################################################################################

#################################################################################
##  Create a function to run the MFA for each municipality (m)                                              
#################################################################################
MFA.municipal <- function(m){

  ##################################################################################
  ##  Sample the inputs for the municipality                                                    
  ##################################################################################

    ## Sample from the input PDFs for the municipality
  
  for(i in PDFs$Input_ID){
    assign(i,sample.pdf(i))
  }

  
   ## Calculate street sweeping efficiency (S) for the municipality by the different degrees of urbanisation
  S.tmp <- Street_sweep[which(Street_sweep$Income_level_ID==2),c(2,5,11:14)]
  
  
  S <- inputs$UCentre_share[m] * rpert(iterations,
                                            as.numeric(S.tmp[1,3]),
                                            as.numeric(S.tmp[1,4]),
                                            as.numeric(S.tmp[1,5]),
                                            as.numeric(S.tmp[1,6]))+
    inputs$DUC_share[m] * rpert(iterations,
                                     as.numeric(S.tmp[2,3]),
                                     as.numeric(S.tmp[2,4]),
                                     as.numeric(S.tmp[2,5]),
                                     as.numeric(S.tmp[2,6]))+
    inputs$SDUC_share[m] * rpert(iterations,
                                      as.numeric(S.tmp[3,3]),
                                      as.numeric(S.tmp[3,4]),
                                      as.numeric(S.tmp[3,5]),
                                      as.numeric(S.tmp[3,6]))+
    inputs$SUrb_share[m]* rpert(iterations,
                                     as.numeric(S.tmp[4,3]),
                                     as.numeric(S.tmp[4,4]),
                                     as.numeric(S.tmp[4,5]),
                                     as.numeric(S.tmp[4,6]))+
    inputs$Rural_share[m] * rpert(iterations,
                                       as.numeric(S.tmp[5,3]),
                                       as.numeric(S.tmp[5,4]),
                                       as.numeric(S.tmp[5,5]),
                                       as.numeric(S.tmp[5,6]))
  
  S[S>100] <- 100
  
  ## Add all inputs to a named list
  MFA.inputs <- list("Pop" = rep(inputs$Population[m],iterations),
                     "Urban_Centre_share" = rep(inputs$UCentre_share[m],iterations),
                     "Dense_Urban_Cluster_share" = rep(inputs$DUC_share[m],iterations),
                     "tP1pc" = tP1pc,
                     "C0" = C0,
                     "C0a" = C0a,
                     "tC1" = tC1,
                     "tC2i" = tC2i,
                     "tC2ii" = tC2ii,
                     "tC2iii" = tC2iii,
                     "tC3" = tC3,
                     "WPp" = WPp,
                     "Rural_share" = rep(inputs$Rural_share[m],iterations),
                     "C3i" = C3i,
                     "C8" = C8,
                     "C9" = C9,
                     "C10" = C10,
                     "C11" = C11,
                     "C14" = C14,
                     "C15" = C15,
                     "C16" = C16,
                     "C11a" = C11a,
                     "C14a" = C14a,
                     "C21a" = C21a,
                     "C23aa" = C23aa,
                     "C23ab" = C23ab ,
                     "C24aa" = C24aa,
                     "C24ab" = C24ab,
                     "LT" = LT,
                     "S" = S
                      )

  ##################################################################################
  ##  Calculate the processes (Solve the MFA)                                                     
  ##################################################################################
  
  process.results <- MFA(MFA.inputs)
  
  ##################################################################################
  ##  Calculate the coefficients and outputs                                                  
  ##################################################################################
  
  coeff.results <- calc_coefficients(process.results)

  output.results <- calc_outputs(process.results,matrix(c(0,inputs$Population[m]),nrow=1))

    res <- list("Processes" = process.results,
              "Coefficients" = coeff.results,
              "Outputs" = output.results)
    
    res2 <- list(res,MFA.inputs)
    
    return(res2)
}
