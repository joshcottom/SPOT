##################################################################################
##
## Script name:   6-AggregateMFA.R
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
## Description:   Aggregates the probabilistic MFA to other levels
##
##################################################################################

##################################################################################
##  Aggregate results to Provincial level and summarise
##################################################################################

## Set a provincial index
p<-1

##  Aggregate the results across all municipalities in the province,
for(province in unique(inputs$Name_1)){
  
  ##index the rows for the current province
  p.rows <- which(inputs$Name_1==province)
  
  ## aggregate the raw results for the current province 
  raw.process.prov[p,,] <- t(apply(raw.mc[,,p.rows],MARGIN=c(1,2),function(x)sum(x,na.rm=T)))
  
  raw.pwg_pop.prov[p,,] <-  t(apply(raw.pwg_pop[,,p.rows],MARGIN=c(1,2),function(x)sum(x,na.rm=T)))
  
  ##increase provincial index
  p <- p+1
}

rm(p)

## Back calculate coefficients and outputs for each iteration at a Provincial level
for(i in 1:iterations){
  raw.coeff.prov[,,i] <- calc_coefficients(raw.process.prov[,,i])
  raw.output.prov[,,i] <- calc_outputs(raw.process.prov[,,i],
                                       pop.agg.prov[match(rownames(raw.process.prov),pop.agg.prov$Group.1),])
  raw.output.prov[,c("pwg","rigid_pwg","flex_pwg","people_no_col"),i] <- raw.pwg_pop.prov[,,i]

}

match(rownames(raw.process.prov),pop.agg.prov$Group.1)

## Summarise National results and add to results table
Results[[2]][[1]][,(length(D2)+1):ncol(Results[[2]][[1]])] <- summarise(raw.process.prov)
Results[[2]][[2]][,(length(D2)+1):ncol(Results[[2]][[2]])] <- summarise(raw.coeff.prov)
Results[[2]][[3]][,(length(D2)+1):ncol(Results[[2]][[3]])] <- summarise(raw.output.prov)



##################################################################################
##  Aggregate results to National level and summarise
##################################################################################

## Aggregate results to national level
raw.process.nat <- apply(raw.mc,MARGIN=c(1,2),function(x)sum(x,na.rm=T))
colnames(raw.process.nat) <- processes$Process_ID
raw.pwg_pop.nat <- apply(raw.pwg_pop,MARGIN=c(1,2),function(x)sum(x,na.rm=T))

## Back calculate coefficients and outputs for each iteration at a national level
for(i in 1:iterations){
  raw.coeff.nat[,i] <- calc_coefficients(raw.process.nat[i,])
  raw.output.nat[,i] <- calc_outputs(raw.process.nat[i,],pop.agg.nat)
  raw.output.nat[c("pwg","rigid_pwg","flex_pwg","people_no_col"),i] <- raw.pwg_pop.nat[i,]
}

## Summarise National results and add to results table
Results[[3]][[1]][,(length(D3)+1):ncol(Results[[3]][[1]])] <- summarise(raw.process.nat)
Results[[3]][[2]][,(length(D3)+1):ncol(Results[[3]][[2]])] <- summarise(t(raw.coeff.nat))
Results[[3]][[3]][,(length(D3)+1):ncol(Results[[3]][[3]])] <- summarise(t(raw.output.nat))


##################################################################################
##  Save raw results if required
##################################################################################

A <- list("Municipalities" = raw.save.m)
B <- list("Province" = list("Processes" = raw.process.prov,
                             "Coefficients" = raw.coeff.prov,
                             "Outputs" = raw.output.prov))

C <- list("Country" = list("Processes" = t(raw.process.nat),
                             "Coefficients" = raw.coeff.nat,
                             "Outputs" = raw.output.nat))

## Save the raw MFA results for aggregations if required
if(length(raw.save.m)>0 & retain_full_munic==T){
  raw.save <- append(raw.save,A)
}

if(retain_full_province==T){
  raw.save <- append(raw.save,B)
}

if(retain_full_country==T){
  raw.save <- append(raw.save,C)
}

