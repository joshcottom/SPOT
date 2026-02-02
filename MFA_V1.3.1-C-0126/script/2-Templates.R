##################################################################################
##
## Script name:   2-Templates.R
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
## Description:   Creates templates to store sampled inputs and results
##
##################################################################################

##################################################################################
##  Define a function that creates a raw MFA result template for municipal level
##  results for a given country                                                        
##################################################################################

raw.mc_template <- function(m.prov){
  
  raw.mc <- array(NA,dim=c(iterations,
                           length(processes$Process_ID),
                           length(m.prov)))
  
  return(raw.mc)
  
}


##################################################################################
##  Create raw MFA result template for aggregated level results                                                        
##################################################################################

##  Create arrays for holding raw provincial level results
raw.process.prov <- array(NA,dim=c(length(unique(inputs$Name_1)),
                                  length(processes$Process_ID),
                                  iterations))

raw.coeff.prov <- array(NA,dim=c(length(unique(inputs$Name_1)),
                                  length(coeffs$Coeff_ID),
                                  iterations))

raw.output.prov <- array(NA,dim=c(length(unique(inputs$Name_1)),
                                  length(outputs$Output_ID),
                                  iterations))

raw.pwg_pop.prov <- array(NA,dim=c(length(unique(inputs$Name_1)),
                                   4,
                                   iterations))

rownames(raw.process.prov) <- unique(inputs$Name_1)
rownames(raw.coeff.prov) <- unique(inputs$Name_1)
rownames(raw.output.prov) <- unique(inputs$Name_1)
colnames(raw.process.prov) <- processes$Process_ID
colnames(raw.coeff.prov) <- coeffs$Coeff_ID
colnames(raw.output.prov) <- outputs$Output_ID


##  Create arrays for holding raw national aggregation results
raw.process.nat <- matrix(NA,nrow = length(processes$Process_ID),
                            ncol = iterations)

raw.coeff.nat <- matrix(NA,nrow = length(coeffs$Coeff_ID),
                          ncol = iterations)

raw.output.nat <- matrix(NA,nrow = length(outputs$Output_ID),
                          ncol = iterations)

raw.pwg_pop.nat <- matrix(NA,nrow = 4,
                          ncol = iterations)

rownames(raw.process.nat) <- processes$Process_ID
rownames(raw.coeff.nat) <- coeffs$Coeff_ID
rownames(raw.output.nat) <- outputs$Output_ID


##################################################################################
##  Create summarized results template                                                        
##################################################################################

##  Extract the reporting statistics names and replace with cleaned names for quantiles.
stat.names <- gsub("Quantiles.","",names(unlist(summary.stats)))

##  Create a function to generate dataframes to hold the summarized results of processes, 
##  coefficient and other outputs from the Monte Carlo MFA results
summarised_results <- function(rows,description_cols){
  x <- data.frame(matrix(NA,
                         ncol = length(processes$Process_ID)+length(description_cols),
                         nrow = length(rows)*(length(stat.names))))
  
  y <-  data.frame(matrix(NA,
                          ncol = length(coeffs$Coeff_ID)+length(description_cols),
                          nrow = length(rows)*(length(stat.names))))
  
  z <- data.frame(matrix(NA,
                         ncol = length(outputs$Output_ID)+length(description_cols),
                         nrow = length(rows)*(length(stat.names))))

  ##  Add column names
    colnames(x) <- c(description_cols,processes$Process_ID)
    colnames(y) <- c(description_cols,coeffs$Coeff_ID)
    colnames(z) <- c(description_cols,outputs$Output_ID)
    
  ## Add to a list and return
    return(list("Processes" = x,
                "Coefficients" =y,
                "Outputs" = z))
    
}


##  Create the summarized result templates description column names
D1 <- c("Statistic","Country","ISO3","Name_1","Name_2","Name_3","Name_4","Unique_ID","Population","Rural_share","Deg_Urban_Lv1","Deg_Urban_Lv2")
D2 <- D1[c(1:4,9:10)]
D3 <- D1[c(1:3,9:10)]

Results <- list("Municipal.results" = Municipal.results <- summarised_results(rows = uniqueID,
                                                                              description_cols = D1),
               "Provincial.results" = Provincial.results <- summarised_results(rows = unique(inputs$Name_1),
                                       description_cols = D2),
               "National.results" = Other_aggregations.results <- summarised_results(rows = unique(inputs$ISO3),
                                                 description_cols = D3)
              )

##################################################################################
##  Populate the result templates with basic information                                                  
##################################################################################

##  Populate Municipal results data frames
for(i in 1:3){
Results[[1]][[i]]$Statistic <- rep(stat.names,length(uniqueID))
Results[[1]][[i]]$Country <- rep(inputs$Country,each=length(stat.names))
Results[[1]][[i]]$ISO3 <- rep(inputs$ISO3,each=length(stat.names))
Results[[1]][[i]]$Name_1 <- rep(inputs$Name_1,each=length(stat.names))
Results[[1]][[i]]$Name_2 <- rep(inputs$Name_2,each=length(stat.names))
Results[[1]][[i]]$Name_3 <- rep(inputs$Name_3,each=length(stat.names))
Results[[1]][[i]]$Name_4 <- rep(inputs$Name_3,each=length(stat.names))
Results[[1]][[i]]$Unique_ID <- rep(inputs$Unique_ID,each=length(stat.names))
Results[[1]][[i]]$Population <- rep(inputs$Population,each=length(stat.names))
Results[[1]][[i]]$Rural_share<- rep(inputs$Rural_share,each=length(stat.names))
Results[[1]][[i]]$Deg_Urban_Lv1 <- rep(inputs$DEGURBA_L1,each=length(stat.names))
Results[[1]][[i]]$Deg_Urban_Lv2 <- rep(inputs$DEGURBA_L2,each=length(stat.names))
}

##  Calculate the rural population for each municipality
inputs$rural_pop <- inputs$Rural_share * inputs$Population

## Aggregate population and rural population to the Provincial level
pop.agg.prov <- aggregate(inputs$Population,by=list(inputs$Name_1),FUN=sum)
rural_pop.agg.prov <- aggregate(inputs$rural_pop,by=list(inputs$Name_1),FUN=sum)

## Reorder aggregated population and rural population to match inital order
pop.agg.prov <- pop.agg.prov[match(rownames(raw.process.prov),pop.agg.prov$Group.1),]
rural_pop.agg.prov <- rural_pop.agg.prov[match(rownames(raw.process.prov),rural_pop.agg.prov$Group.1),]

##  Calculate the aggregated rural population share
rural.pop_pct.agg.prov <- rural_pop.agg.prov$x/pop.agg.prov$x

##  Populate Provincial results data frames
for(i in 1:3){
  Results[[2]][[i]]$Statistic <- rep(stat.names,length(unique(inputs$ISO3)))
  Results[[2]][[i]]$Country <- rep(unique(inputs$Country),each=length(stat.names))
  Results[[2]][[i]]$ISO3 <- rep(unique(inputs$ISO3),each=length(stat.names))
  Results[[2]][[i]]$Name_1 <- rep(unique(inputs$Name_1),each=length(stat.names))
  Results[[2]][[i]]$Population<- rep(pop.agg.prov$x,each=length(stat.names))
  Results[[2]][[i]]$Rural_share<- rep(rural.pop_pct.agg.prov,each=length(stat.names))
}




## Aggregate population and rural population to the Provincial level
pop.agg.nat <- sum(inputs$Population)
rural_pop.agg.nat <- sum(inputs$rural_pop)

##  Calculate the aggregated rural population share
rural.pop_pct.agg.nat <- rural_pop.agg.nat/pop.agg.nat

##  Populate Other aggregations results dataframes
for(i in 1:3){
  Results[[3]][[i]]$Statistic <- rep(stat.names,length(unique(inputs$ISO3)))
  Results[[3]][[i]]$Country <- rep(unique(inputs$Country),each=length(stat.names))
  Results[[3]][[i]]$ISO3 <- rep(unique(inputs$ISO3),each=length(stat.names))
  Results[[3]][[i]]$Population<- rep(pop.agg.nat,each=length(stat.names))
  Results[[3]][[i]]$Rural_share<- rep(rural.pop_pct.agg.nat,each=length(stat.names))
}

##################################################################################
##  Create lists to hold the raw outputs for any municipalities or groupings being
##  saved as well as any sensitivity results                                                      
##################################################################################

##  Create a list to hold the raw MFA result for selected municipalities
raw.save.m <- replicate(length(inputs$Unique_ID),list())
names(raw.save.m) <- inputs$Unique_ID
raw.save <- list()
