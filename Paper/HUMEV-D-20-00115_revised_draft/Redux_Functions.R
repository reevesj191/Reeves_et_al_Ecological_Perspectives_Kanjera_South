#### Core use life functions based on Douglass 2018 #### 
#### Author: Jonathan Reeves

core.redux.kjs <- function(cores, vars){
  
  # data: a dataframe that contains all of the lithic data 
  
  # vars: A vector of column names that represent the variables needed to estimate core reduction.
  #             order matters. i.e c(ID,flake_scars, exploitation surfaces, surface interactions, Cortex_Proportion)
  
xdata <- data.frame(ID = cores[,vars[1]], 
                      Flake_Scars = cores[,vars[2]],
                      Interactions = cores[,vars[3]],
                      Average_Angle = cores[,vars[4]])
  
  
  xdata$Interactions_trs <- sqrt(xdata$Interactions)
  xdata$FlakScars_tr <- sqrt(xdata$Flake_Scars)
  xdata$AverageAngle_tr <- xdata$Average_Angle/100
  
  xdata$raw_predict <- (xdata$FlakScars_tr * -0.78) + ( (xdata$Interactions_tr) * 0.71) + 
    (xdata$AverageAngle_tr * -4.53) +  (xdata$AverageAngle_tr * xdata$FlakScars_tr * 2.46)
  
  xdata$predicted <- (1 / (1 + exp(0 - xdata$raw_predict))) 
  return(xdata$predicted)
}


### Flake Sequence Equation from Braun et al 2008.
flake.sequence <- function(flakes,vars){
  
  
  xdata <- data.frame(ID = flakes[,vars[1]], 
                      Length = flakes[,vars[2]],
                      Width = flakes[,vars[3]],
                      Flake_Scars = flakes[,vars[4]],
                      Scar_Directions = flakes[,vars[5]],
                      Platform_Facets = flakes[,vars[6]],
                      Cortex_Proportion = flakes[,vars[7]])
  
  xdata$Cortex_Qual <- NA
  xdata$Cortex_Qual[xdata$Cortex_Proportion == 0] <- 8
  xdata$Cortex_Qual[xdata$Cortex_Proportion > 0 & xdata$Cortex_Proportion <= .10] <- 7
  xdata$Cortex_Qual[xdata$Cortex_Proportion > .10 & xdata$Cortex_Proportion <= .30] <- 6
  xdata$Cortex_Qual[xdata$Cortex_Proportion > .30 & xdata$Cortex_Proportion <= .50] <- 5
  xdata$Cortex_Qual[xdata$Cortex_Proportion > .50 & xdata$Cortex_Proportion <= .70] <- 4
  xdata$Cortex_Qual[xdata$Cortex_Proportion > .70 & xdata$Cortex_Proportion <= .90] <- 3
  xdata$Cortex_Qual[xdata$Cortex_Proportion > .90 & xdata$Cortex_Proportion <= .99] <- 2
  xdata$Cortex_Qual[xdata$Cortex_Proportion == 1] <- 1
  
  xdata$b.Scar.Dir <- xdata$Scar_Directions * .628
  xdata$b.Plat.Facets <- xdata$Platform_Facets * 1.008
  xdata$b.scarXlogLXW <- (xdata$Flake_Scars/log10(xdata$Length*xdata$Width)) *.159
  xdata$b.cortexXlogLXW <- ((xdata$Cortex_Proportion)/log10(xdata$Length *xdata$Width)) *.097
  xdata$Flake_Stage <- xdata$b.Scar.Dir + xdata$b.Plat.Facets + xdata$b.scarXlogLXW + xdata$b.cortexXlogLXW
  xdata$Flake_Stage <- xdata$Flake_Stage ^2  
  return(round(xdata$Flake_Stage))
  
}

