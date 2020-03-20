# multinomial logistic regression #

# load libraries
library(rjson)
library(plyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(forcats)

# for regression
library(qdapTools)
library(nnet)
library(texreg)
library(stargazer)

# set working directory
if (dir.exists("~/Master thesis/Code/mining-conflicts")){
  setwd("~/Master thesis/Code/mining-conflicts")
} else{
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# get data frame in matrix form
dfconflicts <- data.frame(t(sapply(conflicts,c)))

# select columns
dfconflicts <- dfconflicts %>%
  select(Country, SpecificCommodities, CompanyNames, ReactionStage, GroupsMobilizing, FormsOfMobilization, EnvironmentalImpactsVisible, EnvironmentalImpactsPotential, HealthImpactsVisible, HealthImpactsPotential, SocioeconomicImpactsVisible, SocioeconomicImpactsPotential, ConflictOutcome, EscalationStage)

# create dummy variables
dfconflicts <- cbind(dfconflicts[1:14], mtabulate(dfconflicts$Country))

dfconflicts <- cbind(dfconflicts[1:34], mtabulate(dfconflicts$SpecificCommodities))
dfconflicts <- cbind(dfconflicts[1:51], mtabulate(dfconflicts$ReactionStage))
# dfconflicts <- cbind(dfconflicts[1:96], mtabulate(dfconflicts$ReactionStage))
 
dfconflicts <- dfconflicts %>%
   select(-unknown)

dfconflicts <- cbind(dfconflicts[1:55], mtabulate(dfconflicts$GroupsMobilizing))
# dfconflicts <- cbind(dfconflicts[1:100], mtabulate(dfconflicts$GroupsMobilizing))
dfconflicts <- cbind(dfconflicts[1:59], mtabulate(dfconflicts$CompanyNames))
# dfconflicts <- cbind(dfconflicts[1:104], mtabulate(dfconflicts$CompanyNames))

# impacts either visible yes/no or potential yes/no
dfconflicts$EnvironmentalImpactsVisible <- ifelse(dfconflicts$EnvironmentalImpactsVisible == "none", 0, 1)
dfconflicts$EnvironmentalImpactsPotential <- ifelse(dfconflicts$EnvironmentalImpactsPotential == "none", 0, 1)
dfconflicts$HealthImpactsVisible <- ifelse(dfconflicts$HealthImpactsVisible == "none", 0, 1)
dfconflicts$HealthImpactsPotential <- ifelse(dfconflicts$HealthImpactsPotential == "none", 0, 1)
dfconflicts$SocioeconomicImpactsVisible <- ifelse(dfconflicts$SocioeconomicImpactsVisible == "none", 0, 1)
dfconflicts$SocioeconomicImpactsPotential <- ifelse(dfconflicts$SocioeconomicImpactsPotential == "none", 0, 1)

# DV as factor
dfconflicts$EscalationStage <- as.factor(unlist(dfconflicts$EscalationStage))

# model with all commodities
# multinom5 <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + colombia + costarica + dominicanrepublic + ecuador + elsalvador + guatemala + guyana + honduras + jamaica + mexico + nicaragua + panama + peru + puertorico + venezuela + gold + copper + silver + molybdenum + land + iron_ore + zinc + lead + coal + water + gravel + sand + aluminumbauxite + lithium + nickel + uranium + diamonds + limestone + antimony + coltan + manganese + rare_metals + cement + chemical_products + crude_oil + ferronickel + phosphate + tourism_services + asbestos + chrome + cobalt + electricity + gemstones + industrial_waste + mercury + niobium + potassium + recycled_metals + steel + tantalite + timber + tin + tungsten + vanadium + barite + biological_resources + chrysotile + clay + coke + ferroginous_clay + kaolin + lime + phosphorus + pozzolana + rubber + salt + silica + sodium_borate + stone_materials + tantalum + thorianite + titanium_ores +        preventive + reaction + reparations + economic_actors + excluded_marginalized + local_people + organization + foreign + illegal + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts )
# # BL: uruguay, latent
# screenreg(list(multinom5), custom.model.names =  c("All commodities"))


# change baseline
#dfconflicts$EscalationStage <- relevel(x=dfconflicts$EscalationStage, ref="4")

multinom4 <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + costarica + dominicanrepublic + ecuador + elsalvador + guatemala + guyana + honduras + jamaica + mexico + nicaragua + panama + peru + puertorico + venezuela + uruguay + agricultural_chemical_minerals + base_metals + biological_resources + diamonds + electricity + energy_mineral_resources + ferroalloy_metals + ferrous_metals + gemstones + industrial_waste + light_metals + nonmetallic_minerals + precious_metals + radioactive_ores + recycled_metals + technology_elements + tourism_services + preventive + reaction + reparations + economic_actors + excluded_marginalized + local_people + organization + foreign_company + illegal_mining + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts, maxit=500)
# BL: colombia, latent
screenreg(list(multinom4), custom.model.names =  c("Types of commodities"))


# interpret the odds
coefficients(multinom4)
exp(coef(multinom4))

# multinom function code
nnet::multinom

# predicted probabilities
head(pp <- fitted(multinom4))

