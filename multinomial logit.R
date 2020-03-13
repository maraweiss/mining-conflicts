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

dfconflicts <- dfconflicts %>%
  select(-unknown)

dfconflicts <- cbind(dfconflicts[1:55], mtabulate(dfconflicts$GroupsMobilizing))

dfconflicts <- cbind(dfconflicts[1:59], mtabulate(dfconflicts$CompanyNames))

# impacts either visible yes/no or potential yes/no
dfconflicts$EnvironmentalImpactsVisible <- ifelse(dfconflicts$EnvironmentalImpactsVisible == "none", 0, 1)
dfconflicts$EnvironmentalImpactsPotential <- ifelse(dfconflicts$EnvironmentalImpactsPotential == "none", 0, 1)
dfconflicts$HealthImpactsVisible <- ifelse(dfconflicts$HealthImpactsVisible == "none", 0, 1)
dfconflicts$HealthImpactsPotential <- ifelse(dfconflicts$HealthImpactsPotential == "none", 0, 1)
dfconflicts$SocioeconomicImpactsVisible <- ifelse(dfconflicts$SocioeconomicImpactsVisible == "none", 0, 1)
dfconflicts$SocioeconomicImpactsPotential <- ifelse(dfconflicts$SocioeconomicImpactsPotential == "none", 0, 1)

# DV as factor
dfconflicts$EscalationStage <- as.factor(unlist(dfconflicts$EscalationStage))

# test multinom() function


#multinom1 <- multinom(formula = EscalationStage ~ Argentina + Bolivia + Brazil + Chile + Colombia + CostaRica + DominicanRepublic + Ecuador + ElSalvador + Guatemala + Guyana + Honduras + Jamaica + Mexico + Nicaragua + Panama + Peru + PuertoRico + Uruguay + AccuracyOfLocation + Rural + Urban + gold + copper + silver + molybdenum + land + iron_ore + zinc + lead + coal + gravel + water + sand + aluminumbauxite + lithium + nickel + uranium + diamonds + limestone + antimony + cement + coltan + manganese + rare_metals + chemical_products + crude_oil + ferronickel + phosphate + potassium + tourism_services + asbestos + chrome + cobalt + electricity + gemstones + industrial_waste + mercury + niobium + recycled_metals + steel + tantalite + timber + tin + tungsten + vanadium + barite + biological_resources + chrysotile + clay + coke + ferroginous_clay + kaolin + lime + phosphorus + pozzolana + rubber + salt + silica + sodium_borate + stone_materials + tantalum + thorianite + preventive + reaction + reparations + economic_actors + organization + excluded_marginalized + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts )
#summary(multinom1)
#screenreg(list(multinom1), custom.model.names = c("A"))

#multinom2 <- multinom(formula = EscalationStage ~ Argentina + Bolivia + Brazil + Chile + Colombia + CostaRica + DominicanRepublic + Ecuador + ElSalvador + Guatemala + Guyana + Honduras + Jamaica + Mexico + Nicaragua + Panama + Peru + PuertoRico + Uruguay + regional_level + country_level + Rural + Urban + precious_metals + base_metals + light_metals + technology_elements + ferrous_metals + ferroalloy_metals + agricultural_chemical_minerals + nonmetallic_minerals + land + energy_mineral_resources + water + diamonds + chemical_products+ tourism_services + electricity + gemstones + industrial_waste + recycled_metals + timber + biological_resources + pozzolana + rubber + silica +thorianite + stone_materials + preventive + reaction + reparations + local_people + economic_actors + organization + excluded_marginalized + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts )
# Baseline2: Venezuela, country_level, Semiurban

#screenreg(list(multinom2), custom.model.names = c("B"))

# multinom3 <- multinom(formula = EscalationStage ~ Argentina + Bolivia + Brazil + Chile + Colombia + CostaRica + DominicanRepublic + Ecuador + ElSalvador + Guatemala + Guyana + Honduras + Jamaica + Mexico + Nicaragua + Panama + Peru + PuertoRico + Uruguay + precious_metals + base_metals + light_metals + technology_elements + ferrous_metals + ferroalloy_metals + agricultural_chemical_minerals + nonmetallic_minerals + land + energy_mineral_resources + water + diamonds + chemical_products+ tourism_services + electricity + gemstones + industrial_waste + recycled_metals + timber + biological_resources + pozzolana + rubber + silica +thorianite + stone_materials + preventive + reaction + reparations + local_people + economic_actors + organization + excluded_marginalized + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts )
# # Baseline2: Venezuela, country_level, Semiurban# 
# screenreg(list(multinom3), custom.model.names = c("C"))

multinom4 <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + colombia + costarica + dominicanrepublic + ecuador + elsalvador + guatemala + guyana + honduras + jamaica + mexico + nicaragua + panama + peru + puertorico + venezuela + agricultural_chemical_minerals + base_metals + biological_resources + diamonds + electricity + energy_mineral_resources + ferroalloy_metals + ferrous_metals + gemstones + industrial_waste + light_metals + nonmetallic_minerals + precious_metals + radioactive_ores + recycled_metals + technology_elements + tourism_services + preventive + reaction + reparations + economic_actors + excluded_marginalized + local_people + organization + foreign + illegal + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts )
# BL: uruguay, latent
screenreg(list(multinom4), custom.model.names =  c("c"))
