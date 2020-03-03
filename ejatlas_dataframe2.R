# get data frame in matrix form
dfconflicts <- data.frame(t(sapply(conflicts,c)))

# select columns
dfconflicts <- dfconflicts %>%
  select(Country,AccuracyOfLocation, SpecificCommodities, ProjectArea, LevelOfInvestment, TypeOfPopulation, AffectedPopulation, CompanyNames, EnvironmentalJusticeOrganizations, ReactionStage, GroupsMobilizing, FormsOfMobilization, EnvironmentalImpactsVisible, EnvironmentalImpactsPotential, HealthImpactsVisible, HealthImpactsPotential, SocioeconomicImpactsVisible, SocioeconomicImpactsPotential, ConflictOutcome, EscalationStage)

# investigate data frame

# create dummy variables
library(qdapTools)

dfconflicts <- cbind(dfconflicts[1:20], mtabulate(dfconflicts$Country))

dfconflicts <- cbind(dfconflicts[1:40], mtabulate(dfconflicts$AccuracyOfLocation))

dfconflicts <- cbind(dfconflicts[1:43], mtabulate(dfconflicts$TypeOfPopulation)) # remove unknown
dfconflicts <- dfconflicts %>%
  select(-Unknown)

dfconflicts <- cbind(dfconflicts[1:46], mtabulate(dfconflicts$SpecificCommodities))

dfconflicts <- cbind(dfconflicts[1:72], mtabulate(dfconflicts$ReactionStage))

dfconflicts <- dfconflicts %>%
  select(-unknown)

dfconflicts <- cbind(dfconflicts[1:76], mtabulate(dfconflicts$GroupsMobilizing))

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
library(nnet)
library(texreg)
library(stargazer)
?multinom

#multinom1 <- multinom(formula = EscalationStage ~ Argentina + Bolivia + Brazil + Chile + Colombia + CostaRica + DominicanRepublic + Ecuador + ElSalvador + Guatemala + Guyana + Honduras + Jamaica + Mexico + Nicaragua + Panama + Peru + PuertoRico + Uruguay + AccuracyOfLocation + Rural + Urban + gold + copper + silver + molybdenum + land + iron_ore + zinc + lead + coal + gravel + water + sand + aluminumbauxite + lithium + nickel + uranium + diamonds + limestone + antimony + cement + coltan + manganese + rare_metals + chemical_products + crude_oil + ferronickel + phosphate + potassium + tourism_services + asbestos + chrome + cobalt + electricity + gemstones + industrial_waste + mercury + niobium + recycled_metals + steel + tantalite + timber + tin + tungsten + vanadium + barite + biological_resources + chrysotile + clay + coke + ferroginous_clay + kaolin + lime + phosphorus + pozzolana + rubber + salt + silica + sodium_borate + stone_materials + tantalum + thorianite + preventive + reaction + reparations + economic_actors + organization + excluded_marginalized + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts )
#summary(multinom1)
#screenreg(list(multinom1), custom.model.names = c("A"))

multinom2 <- multinom(formula = EscalationStage ~ Argentina + Bolivia + Brazil + Chile + Colombia + CostaRica + DominicanRepublic + Ecuador + ElSalvador + Guatemala + Guyana + Honduras + Jamaica + Mexico + Nicaragua + Panama + Peru + PuertoRico + Uruguay + regional_level + country_level + Rural + Urban + precious_metals + base_metals + light_metals + technology_elements + ferrous_metals + ferroalloy_metals + agricultural_chemical_minerals + nonmetallic_minerals + land + energy_mineral_resources + water + diamonds + chemical_products+ tourism_services + electricity + gemstones + industrial_waste + recycled_metals + timber + biological_resources + pozzolana + rubber + silica +thorianite + stone_materials + preventive + reaction + reparations + local_people + economic_actors + organization + excluded_marginalized + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts )
# Baseline2: Venezuela, country_level, Semiurban

screenreg(list(multinom2), custom.model.names = c("B"))

multinom3 <- multinom(formula = EscalationStage ~ Argentina + Bolivia + Brazil + Chile + Colombia + CostaRica + DominicanRepublic + Ecuador + ElSalvador + Guatemala + Guyana + Honduras + Jamaica + Mexico + Nicaragua + Panama + Peru + PuertoRico + Uruguay + precious_metals + base_metals + light_metals + technology_elements + ferrous_metals + ferroalloy_metals + agricultural_chemical_minerals + nonmetallic_minerals + land + energy_mineral_resources + water + diamonds + chemical_products+ tourism_services + electricity + gemstones + industrial_waste + recycled_metals + timber + biological_resources + pozzolana + rubber + silica +thorianite + stone_materials + preventive + reaction + reparations + local_people + economic_actors + organization + excluded_marginalized + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts )
# Baseline2: Venezuela, country_level, Semiurban

screenreg(list(multinom2), custom.model.names = c("C"))
