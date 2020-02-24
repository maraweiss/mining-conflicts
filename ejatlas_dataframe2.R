# get data frame in matrix form
dfconflicts <- data.frame(t(sapply(conflicts,c)))

# select columns
dfconflicts <- dfconflicts %>%
  select(Country,AccuracyOfLocation, SpecificCommodities, ProjectArea, LevelOfInvestment, TypeOfPopulation, AffectedPopulation, CompanyNames, EnvironmentalJusticeOrganizations, ReactionStage, GroupsMobilizing, FormsOfMobilization, EnvironmentalImpactsVisible, EnvironmentalImpactsPotential, HealthImpactsVisible, HealthImpactsPotential, SocioeconomicImpactsVisible, SocioeconomicImpactsPotential, ConflictOutcome, EscalationStage)

# investigate data frame



# create dummy variables
library(qdapTools)

dfconflicts <- cbind(dfconflicts[1:20], mtabulate(dfconflicts$Country))

dfconflicts$AccuracyOfLocation <- factor(x = dfconflicts$AccuracyOfLocation, levels = c("low", "medium", "high"))

dfconflicts <- cbind(dfconflicts[1:40], mtabulate(dfconflicts$TypeOfPopulation)) # remove unknown
dfconflicts <- dfconflicts %>%
  select(-Unknown)

dfconflicts <- cbind(dfconflicts[1:43], mtabulate(dfconflicts$SpecificCommodities))

dfconflicts <- cbind(dfconflicts[1:105], mtabulate(dfconflicts$ReactionStage))

dfconflicts <- dfconflicts %>%
  select(-unknown)

dfconflicts <- cbind(dfconflicts[1:109], mtabulate(dfconflicts$GroupsMobilizing))

# impacts either visible yes/no or potential yes/no

# DV as factor
dfconflicts$EscalationStage <- as.factor(unlist(dfconflicts$EscalationStage))

# test multinom() function
library(nnet)
library(texreg)
library(stargazer)
?multinom

multinom1 <- multinom(formula = EscalationStage ~ Argentina + Bolivia + Brazil + Chile + Colombia + CostaRica + DominicanRepublic + Ecuador + ElSalvador + Guatemala + Guyana + Honduras + Jamaica + Mexico + Nicaragua + Panama + Peru + PuertoRico + Uruguay + AccuracyOfLocation + Rural + Urban + gold + copper + silver + molybdenum + land + iron_ore + zinc + lead + coal + gravel + water + sand + aluminumbauxite + lithium + nickel + uranium + diamonds + limestone + antimony + cement + coltan + manganese + rare_metals + chemical_products + crude_oil + ferronickel + phosphate + potassium + tourism_services + asbestos + chrome + cobalt + electricity + gemstones + industrial_waste + mercury + niobium + recycled_metals + steel + tantalite + timber + tin + tungsten + vanadium + barite + biological_resources + chrysotile + clay + coke + ferroginous_clay + kaolin + lime + phosphorus + pozzolana + rubber + salt + silica + sodium_borate + stone_materials + tantalum + thorianite + preventive + reaction + reparations + economic_actors + organization + excluded_marginalized, data = dfconflicts )
summary(multinom1)
screenreg(list(multinom1), custom.model.names = c("A"))

