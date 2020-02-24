# get data frame in matrix form
dfconflicts <- data.frame(t(sapply(conflicts,c)))

# select columns
dfconflicts <- dfconflicts %>%
  select(Country,AccuracyOfLocation, SpecificCommodities, ProjectArea, LevelOfInvestment, TypeOfPopulation, AffectedPopulation, CompanyNames, EnvironmentalJusticeOrganizations, ReactionStage, GroupsMobilizing, FormsOfMobilization, EnvironmentalImpactsVisible, EnvironmentalImpactsPotential, HealthImpactsVisible, HealthImpactsPotential, SocioeconomicImpactsVisible, SocioeconomicImpactsPotential, ConflictOutcome, EscalationStage)

# investigate data frame



# create dummy variables
library(qdapTools)
dfconflicts <- cbind(dfconflicts[1:20], mtabulate(dfconflicts$Country))

#dfconflicts <- cbind(dfconflicts[1:41], mtabulate(dfconflicts$AccuracyOfLocation)) # as dummies or categorical variable 1-2-3?

dfconflicts <- cbind(dfconflicts[1:41], mtabulate(dfconflicts$SpecificCommodities))

dfconflicts <- cbind(dfconflicts[1:44], mtabulate(dfconflicts$TypeOfPopulation)) # remove unknown


dfconflicts <- cbind(dfconflicts[1:107], mtabulate(dfconflicts$ReactionStage)) # remove unknown

dfconflicts <- cbind(dfconflicts[1:41], mtabulate(dfconflicts$GroupsMobilizing))

# impacts either visible yes/no or potential yes/no

# as factor
dfconflicts$EscalationStage <- as.factor(dfconflicts$EscalationStage)

# test multinom() function
library(nnet)
?multinom
multinom1 <- multinom(EscalationStage ~ Argentina + Bolivia + Brazil + Chile + Colombia + CostaRica + DominicanRepublic + Ecuador + ElSalvador + Guatemala + Guyana + Honduras + Jamaica + Mexico + Nicaragua + Panama + Peru + PuertoRico + Uruguay, weights = count, data = dfconflicts )

