# get data frame in matrix form
dfconflicts <- data.frame(t(sapply(conflicts,c)))

# select columns
dfconflicts <- dfconflicts %>%
  select(Country,AccuracyOfLocation, SpecificCommodities, ProjectArea, LevelOfInvestment, TypeOfPopulation, AffectedPopulation, CompanyNames, InternationalFinanceInstitutions, EnvironmentalJusticeOrganizations, ReactionStage, GroupsMobilizing, FormsOfMobilization, EnvironmentalImpactsVisible, EnvironmentalImpactsPotential, HealthImpactsVisible, HealthImpactsPotential, SocioeconomicImpactsVisible, SocioeconomicImpactsPotential, ConflictOutcome, EscalationStage)


# create dummy variables
library(qdapTools)
dfconflicts <- cbind(dfconflicts[1:21], mtabulate(dfconflicts$Country))

#dfconflicts <- cbind(dfconflicts[1:41], mtabulate(dfconflicts$AccuracyOfLocation)) # as dummies or categorical variable 1-2-3?

dfconflicts <- cbind(dfconflicts[1:41], mtabulate(dfconflicts$SpecificCommodities))

dfconflicts <- cbind(dfconflicts[1:44], mtabulate(dfconflicts$TypeOfPopulation)) # remove unknown


dfconflicts <- cbind(dfconflicts[1:107], mtabulate(dfconflicts$ReactionStage)) # remove unknown

dfconflicts <- cbind(dfconflicts[1:41], mtabulate(dfconflicts$GroupsMobilizing))

# impacts either visible yes/no or potential yes/no

# change names to names without spaces

# test multinom() function
library(nnet)
?multinom
multinom(EscalationStage ~ ..., data = dfconflicts )

