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
  select(NameOfConflict, Country, SpecificCommodities, CompanyNames, ReactionStage, GroupsMobilizing, FormsOfMobilization, EnvironmentalImpactsVisible, EnvironmentalImpactsPotential, HealthImpactsVisible, HealthImpactsPotential, SocioeconomicImpactsVisible, SocioeconomicImpactsPotential, ConflictOutcome, EscalationStage)

# delete observations for unknown reaction stage and recycled metals conflict
dfconflicts <- dfconflicts %>%
  filter(ReactionStage != "unknown" & SpecificCommodities != "recycled_metals")

# create dummy variables
dfconflicts <- cbind(dfconflicts[1:15], mtabulate(dfconflicts$Country))

dfconflicts <- cbind(dfconflicts[1:26], mtabulate(dfconflicts$SpecificCommodities))

dfconflicts <- cbind(dfconflicts[1:36], mtabulate(dfconflicts$ReactionStage))

dfconflicts <- cbind(dfconflicts[1:40], mtabulate(dfconflicts$GroupsMobilizing))

dfconflicts <- cbind(dfconflicts[1:44], mtabulate(dfconflicts$CompanyNames))

# impacts either visible yes/no or potential yes/no
dfconflicts$EnvironmentalImpactsVisible <- ifelse(dfconflicts$EnvironmentalImpactsVisible == "none", 0, 1)
dfconflicts$EnvironmentalImpactsPotential <- ifelse(dfconflicts$EnvironmentalImpactsPotential == "none", 0, 1)
dfconflicts$HealthImpactsVisible <- ifelse(dfconflicts$HealthImpactsVisible == "none", 0, 1)
dfconflicts$HealthImpactsPotential <- ifelse(dfconflicts$HealthImpactsPotential == "none", 0, 1)
dfconflicts$SocioeconomicImpactsVisible <- ifelse(dfconflicts$SocioeconomicImpactsVisible == "none", 0, 1)
dfconflicts$SocioeconomicImpactsPotential <- ifelse(dfconflicts$SocioeconomicImpactsPotential == "none", 0, 1)

# DV as factor
dfconflicts$EscalationStage <- as.factor(unlist(dfconflicts$EscalationStage))


# change baseline
#dfconflicts$EscalationStage <- relevel(x=dfconflicts$EscalationStage, ref="4")

# interpret the odds
# coefficients(multinom4)
# exp(coef(multinom4))
# 
# # multinom function code
# nnet::multinom
# 
# # predicted probabilities
# head(pp <- fitted(multinom4))


##### MODEL SELECTION #####

# only Countries
multi_a <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay, data = dfconflicts, maxit=500)
screenreg(list(multi_a), custom.model.names= c("Only Countries"))

# Countries + Commodities
multi_b <- multinom(formula = EscalationStage ~ arg_uru + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela +  base_and_ferroalloy_metals + biological_resources + non_ferrous_metals + energy_sources + nonmetallic_minerals + precious_metals, data = dfconflicts, maxit=500)
screenreg(list(multi_b), custom.model.names= c("countries + commodities"))

# Countries + Commodities + Actors
multi_c <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay + agricultural_chemical_minerals + base_metals + biological_resources + diamonds + electricity + energy_mineral_resources + ferroalloy_metals + ferrous_metals + gemstones + industrial_waste + light_metals + nonmetallic_minerals + precious_metals + radioactive_ores + recycled_metals + technology_elements + tourism_services + excluded_marginalized + local_people + organization, data = dfconflicts, maxit=500)

# Countries + Commodities + Actors + Impacts
multi_d <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay + agricultural_chemical_minerals + base_metals + biological_resources + diamonds + electricity + energy_mineral_resources + ferroalloy_metals + ferrous_metals + gemstones + industrial_waste + light_metals + nonmetallic_minerals + precious_metals + radioactive_ores + recycled_metals + technology_elements + tourism_services + excluded_marginalized + local_people + organization + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts, maxit=500)

# Countries + Commodities + Actors + Impacts + Company
multi_e <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay + agricultural_chemical_minerals + base_metals + biological_resources + diamonds + electricity + energy_mineral_resources + ferroalloy_metals + ferrous_metals + gemstones + industrial_waste + light_metals + nonmetallic_minerals + precious_metals + radioactive_ores + recycled_metals + technology_elements + tourism_services + excluded_marginalized + local_people + organization + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential + foreign_company + illegal_mining, data = dfconflicts, maxit=500)

# Countries + Commodities + Actors + Impacts + Company + ReactionStage
multi_f <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay + agricultural_chemical_minerals + base_metals + biological_resources + diamonds + electricity + energy_mineral_resources + ferroalloy_metals + ferrous_metals + gemstones + industrial_waste + light_metals + nonmetallic_minerals + precious_metals + radioactive_ores + recycled_metals + technology_elements + tourism_services + economic_actors + excluded_marginalized + local_people + organization + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential + foreign_company + illegal_mining + preventive + reaction + reparations, data = dfconflicts, maxit=500)


# only commodities new2
multi_g <- multinom(formula = EscalationStage ~ base_and_ferroalloy_metals + biological_resources + non_ferrous_metals + energy_sources + nonmetallic_minerals + precious_metals, data = dfconflicts, maxit= 500)
screenreg(list(multi_g), custom.model.names = c("only commodities"))

# only actors
multi_h <- multinom(formula = EscalationStage ~ economic_actors + organization + excluded_marginalized + local_people, data = dfconflicts, maxit = 500)
screenreg(list(multi_h), custom.model.names = c("only actors"))

# only impacts
multi_i <- multinom(formula = EscalationStage ~ EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts, maxit = 500)
screenreg(list(multi_i), custom.model.names = c("only impacts"))

# only operator
multi_j <- multinom(formula = EscalationStage ~ foreign_company + illegal_mining, data = dfconflicts, maxit = 500)
screenreg(list(multi_j), custom.model.names = c("only operator"))

# only reactionstage
multi_k <- multinom(formula = EscalationStage ~ latent + reaction + reparations, data = dfconflicts, maxit = 500)
screenreg(list(multi_k), custom.model.names = c("only reactionstage"))


# countries + impacts
multi_l <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts, maxit = 500)
screenreg(list(multi_l), custom.model.names = c("countries + impacts"))

# countries + operator
multi_m <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay + foreign_company + illegal_mining, data = dfconflicts, maxit = 500)
screenreg(list(multi_m), custom.model.names = c("countries + operator"))

# countries + reactionstage
multi_n <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay + latent + reaction + reparations, data = dfconflicts, maxit = 500)
screenreg(list(multi_n), custom.model.names = c("countries + reactionstage"))

# countries + actors
multi_o <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay + economic_actors + excluded_marginalized + local_people + organization, data = dfconflicts, maxit = 500)
screenreg(list(multi_o), custom.model.names = c("countries + actors"))

# countries + impacts + reactionstage
multi_p <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential + latent + reaction + reparations, data = dfconflicts, maxit = 500)
screenreg(list(multi_p), custom.model.names = c("countries + impacts + reactionstage"))

# countries + actors + operator
multi_q <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay + economic_actors + excluded_marginalized + local_people + organization + foreign_company + illegal_mining, data = dfconflicts, maxit = 500)
screenreg(list(multi_q), custom.model.names = c("countries + actors + operator"))

# countries + impacts + reactionstage + actors + operator
multi_r <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential + latent + reaction + reparations + economic_actors + excluded_marginalized + local_people + organization + foreign_company + illegal_mining, data = dfconflicts, maxit = 500)
screenreg(list(multi_r), custom.model.names = c("countries + impacts + reactionstage + actors + operator"))

# countries + impacts + reactionstage + actors + operator + commodities
multi_s <- multinom(formula = EscalationStage ~ argentina + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + uruguay + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential + latent + reaction + reparations + economic_actors + excluded_marginalized + local_people + organization + foreign_company + illegal_mining + agricultural_chemical_minerals + base_metals + biological_resources + diamonds + electricity + energy_mineral_resources + ferroalloy_metals + ferrous_metals + gemstones + industrial_waste + light_metals + nonmetallic_minerals + precious_metals + radioactive_ores  + recycled_metals + technology_elements + tourism_services, data = dfconflicts, maxit = 500)
screenreg(list(multi_s), custom.model.names = c("countries + impacts + reactionstage + actors + operator + commodities"))

# commodities + actors
multi_t <- multinom(formula = EscalationStage ~ agricultural_chemical_minerals + base_metals + biological_resources + diam_gem + energy_mineral_resources + ferroalloy_metals + ferrous_metals + light_metals + nonmetallic_minerals + precious_metals + radioactive_ores  + technology_elements + recycled_metals + economic_actors + organization + excluded_marginalized + local_people, data = dfconflicts, maxit= 500)
screenreg(list(multi_t), custom.model.names = c("commodities + actors"))

# commodities + reactionstage
multi_u <- multinom(formula = EscalationStage ~ agricultural_chemical_minerals + base_metals + biological_resources + diam_gem + energy_mineral_resources + ferroalloy_metals + ferrous_metals + light_metals + nonmetallic_minerals + precious_metals + radioactive_ores  + technology_elements + recycled_metals + latent + reaction + reparations, data = dfconflicts, maxit= 500)
screenreg(list(multi_u), custom.model.names = c("commodities + reactionstage"))

# commodities + operator
multi_v <- multinom(formula = EscalationStage ~ agricultural_chemical_minerals + base_metals + biological_resources + diam_gem + energy_mineral_resources + ferroalloy_metals + ferrous_metals + light_metals + nonmetallic_minerals + precious_metals + radioactive_ores  + technology_elements + recycled_metals + foreign_company + illegal_mining, data = dfconflicts, maxit= 500)
screenreg(list(multi_v), custom.model.names = c("commodities + operator"))

# commodities + impacts
multi_w <- multinom(formula = EscalationStage ~ agricultural_chemical_minerals + base_metals + biological_resources + diam_gem + energy_mineral_resources + ferroalloy_metals + ferrous_metals + light_metals + nonmetallic_minerals + precious_metals + radioactive_ores  + technology_elements + recycled_metals + foreign_company + illegal_mining +EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts, maxit= 500)
screenreg(list(multi_w), custom.model.names = c("commodities + impacts"))


# Countries + Commodities + Actors + Operator
multi_x <- multinom(formula = EscalationStage ~ arg_uru + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + base_and_ferroalloy_metals + biological_resources + non_ferrous_metals + energy_sources + nonmetallic_minerals + precious_metals + economic_actors + excluded_marginalized + local_people + organization + foreign_company + illegal_mining, data = dfconflicts, maxit=500)
screenreg(list(multi_x), custom.model.names = c("countries + commodities + actors + operator"))

# Countries + Commodities + Impacts + ReactionStage
multi_y <- multinom(formula = EscalationStage ~ arg_uru + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + base_and_ferroalloy_metals + biological_resources + non_ferrous_metals + energy_sources + nonmetallic_minerals + precious_metals + latent + reaction + reparations + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential, data = dfconflicts, maxit=500)
screenreg(list(multi_y), custom.model.names = c("countries + commodities + impacts + reactionstage"))

# Countries + Commodities + Impacts + ReactionStage + Actors + Operator
multi_z <- multinom(formula = EscalationStage ~ arg_uru + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + base_and_ferroalloy_metals + biological_resources + non_ferrous_metals + energy_sources + nonmetallic_minerals + precious_metals + latent + reaction + reparations + EnvironmentalImpactsVisible + EnvironmentalImpactsPotential + HealthImpactsVisible + HealthImpactsPotential + SocioeconomicImpactsVisible + SocioeconomicImpactsPotential + economic_actors + excluded_marginalized + local_people + organization + foreign_company + illegal_mining, data = dfconflicts, maxit=500)
screenreg(list(multi_z), custom.model.names = c("countries + commodities + impacts + reactionstage + actors + operator"))

# Countries + Commodities + ReactionStage + Actors + Operator
multi_z2 <- multinom(formula = EscalationStage ~ arg_uru + bolivia + brazil + chile + caribbean + centralamerica + ecuador + mexico + peru + venezuela + base_and_ferroalloy_metals + biological_resources + non_ferrous_metals + energy_sources + nonmetallic_minerals + precious_metals + latent + reaction + reparations + economic_actors + excluded_marginalized + local_people + organization + foreign_company + illegal_mining, data = dfconflicts, maxit=500)
screenreg(list(multi_z2), custom.model.names = c("countries + commodities + reactionstage + actors + operator"))

# odds ratios
coef(multi_z2)
exp(coef(multi_z2))

summary(multi_z2)$coefficients[2,12] # 2 <- row, 12 <- column
# commodities 12:17
coef_comm <- summary(multi_z2)$coefficients[1:3,12:17] # coefficients of commodities
exp_comm <- exp(summary(multi_z2)$coefficients[1:3,12:17])

# groups 21:24
coef_groups <- summary(multi_z2)$coefficients[1:3,21:24]
exp_groups <- exp(summary(multi_z2)$coefficients[1:3, 21:24])

# operator 25:26
coef_oper <- summary(multi_z2)$coefficients[1:3,25:26]
exp_oper <- exp(summary(multi_z2)$coefficients[1:3,25:26])

# reaction stage 18:20
coef_react <- summary(multi_z2)$coefficients[1:3,18:20]
exp_react <- exp(summary(multi_z2)$coefficients[1:3,18:20])

# compare models
screenreg(list(multi_b, multi_x, multi_y, multi_z, multi_z2), custom.model.names = c("Countries + Commodities", "Countries + Commodities + Actors + Operator", "Countries + Commodities + Impacts + ReactionStage", "All variables", "All minus impacts"))

stargazer(list(multi_b, multi_x, multi_y, multi_z, multi_z2), type = "html", summary = FALSE, out= "testrun9.htm")

stargazer(list(multi_b, multi_x, multi_y), title = "Regression Results",
          align = TRUE, covariate.labels = c("Argentina and Uruguay", "Bolivia", "Brazil", "Chile", "Caribbean Countries", "Central American Countries", "Ecuador", "Mexico", "Peru", "Venezuela", "Base- and Ferroalloy Metals", "Biological Resources", "Non-ferrous Metals", "Energy Sources", "Nonmetallic Minerals", "Precious Metals", "Economic Actors", "Excluded/Marginalized Groups", "Local People", "Organization", "Foreign Company", "Illegal Mining", "Latent mobilization", "In reaction", "Mobilizations for reparations"), no.space =TRUE, type ="latex", style = "default", column.labels = c("Model A", "Model B", "Model C"), column.separate = c(3,3,3), font.size = "footnotesize", out = "testrun13.tex")


# final model results table for latex
stargazer(multi_z2, title = "Regression Results: Model E", align = TRUE, covariate.labels = c("Argentina and Uruguay", "Bolivia", "Brazil", "Chile", "Caribbean Countries", "Central American Countries", "Ecuador", "Mexico", "Peru", "Venezuela","Base- and Ferroalloy Metals", "Biological Resources", "Non-ferrous Metals", "Energy Sources", "Nonmetallic Minerals", "Precious Metals","Mobilization:latent", "Mobilization:in reaction", "Mobilization:for reparations", "Economic Actors", "Excluded/Marginalized Groups", "Local People", "Organization", "Foreign Company", "Illegal Mining"), no.space = TRUE, type = "latex", style = "default", column.labels = c("legal action", "violent action", "(inter)nationalization"), font.size = "footnotesize", out = "results_table.tex")

# stargazer(linear.1, linear.2, probit.model, title="Regression Results",
#           align=TRUE, dep.var.labels=c("Overall Rating","High Rating"),
#           covariate.labels=c("Handling of Complaints","No Special Privileges",
#                              "Opportunity to Learn","Performance-Based Raises","Too Critical","Advancement"),
#           omit.stat=c("LL","ser","f"), no.space=TRUE)


# vcov(multi_a)
# coef(multi_a)
# exp(coef(multi_a))
# 
# coef(multi_b)
# exp(coef(multi_b))
# vcov(multi_b)
# 
# PseudoR2(multi_a, which = NULL)
