# define variable escalation stage # stage 5 to 8 # 

# load libraries
library(rjson)
library(plyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(forcats)

library(hash)

# set working directory
if (dir.exists("~/Master thesis/Code/mining-conflicts")){
  setwd("~/Master thesis/Code/mining-conflicts")
} else{
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
# hash environment
stages <- hash::hash()

# define stages according to forms of mobilization
stages[["street protest/marches"]] <- 5
stages[["public campaigns"]] <- 5
stages[["media based activism/alternative media"]] <- 5
stages[["official complaint letters and petitions"]] <- 5
stages[["development of a network/collective action"]] <- 0
stages[["involvement of national and international ngos"]] <- 8
stages[["lawsuits, court cases, judicial activism"]] <- 6
stages[["blockades"]] <- 5
stages[["objections to the eia"]] <- 2
stages[["creation of alternative reports/knowledge"]] <- 0
stages[["arguments for the rights of mother nature"]] <- 0
stages[["appeals/recourse to economic valuation of the environment"]] <- 0
stages[["strikes"]] <- 5
stages[["occupation of buildings/public spaces"]] <- 5
stages[["development of alternative proposals"]] <- 0
stages[["community-based participative research (popular epidemiology studies, etc)"]] <- 0
stages[["land occupation"]] <- 5
stages[["referendum other local consultations"]] <- 5
stages[["artistic and creative actions (eg guerilla theatre, murals)"]] <- 5
stages[["property damage/arson"]] <- 7
stages[["boycotts of official procedures/non-participation in official processes"]] <- 5
stages[["shareholder/financial activism"]] <- 3
stages[["threats to use arms"]] <- 7
stages[["refusal of compensation"]] <- 3
stages[["hunger strikes and self immolation"]] <- 5
stages[["general assemblies, public forums or discussion tables"]] <- 2
stages[["retention or kidnapping"]] <- 7
stages[["(proposal of) declaration of location as ecological municipality, natural or religious sanctuary or cultural heritage"]] <- 3
stages[["new environmental impact assessment/study"]] <- 0
stages[["presentation of the case to the permanent peoples tribunal"]] <- 8
stages[["sabotage"]] <- 7
stages[["boycotts of companies-products"]] <- 5
stages[["hunger strike"]] <- 5

# define stages according to conflict responses
stages[["application of existing regulations"]] <- 0
stages[["strengthening of participation"]] <- 0
stages[["corruption"]]<- 7
stages[["criminalization of activists"]] <- 7
stages[["repression"]] <- 7
stages[["violent targeting of activists"]] <- 7
stages[["under negotiation"]] <- 0
stages[["compensation"]] <- 3
stages[["deaths, assassinations, murders"]] <- 7
stages[["migration/displacement"]] <- 5
stages[["new environmental impact assessment/study"]] <- 0
stages[["new legislation"]] <- 0
stages[["project temporarily suspended"]] <- 0
stages[["court decision (victory for environmental justice)"]] <- 6
stages[["institutional changes"]] <- 0
stages[["land demarcation"]] <- 0
stages[["project cancelled"]] <- 0
stages[["court decision (failure for environmental justice)"]] <- 6
stages[["court decision (undecided)"]] <- 6
stages[["negotiated alternative solution"]] <- 0
stages[["environmental improvements, rehabilitation/restoration of area"]] <- 0
stages[["technical solutions to improve resource supply/quality/distribution"]] <- 0
stages[["withdrawal of company/investment"]] <- 0
stages[["fostering a culture of peace"]] <- 0
stages[["declaration of location as ecological municipality, natural or religious sanctuary or cultural heritage"]] <- 0
stages[["moratoria"]] <- 0

# see stages
keys(stages)
values(stages)

stages

# create for loop
for(i in seq(1,length(conflicts))){
  max_stage = 0
  for (j in conflicts[[i]]$FormsOfMobilization){
    print(paste(c(i,j),sep=","))
    if (stages[[j]] > max_stage){
      max_stage = stages[[j]]}}
  for (k in conflicts[[i]]$ConflictOutcome){
    print(paste(c(i,k),sep=","))
    if (stages[[k]] > max_stage){
      max_stage = stages[[k]]}}
  
  conflicts[[i]]$EscalationStage = max_stage}

# count values for variable escalation stage
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "EscalationStage"), "["))), desc(x))

# Barplot EscalationStage
stages_data <- plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "EscalationStage"), "["))), desc(x))

stages_data <- stages_data %>%
  dplyr::select(escalation_stage = "x", count = "freq")

stages_data$escalation_stage <- as.factor(stages_data$escalation_stage)

# plot
stages_data %>%
  #mutate(escalation_stage = fct_reorder(escalation_stage, count))%>%
  ggplot( aes(x=escalation_stage, y=count)) +
  geom_bar(stat="identity", fill="dark blue", alpha=.6, width=.4) +
  #coord_flip() +
  labs(title = "Conflicts per Escalation Stage", x = "Escalation Stages", y = "Number of Conflicts")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


