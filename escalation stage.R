library(hash)
conflictstufen <- hash()

#conflictstufen = {"assassination": 7, "protests": 3, ...}

conflictstufen[["street protest/marches"]] <- 4
conflictstufen[["public campaigns"]] <- 4
conflictstufen[["media based activism/alternative media"]] <- 4
conflictstufen[["official complaint letters and petitions"]] <- 4
conflictstufen[["development of a network/collective action"]] <- 4
conflictstufen[["involvement of national and international ngos"]] <- 8
conflictstufen[["lawsuits, court cases, judicial activism"]] <- 6
conflictstufen[["blockades"]] <- 5
conflictstufen[["objections to the eia"]] <- 3
conflictstufen[["creation of alternative reports/knowledge"]] <- 3
conflictstufen[["arguments for the rights of mother nature"]] <- 3
conflictstufen[["appeals/recourse to economic valuation of the environment"]] <- 3
conflictstufen[["strikes"]] <- 4
conflictstufen[["occupation of buildings/public spaces"]] <- 5
conflictstufen[["development of alternative proposals"]] <- 3
conflictstufen[["community-based participative research (popular epidemiology studies, etc)"]] <- 3
conflictstufen[["land occupation"]] <- 5
conflictstufen[["referendum other local consultations"]] <- 4
conflictstufen[["artistic and creative actions (eg guerilla theatre, murals)"]] <- 4
conflictstufen[["property damage/arson"]] <- 5
conflictstufen[["boycotts of official procedures/non-participation in official processes"]] <- 4
conflictstufen[["shareholder/financial activism"]] <- 3
conflictstufen[["threats to use arms"]] <- 7
conflictstufen[["refusal of compensation"]] <- 3
conflictstufen[["hunger strikes and self immolations and self immolation"]] <- 4
conflictstufen[["general assemblies, public forums or discussion tables"]] <- 3
conflictstufen[["sabotage"]] <- 7
conflictstufen[["retention or kidnapping"]] <- 7
conflictstufen[["retention or kidnapping "]] <- 7 #sollte keinen abstand haben
conflictstufen[["presentation of the case to the permanent peoples tribunal"]] <- 8
conflictstufen[["new environmental impact assessment/study"]] <- 3
conflictstufen[["boycotts of companies-products"]] <- 4
conflictstufen[["(proposal of) declaration of location as ecological municipality, natural or religious sanctuary or cultural heritage"]] <- 3
conflictstufen[[" (proposal of) declaration of location as ecological municipality, natural or religious sanctuary or cultural heritage"]] <- 3
conflictstufen[["hunger strikes and self immolation"]] <- 4

conflictstufen[["application of existing regulations"]] <- 0
conflictstufen[["strengthening of participation"]] <- 0
conflictstufen[["corruption"]]<- 7
conflictstufen[["criminalization of activists"]] <- 7
conflictstufen[["repression"]] <- 7
conflictstufen[["violent targeting of activists"]] <- 7
conflictstufen[["under negotiation"]] <- 0
conflictstufen[["deaths, assassinations, murders"]] <- 7
conflictstufen[["compensation"]] <- 3
conflictstufen[["migration/displacement"]] <- 7
conflictstufen[["new environmental impact assessment/study"]] <- 3
conflictstufen[["new legislation"]] <- 0
conflictstufen[["project temporarily suspended"]] <- 0
conflictstufen[["court decision (victory for environmental justice)"]] <- 6
conflictstufen[["institutional changes"]] <- 0
conflictstufen[["land demarcation"]] <- 0
conflictstufen[["project cancelled"]] <- 0
conflictstufen[["court decision (failure for environmental justice)"]] <- 6
conflictstufen[["court decision (undecided)"]] <- 6
conflictstufen[["negotiated alternative solution"]] <- 0
conflictstufen[["environmental improvements, rehabilitation/restoration of area"]] <- 0
conflictstufen[["technical solutions to improve resource supply/quality/distribution"]] <- 0
conflictstufen[["withdrawal of company/investment"]] <- 0
conflictstufen[["fostering a culture of peace"]] <- 0
conflictstufen[["declaration of location as ecological municipality, natural or religious sanctuary or cultural heritage"]] <- 0
conflictstufen[["moratoria"]] <- 0




keys(conflictstufen)
values(conflictstufen)

conflictstufen

# create for loop
for(i in seq(1,length(conflicts))){
  maximale_stufe = 0
  for (j in conflicts[[i]]$FormsOfMobilization){
    print(paste(c(i,j),sep=","))
    if (conflictstufen[[j]] > maximale_stufe){
      maximale_stufe = conflictstufen[[j]]}}
  for (k in conflicts[[i]]$ConflictOutcome){
    print(paste(c(i,k),sep=","))
    if (conflictstufen[[k]] > maximale_stufe){
      maximale_stufe = conflictstufen[[k]]}}
  conflicts[[i]]$EscalationStage = maximale_stufe}

# conflicts[[87]]$ConflictOutcome
#character(0)