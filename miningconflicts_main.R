# ej atlas conflicts json file #

# load libraries
library(rjson)
library(plyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(forcats)

# set working directory
if (dir.exists("~/Master thesis/Code/mining-conflicts")){
  setwd("~/Master thesis/Code/mining-conflicts")
} else{
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# load ejatlas data from json file
conflicts <- fromJSON(file = "ejatlas.json")

##################### COUNTRY ############################################

# count and arrange in descending order
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "Country"), "["))), desc(freq))

# remove whitespace
for(i in seq(1,length(conflicts))){
     conflicts[[i]]$Country <- sub(" ", "", conflicts[[i]]$Country)

# lowercase all
conflicts[[i]]$Country <- unlist(lapply(conflicts[[i]]$Country, tolower))
}



######################## COMPANY ORIGIN and ILLEGAL MINING ################

# get all companynames
conflicts %>%
  map("CompanyNames")
  
# manually complete information for cases without company names or companies without info on origin
  conflicts[[4]]$CompanyNames <- c("illegal")
  conflicts[[6]]$CompanyNames <- c("illegal")
  conflicts[[42]]$CompanyNames <- c("illegal")
  conflicts[[46]]$CompanyNames <- c("illegal")
  conflicts[[58]]$CompanyNames <- c("illegal")
  conflicts[[59]]$CompanyNames <- c("illegal")
  conflicts[[79]]$CompanyNames <- c("illegal")
  conflicts[[114]]$CompanyNames <- c("illegal")
  conflicts[[117]]$CompanyNames <- c("illegal")
  conflicts[[118]]$CompanyNames <- c("illegal")
  conflicts[[156]]$CompanyNames <- c("illegal")
  conflicts[[181]]$CompanyNames <- c("illegal")
  conflicts[[183]]$CompanyNames <- c("illegal")
  conflicts[[185]]$CompanyNames <- c("illegal")
  conflicts[[187]]$CompanyNames <- c("illegal")
  conflicts[[189]]$CompanyNames <- c("illegal")
  conflicts[[231]]$CompanyNames <- c("illegal")
  conflicts[[245]]$CompanyNames <- c("illegal")
  conflicts[[282]]$CompanyNames <- c("illegal")
  
  conflicts[[27]]$CompanyNames <- c("Sundance Minerals Ltd. Of Vancouver from Canada")
  conflicts[[33]]$CompanyNames <- c("United States")
  conflicts[[37]]$CompanyNames <- c("Volcan Compaa Minera from Peru")
  conflicts[[45]]$CompanyNames <- c("Teck Resources Limited from Candada")
  conflicts[[47]]$CompanyNames <- c("BlackFire from Canada")
  conflicts[[49]]$CompanyNames <- c("Minas Buenaventura from Peru")
  conflicts[[50]]$CompanyNames <- c("Hochschild Mining from Peru") 
  conflicts[[71]]$CompanyNames <- c("Union Mining from The Netherlands")
  conflicts[[83]]$CompanyNames <- c("Drummond Company Inc. from United States")
  conflicts[[87]]$CompanyNames <- c("Bolivia")
  conflicts[[91]]$CompanyNames <- c("Codelco from Chile")
  conflicts[[95]]$CompanyNames <- c("Colombia")
  conflicts[[108]]$CompanyNames <- c("South World Consulting from Chile")
  conflicts[[121]]$CompanyNames <- c("Carbozulia from Venezuela")
  conflicts[[137]]$CompanyNames <- c("Manto Rojo from Mexico")
  conflicts[[166]]$CompanyNames <- c("Alexander Mining Plc. from United Kingdom")
  conflicts[[194]]$CompanyNames <- c("Dominium Minerals Corp. from United States")
  conflicts[[215]]$CompanyNames <- c("Coer dAlene Mines from United States")
  conflicts[[216]]$CompanyNames <- c("Mexico")
  conflicts[[244]]$CompanyNames <- c("Brazil")
  conflicts[[259]]$CompanyNames <- c("Caribbean Resources Group Corporation from Guatemala")
  conflicts[[268]]$CompanyNames <- c("Argentina")
  conflicts[[269]]$CompanyNames <- c("Argentina")
  conflicts[[270]]$CompanyNames <- c("Canada")
  conflicts[[291]]$CompanyNames <- c("Bolivia")
  
  
  
# all companynames with term "illegal"/"ilegal" inside the strings (e.g. Mineria ilegal) are renamed as "illegal" 
for(i in seq(1,length(conflicts))){
  # remove whitespace
  conflicts[[i]]$CompanyNames = gsub(" ", "", conflicts[[i]]$CompanyNames)
  # lowercase all
  conflicts[[i]]$CompanyNames = unlist(lapply(conflicts[[i]]$CompanyNames, tolower))
   if (!is.null(conflicts[[i]]$CompanyNames)) {
    companies = conflicts[[i]]$CompanyNames
    if(length(companies) > 0){
      for (j in seq(1, length(companies))){
        if(grepl("\\b(\\w*ilegal\\w*)\\b", companies[j])){
          conflicts[[i]]$CompanyNames[j] = "illegal"
        }
        else if (grepl("\\b(\\w*illegal\\w*)\\b", companies[j])){
          conflicts[[i]]$CompanyNames[j] = "illegal"
        }
      }
      # all companynames with "from Country XY" are renamed as the country
      countries = sapply(strsplit(companies,"from"), `[`, 2)
      for (j in seq(1, length(companies))){
        if(!is.na(countries[j])){
          conflicts[[i]]$CompanyNames[j] = countries[j]
        }
      }
      if (all(unique(conflicts[[i]]$CompanyNames) == conflicts[[i]]$Country)){
        conflicts[[i]]$CompanyNames = "local"
      }
      else{
        if ('illegal' %in% conflicts[[i]]$CompanyNames){
          conflicts[[i]]$CompanyNames = "illegal"
        }
        else{
          conflicts[[i]]$CompanyNames = "foreign"
        }
      }
    }
  }
} 
  
  # now all conflicts in which CompanyNames equals Country are "local", when the term illegal/ilegal shows up, they are "illegal", else they are "foreign"
  plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "CompanyNames"), "["))), desc(freq))
  
  

  ############################## COMMODITY ###################################
  
  # count and arrange in descending order
  plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "SpecificCommodities"), "["))), desc(freq))
  
  # substitute values in list
  substitutes_commodity = read_delim("./substitutes/commodities.csv", delim=';', escape_double=FALSE, escape_backslash=TRUE, quote='"')
  
  # remove values
  remove_commodity = c("colimdeno")
  
  # add elements
  conflicts[[120]]$SpecificCommodities <- c("copper")
  conflicts[[242]]$SpecificCommodities <- c("copper")
  
  # create for loop
  for(i in seq(1,length(conflicts))){
    if(typeof(conflicts[[i]]$SpecificCommodities)=="list"){
      print(conflicts[[i]]$SpecificCommodities[1])
      #conflicts[[i]]$SpecificCommodities = unlist(conflicts[[i]]$SpecificCommodities)
      conflicts[[i]]$SpecificCommodities = ""
    }
    # remove whitespace
    conflicts[[i]]$SpecificCommodities <- trimws(conflicts[[i]]$SpecificCommodities, which = c("both"))
    
    # lowercase all
    conflicts[[i]]$SpecificCommodities <- unlist(lapply(conflicts[[i]]$SpecificCommodities, tolower))
    
    # split elements separated by ", "
    conflicts[[i]]$SpecificCommodities <- unlist(strsplit(as.character(conflicts[[i]]$SpecificCommodities), ", "))
    
    # split elements separated by " and "
    conflicts[[i]]$SpecificCommodities <- unlist(strsplit(as.character(conflicts[[i]]$SpecificCommodities), " and "))
    
    # split elements separated by "\n"
    conflicts[[i]]$SpecificCommodities <- unlist(strsplit(as.character(conflicts[[i]]$SpecificCommodities), " \n" ))
    
    # split elements by " - "
    conflicts[[i]]$SpecificCommodities <- unlist(strsplit(as.character(conflicts[[i]]$SpecificCommodities), " - " ))
    
    # split elements by " y "
    conflicts[[i]]$SpecificCommodities <- unlist(strsplit(as.character(conflicts[[i]]$SpecificCommodities), " y "))
    
    # remove "/"
    conflicts[[i]]$SpecificCommodities <- sub("/", "", conflicts[[i]]$SpecificCommodities)
    
    
    for(j in seq(nrow(substitutes_commodity))){
      conflicts[[i]]$SpecificCommodities = gsub(toString(substitutes_commodity[j,]$old), toString(substitutes_commodity[j,]$new), conflicts[[i]]$SpecificCommodities)
      conflicts[[i]]$SpecificCommodities <- unique(conflicts[[i]]$SpecificCommodities)
    }
    
    for(j in remove_commodity){
      conflicts[[i]]$SpecificCommodities = conflicts[[i]]$SpecificCommodities[conflicts[[i]]$SpecificCommodities != j]
    }
  }
  
  # show clean list
  for(i in conflicts){
    print(i$SpecificCommodities)
  }
  
  # change 'cal' to 'lime'
  conflicts[[61]]$SpecificCommodities[2] <- "lime"

  # count again
  plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "SpecificCommodities"), "["))), desc(freq))
  
  # categorization of commodities
  
  # substitute values in list
  substitutes_commodity2 = read_delim("./substitutes/commodities_categorization.csv", delim=';', escape_double=FALSE, escape_backslash=TRUE, quote='"')
  
  # for loop for substitution
  for(i in seq(1,length(conflicts))){
    if(typeof(conflicts[[i]]$SpecificCommodities)=="list"){
      print(conflicts[[i]]$SpecificCommodities[1])
      conflicts[[i]]$SpecificCommodities = ""
    }
    for(j in seq(nrow(substitutes_commodity2))){
      conflicts[[i]]$SpecificCommodities = gsub(toString(substitutes_commodity2[j,]$old), toString(substitutes_commodity2[j,]$new), conflicts[[i]]$SpecificCommodities)
      conflicts[[i]]$SpecificCommodities <- unique(conflicts[[i]]$SpecificCommodities)
    }
  }
  
  # count again
  plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "SpecificCommodities"), "["))), desc(freq))
  


################################ REACTION STAGE ##############################

# count and arrange in descending order
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "ReactionStage"), "["))), desc(freq))
  
# create substitute values
substitutes_reaction = read_delim("./substitutes/reaction.csv", delim=';', escape_double=FALSE, escape_backslash=TRUE, quote='"')

# create for loop
for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$ReactionStage)=="list"){
    print(conflicts[[i]]$ReactionStage[1])
    conflicts[[i]]$ReactionStage = ""
  }

  # substitute pseudonyms
  for(j in seq(nrow(substitutes_reaction))){
    conflicts[[i]]$ReactionStage <- gsub(toString(substitutes_reaction[j,]$old), toString(substitutes_reaction[j,]$new), conflicts[[i]]$ReactionStage)
  }
}

# add unknown to empty conflict
conflicts[[91]]$ReactionStage <- c("unknown")

# count again
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "ReactionStage"), "["))), desc(freq))


############################### MOBILIZING GROUPS ###########################

# count and arrange in descending order
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "GroupsMobilizing"), "["))), desc(freq))

# create substitute values
substitutes_groups = read_delim("./substitutes/groups.csv", delim=';', escape_double=FALSE, escape_backslash=TRUE, quote='')

# remove empty elements
remove_groups = c("")

# add elements
conflicts[[120]]$GroupsMobilizing <- c("neighbours/citizens/communities", "local government/political parties", "local ejos")
conflicts[[242]]$GroupsMobilizing <- c("neighbours/citizens/communities", "local ejos")

# create for loop
for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$GroupsMobilizing)=="list"){
    print(conflicts[[i]]$GroupsMobilizing[1])
    conflicts[[i]]$GroupsMobilizing = ""
  }
  # remove whitespace
  conflicts[[i]]$GroupsMobilizing = trimws(conflicts[[i]]$GroupsMobilizing, which = c("both"))
  
  # lowercase all
  conflicts[[i]]$GroupsMobilizing = unlist(lapply(conflicts[[i]]$GroupsMobilizing, tolower))
  
  # split elements separated by "\n"
  conflicts[[i]]$GroupsMobilizing <- unlist(strsplit(as.character(conflicts[[i]]$GroupsMobilizing), "\n" ))
  
  # substitute pseudonyms
  for(j in seq(nrow(substitutes_groups))){
    conflicts[[i]]$GroupsMobilizing <- gsub(toString(substitutes_groups[j,]$old), toString(substitutes_groups[j,]$new), conflicts[[i]]$GroupsMobilizing)
  }
  for(j in remove_groups){
    conflicts[[i]]$GroupsMobilizing = conflicts[[i]]$GroupsMobilizing[conflicts[[i]]$GroupsMobilizing != j]
    
  }
  conflicts[[i]]$GroupsMobilizing = gsub(".","", conflicts[[i]]$GroupsMobilizing, fixed = TRUE) #to not get "." in results
}

# show clean list
# for(i in conflicts){
#  print(i$GroupsMobilizing)
# }

# count again
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "GroupsMobilizing"), "["))), desc(freq))

# Figure Mobilizing Groups

groups_data <- plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "GroupsMobilizing"), "["))), desc(freq))

groups_data <- groups_data %>%
  dplyr::select(mobilizing_groups = "x", count = "freq")

# barplot with ggplot2
library(ggplot2)
library(forcats)

# Reorder following the value of another column:
groups_data %>%
  mutate(mobilizing_groups = fct_reorder(mobilizing_groups, count)) %>%
  ggplot( aes(x=mobilizing_groups, y=count)) +
  geom_bar(stat="identity", fill="dark red", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# categorization of groups

# substitute values in list
substitutes_groups2 = list(
  c("local_people", "neighbours/citizens/communities"),
  c("local_people", "recreational users"),
  c("local_people", "conservationists"),
  c("local_people", "youth"),
  c("organization", "local ejos"),
  c("organization", "social movements"),
  c("organization", "local government/political parties"),
  c("organization", "international ejos"),
  c("organization", "religious groups"),
  c("organization", "trade unions"),
  c("organization", "international politics"),
  c("organization", "national governmental actors"),
  c("economic_actors", "farmers"),
  c("economic_actors", "local scientists/professionals"),
  c("economic_actors", "artisanal miners"),
  c("economic_actors", "fisher people"),
  c("economic_actors", "industrial workers"),
  c("economic_actors", "pastoralists"),
  c("economic_actors", "landless peasants"),
  c("economic_actors", "informal workers"),
  c("excluded_marginalized", "indigenous groups or traditional communities"),
  c("excluded_marginalized", "women"),
  c("excluded_marginalized", "ethnically/racially discriminated groups")
)

# create for loop
for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$GroupsMobilizing)=="list"){
    print(conflicts[[i]]$GroupsMobilizing[1])
    conflicts[[i]]$GroupsMobilizing = ""
  }
  
  # substitute pseudonyms
  for(j in substitutes_groups2){
    conflicts[[i]]$GroupsMobilizing = gsub(j[2], j[1], conflicts[[i]]$GroupsMobilizing)
    conflicts[[i]]$GroupsMobilizing <- unique(conflicts[[i]]$GroupsMobilizing)
  }
  conflicts[[i]]$GroupsMobilizing = gsub(".","", conflicts[[i]]$GroupsMobilizing, fixed = TRUE) #to not get "." in results
}

# count again
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "GroupsMobilizing"), "["))), desc(freq))

# as table

## ERROR with using groups_categorization.csv file for substitution ##

######################### FORMS OF MOBILIZATION ############################

# count and arrange in descending order
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "FormsOfMobilization"), "["))), desc(freq))

# create substitute values
substitutes_forms = read_delim("./substitutes/forms.csv", delim=';', escape_double=FALSE, escape_backslash=TRUE, quote='')

#remove elements
remove_forms = c(
  "indgenas wayu han planteado canales de dilogo con las autoridades oficiales. crean la escuela yalayalamana, donde se trabaja con nios en el desarrollo y fortalecimiento autonmico en sus comunidades, as como en la promocin de su cultura y formas de vida y la defensa de sus territorios de las compaas transnacionales carboneras y gasferas. se ha generado una articulacin entre wayu de varias zonas, siendo que los wayuu del guasare han podido evidenciar los efectos de la minera de carbn a los wayu del socuy. han surgido declaraciones de organizaciones como wainjirawa y aceiluz que se distancian de los wayu que se han enriquecido en puestos de gobierno. se han realizado acciones como bloqueos de tractores  para evitar que se abrieran caminos en una comunidad sin la autorizacin de esta. tambin comunidades wayu junto a organizaciones sociales han realizado diferentes manifestaciones de calle y street protest/marches. organizaciones ecologistas reclaman que no se haya realizado ninguna consulta a los indgenas wayuu del socuy ni se haya presentado ningn estudio de impacto ambiental (eia), tal y como lo estipula la ley del ambiente. ecologistas han realizado street protest/marches y bicicletadas exigiendo detener la expansin del extractivismo de carbn en el zulia. organizaciones de derechos humanos exigen el respeto de los ddhh de los wayu y el cese de las agresiones contra estos. se ha exigido junto con los indgenas la realizacin de investigaciones e indemnizaciones de las vctimas de las violaciones de ddhh en la goajira.",
  "a pesar de que aos atrs el nivel de movilizacin de las organizaciones indgenas warao, junto con otras agrupaciones ambientales y de derechos humanos nacionales e internacionales, tuvo una importante fuerza en relacin al rechazo a proyectos de desarrollo (como el caso de contaminacin petrolera de bp-pedernales), en la actualidad la intensidad de las luchas ha decado, sea por las precarias condiciones de vida de los indgenas, por su debilidad institucional, por la intervencin y cooptacin estatal, las dificultades de las nuevas generaciones para enfrentar los enormes desafos presentes, entre otros.",
  "las respuestas firmes y sostenidas fueron realizadas principalmente por parte de los yukpa del ro yaza, liderados por el cacique sabino romero. numerosas campaas fueron realizadas, con el apoyo de movimientos como la asociacin nacional de medios comunitarios, libres y alternativos (anmcla), lo que permiti darle visibilidad al conflicto, alcanzar simpatas en muchas otras organizaciones sociales y bases de apoyo del gobierno nacional, as como interpelar a los gobernantes para que se hicieran responsables del problema.  dichas campaas lograban generar una narrativa que vinculaba la justicia social, la lucha indgena, la lucha por la tierra y el ambiente, y la idea de revolucin.",
  "el lder sabino romero, en el marco de la entrega de tierras a los indgenas, se pronuncia a favor de que se compren las bienhechuras a los hacendados para evitar conflictos, pero advierte que de no llegar a un acuerdo satisfactorio, seguirn recuperando sus territorios ancestrales.",
  "visita a accionistas en vancouver, bc canada (2017) para protestar. habitantes de ixtacamaxtitln realizan la caravana por la vida y contra la minera (marzo 2018).",
  "las movilizaciones se inician como resistencia preventiva (fase previa a la realizacin del proyecto). en un primer momento, a raz de los anuncios del presidente chvez en 2011, se producen declaraciones de diversas organizaciones indgenas que tempranamente sealan las amenazas que representa este proyecto para la reproduccin de la vida en sus territorios . con la promulgacin del decreto 2.248 del 24 de febrero de 2016 para la creacin de la zona de desarrollo estratgico nacional arco minero del orinoco, numerosas declaraciones comienzan a surgir y diversas movilizaciones de protesta se van desarrollando.",
  "el da 9 de junio de 2016, la asamblea nacional convino desaprobar, mediante un acuerdo, el decreto dictado por el presidente maduro para autorizar el proyecto del amo . este acuerdo no tiene la capacidad para invalidar el decreto.",
  "a pesar de que el gobierno nacional ha planteado que los indgenas estn a favor del proyecto , ha surgido declaraciones pblicas y comunicados de importantes plataformas y organizaciones indgenas locales y amaznicas en las cules se expresa su desacuerdo con ste .",
  "resistance towards mining itself is very much absent because mining is the dominant source of livelihood. the local protest are largely directed towards the dominance of the largest companies and state regulation, which are both seen as obstacles to the profitability of local mining. the case is complex however, and most recently there has been widespread concerns expressed by inhabitants in zaruma after an elementary school was swallowed by a hole in the ground due to illegal mining underneath the city of zaruma.",
  "el presidente de la comunidad campesina san martn de sechura, francisco ayala y del frente de defensa de la baha, amaru ipanaqu, advirtieron que los mltiples conflictos sociales latentes en esta provincia piurana la convierten en una bomba de tiempo.",
  "los dirigentes han enumerado los conflictos como el que mantienen con la empresa savia, por la explotacin de gas que pretende ejecutarse en la baha.",
  "mientras que con la empresa vale que explota los fosfatos de bayvar, denuncian el incumplimiento de contrato en la colocacin de puestos de trabajos.",
  "asimismo, indicaron que se mantiene el conflicto de los pescadores anchoveteros con el gobierno nacional al rechazar el d.s. n 05, mientras que los agricultores del desierto de sechura se oponen al proyecto minero de americas potash.",
  "civic complaints because of non payment of royalties",
  "the indians many times called for carrying on the exploration of the deposit themselves.",
  "argumentos sobre el patrimonio histrico y a la importancia del jade para la civilizacin maya ",
  "reclamos locales",
  "reclamo oficial de el salvador",
  "reclamo por la ausencia de estudios de impacto ambiental por parte del gobierno nacional, lo cual es requerido tanto por la crvb (art. 129) como por la legislacin ambiental venezolana",
  "detener el proyecto por los extraordinarios e irreversibles impactos ambientales que tendr no solo para los ecosistemas guayaneses sino tambin para toda la vida integral del pas",
  "al menos desde 2013, organizaciones de pueblos indgenas de la regin guayana han solicitado moratoria de los proyectos mineros en sus territorios",
  "indgenas e incluso la opinin pblica en general reconocen los perniciosos efectos provocados por el terrible avance de la minera ilegal en varias de las principales cuencas de los estados bolvar y amazonas. el gobierno nacional alega que el proyecto de mega-minera es para combatir este flagelo. sin embargo, las organizaciones indgenas y movimientos sociales que se oponen al amo indican que este proyecto no es la solucin al problema y que en cambio agravara sus devastadores efectos.",
  "prcticamente todos los pueblos indgenas han exigido que se demarquen y entreguen las tierras ancestrales que les corresponden por ley, como lo indica la constitucin bolivariana de venezuela",
  "se ha indicado que la zden arco minero del orinoco viola los decretos de abrae que incluye reas bajo rgimen de administracin especial (abrae), el parque nacional jaua-sarisariama, la reserva forestal el caura, monumentos naturales ichn-guanacoco, cerro guiquinima y la zona protectora sur del estado bolvar, cuyo mbito de proteccin no puede ser intervenido por una actividad tan impactante como la minera, lo que sera dejar sin efecto un necesario rgimen de proteccin vigente, que, en lugar de ser suprimido, debera ser ampliado y mejorado.",
  "reclamo por la ausencia de consulta previa en caso de que se programen actividades que podran impactar negativamente los hbitats de los pueblos indgenas, la cual est firmemente establecida tanto en la legislacin venezolana como internacional (convenio 169 de la oit).",
  "se exige que se conozcan los detalles de los acuerdos firmados dado que estos no han sido publicados y parecen ser un secreto para la poblacin.",
  "se ha sealado que los acuerdos con la minera gold reserve son contratos leoninos donde el estado venezolano asume prcticamente todos los riesgos y consecuencias de la iniciativa .",
  "se ha sealado que la explotacin minera propuesta no supera el rentismo, no fortalecer el aparato productivo nacional y no resuelve los graves problemas de nuestros tiempos .",
  "se han producido resistencias de los yanomami al ingreso de mineros en sus territorios, aunque los indgenas son rebasados por la superioridad de las armas de stos y de otros invasores. esto ha provocado una situacin en la cual se han mermado las denuncias, dado los altos niveles de intimidacin.",
  "las poblaciones indgenas exigen consulta previa.",
  "visitas tcnicas al sitio (vigilancia)",
  "activists accuse barrick gold of being a climate criminal due to the destruction of glaciers and that the company should be tried in the climate justice tribunal. because of complaints in chile, barrick gold has stopped the pascua lama project in 2013-14.",
  "spreading a viral video in the town, stopping trucks, and going in a demonstration to the harbour carrying candles (\"velas\" - hence, let us make a \"velatn\"). also, legal appeals to the the environmental authorities, to the supreme court, and intervention of local politicians.",
  "demanda internacional",
  "segn la coordinadora de organizaciones indgenas de amazonas (coiam), la propia poblacin de san fernando de atabapo ha venido denunciando la minera ilegal en el cauce del ro atabapo y en toda la zona cercana al parque nacional yapacana. el pueblo baniva a travs de la asociacin de pueblos indgenas de venezuela apiven ha solicitado la intervencin de la gobernacin del estado amazonas para combatir la presencia de grupos que explotan la minera de coltn en la zona, al tiempo que han elevado peticin para implementar la demarcacin y titulacin de sus tierras ancestrales.  coiam ha realizado varios pronunciamientos sobre el problema de la minera ilegal en esta entidad, haciendo llamados especficos a tomar medidas para enfrentarla en el municipio atabapo, y denunciando la falta de presencia del estado y la insuficiencia de polticas para enfrentar el problema. han sido impulsados pronunciamientos conjuntos de las agrupaciones indgenas de todo el estado amazonas como orpia, conive y coiam, llevando tambin denuncias a la fiscala general de la repblica, la defensora del pueblo, vicepresidencia de la repblica, ministerio de pueblos indgenas, ministerio de la defensa, alto mando militar de la zona y al tribunal supremo de justicia.",
  "exigencia de control de las autoridades competentes",
  "defense of sacredness of the territory",
  "argumentos sobre el patrimonio histrico y a la importancia del jade para la civilizacin maya",
  "land demarcation",
  "the community issued an ultimatum to the company to go away",
  ""
)

# add elements
conflicts[[120]]$FormsOfMobilization <- c("property damage/arson", "blockades", "street protest/marches")
conflicts[[151]]$FormsOfMobilization <- c("blockades", "development of a network/collective action","involvement of national and international ngos", "lawsuits, court cases, judicial activism","media based activism/alternative media", "objections to the eia", "official complaint letters and petitions", "public campaigns","street protest/marches", "hunger strikes and self immolation", "shareholder/financial activism")
conflicts[[157]]$FormsOfMobilization <- c("blockades", "official complaint letters and petitions", "street protest/marches", "hunger strikes and self immolation", "arguments for the rights of mother nature") # in appendix
conflicts[[242]]$FormsOfMobilization <- c("street protest/marches")

# create for loop
for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$FormsOfMobilization)=="list"){
    print(conflicts[[i]]$FormsOfMobilization[1])
    conflicts[[i]]$FormsOfMobilization = ""
  }
  # remove whitespace
  conflicts[[i]]$FormsOfMobilization = trimws(conflicts[[i]]$FormsOfMobilization, which = c("both"))
  
  # lowercase all
  conflicts[[i]]$FormsOfMobilization = unlist(lapply(conflicts[[i]]$FormsOfMobilization, tolower))
  
  # split elements separated by " // "
  conflicts[[i]]$FormsOfMobilization = unlist(strsplit(conflicts[[i]]$FormsOfMobilization, " // "))
  
  # substitute pseudonyms
  for(j in seq(nrow(substitutes_forms))){
    conflicts[[i]]$FormsOfMobilization <- gsub(toString(substitutes_forms[j,]$old), toString(substitutes_forms[j,]$new), conflicts[[i]]$FormsOfMobilization)
    conflicts[[i]]$FormsOfMobilization <- unique(conflicts[[i]]$FormsOfMobilization)
  }
  for(j in remove_forms){
    conflicts[[i]]$FormsOfMobilization = conflicts[[i]]$FormsOfMobilization[conflicts[[i]]$FormsOfMobilization != j]
    
  }
   conflicts[[i]]$FormsOfMobilization = gsub(".","", conflicts[[i]]$FormsOfMobilization, fixed = TRUE) #to not get "." in results
}

# # show clean list
# for(i in conflicts){
#   print(i$FormsOfMobilization)
# }

# count again
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "FormsOfMobilization"), "["))), desc(freq))

# Figure FormsOfMobilization

forms_data <- plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "FormsOfMobilization"), "["))), desc(freq))

# rename columns
forms_data <- forms_data %>%
  dplyr::select(forms_of_mobilization = x, count = freq)

#filter > 10
forms_data <- forms_data %>%
  dplyr::filter(count > 10)

# barplot with ggplot2
library(ggplot2)
library(forcats)

# Reorder following the value of another column:
forms_data %>%
  mutate(forms_of_mobilization = fct_reorder(forms_of_mobilization, count)) %>%
  ggplot( aes(x=forms_of_mobilization, y=count)) +
  geom_bar(stat="identity", fill="dark blue", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()


################################# CONFLICT OUTCOMES ########################

# count and arrange in descending order
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "ConflictOutcome"), "["))), desc(freq))

# create substitute values
substitutes_outcomes = read_delim("./substitutes/outcomes.csv", delim=';', escape_double=FALSE, escape_backslash=TRUE, quote='')

remove_outcomes = c("",
                    "the anti-asbestos association of bahia was present at rio 20 complaining against the companies.", #form of mobilization
      "los varios planes propuestos por el gobierno nacional para erradicar la minera ilegal han fracasado estrepitosamente. los indgenas organizados del caura continan produciendo declaraciones llamando la atencin del gobierno nacional. a juicio de kuyujani, hay una falta de autoridad en la zona, no se respeta el reglamento de uso de la cuenca del caura, pero sobre todo no se ha demarcado y entregado sus tierras (guzmn, 2010), lo que ha producido estos resultados actuales, que siguen empeorndose.", #description
      "el gobierno venezolano, aludiendo entre otras razones la lucha contra la minera ilegal, impulsa en la actualidad un megaproyecto minero denominado el arco minero del orinoco que tendra un importante impacto en los territorios del caura y sus pobladores.  los indgenas yekwana-sanema y pemn de la cuenca del ro caura emitieron un comunicado en contra del proyecto del arco minero (radio noticias venezuela, 2016).", #description
      "to emphasize conservation (against deforestation and mining) because the monarch buttefly sanctuary of el rosario is in angangueo.", #description
      "la minera ilegal en la zona sigue en dramtica expansin. la organizacin horonami anuncia que contina esperando respuesta de las autoridades ante las numerosas denuncias realizadas. los varios planes propuestos por el gobierno nacional para erradicar la minera ilegal en la regin han fracasado estrepitosamente. el papel de las fuerzas militares es fundamental en la bsqueda de soluciones al problema, sin embargo esta institucin ha mostrado una creciente desidia ante el problema y se ha sealado complicidad por parte de algunos integrantes de la institucin. los conflictos entre indgenas y militares podran recrudecerse.", #description
      "las declaraciones y movilizaciones en protesta de las organizaciones indgenas amaznicas se mantienen, sin poder saber cul podra ser el rumbo de estas luchas en estos complejos escenarios.", #description
      "acuerdos de compensacion incumplidos", #no category available
      "colapso del yacimiento", #no category available
      "surgimiento de asambleas que, debido a la experiencia negativa de bajo la alumbrera, rechazaron otros proyectos en la zona (principalmente, agua rica y pilciao 16).", #no category available
      "ha habido un referendum que las autoridades quieren desconocer. se discute si la poblacin es indgenra y se aplica el convenio 169.", #no category available
      "consulta comunitaria, sin xito. liderazgo de mujeres (magdalena sarat y otras). movimiento pacfico de comunidades indigenas, durante ocho aos.", #description
      "el juicio de nulidad en 2004 haba recaido en el tribunal federal de justicia fiscal y administrativa con sede en la ciudad de mxico. el uno de septiembre de 2004 la sala superior de este tribunal emiti la sentencia, en que resuelve la nulidad de la autorizacin de cambio de uso de suelo y la ambiental. o sea, fue una resolucin definitiva en la cual el caso quedaba como cosa juzgada.", #description
      "por desgracia , dada la corrupcin y la ingobernabilidad, y la complicidad de las autoridades de los diferentes niveles la empresa minera est operando en la etapa de explotacin desde 2007, sin contar con un solo permiso licencia o autorizacin.", #description
      "para este ao 2013, ya desapareci el cerro de san pedro, emblema del escudo de armas de nuestra ciudad y estado. ni un solo habitante acept la reubicacin, permanecen en sus hogares, pero eso no fue impedimento para las ambiciones de la empresa. detona 25 toneladas de explosivos diariamente a 30 metros de la zona habitada, poniendo en peligro la vida de sus habitantes ante el posible derrumbe de sus fincas.", #description

      "el gobierno nacional contina reconociendo el problema. en general se han establecido compromisos que tienen que ver con acelerar demarcacin y entrega de tierras a los indgenas, as como integrarlos a un plan de trabajo para conseguir el sustento, sea en otras actividades laborales o bien en minas donde si estara permitida esta actividad.", #description

      "en la actualidad, el gobierno nacional impulsa operativos especiales de seguridad, a modo de intervencin directa en los territorios por parte de las fuerzas armadas y otros organismos especializados (la llamada operacin de liberacin del pueblo  olp), en las zonas mineras del estado bolvar, para hacer frente a la inseguridad y la extraccin ilegal de minerales.", #description

      "el impulso del mega-proyecto del arco minero del orinoco es presentado como una solucin para regularizar la minera en la zona a travs de la explotacin formal de los recursos minerales all ubicados.", #description

      "indgenas pemn del alto paragua han sealado que estn decididos a todo si no se cumplen acuerdos y anuncian que permanecern unidos. estos pueblos han creado brigadas de seguridad o grupos de autodefensa para reivindicar sus derechos y combatir la inseguridad en sus territorios.", #description

      "las polticas impulsadas por el gobierno nacional respecto al problema de la  minera ilegal en la regin han fracasado. se siguen dilatando sus soluciones, lo que est provocando que siga en aumento la presencia de mineros ilegales y grupos armados que van ocupando progresivamente ms territorios indgenas y de reas protegidas.", #description

      "rodrigo tot was the winner of goldman environmental prize in 2017. he is an indigenous leader in guatemalas agua caliente,  who led his community to a landmark court decision that ordered the government to issue land titles to the qeqchi people and kept environmentally destructive mining operation from expanding into his community.", #additional information

      "los conocidos perjuicios de la mega-minera a cielo abierto han hecho que las organizaciones indgenas del amazonas se pronuncien en contra del arco minero.", #description

      "el estado venezolano ha desatendido las reivindicaciones constitucionales de los pueblos indgenas y luego de 17 aos de promulgarse la constitucin de 1999, an no se demarcan y asignan la gran mayora de sus tierras ancestrales, ni tampoco se realizan consultas previas antes de impulsar diversos proyectos de desarrollo.", #description

      "a pesar de la modificacin del decreto 1.606, el gobierno nacional mantiene el inters de relanzar el proyecto de extraccin de carbn. indgenas, junto a organizaciones sociales y ambientalistas permanecen movilizados para evitar que se concrete y oficialice la expansin extractiva en esta regin. la zona de la guajira se encuentra militarizada y fuertes tensiones continan en la zona.", #description
      "no se ha completado el proceso de entrega de tierras a los indgenas, ni se han terminado de pagar todas las bienhechuras que le corresponden al territorio. esta situacin sigue dejando muy vulnerados sus derechos, consagrados en la constitucin nacional.", #description
      "se mantiene una poltica de cooptacin, asimilacin y divisin de las comunidades indgenas, que ha logrado afiliar al ministerio de pueblos indgenas a varios de sus lderes. esto ha afectado sensiblemente las luchas de los yukpa como pueblo, siendo que los grupos ms aguerridos son minoritarios.", #description

      "ante la severa crisis que vive el pas, el gobierno nacional busca con premura levantar y relanzar la extraccin de carbn en el zulia. un mega-plan podra buscar activar nuevas minas en la zona media de la sierra de perij, lo que afectara considerablemente la superviviencia de los yukpa, bar, wayu y japreria de la zona.", #description

      "the mine extraction activity is still ongoing.", #redundant information

      "negociacin de la empresa con cientficos para preservar el patrimonio cultural. intervencin de ong para impedir la avanzada minera, comprando propiedades cercanas. requerimiento por parte de las autoridades de plan de cierre.", #description

      "las crticas y peticiones formuladas por los grupos movilizados no han sido escuchadas y atendidas",
      "hasta la fecha el gobierno nacional no ha dado a conocer los detalles de los acuerdos alcanzados con las compaas transnacionales para este proyecto",
      "no se ha realizado una consulta previa e informada a todos los pueblos indgenas de la zona a ser impactada, como lo establece la constitucin nacional",
      "no se han realizado los estudios de impacto ambiental que determinen con precisin los enormes daos que causara el proyecto",
      "respecto al recurso de nulidad introducido, el 22 de junio de 2016 la sala poltico-administrativa del tribunal supremo de justicia admiti la demanda interpuesta",
      "el documento de la asamblea nacional que desaprueba el decreto del arco minero no ha tenido mayor efecto para tratar de detener el avance del proyecto", #ERROR?
      "a pesar de las solicitudes y crticas planteadas, el proyecto sigue avanzando, actualmente en proceso de exploracin", # description (all above)

      "se ha desarrollado un intenso debate en el seno de las organizaciones populares, con fuertes tendencias al rechazo al proyecto",
      "el gobierno nacional ha creado en junio de 2016 un ministerio de minera ecolgica , a principios de agosto ha promulgado la prohibicin del uso de mercurio en la actividad minera  y a principios de junio anunciaba que firmara un decreto especial para la proteccin de cuencas y ros del arco minero del orinoco , lo cual a la fecha no se ha hecho oficial. el gobierno nacional ha insistido en que trabajar con las mejores tecnologas para impulsar una minera ecolgica y amigable con el ambiente .", #ERROR?

      "el gobierno nacional est muy lejos de completar la demarcacin, titulacin y entrega de tierras ancestrales a pueblos indgenas, como lo establece la constitucin bolivariana.",

      "la minera ilegal continua avanzando en los territorios de la regin guayana, dejando terribles consecuencias en los ecosistemas y los pueblos indgenas", #description (all above)

      "numerosas peticiones planteadas por los pueblos indgenas y otras organizaciones que los acompaan, no han sido atendidas hasta la fecha. se han llevado adelante operativos militares para atacar la minera ilegal y se han realizado varias confiscaciones de coltn. sin embargo, estas medidas no han podido acabar con este problema.", #description

      "dado el valor del coltn en el mercado mundial, la expansin y fortalecimiento de la minera ilegal en venezuela, y tomando en cuenta los terribles antecedentes de conflictos provocados por la extraccin del mismo en pases como el congo, existe una potencialidad de escalada del conflicto actual que se produce en torno a este mineral en el pas.",

      "una respuesta oficial ante esta amenaza, la militarizacin de las zonas de reservas propuesta para el caso venezolano desde 2010, tiene tambin consecuencias en la reproduccin de la vida de los pueblos indgenas.", #description

      "el gobierno venezolano ha propuesto como una de las soluciones al problema de la minera ilegal, impulsar la megaminera formal: en 2011, el ministerio de industrias trazaba un plan minero con la creacin de una empresa estatal de coltn, en la actualidad lleva adelante las fases preliminares del mega-proyecto del arco minero del orinoco, una franja de recursos minerales al sur del ro orinoco con una extensin de ms de 111.000 km2, en el cual se propone la extraccin masiva de oro, diamantes y coltn, entre otros, y que cubre tambin al municipio cedeo del estado bolvar.", #description

      "a decision from cidh (interamerican commission for human rights) asked for the closure of the mine. this was notimplemeted by the government of guatemala under president alvaro colom.", #redundant
      "in 2018, there are complaints al local level and an appeal has been made to the united nations by zoque indigenous people.", #no outcome
      "mostr impactos ambientales y sociales causado por empresa china, lo cual fue importante para otro conflicto (loncopu), donde la misma empresa quera explotar un yacimiento de cobre.", #description

      "reunin de pruebas de contaminacin por parte del fiscal.", #no category available
      "renuncia del ttulo minero por parte de la empresa", #no category available

      "strong presence of indigenous women leaders", #description
      "visibilidad de la denuncia de impacto en glaciares de empresas mineras", #environmental impacts
      "campaa masiva de cartas al congreso y semarnat", #forms of mobilization

      "pruebas de contaminacin reunidas por fiscal.", #no category available

      "las numerosas polticas impulsadas por el gobierno nacional respecto al problema de la  minera ilegal en la regin han fracasado. se siguen dilatando sus soluciones, lo que est provocando que siga en aumento la presencia de mineros ilegales y grupos armados que van ocupando progresivamente ms territorios. complejas relaciones de poder y grupos de inters interfieren en la construccin de alternativas a este problema.",
      "persisten los daos ambientales y las agresiones a los pueblos indgenas, quienes adems no se sienten protegidos por las fuerzas militares presentes en amazonas. el estado venezolano ha desatendido las reivindicaciones constitucionales de los pueblos indgenas y luego de 17 aos de promulgarse la constitucin de 1999, an no se demarcan y asignan la gran mayora de sus tierras ancestrales, ni tampoco se realizan consultas previas antes de impulsar diversos proyectos de desarrollo.",

      "el proyecto del arco minero del orinoco, un plan de impulso a la megaminera en una larga franja al norte del estado bolvar, ha sido presentado al pas como un mecanismo para formalizar la actividad minera. las grandes dimensiones del proyecto tendr impactos tambin en el estado amazonas, por lo que las organizaciones indgenas de la entidad se han pronunciado en contra de dicho proyecto.", #description
      "las numerosas polticas impulsadas por el gobierno nacional respecto al problema de la  minera ilegal en la regin han fracasado. se siguen dilatando sus soluciones, lo que est provocando que siga en aumento la presencia de mineros ilegales y grupos armados que van ocupando progresivamente ms territorios. complejas relaciones de poder y grupos de inters interfieren en la construccin de alternativas a este problema.", #description
      "persisten los daos ambientales y las agresiones a los pueblos indgenas, quienes adems no se sienten protegidos por las fuerzas militares presentes en amazonas. el estado venezolano ha desatendido las reivindicaciones constitucionales de los pueblos indgenas y luego de 17 aos de promulgarse la constitucin de 1999, an no se demarcan y asignan la gran mayora de sus tierras ancestrales, ni tampoco se realizan consultas previas antes de impulsar diversos proyectos de desarrollo.", #description

      "el proyecto del arco minero del orinoco, un plan de impulso a la megaminera en una larga franja al norte del estado bolvar, ha sido presentado al pas como un mecanismo para formalizar la actividad minera. las grandes dimensiones del proyecto tendr impactos tambin en el estado amazonas, por lo que las organizaciones indgenas de la entidad se han pronunciado en contra de dicho proyecto.", #description
      "a public consultation was done on 21/11/11 in the village of torata, que le dijo un rotundo no a la instalacin de la empresa minera anglo american quellaveco s.a. que pretende ubicar su proyecto en las cabeceras de cuenca del ltimo afluente que cuenta este valle enclavado en la regin moquegua y que sirve de sustento a miles de agricultores.\n\nla consulta vecinal convocada por el gobierno local y las organizaciones vivas del distrito de torata concluy con un contundente 79.95% que le dice no, contra un 16.00% que voto por el si, ante la pregunta esta ud. de acuerdo con la actividad que quiere realizar la empresa minera anglo american quellaveeco s.a.?\n\nentre tanto respecto al uso de los recursos hdricos, fue ms contundente con un 81.32% por el no y un 10.77% que dijo si a la pregunta esta ud. de acuerdo con el uso de aguas subterraneas y superficiales, para las actividades minaras del proyecto minero anglo american quellaveeco s.a y otros proyectos mineros en el distrito de torata?.", #forms of mobilization

      "srong pro mining state policy after the coup against president zelaya", #description
      "informe negativo de la subsecretara de industria, comercio y minera sobre la existencia de metales.", #description-other info
      "provincia de buenos aires.", #redundant
      "appeal to un organizations for human rights", #no outcome
      "disputed local royalties", #description-content
      "a public consultation or referendum took place in 2009, against the project.", #form of mobilization
      "posibilidad de convocar a referendo nacional", #not real outcome
      "the opposition failed to stop the mine, and there is after 2012 a process of plan de cierre because gold deposits have been taken out.", #description
      "there have been attempts at bringing the company to criminal justice (environmental crimes).\n\nthere has also been shareholder activism: el 12 de junio 2013 activistas de london mining network, war on want y la red chile 40 aos participaron como accionistas disidentes en la asamblea general de antofagasta minerals plc, un gigante minero chileno-britnica que cotiza en la bolsa de londres. los activistas mostraron su solidaridad con el pueblo de caimanes en el norte de chile.", #forms of mobilization
      "el proyecto est solicitando su permiso de exploracin", #description
      "persisten los daos ambientales y las agresiones a los pueblos indgenas, quienes adems no se sienten protegidos por las fuerzas militares presentes en amazonas. el gobierno venezolano ha propuesto como una de las soluciones al problema de la minera ilegal, impulsar la megaminera formal a travs del proyecto del arco minero del orinoco, en el cual se propone la extraccin masiva de oro, diamantes y coltn, entre otros, en un rea que cubre toda la franja norte del estado bolvar. aunque dicho proyecto no abarca directamente el estado amazonas, si lo incluye en sus reas de influencia (impactos sociales, ambientales y culturales), sobre todo el municipio manapiare, fronterizo con el rea 1 del arco (municipio cedeo, estado bolvar).", #description (intimidation)

      "realization of a public hearing by plataforma dhesca brazil. \n\ncreation of a virtual space of discussion of local problems by mopsam.", #general assemblies, public forums or discussion tables -> not as own category
  "conformacin de redes de asambleas provinciales. este es el segundo caso de una formacin de una asamblea contra la megaminera metalfera en neuqun (el primer caso son las asambleas de loncopu y campana mahuida), y articula con asambleas de junn de los andes, san martn de los andes, alumin, entre otras." #general assemblies, public forums or discussion tables -> not as own category
)
# create for loop
for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$ConflictOutcome)=="list"){
    print(conflicts[[i]]$ConflictOutcome[1])
    conflicts[[i]]$ConflictOutcome = ""
  }
  # remove whitespace
  conflicts[[i]]$ConflictOutcome = trimws(conflicts[[i]]$ConflictOutcome, which = c("both"))
  
  # lowercase all
  conflicts[[i]]$ConflictOutcome = unlist(lapply(conflicts[[i]]$ConflictOutcome, tolower))
  
  # split elements separated by " // "
  conflicts[[i]]$ConflictOutcome = unlist(strsplit(conflicts[[i]]$ConflictOutcome, " // "))
  
  # substitute pseudonyms
  for(j in seq(nrow(substitutes_outcomes))){
    conflicts[[i]]$ConflictOutcome <- gsub(toString(substitutes_outcomes[j,]$old), toString(substitutes_outcomes[j,]$new), conflicts[[i]]$ConflictOutcome)
    conflicts[[i]]$ConflictOutcome <- unique(conflicts[[i]]$ConflictOutcome)
  }
  for(j in remove_outcomes){
    conflicts[[i]]$ConflictOutcome = conflicts[[i]]$ConflictOutcome[conflicts[[i]]$ConflictOutcome != j]
    
  }
}


# add elements
conflicts[[120]]$ConflictOutcome <- c("criminalization of activists", "declaration of location as ecological municipality, natural or religious sanctuary or cultural heritage" )
conflicts[[242]]$ConflictOutcome <- c("project temporarily suspended")

# count again
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "ConflictOutcome"), "["))), desc(freq))

# Figure Conflict Outcomes/Responses

# create dataframe for plot
outcomes_data <- plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "ConflictOutcome"), "["))), desc(freq))

# rename columns
outcomes_data <- outcomes_data %>%
  dplyr::select(outcomes = x, count = freq)

# group into violent and institutional outcomes
outcomes_violent <- outcomes_data[c(3,4,5,6,9,10),]
outcomes_institutional <- outcomes_data[c(1,2,7,8,11,12,13,14,15,16,17,18,19,20,21,22,23),] # without 24,25,26 because count > 10

# barplot with ggplot2
library(ggplot2)
library(forcats)

# Reorder following the value of another column:
outcomes_data %>%
  mutate(outcomes = fct_reorder(outcomes, count)) %>%
  ggplot( aes(x=outcomes, y=count)) +
  geom_bar(stat="identity", fill="dark green", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# plot violent outcomes
outcomes_violent %>%
  mutate(outcomes = fct_reorder(outcomes, count)) %>%
  ggplot( aes(x=outcomes, y=count)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# plot institutional outcomes
outcomes_institutional %>%
  mutate(outcomes = fct_reorder(outcomes, count)) %>%
  ggplot( aes(x=outcomes, y=count)) +
  geom_bar(stat="identity", fill="dark green", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

###################### ENVIRONMENTAL IMPACTS  #############################

# count and arrange in descending order
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "EnvironmentalImpactsVisible"), "["))), desc(freq))
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "EnvironmentalImpactsPotential"), "["))), desc(freq))

# rename first part before comma and remove rest (duplicates)

# Biodiversity loss (wildlife, agro-diversity)
# Surface water pollution / Decreasing water (physico-chemical, biological) quality
# Floods (river, coastal, mudflow)

# substitute values in list
substitutes_eimpacts = list(
  c("biodiversity loss \\(wildlife, agro-diversity\\)", "biodiversity loss \\(wildlife"),
  c("surface water pollution / decreasing water \\(physico-chemical, biological\\) quality", "surface water pollution / decreasing water \\(physico-chemical"),
  c("floods \\(river, coastal, mudflow\\)", "floods \\(river")
)

remove_eimpacts = c("agro-diversity)", "biological) quality", "mudflow)", "coastal")

# create for loop for ENVIRONMENTAL IMPACTS VISIBLE

for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$EnvironmentalImpactsVisible)=="list"){
    print(conflicts[[i]]$EnvironmentalImpactsVisible[1])
    #conflicts[[i]]$SpecificCommodities = unlist(conflicts[[i]]$SpecificCommodities)
    conflicts[[i]]$EnvironmentalImpactsVisible = ""
  }
  # remove whitespace
  conflicts[[i]]$EnvironmentalImpactsVisible = trimws(conflicts[[i]]$EnvironmentalImpactsVisible, which = c("both"))

  # lowercase all
  conflicts[[i]]$EnvironmentalImpactsVisible = unlist(lapply(conflicts[[i]]$EnvironmentalImpactsVisible, tolower))


  # substitute pseudonyms
  for(j in substitutes_eimpacts){
    conflicts[[i]]$EnvironmentalImpactsVisible = gsub(j[2], j[1], conflicts[[i]]$EnvironmentalImpactsVisible)
    conflicts[[i]]$EnvironmentalImpactsVisible <- unique(conflicts[[i]]$EnvironmentalImpactsVisible)
  }
  for(j in remove_eimpacts){
    conflicts[[i]]$EnvironmentalImpactsVisible = conflicts[[i]]$EnvironmentalImpactsVisible[conflicts[[i]]$EnvironmentalImpactsVisible != j]
  }
}

# create for loop for ENVIRONMENTAL IMPACTS POTENTIAL

for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$EnvironmentalImpactsPotential)=="list"){
    print(conflicts[[i]]$EnvironmentalImpactsPotential[1])
    #conflicts[[i]]$SpecificCommodities = unlist(conflicts[[i]]$SpecificCommodities)
    conflicts[[i]]$EnvironmentalImpactsPotential = ""
  }
  # remove whitespace
  conflicts[[i]]$EnvironmentalImpactsPotential = trimws(conflicts[[i]]$EnvironmentalImpactsPotential, which = c("both"))

  # lowercase all
  conflicts[[i]]$EnvironmentalImpactsPotential = unlist(lapply(conflicts[[i]]$EnvironmentalImpactsPotential, tolower))

  # substitute pseudonyms
  for(j in substitutes_eimpacts){
    conflicts[[i]]$EnvironmentalImpactsPotential = gsub(j[2], j[1], conflicts[[i]]$EnvironmentalImpactsPotential)
    conflicts[[i]]$EnvironmentalImpactsPotential <- unique(conflicts[[i]]$EnvironmentalImpactsPotential)
  }
  for(j in remove_eimpacts){
    conflicts[[i]]$EnvironmentalImpactsPotential = conflicts[[i]]$EnvironmentalImpactsPotential[conflicts[[i]]$EnvironmentalImpactsPotential != j]
  }
}

# count and arrange in descending order
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "EnvironmentalImpactsVisible"), "["))), desc(freq))
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "EnvironmentalImpactsPotential"), "["))), desc(freq))

################################ HEALTH IMPACTS ###########################

# arrange in descending order
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "HealthImpactsVisible"), "["))), desc(freq))
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "HealthImpactsPotential"), "["))), desc(freq))

# rename first part before comma and remove rest (duplicates)

# Exposure to unknown or uncertain complex risks (radiation, etc)
# Violence related health impacts (homicides, rape, etc..)
# Mental problems including stress, depression and suicide
# Health problems related to alcoholism, prostitution

# substitute values in list
substitutes_himpacts = list(
  c("exposure to unknown or uncertain complex risks (radiation, etc)", "exposure to unknown or uncertain complex risks \\(radiation"),
  c("violence related health impacts (homicides, rape, etc..)", "violence related health impacts \\(homicides"),
  c("mental problems including stress, depression and suicide", "mental problems including stress"),
  c("health problems related to alcoholism, prostitution", "health problems related to alcoholism")
)

remove_himpacts = c("etc)", "rape", "etc..)", "depression and suicide", "prostitution")

# create for loop for HEALTH IMPACTS VISIBLE
for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$HealthImpactsVisible)=="list"){
    print(conflicts[[i]]$HealthImpactsVisible[1])
    #conflicts[[i]]$SpecificCommodities = unlist(conflicts[[i]]$SpecificCommodities)
    conflicts[[i]]$HealthImpactsVisible = ""
  }
  # remove whitespace
  conflicts[[i]]$HealthImpactsVisible = trimws(conflicts[[i]]$HealthImpactsVisible, which = c("both"))

  # lowercase all
  conflicts[[i]]$HealthImpactsVisible = unlist(lapply(conflicts[[i]]$HealthImpactsVisible, tolower))


  # substitute pseudonyms
  for(j in substitutes_himpacts){
    conflicts[[i]]$HealthImpactsVisible = gsub(j[2], j[1], conflicts[[i]]$HealthImpactsVisible)
    conflicts[[i]]$HealthImpactsVisible <- unique(conflicts[[i]]$HealthImpactsVisible)
  }
  for(j in remove_himpacts){
    conflicts[[i]]$HealthImpactsVisible = conflicts[[i]]$HealthImpactsVisible[conflicts[[i]]$HealthImpactsVisible != j]
  }
}

# create for loop for HEALTH IMPACTS POTENTIAL
for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$HealthImpactsPotential)=="list"){
    print(conflicts[[i]]$HealthImpactsPotential[1])
    #conflicts[[i]]$SpecificCommodities = unlist(conflicts[[i]]$SpecificCommodities)
    conflicts[[i]]$HealthImpactsPotential = ""
  }
  # remove whitespace
  conflicts[[i]]$HealthImpactsPotential = trimws(conflicts[[i]]$HealthImpactsPotential, which = c("both"))

  # lowercase all
  conflicts[[i]]$HealthImpactsPotential = unlist(lapply(conflicts[[i]]$HealthImpactsPotential, tolower))


  # substitute pseudonyms
  for(j in substitutes_himpacts){
    conflicts[[i]]$HealthImpactsPotential = gsub(j[2], j[1], conflicts[[i]]$HealthImpactsPotential)
    conflicts[[i]]$HealthImpactsPotential <- unique(conflicts[[i]]$HealthImpactsPotential)
  }
  for(j in remove_himpacts){
    conflicts[[i]]$HealthImpactsPotential = conflicts[[i]]$HealthImpactsPotential[conflicts[[i]]$HealthImpactsPotential != j]
  }
}

# arrange in descending order
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "HealthImpactsVisible"), "["))), desc(freq))
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "HealthImpactsPotential"), "["))), desc(freq))


########################## SOCIOECONOMIC IMPACTS ############################

# arrange in descending order
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "SocioeconomicImpactsVisible"), "["))), desc(freq))
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "SocioeconomicImpactsPotential"), "["))), desc(freq))

# rename first part before comma and remove rest (duplicates)
# Lack of work security, labour absenteeism, firings, unemployment
# Social problems (alcoholism, prostitution, etc..)

# substitute values in list
substitutes_simpacts = list(
  c("lack of work security, labour absenteeism, firings, unemployment", "lack of work security"),
  c("social problems (alcoholism, prostitution, etc..)", "social problems \\(alcoholism")
)

remove_simpacts = c("labour absenteeism", "firings", "unemployment", "prostitution", "etc..)")

# create for loop for SOCIOECONOMIC IMPACTS VISIBLE
for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$SocioeconomicImpactsVisible)=="list"){
    print(conflicts[[i]]$SocioeconomicImpactsVisible[1])
    #conflicts[[i]]$SpecificCommodities = unlist(conflicts[[i]]$SpecificCommodities)
    conflicts[[i]]$SocioeconomicImpactsVisible = ""
  }
  # remove whitespace
  conflicts[[i]]$SocioeconomicImpactsVisible = trimws(conflicts[[i]]$SocioeconomicImpactsVisible, which = c("both"))

  # lowercase all
  conflicts[[i]]$SocioeconomicImpactsVisible = unlist(lapply(conflicts[[i]]$SocioeconomicImpactsVisible, tolower))


  # substitute pseudonyms
  for(j in substitutes_simpacts){
    conflicts[[i]]$SocioeconomicImpactsVisible= gsub(j[2], j[1], conflicts[[i]]$SocioeconomicImpactsVisible)
    conflicts[[i]]$SocioeconomicImpactsVisible <- unique(conflicts[[i]]$SocioeconomicImpactsVisible)
  }
  for(j in remove_simpacts){
    conflicts[[i]]$SocioeconomicImpactsVisible = conflicts[[i]]$SocioeconomicImpactsVisible[conflicts[[i]]$SocioeconomicImpactsVisible != j]
  }
}

# create for loop for SOCIOECONOMIC IMPACTS POTENTIAL
for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$SocioeconomicImpactsPotential)=="list"){
    print(conflicts[[i]]$SocioeconomicImpactsPotential[1])
    #conflicts[[i]]$SpecificCommodities = unlist(conflicts[[i]]$SpecificCommodities)
    conflicts[[i]]$SocioeconomicImpactsPotential = ""
  }
  # remove whitespace
  conflicts[[i]]$SocioeconomicImpactsPotential = trimws(conflicts[[i]]$SocioeconomicImpactsPotential, which = c("both"))

  # lowercase all
  conflicts[[i]]$SocioeconomicImpactsPotential = unlist(lapply(conflicts[[i]]$SocioeconomicImpactsPotential, tolower))


  # substitute pseudonyms
  for(j in substitutes_simpacts){
    conflicts[[i]]$SocioeconomicImpactsPotential= gsub(j[2], j[1], conflicts[[i]]$SocioeconomicImpactsPotential)
    conflicts[[i]]$SocioeconomicImpactsPotential <- unique(conflicts[[i]]$SocioeconomicImpactsPotential)
  }
  for(j in remove_simpacts){
    conflicts[[i]]$SocioeconomicImpactsPotential = conflicts[[i]]$SocioeconomicImpactsPotential[conflicts[[i]]$SocioeconomicImpactsPotential != j]
  }
}

# arrange in descending order again
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "SocioeconomicImpactsVisible"), "["))), desc(freq))
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "SocioeconomicImpactsPotential"), "["))), desc(freq))
