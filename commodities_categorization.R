# categorization of commodities
substitutes_commodity2 = list(
  c("ferrous_metals", "iron_ore"),
  c("ferrous_metals", "steel"),
  c("ferroalloy_metals", "manganese"),
  c("ferroalloy_metals", "chrome"),
  c("ferroalloy_metals", "ferronickel"),
  c("ferroalloy_metals", "nickel"),
  c("ferroalloy_metals", "tungsten"),
  c("ferroalloy_metals", "molybdenum"),
  c("ferroalloy_metals", "vanadium"),
  c("ferroalloy_metals", "cobalt"),
  c("ferroalloy_metals", "niobium"),
  c("ferroalloy_metals", "coltan"),
  c("base_metals", "copper"),
  c("base_metals", "tin"),
  c("base_metals", "zinc"),
  c("base_metals", "lead"),
  c("light_metals", "aluminiumbauxite"),
  c("light_metals", "magnesium"),
  c("light_metals", "titanium_ores"),
  c("technology_elements", "antimony"),
  c("technology_elements", "lithium"),
  c("technology_elements", "mercury"),
  c("technology_elements", "rare_metals"),
  c("technology_elements", "tantalite"),
  c("technology_elements", "tantalum"),
  c("precious_metals", "gold"),
  c("precious_metals", "silver"),
  c("agricultural_chemical_minerals", "phosphorus"),
  c("agricultural_chemical_minerals", "phosphate"),
  c("agricultural_chemical_minerals", "potassium"),
  c("agricultural_chemical_minerals", "salt"),
  c("agricultural_chemical_minerals", "limestone"),
  c("agricultural_chemical_minerals", "lime"),
  
  #c("agricultural_chemical_minerals", "biological resources"),
  #c("agricultural_chemical_minerals", "chemical resources"),
  c("nonmetallic_minerals","cement"),
  c("nonmetallic_minerals", "asbestos"),
  c("nonmetallic_minerals", "chrysotile"),
  c("nonmetallic_minerals", "ferroginous_clay"),
  c("nonmetallic_minerals", "clay"),
  c("nonmetallic_minerals", "sand"),
  c("nonmetallic_minerals", "gravel"),
  c("nonmetallic_minerals", "kaolin"),
  c("nonmetallic_minerals", "barite"),
  c("nonmetallic_minerals", "sodium_borate"),
  c("energy_mineral_resources", "coal"),
  c("energy_mineral_resources", "coke"),
  c("energy_mineral_resources", "crude_oil"),
  c("energy_mineral_resources", "uranium") #etc
)

# for loop for substitution
for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$SpecificCommodities)=="list"){
    print(conflicts[[i]]$SpecificCommodities[1])
    #conflicts[[i]]$SpecificCommodities = unlist(conflicts[[i]]$SpecificCommodities)
    conflicts[[i]]$SpecificCommodities = ""
  }
  for(j in substitutes_commodity2){
    conflicts[[i]]$SpecificCommodities <- gsub(j[2], j[1], conflicts[[i]]$SpecificCommodities)
    conflicts[[i]]$SpecificCommodities <- unique(conflicts[[i]]$SpecificCommodities)
  }
}

# count again
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "SpecificCommodities"), "["))), desc(freq))
