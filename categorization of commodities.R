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
