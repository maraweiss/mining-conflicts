# accuracy of location & type of population #

## ACCURACY OF LOCATION ##

# count and arrange in descending order
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "AccuracyOfLocation"), "["))), desc(freq))

substitutes_location = read_delim("./substitutes/accuracy_of_location.csv", delim=',', escape_double=FALSE, escape_backslash=TRUE, quote='"')

for(i in seq(1,length(conflicts))){
  if(typeof(conflicts[[i]]$AccuracyOfLocation)=="list"){
    print(conflicts[[i]]$AccuracyOfLocation[1])
    conflicts[[i]]$AccuracyOfLocation = ""
  }
  for(j in seq(nrow(substitutes_location))){
    conflicts[[i]]$AccuracyOfLocation= gsub(toString(substitutes_location[j,]$old), toString(substitutes_location[j,]$new), conflicts[[i]]$AccuracyOfLocation)
  }
}

# count again
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "AccuracyOfLocation"), "["))), desc(freq))

## TYPE OF POPULATION ##

# count and arrange in descending order
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "TypeOfPopulation"), "["))), desc(freq))

# add unknown to empty conflict
conflicts[[277]]$TypeOfPopulation <- c("Unknown")

# change Semi-Urban to Semiurban
for(i in seq(1,length(conflicts))){
  conflicts[[i]]$TypeOfPopulation <- sub("-", "", conflicts[[i]]$TypeOfPopulation)}

# count again
plyr::arrange(plyr::count(unlist(lapply(map(conflicts, "TypeOfPopulation"), "["))), desc(freq))

