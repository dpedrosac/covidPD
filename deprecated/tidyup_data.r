
## First specify the packages of interest
packages = c("tidyverse") #

## Now load or install all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## General settings
# =================================================================================================
wdir <- setwd("/media/storage/covidPD")					# set working directory
data = as.data.frame(read.csv(file.path(wdir, "data", "raw_data.csv")))	# read data to data frame
na_if(data, 99) # Convert values of "99" to NA, probably also needed for other pseudo values

description = 	[
				"Subject", 
				"Date at which questionnaire was completed",
				"Country of residence",
				"Consent obtained ?", 
				"Disease duration according to medical records?",
				"Hoehn and Yahr disease stage ?, according to ?",
				...
				] # TODO: this should be moved to a different (txt) file according to colnames.txt

## Provide information for the levels provided
# =================================================================================================
data$regular_caregiver_categorial.B1a <- as.factor(data$regular_caregiver_categorial.B1a)
values = c("no", "yes")
for (v in c(1:2)) {
	#print(v)
	levels(data$regular_caregiver_categorial.B1a)[levels(data$regular_caregiver_categorial.B1a)==v-1] <- values[v]
}


data$D5_Maritalstatus <- as.factor(data$D5_Maritalstatus)
values = c("yes", "no", "maybe", "wishnot", "yesmany")
for (v in c(1:5)) {
	print(values[v])
	levels(data$D5_Maritalstatus)[levels(data$D5_Maritalstatus)==v] <- values[v]
}
