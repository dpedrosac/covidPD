# This is code to run analyses for the covidPD project;
# Code developed by Marlena van Munster, Marcel Printz and David Pedrosa

## First specify the packages of interest
packages = c("readxl", "tableone", "dplyr") #

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## In case of multiple people working on one project, this helps to create an automatic script
# =================================================================================================
username = Sys.info()["login"]
## @Marcel und Marlena, please insert your username here, so just enter Sys.info()["login"] in R and fill out username and add your folder at which you save the data below
if (username == "dpedr") {
	wdir = "D:/covidPD"
} else if (username == "david") {
	wdir = "/media/storage/covidPD/"
}
} else if (username == "marlena") {
	wdir = "/media/storage/covidPD/"
if (username == "Marcel Printz") {
        wdir = C:\Users\Marcel Printz\Documents\iCARE PD
} else if (username == "marcel") {
	wdir = "/media/storage/covidPD/"
}
setwd(wdir)

# Load data and tidy up data
# =================================================================================================
df 		<- read.csv(file.path(wdir, "data", "raw_data.csv")) # read data frama from raw data
cols		<- read.csv(file.path(wdir, "colnames.txt"), header=FALSE, stringsAsFactors=FALSE)$V1 # read column names
colnames(df) 	<- cols[1:length(colnames(df))]								# replace column names with cleaned version in 'colnames.txt'
values2replace 	<- 93:99 # replaced everything that was not coded appropiatelya

for (val in values2replace){ # replaces all values that make no sense with NA
	df = df %>% mutate(df=replace(df, df==val, NA))
}

## Create demographics for each country
# =================================================================================================
vars <- c("age.D1", "gender.D2", "educational_level.D8", "disease_duration.A1", "disease_stage.A1")
tableOne <- CreateTableOne(vars=vars, data=df, strata="country") # @Marcel, here the vars of interest should be renamed and there should be a way to calculate medians instead of means if necessary (e.g., "A2_stageofPD") see also https://rpubs.com/kaz_yos/CreateTableOne1
