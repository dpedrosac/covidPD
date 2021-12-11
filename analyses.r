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

username = Sys.info()["login"]
## @Marcel und Marlena, please insert your username here, so just enter Sys.info()["login"] in R and fill out username and add your folder at which you save the data below
if (username == "dpedr") {
	wdir = "D:/covidPD"
} else if (username == "david") {
	wdir = "/media/storage/covidPD/"
}
setwd(wdir)

# Load data and tidy up data
df 				<- read.csv(file.path(wdir, "data", "raw_data.csv")) # read data frama from raw data
cols			<- read.csv(file.path(wdir, "colnames.txt"), header=FALSE, stringsAsFactors=FALSE)$V1 # read column names
colnames(df) 	<- cols[1:length(colnames(df))]								# replace column names with cleaned version in 'colnames.txt'
values2replace = 93:99 # replaced everything that was not coded appropiatelya
for (val in values2replace){ # replaces all values that make no sense with NA
	df = df %>% mutate(df=replace(df, df==val, NA))
}

# Create dataframe with one column per disese to be eyeballed; at the end a file should be present with all modifications so that the 
# Charlson Comorbidity Index can be estimated easily, for references (cf. https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0112479)
filename_comorbidities = file.path(wdir, "data", "comorbidities_mod.csv")
if (!file.exists(filename_comorbidities)) {
	df_cormobidities_list = data.frame(items=df$comorbidities_sum.A4)
	df_cormobidities_cols = df_cormobidities_list %>% separate(items, paste0("condition", c(1:10)), fill="right", sep="-")
	write.csv(df_cormobidities_cols, file.path(wdir, "data", "comorbidities_raw.csv"))
} else {
	df_cormobidities_cols <- read.csv2(filename_comorbidities)
}

## Create demographics for each country
vars <- c("age.D1", "gender.D2", "educational_level.D8", "disease_duration.A1", "disease_stage.A1")
tableOne <- CreateTableOne(vars=vars, data=df, strata="country") # @Marcel, here the vars of interest should be renamed and there should be a way to calculate medians instead of means if necessary (e.g., "A2_stageofPD") see also https://rpubs.com/kaz_yos/CreateTableOne1
