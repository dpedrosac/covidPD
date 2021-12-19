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
df 				    <- read.csv(file.path(wdir, "data", "raw_data.csv")) # read data frama from raw data
cols			    <- read.csv(file.path(wdir, "colnames.txt"), header=FALSE, stringsAsFactors=FALSE)$V1 # read column names
colnames(df) 	<- cols[1:length(colnames(df))]								# replace column names with cleaned version in 'colnames.txt'
df[(df=>93 & df<=99)]= NA # convert all values that are not meaningful to NAs

# Adapt the factors of the dataframe
df$gender.D2 = as.factor(df$gender.D2)
levels(df$gender.D2) <- c("female", "male")

df$ability_to_access_care_priorCovid.B9 = as.factor(df$ability_to_access_care_priorCovid.B9)
levels(df$ability_to_access_care_priorCovid.B9) <- c("Never", "Rarely", "Sometimes", "Regularly", "Always", "not applicable")

df$needed_healthcare_but_did_not_receive_it_duringCovid.C4 = as.factor(df$needed_healthcare_but_did_not_receive_it_duringCovid.C4)
levels(df$needed_healthcare_but_did_not_receive_it_duringCovid.C4) <- c("Never", "Rarely", "Sometimes", "Regularly", "Always", "not applicable")

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

# Add the neurologist 100.000 inhabitants and average population for German data

df_demographics <-read.csv(file.path(wdir, "data", "demographics_per_postal_code.csv"))

for (item in 1:length(df_demographics$plz)){ # dodgy way of entering zeros when postal codes have less than 5 digits
	if (!is.na(df_demographics$plz[item]) & nchar(df_demographics$plz[item]) < 3){
		df_demographics$plz[item] <- paste0("0", df_demographics$plz[item])
	} else {
	df_demographics$plz[item] <- df_demographics$plz[item]
	}
}	

idx_GER <- which(df$country=="GE")
plzGER <- unique(df$postal_code[idx_GER])
extracted_data <- data.frame( # create empty dataframe of the size of data_raw
  populationGER=rep(NA, dim(df)[1]), 
  neurologistsGER=rep(NA, dim(df)[1]),
  physiciansGER=rep(NA, dim(df)[1]))

for (vals in plzGER) {
  idx_demographics = which(df_demographics$plz==vals)
  idx_df = which(df$postal_code==vals)
  if (identical(idx_demographics, integer(0))){
    extracted_data$populationGER[idx_df] = NA
    extracted_data$neurologistsGER[idx_df] = NA
    extracted_data$physiciansGER[idx_df] = NA
  } else {
    extracted_data$populationGER[idx_df] = df_demographics$sqm[idx_demographics]
    extracted_data$neurologistsGER[idx_df] = df_demographics$neurologists[idx_demographics]
    extracted_data$physiciansGER[idx_df] = df_demographics$physicians[idx_demographics]
  }
} 

df = cbind(df, extracted_data)
dfGER = df[idx_GER,]

## Create demographics for specific values country
Vars <- c("age.D1", "gender.D2", "disease_duration.A1", "disease_stage.A2", "populationGER", "neurologistsGER", "physiciansGER", "educational_level.D8", "pdq8_total.A3")
nonnormalVars = c("disease_stage.A2") # avoids returning mean in favor of median 
factVars <- c("gender.D2", "educational_level.D8") # Here only values with categorial (ordinal distribution should be added) 
tableOne <- CreateTableOne(vars=Vars, factorVars=factVars, data=dfGER) # @Marcel, here the vars of interest should be renamed and there should be a way to calculate medians instead of means if necessary (e.g., "A2_stageofPD") see also https://rpubs.com/kaz_yos/CreateTableOne1

print(tableOne, nonnormal=c("disease_Stage.A2"))

# Start analysing some major questions 
# Extract the ability to access care before and after onset of pandemia B9 vs. C4
df_careCovid = df %>% select(ability_to_access_care_priorCovid.B9, needed_healthcare_but_did_not_receive_it_duringCovid.C4) %>% drop_na()
colnames(df_careCovid) <- c("pre", "post")
df_careCovid <- dplyr::mutate(df_careCovid, ID = row_number()) %>% pivot_longer(
    cols = c("pre", "post"), 
    names_to = "timing", 
    values_to = "ratings"
  )

df_careCovid[df_careCovid==-999] =NA
df_careCovid$ID <- as.factor(df_careCovid$ID)

summary_care 	<- df_careCovid %>% filter(timing=="pre") %>% group_by(ratings)  %>% summarize(pre=sum(ratings))
temp_data 		<- df_careCovid %>% filter(timing=="post") %>% group_by(ratings)  %>% summarize(post=sum(ratings))
summary_care 	<- cbind(summary_care, temp_data$post)
colnames(summary_care) <- c("ratings", "pre", "post")
summary_care$pre <- summary_care$pre/sum(summary_care$pre)
summary_care$post <- summary_care$post/sum(summary_care$post)
summary_care <- dplyr::mutate(summary_care, ID = row_number()) %>% pivot_longer(
    cols = c("pre", "post"), 
    names_to = "timing", 
    values_to = "percentage"
  )

p <- ggplot() +
  geom_bar(data = summary_care,
           aes(x = timing, y = percentage, fill = ratings), stat = 'identity', width = 0.5)

