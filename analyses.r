# This is code to run analyses for the covidPD project;
# Code developed by Marlena van Munster, Marcel Printz and David Pedrosa

# Version 1.1 # 2021-12-19, added some analyses with TableOne and first pre/post statistics

## First specify the packages of interest
packages = c("readxl", "tableone", "dplyr", "tidyverse", "coin", "rstatix") #

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

# ==================================================================================================
# Load and tidy up data
df 				    <- read.csv(file.path(wdir, "data", "raw_data.csv")) # read data frama from raw (csv/xlsx-file)
cols			    <- read.csv(file.path(wdir, "colnames.txt"), header=FALSE, stringsAsFactors=FALSE)$V1 # read column names to frename in next line
colnames(df) 		<- cols[1:length(colnames(df))]	# replace column names with cleaned version in 'colnames.txt'
df[(df>=93 & df<=99)]= NA # convert values that are not meaningful into NAs

# Adapt the factors of the dataframe
df$gender.D2 = as.factor(df$gender.D2)
levels(df$gender.D2) <- c("female", "male")

# Here some factors need to be added such as educational_level, ...B9, ...C4 and more if used at later stages

# ==================================================================================================
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

# ==================================================================================================
# Add neurologist per 100.000 inhabitants and average population for German data (for details cf. preprocess_geospatial_data.r)
df_demographics <-read.csv(file.path(wdir, "data", "demographics_per_postal_code.csv"))

for (item in 1:length(df_demographics$plz)){ # dodgy way of entering zeros when postal codes have less than 5 digits
	if (!is.na(df_demographics$plz[item]) & nchar(df_demographics$plz[item]) < 3){
		df_demographics$plz[item] <- paste0("0", df_demographics$plz[item])
	} else {
	df_demographics$plz[item] <- df_demographics$plz[item]
	}
}	

extracted_data 	<- data.frame( # create empty dataframe of the size of data_raw
  populationGER=rep(NA, dim(df)[1]), 
  neurologistsGER=rep(NA, dim(df)[1]),
  physiciansGER=rep(NA, dim(df)[1]))

# ==================================================================================================
# only use regional data of interest, that is German data
idx_GER 		<- which(df$country=="GE") # index of German data
plzGER 			<- unique(df$postal_code[idx_GER]) # all postal codes available in the plzGER data

for (vals in plzGER) { # for loop intended to extract population, neurologists and total physician density to matrix
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

df 				<- cbind(df, extracted_data) # add data to data matrix to work with
dfGER 			<- df[idx_GER,]

# ==================================================================================================
## Create TableOne for specific values
Vars 			<- c("age.D1", "gender.D2", "disease_duration.A1", "disease_stage.A2", "populationGER", "neurologistsGER", "physiciansGER", "educational_level.D8", "pdq8_total.A3")
nonnormalVars 	<- c("disease_stage.A2") # avoids returning mean in favor of median 
factVars 		<- c("gender.D2", "educational_level.D8") # Here only values with categorial (ordinal distribution should be added) 
tableOne 		<- CreateTableOne(vars=Vars, factorVars=factVars, data=dfGER) # @Marcel, here the vars of interest should be renamed
print(tableOne, nonnormal=c("disease_Stage.A2"))

# ==================================================================================================
# Start analysing major questions 
# 1. Extract ability to access care before and after onset of pandemia (cf. ...B9 vs. ...C4)
df_careCovid 			<- df %>% select(ability_to_access_care_priorCovid.B9, needed_healthcare_but_did_not_receive_it_duringCovid.C4) %>% drop_na()
colnames(df_careCovid) 	<- c("pre", "post") # change colnames in  meaningful way 
df_careCovid 			<- dplyr::mutate(df_careCovid, ID = row_number()) %>% 
							pivot_longer(
											cols = c("pre", "post"), 
											names_to = "timing", 
											values_to = "ratings")

df_careCovid[df_careCovid==-999] 	<- NA #TODO @Marlena, what is coded as -999?
df_careCovid$ID 					<- as.factor(df_careCovid$ID) # ID needs to be coded as factor

df_careCovid %>%
  group_by(timing) %>%
  get_summary_stats(ratings, show = c("mean", "sd", "median", "iqr"))

#df$ability_to_access_care_priorCovid.B9 = as.factor(df$ability_to_access_care_priorCovid.B9)
#levels(df$ability_to_access_care_priorCovid.B9) <- c("Never", "Rarely", "Sometimes", "Regularly", "Always", "not applicable")

#df$needed_healthcare_but_did_not_receive_it_duringCovid.C4 = as.factor(df$needed_healthcare_but_did_not_receive_it_duringCovid.C4)
#levels(df$needed_healthcare_but_did_not_receive_it_duringCovid.C4) <- c("Never", "Rarely", "Sometimes", "Regularly", "Always", "not applicable")

# Create a summary of available results
summary_care 			<- df_careCovid %>% filter(timing=="pre") %>% group_by(ratings)  %>% summarize(pre=sum(ratings))
temp_data 				<- df_careCovid %>% filter(timing=="post") %>% group_by(ratings)  %>% summarize(post=sum(ratings))
summary_care 			<- cbind(summary_care, temp_data$post)
colnames(summary_care) 	<- c("ratings", "pre", "post")
summary_care$pre 		<- summary_care$pre/sum(summary_care$pre)
summary_care$post 		<- summary_care$post/sum(summary_care$post)
summary_care 			<- dplyr::mutate(summary_care, ID = row_number()) %>% 
								pivot_longer(
											cols = c("pre", "post"), 
											names_to = "timing", 
											values_to = "percentage"
										  )

# Start plotting results
bar_width 	<- .7 
y_start 	<- summary_care %>% filter(timing=="pre") %>% summarize(y_start=cumsum(percentage))
y_end 		<- summary_care %>% filter(timing=="post") %>% summarize(y_end=cumsum(percentage))
df_lines 	<- data.frame(y_start=y_start, y_end=y_end, x_start=rep(1+bar_width/2,5), x_end=rep(2-bar_width/2,5))

summary_care$timing <- as.factor(summary_care$timing)
levels(summary_care$timing) <- c("pre", "post") # expliict definition of order necessary as otherwise data is sorted alphabetically (cf. https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph)

p <- ggplot() +
	geom_col(data = summary_care,
           aes(x = timing, y = percentage, fill = factor(ratings)), position_fill(reverse = TRUE),  width = bar_width) + 
	scale_fill_brewer(palette = 1) + 
	geom_segment(data = df_lines, 
			aes(x = x_start, xend = x_end, y = y_start, yend = y_end))
# @ Marcel/Marlena please check if this makes sense. Not quite clear to me which values = 1 and which = 5 and if this reflects worsening or not 
p

symmetry_test(ratings~as.factor(timing) | ID, data=df_careCovid, distribution="exact")
stat.test <- df_careCovid  %>%
  sign_test(ratings ~ timing) %>%
  add_significance()
stat.test

# ==================================================================================================
# Start analysing major questions 
# 2. Extract values of interest pre and post (cf. ...B17 vs. ...C5)
df_careCovid 			<- df %>% select(satisfaction_PDcare_preCovid.B17, satisfaction_with_care_duringCovid.C6) %>% drop_na()
colnames(df_careCovid) 	<- c("pre", "post") # change colnames in  meaningful way 
df_careCovid 			<- dplyr::mutate(df_careCovid, ID = row_number()) %>% 
							pivot_longer(
											cols = c("pre", "post"), 
											names_to = "timing", 
											values_to = "ratings")

df_careCovid$ratings[df_careCovid$ratings==-999] 	<- NA #TODO @Marlena, what is coded as -999?
df_careCovid$ID 					<- as.factor(df_careCovid$ID) # ID needs to be coded as factor

df_careCovid %>%
  group_by(timing) %>%
  get_summary_stats(ratings, show = c("mean", "sd", "median", "iqr"))

# Create a summary of available results
summary_care 			<- df_careCovid %>% filter(timing=="pre") %>% drop_na() %>% group_by(ratings)  %>% summarize(pre=sum(ratings))
temp_data 				<- df_careCovid %>% filter(timing=="post") %>% drop_na()%>% group_by(ratings)  %>% summarize(post=sum(ratings))
summary_care 			<- merge(summary_care, temp_data, by="ratings", all = T)
summary_care[is.na(summary_care)]=0.001

colnames(summary_care) 	<- c("ratings", "pre", "post")
summary_care$pre 		<- summary_care$pre/sum(summary_care$pre)
summary_care$post 		<- summary_care$post/sum(summary_care$post)
summary_care 			<- dplyr::mutate(summary_care, ID = row_number()) %>% 
								pivot_longer(
											cols = c("pre", "post"), 
											names_to = "timing", 
											values_to = "percentage"
										  )

# Start plotting results
bar_width 	<- .7 
y_start 	<- summary_care %>% filter(timing=="pre") %>% summarize(y_start=cumsum(percentage))
y_end 		<- summary_care %>% filter(timing=="post") %>% summarize(y_end=cumsum(percentage))
df_lines 	<- data.frame(y_start=y_start, y_end=y_end, x_start=rep(1+bar_width/2,5), x_end=rep(2-bar_width/2,5))

summary_care$timing <- as.factor(summary_care$timing)
levels(summary_care$timing) <- c("pre", "post") # expliict definition of order necessary as otherwise data is sorted alphabetically (cf. https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph)

p <- ggplot() +
	geom_col(data = summary_care,
           aes(x = timing, y = percentage, fill = factor(ratings)), position_fill(reverse = TRUE),  width = bar_width) + 
	scale_fill_brewer(palette = 1) + 
	geom_segment(data = df_lines, 
			aes(x = x_start, xend = x_end, y = y_start, yend = y_end))
# @ Marcel/Marlena please check if this makes sense. Not quite clear to me which values = 1 and which = 5 and if this reflects worsening or not 
p

symmetry_test(ratings~as.factor(timing) | ID, data=df_careCovid, distribution="exact")
stat.test <- df_careCovid  %>%
  sign_test(ratings ~ timing) %>%
  add_significance()
stat.test

# ==================================================================================================
# Start with Odds rations 
# 1. Use needed_healthcare_but_did_not_receive_it_duringCovid.C4, which ranges from 1 to 5 (and NA). 

# source: publication 2021 @Marlena, can you add the doi?
#+----------------------------+--------------------------------------+
#| Question from COVID Survey |  Representative for what barrierer   |
#+----------------------------+--------------------------------------+
#| 1. A2, B1                  | Autonomy                             |
#| 2. A1, A3/ A4              | Health Status                        |
#| 4. B3, B5                  | Health Belief                        |
#| 3. D8, D9                  | Health Literacy                      |
#| 5. B14a                    | Communication (personal)             |
#| 6. PDQ-sum score           | Self-efficacy                        |
#| 7. B7a, B9a/b              | Transportation                       |
#| 8. B11, B12, B13, D10      | Cost of care                         |
#| 9. not applicable          | Difficulties of Diagnosis            |
#| 10. C3, B6a, B6            | Coordination in care                 |
#| 11. B15, B14a, C2c         | Communication (system)               |
#| 12. B16, D6, D7, B9b, B10  | Disparty in Health Services          |
#| 13. B7, B8, B9,            | Unavailability of Specalist Services |
#+----------------------------+--------------------------------------+
# created with: https://ozh.github.io/ascii-tables/

dependent_variable = "needed_healthcare_but_did_not_receive_it_duringCovid.C4"
factorsOR1 <- c(
						"disease_stage.A2", "regular_caregiver_categorial.B1a",
						"disease_duration.A1", "pdq8_total.A3", 	# ...	@ Marcel and Marlena: here further factors can be added. The list is supposed to be filled with Marlenas 
						"educational_level.D8", "income.D9",										# categories per line and groups in the next line should be the group to which it corresponds  					
						"GP_expertise.B3", "neurologists_expertise.B5", 
						"reason_for_communication_challenges_sum_categorized.B14a",
						"overcoming_barriers_sum.B7a", "ability_to_access_care_priorCovid_categorized.B9a",
						
						"forgot_treatment_due_to_cost_preCovid_categorized.B11",
						"extended_health_insurance_due_to_PD.B12", "financial_difficulties_from_treatment_costs_categorized.B13", 
						"extent_of_financial_stability.D10",
						"confidence_in_accessing_necessary_services_remotely_categorized.C3_3", "cooperation_healthcare_providers.B6a", 
						
						"visit_healthcare_providers_sum.B6", 
						"reason_for_experiencing_stigmatisation_sum_categorized_RC1.B15", "received_remote_sessions_duringCovid_categorized.C2a", 
						"personal_accessibility_barriers_sum.B16a", 
						"type_of_community.D6", "living_situation.D7",
						"barriers_to_care_sum_categorized.B9b",
						"ease_pbtaining_healthcare_preCovid.B10",
						"geographical_barriers_healthcare_providers.B7", 
						"local_availability_sum.B8", "ability_to_access_care_priorCovid.B9")

group 				<- c(	1,1,
							2,2, # Health status
							3,3,
							4,4,
							5,
							7,7,
							8,8,8,8,
							10, 10, 10,
							11, 11,
							12,12,12,12,12,
							13,13,13
						)

extracted_data <- df %>% select(dependent_variable, factorsOR1) # create dataframe with data of interest
extracted_data <- extracted_data %>%
  mutate(
    dv = fct_collapse(
      as.factor(needed_healthcare_but_did_not_receive_it_duringCovid.C4),
      "no" = c("1", "2"),
      "yes" = c("3", "4", "5"))) %>%
	  filter(!is.na(dv))
	  
results1 = c() # pre-allocate space
for (fac in factorsOR1) { # for loop to get results from all facors of interest
  mod <- as.formula(sprintf("I(dv=='yes') ~ %s", fac)) # formula needed for the GLM
  fit_temp = glm(mod, data=extracted_data, family="binomial") # GLM with formula from above
  results1 = rbind(results1, c(exp(coef(fit_temp)[2]),  exp(confint.default(fit_temp)[2]),  exp(confint.default(fit_temp)[4])))
}

df_OR1 <- data.frame(yAxis = length(factorsOR1):1, 
                     factors=factorsOR1,
                     factors_group=group,
                     boxOdds = results1[,1], 
                     boxCILow = results1[,2],
                     boxCIHigh = results1[,3])

# Sort values according to OR
df_OR1 %>%
  dplyr::arrange(boxOdds) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(factors=factor(factors, levels=factors)) %>%  # This trick update the factor levels
  ggplot(aes(x = boxOdds, y = factors)) +
  geom_vline(aes(xintercept = 1), size = .75, linetype = "dashed", color="grey") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 1.5, color = "brown") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  #scale_y_discrete(breaks = yAxis, labels = factorsOR1) +
  scale_x_continuous(breaks = seq(-5,5,1) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Odds ratio (log scale)") #+
  #xlim(0, 8)


# Sort values according to OR
df_OR1 %>%
  dplyr::arrange(group, boxOdds) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(factors=factor(factors, levels=factors)) %>%  # This trick update the factor levels
  ggplot(aes(x = boxOdds, y = factors)) +
  geom_vline(aes(xintercept = 1), size = .75, linetype = "dashed", color="grey") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 1.5, color = group) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  #scale_y_discrete(breaks = yAxis, labels = factorsOR1) +
  scale_x_continuous(breaks = seq(-5,5,1) ) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Odds ratio (log scale)") #+
  #xlim(0, 8)

