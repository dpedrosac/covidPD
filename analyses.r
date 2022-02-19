# This is code to run analyses for the covidPD project;
# Code developed by Marlena van Munster, Marcel Printz and David Pedrosa

# Version 1.6 # 2022-02-19, recoded all values, refined analyses

## First specify the packages of interest
packages = c(	"readxl", "tableone", "dplyr", "tidyverse", "coin", "lemon", 
				"rstatix", "RColorBrewer", "mice", "caret", "VIM", "doParallel" )

# ==================================================================================================
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
if (username == "dpedr") {
	wdir = "D:/covidPD"
	cl <- makePSOCKcluster(5)
	registerDoParallel(cl)
} else if (username == "david") {
	wdir = "/media/storage/covidPD/"
} else if (username == "root") {
	wdir = "/Users/marlenavm/Desktop/COVID-PD"
}
setwd(wdir)

# ==================================================================================================
# Load and tidy up raw data with the majority of columns of interest (further data to be added below)

df_total 								<- read.csv(file.path(wdir, "data", "raw_data.csv")) # read data frama from raw (csv/xlsx-file)
cols			    					<- read.csv(file.path(wdir, "colnames.txt"), header=FALSE, stringsAsFactors=FALSE)$V1 # read column names to frename in next line
colnames(df_total) 						<- cols[1:length(colnames(df_total))]	# replace column names with cleaned version in 'colnames.txt'
df_total[(df_total>=93 & df_total<=99)] <- NA 	# convert values that are not meaningful into NAs
df_total[df_total<0]					<- NA 	# convert values that are not meaningful into NAs

# ==================================================================================================
# Comorbidity index
filename_comorbidities  		<- file.path(wdir, "data", "vanWalravenElixhauserIndex.csv")
df_comorbidities 			  	<- read.csv2(filename_comorbidities)
df_comorbidities 			  	<- data.frame(vWEI=df_comorbidities$value)

# ==================================================================================================
# Neurologists per 100.000 inhabitants and average population (GER) (for details cf. preprocess_geospatial_data.r)
df_demographics <-read.csv(file.path(wdir, "data", "demographics_per_postal_code.csv"))
for (item in 1:length(df_demographics$plz)){ # dodgy way of entering zeros when postal codes have less than 5 digits
	if (!is.na(df_demographics$plz[item]) & nchar(df_demographics$plz[item]) < 3){
		df_demographics$plz[item] <- paste0("0", df_demographics$plz[item])
	} else {
	df_demographics$plz[item] <- df_demographics$plz[item]
	}
}	

population_data 	<- data.frame( # create empty dataframe of the size of data_raw
  populationGER=rep(NA, dim(df_total)[1]), 
  neurologistsGER=rep(NA, dim(df_total)[1]),
  physiciansGER=rep(NA, dim(df_total)[1]))

# ==================================================================================================
# only use regional data of interest, that is German data
idx_GER 		<- which(df_total$country=="GE") # index of German data
plzGER 			<- unique(df_total$postal_code[idx_GER]) # all postal codes available in the plzGER data

for (vals in plzGER) { # for loop intended to extract population, neurologists and total physician density to matrix
  idx_demographics = which(df_demographics$plz==vals)
  idx_df = which(df_total$postal_code==vals)
  if (identical(idx_demographics, integer(0))){
    population_data$populationGER[idx_df] = NA
    population_data$neurologistsGER[idx_df] = NA
    population_data$physiciansGER[idx_df] = NA
  } else {
    population_data$populationGER[idx_df] = df_demographics$sqm[idx_demographics]
    population_data$neurologistsGER[idx_df] = df_demographics$neurologists[idx_demographics]
    population_data$physiciansGER[idx_df] = df_demographics$physicians[idx_demographics]
  }
} 

# Concatenate data into one dataframe
df_total 		<- data.frame(cbind(df_total, population_data, df_comorbidities)) # add data to data matrix to work with
dfGER 			<- df_total[idx_GER,]

# ==================================================================================================
## Tidy up factors before analyses (adapt corresponding levels)
# PART ...A
df_total$disease_duration_cat <- as.factor(df_total$disease_duration.A1)
levels(df_total$disease_duration_cat) <- c(">2 years", "2-5 years", "5-10 years", "10-15 years", ">15 years") 

#df_total$disease_stage_name <- as.factor(df_total$disease_stage.A2)
df_total <- df_total %>% mutate(disease_stage_name = factor(disease_stage.A2, labels = paste("Hoehn & Yahr", as.roman(c(1:5)))))
df_total$disease_stage.A2 <- as.integer(df_total$disease_stage.A2)

# PART ...B
df_total <- df_total %>% mutate(regular_caregiver_categorial.B1a = factor(regular_caregiver_categorial.B1a, labels = c("1", "2")))
df_total$regular_caregiver_categorial.B1a <- as.integer(df_total$regular_caregiver_categorial.B1a)

df_total <- df_total %>% mutate(GP_expertise.B3 = fct_relevel(factor(GP_expertise.B3), c("5", "4", "3", "2", "1")))
df_total$GP_expertise.B3 <- as.integer(df_total$GP_expertise.B3)

df_total <- df_total %>% mutate(neurologists_expertise.B5 = fct_relevel(factor(neurologists_expertise.B5), c("5", "4", "3", "2", "1")))
df_total$neurologists_expertise.B5 <- as.integer(df_total$neurologists_expertise.B5)

df_total <- df_total %>% mutate(cooperation_healthcare_providers_yesorno.B6a = fct_relevel(factor(cooperation_healthcare_providers_yesorno.B6a), c("4", "3", "2", "1")))
df_total$cooperation_healthcare_providers_yesorno.B6a <- as.integer(df_total$cooperation_healthcare_providers_yesorno.B6a)

df_total <- df_total %>% mutate(overcoming_barriers_sum.B7a = factor(overcoming_barriers_sum.B7a, labels = c("2", "1")))
df_total$overcoming_barriers_sum.B7a <- as.integer(df_total$overcoming_barriers_sum.B7a)

df_total <- df_total %>%  mutate(local_availability_sum_categorized.B8 = fct_relevel(factor(local_availability_sum_categorized.B8), c("4", "3", "2", "1")))
df_total$local_availability_sum_categorized.B8 <- as.integer(df_total$local_availability_sum_categorized.B8)

df_total <- df_total %>%  mutate(ease_obtaining_healthcare_priorCovid.B10 = fct_relevel(factor(ease_obtaining_healthcare_priorCovid.B10), c("4", "3", "2", "1")))
df_total$ease_obtaining_healthcare_priorCovid.B10 <- as.integer(df_total$ease_obtaining_healthcare_priorCovid.B10)

df_total <- df_total %>%  mutate(ease_obtaining_healthcare_priorCovid_categorized.B10 = fct_relevel(factor(ease_obtaining_healthcare_priorCovid_categorized.B10), c("2", "1")))
df_total$ease_obtaining_healthcare_priorCovid_categorized.B10 <- as.integer(df_total$ease_obtaining_healthcare_priorCovid_categorized.B10)

df_total$extended_health_insurance_due_to_PD.B12[df_total$extended_health_insurance_due_to_PD.B12==9] <- NA

df_total <- df_total %>%  mutate(satisfaction_PDcare_priorCovid.B17 = fct_relevel(factor(satisfaction_PDcare_priorCovid.B17), c("4", "3", "2", "1")))
df_total$satisfaction_PDcare_priorCovid.B17 <- as.integer(df_total$satisfaction_PDcare_priorCovid.B17)

# PART ...C
df_total <- df_total %>%  mutate(access_to_technology.C2c2 = fct_relevel(factor(access_to_technology.C2c2), c("4", "3", "2", "1")))
df_total$access_to_technology.C2c2 <- as.integer(df_total$access_to_technology.C2c2)

df_total <- df_total %>%  mutate(access_to_technology_categorized.C2c2 = fct_relevel(factor(access_to_technology_categorized.C2c2), c("2", "1")))
df_total$access_to_technology_categorized.C2c2 <- as.integer(df_total$access_to_technology_categorized.C2c2)

df_total <- df_total %>%  mutate(confidence_in_accessing_necessary_services_remotely.C3_3 = fct_relevel(factor(confidence_in_accessing_necessary_services_remotely.C3_3), c("4", "3", "2", "1")))
df_total$confidence_in_accessing_necessary_services_remotely.C3_3 <- as.integer(df_total$confidence_in_accessing_necessary_services_remotely.C3_3)

df_total <- df_total %>%  mutate(satisfaction_with_care_duringCovid.C6 = fct_relevel(factor(satisfaction_with_care_duringCovid.C6), c("4", "3", "2", "1")))
df_total$satisfaction_with_care_duringCovid.C6 <- as.integer(df_total$satisfaction_with_care_duringCovid.C6)

df_total <- df_total %>%  mutate(satisfaction_with_care_duringCovid_categorized.C6 = fct_relevel(factor(satisfaction_with_care_duringCovid_categorized.C6), c("2", "1")))
df_total$satisfaction_with_care_duringCovid_categorized.C6 <- as.integer(df_total$satisfaction_with_care_duringCovid_categorized.C6)

#PART ...D
df_total$gender.D2 			<- as.factor(df_total$gender.D2)
levels(df_total$gender.D2) 	<- c("female", "male")

df_total <- df_total %>%  mutate(extent_of_financial_stability.D10 = fct_relevel(factor(extent_of_financial_stability.D10), c("5", "4", "3", "2", "1")))
df_total$extent_of_financial_stability.D10 <- as.integer(df_total$extent_of_financial_stability.D10)

df_total <- df_total %>%  mutate(extent_of_financial_stability_categorized.D10 = fct_relevel(factor(extent_of_financial_stability_categorized.D10), c("2", "1")))
df_total$extent_of_financial_stability_categorized.D10 <- as.integer(df_total$extent_of_financial_stability_categorized.D10)

df_total$educational_level.D8 <- as.factor(df_total$educational_level.D8)
df_total$educational_level.D8[df_total$educational_level.D8==13] <- NA

df_total <- df_total %>% droplevels() %>%  mutate(educational_level.D8 = fct_relevel(factor(educational_level.D8), c("4", "3", "2", "1"))) 
df_total <- df_total %>%  mutate(income.D9 = fct_relevel(factor(income.D9), c("3", "2", "1")))

df_total$education_cat <- df_total$educational_level.D8
levels(df_total$education_cat) <- c("primary education", "secondary education", "post secondary education", "highest education level possible")

# ...OTHER
df_total <- df_total %>% mutate(quantile_population = ntile(populationGER, 5)) # converts German population data to categories according to quantiles

# TODO: What do the following lines mean?
# ___________________________________________________________________________________________________________________________________________________________________________________________
# Adapting using remode() from car package could be: df_total$GP_expertise.B3_recode <- recode(df_total$GP_expertise.B3, "1=5; 2=4; 3=3; 4=2; 5=1") 

#C6: satisfaction_with_care_duringCovid_categorized.C6 not adapted, because it is already categorized. @Marlena: Which values does category "1" and "2" include?
#in the raw dataset there are variables C6 (contains values from 1-4) and categorized.C6 (contains values from 1-2)

#D6: type_of_community.D6 not adapted, because it is already categorized. @Marlena: Which values does category "1", "2" and "3" include?
#in the raw dataset there are variables D6 (contains values from 1-6) and categorized.D6 (contains values from 1-3) - here we could think of merging category 3 into category 2 because there are very little people in 3

# ==================================================================================================
## Create TableOne for specific values

Vars 					<- c(	"age.D1", "gender.D2", "disease_duration_cat", "disease_stage_name", "populationGER", "neurologistsGER", 
						"physiciansGER", "education_cat", "pdq8_total.A3", "vWEI")
df_tableOne 			<- df_total %>% select(all_of(Vars))

colnames_Vars			<- c("Age", "Gender", "Disease duration", "Disease stage", "Inhabiltants per sqkm", "Neurologists per sqkm", 
								"General practitioners per sqkm", "Education level according to ISCED", "PDQ-8 scores [in %]", "Van-Walraven-Elixhauser comorbidity index")
colnames(df_tableOne) 	<- colnames_Vars
factVars 				<- c("Gender", "Education level according to ISCED", "Disease duration", "Disease stage") # Here only values with categorial (ordinal distribution should be added) 
tableOne 				<- CreateTableOne(vars=colnames_Vars, factorVars=factVars, data=df_tableOne) # @Marcel, here the vars of interest should be renamed
print(tableOne, nonnormal=c("Disease stage", "Education level according to ISCED"))

# ==================================================================================================
# Start analysing major questions 
# Extract values of interest pre and during (cf. ...B17 vs. ...C6) 
df_careCovid 			<- df_total %>% select(satisfaction_PDcare_priorCovid.B17, satisfaction_with_care_duringCovid.C6) %>% drop_na()
colnames(df_careCovid) 	<- c("before", "during") # change colnames in meaningful way 
df_careCovid 			<- dplyr::mutate(df_careCovid, ID = row_number()) %>% 
							pivot_longer(
											cols = c("before", "during"), 
											names_to = "timing", 
											values_to = "ratings")

df_careCovid$ratings[(df_careCovid$ratings==-999 | df_careCovid$ratings ==5)] 	<- NA # -999 means "not answered because question not applicable based on a previous answer", a bit redundant after lines 41f.
df_careCovid$ID 									<- as.factor(df_careCovid$ID) # ID needs to be coded as factor

df_careCovid %>%
  group_by(timing) %>%
  get_summary_stats(ratings, show = c("mean", "sd", "median", "iqr"))  

# Create a summary of available results
summary_care 			<- df_careCovid %>% filter(timing=="before") %>% drop_na() %>% group_by(ratings)  %>% summarize(before=sum(ratings))
temp_data 				<- df_careCovid %>% filter(timing=="during") %>% drop_na()%>% group_by(ratings)  %>% summarize(during=sum(ratings))
summary_care 			<- merge(summary_care, temp_data, by="ratings", all = T)
summary_care[is.na(summary_care)]=0.001

colnames(summary_care) 	<- c("ratings", "before", "during")
summary_care$before 	<- summary_care$before/sum(summary_care$before)
summary_care$during 	<- summary_care$during/sum(summary_care$during)
summary_care 			<- dplyr::mutate(summary_care, ID = row_number()) %>% 
								pivot_longer(
											cols = c("before", "during"), 
											names_to = "timing", 
											values_to = "percentage")

summary_care <- summary_care %>% filter(ratings>0)
symmetry_test(ratings~as.factor(timing) | ID, data=df_careCovid, distribution="exact")
stat.test <- df_careCovid  %>%
  sign_test(ratings ~ timing) %>%
  add_significance() %>%
  add_xy_position(x = "ratings", dodge = 0.8)
stat.test

# Start plotting results for changes in perceived care during pandemic
bar_width 	<- .7 
y_start 	<- summary_care %>% filter(timing=="before") %>% summarize(y_start=cumsum(percentage))
y_end 		<- summary_care %>% filter(timing=="during") %>% summarize(y_end=cumsum(percentage))
df_lines 	<- data.frame(y_start=y_start, y_end=y_end, x_start=rep(1+bar_width/1.99,4), x_end=rep(2-bar_width/1.99,4))

summary_care$timing <- as.factor(summary_care$timing)
levels(summary_care$timing) <- c("before", "during") # explicit definition of order necessary as otherwise data is sorted alphabetically (cf. https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph)

# ==================================================================================================
# Figure 1: Satisfaction with PD-care before and  during COVID-19 pandemic
# items: a. satisfaction_PDcare_priorCovid.B17 vs. b. satisfaction_with_care_duringCovid.C6

p_satisfaction_with_care <- ggplot(summary_care, aes(x = timing, y = percentage)) +
	geom_col(data = summary_care,
           aes(x = timing, y = percentage, fill = factor(ratings)), 
		   position_fill(reverse = TRUE),  width = bar_width) + 
	# stat_pvalue_manual(stat.test, label = "p", y.position = 1.4, step.increase = 0.2) +
	scale_y_continuous(labels = scales::percent) +
	geom_text(data=summary_care, aes(label = sprintf("%0.2f", round(100*percentage, digits = 2))), colour="black", size = 3, position = position_stack(vjust = 0.5)) +
	scale_x_discrete(labels= c('Before \nCOVID-19 pandemic', 'During \nCOVID-19 pandemic')) +
	scale_fill_brewer(palette = 1, name="Satisfaction\nwith PD-related care",
					  labels=c("very satisfied", "rather satisfied","rather unsatisfied","very unsatisfied")) + 
	labs(x = "",y = "Percentage of questionnaires", caption=sprintf("Data from n = %s participants", 399)) + 
	geom_segment(data = df_lines, colour="black", size=.25,
			aes(x = x_start, xend = x_end, y = y_start, yend = y_end)) + 
	theme_minimal() +
	theme(text = element_text(size = 12),
		  plot.caption = element_text(hjust = 0, face="italic"), 
		  legend.title = element_text(hjust = .5, color = "black", size = 12, face = "bold"),
		  axis.text = element_text(size = 12))
p_satisfaction_with_care
# ==================================================================================================


# ==================================================================================================
# Start with Odds rations 
# Use needed_healthcare_but_did_not_receive_it_duringCovid.C4, which ranges from 1 to 5 (and NA). 

# source: https://doi.org/10.3233/jpd-212735
#+----------------------------+--------------------------------------+
#| Question from COVID Survey |  Representative for what barrierer   |
#+----------------------------+--------------------------------------+
#| 1. A2, B1                  | Autonomy                             |
#| 2. A1, A4, vWEI            | Health Status                        |
#| 3. D8, D9                  | Health Literacy                      |
#| 4. B3, B5                  | Health Belief                        |
#| 5. B14a                    | Communication (personal)             |
#| 6. PDQ-sum score           | Self-efficacy                        |
#| 7. B7a, B9a/b              | Transportation                       |
#| 8. B11, B12, B13, D10      | Cost of care                         |
#| 9. NA          	      | Difficulties of Diagnosis            |
#| 10. C3, B6a, B6            | Coordination in care                 |
#| 11. B15, B14, C2c          | Communication (system)               |
#| 12. B16, D6, D7, B9b, B10  | Disparty in Health Services          |
#| 13. B7, B8, B9,            | Unavailability of Specalist Services |
#| 14. D2 	       	      | Other				     |
#+----------------------------+--------------------------------------+
# created with: https://ozh.github.io/ascii-tables/

dependent_variable = "needed_healthcare_but_did_not_receive_it_duringCovid.C4"
factorsOR1 <- c( # sorted according to the barriers proposed in https://doi.org/10.3233/jpd-212735
						"disease_stage.A2", "regular_caregiver_categorial.B1a", #checked
						# ============================================================== 2->#
						"disease_duration.A1", "comorbidities_sum.A4", "vWEI", 	#checked
						# ============================================================== 3->#						
						"educational_level.D8", "income.D9",  		#checked			
						# ============================================================== 4->#						
						"GP_expertise.B3", "neurologists_expertise.B5", #checked
						# ============================================================== 5->#												
						"reason_for_communication_challenges_sum_categorized.B14a", #checked
						# ============================================================== 6->#						
						"pdq8_categorial.A3", #checked
						# ============================================================== 7->#												
						"overcoming_barriers_sum.B7a", "inability_to_access_care_sum_categorized_priorCovid.B9a", #checked
						# ============================================================== 8->#						
						"forgot_treatment_due_to_cost_priorCovid_categorized.B11", #checked
						"extended_health_insurance_due_to_PD.B12", "financial_difficulties_from_treatment_costs_categorized.B13", #migth need to relevel B12
						"extent_of_financial_stability_categorized.D10", #checked 
						# ============================================================== 9->#						
						# 9 not applicable to our dataset
						# ============================================================== 10->#						
						"confidence_in_accessing_necessary_services_remotely_categorized.C3_3", "cooperation_healthcare_providers_yesorno.B6a", #checked
						"visit_healthcare_providers_sum.B6", #checked
						# ============================================================== 11->#												
						"reason_for_experiencing_stigmatisation_sum_categorized_RC1.B15", "received_remote_sessions_duringCovid.C2", #might need to relevel C2
						"access_to_technology_categorized.C2c2", "communication_challenges_priorCovid.B14", #checked
						# ============================================================== 12->#												
						"personal_accessibility_barriers_sum.B16a", "personal_accessibility_barriers_sum_categorized.B16a", #checked
						"type_of_community_categorized.D6", "living_situation_categorized.D7",
						"barriers_to_care_sum_categorized.B9b", #checked
						"ease_obtaining_healthcare_priorCovid_categorized.B10", #checked
						# ============================================================== 13->#						
						"geographical_barriers_healthcare_providers.B7", "quantile_population", #checked					
						"local_availability_sum_categorized.B8", "ability_to_access_care_priorCovid.B9", "neurologistsGER", #checked
						# ============================================================== 14->#							
						"gender.D2") #checked

group 				<- c(	1,1, # grouping is needed for later and corresponds to factorsOR1
							2,2, # Health status
							3,3,3, # Health Literacy
							4,4, # ... see below for further categories ...
							5,
							6,
							7,7,
							8,8,8,8,
							10, 10, 10,
							11, 11, 11, 11,
							12,12,12,12,12, 12,
							13,13,13,13,13,
							14
						)

barriers = c(	"Autonomy", "Health Status", "Health Belief", "Health Literacy", "Communication (personal)", "Self-efficacy", 
				"Transportation", "Cost of care", "Difficulties of Diagnosis", "Coordination in care", "Communication (system)",
				"Disparty in Health Services", "Unavailability of Specalist Services", "Other")

# Extract data of interest into new dataframe and estimate (isolated) OR to the dv ("needed_healthcare_but_did_not_receive_it_duringCovid.C4")
df_OR1_complete <- df_total %>% dplyr::select(dependent_variable, factorsOR1) # create dataframe with data of interest, that is dv and regressors
df_OR1_complete <- df_OR1_complete %>%
  mutate(
    dv = fct_collapse(
      as.factor(needed_healthcare_but_did_not_receive_it_duringCovid.C4),
      "no" = c("1", "2"),
      "yes" = c("3", "4", "5"))) %>%
	  filter(!is.na(dv))

# ==================================================================================================
# Get Odds ratios for every factor of interest	  
df_OR1_complete$educational_level.D8 <- as.integer(df_OR1_complete$educational_level.D8)
df_OR1_complete$income.D9 <- as.integer(df_OR1_complete$income.D9)

results1 = c()
for (fac in factorsOR1) { # for loop over factors of interest
  mod <- as.formula(sprintf("I(dv=='yes') ~ %s", fac)) # formula for (unadjusted) GLM
  fit_temp = glm(mod, data=df_OR1_complete, family="binomial") # estimate model
  results1 = rbind(	results1, c(exp(coef(fit_temp)[2]), 	# OR
					exp(confint.default(fit_temp)[2]),  	# lower CI
					exp(confint.default(fit_temp)[4]),  	# upper CI
					summary(fit_temp)$coefficients[2,4])) 	# significance value
}

results_OR1 <- data.frame(yAxis = length(factorsOR1):1, 
                     factors=factorsOR1,
                     factors_group=group,
                     boxOdds = results1[,1], 
                     boxCILow = results1[,2],
                     boxCIHigh = results1[,3],
					 pvalue=results1[,4])
results_OR1 <- results_OR1 %>% mutate(significance = case_when(pvalue <= .001 ~ 'p < .001', 
                                (pvalue < .05 & pvalue >.001) ~ 'p < .05',
								pvalue > .05 ~ 'ns')) %>% 
							   mutate(dot_color = case_when(pvalue <= .001 ~ "#6B340D", 
                                (pvalue < .05 & pvalue >.001) ~ "#B8A31F",
								pvalue > .05 ~ "#08306B"))
							
# ==================================================================================================
# Visualisation of (unadjusted) OR results:
# General commands
hlines 			<- cumsum(rle(group)$lengths)+.5 	# creates the limits for lines in between, to separate barriers cf. https://doi.org/10.3233/jpd-212735 
my_blue 		<- brewer.pal(n = 9, "Blues")		# next few lines create own color_palette
blue_palette 	<- colorRampPalette(c(my_blue[3], my_blue[6], my_blue[9]), space = "Lab")
my_blue2 		<- blue_palette(length(barriers))
dot_color 		<- results_OR1$dot_color

# Prepare data
results_OR1$factors_group <- as.factor(results_OR1$factors_group) # convert groups to factors 
levels(results_OR1$factors_group) <- barriers

predictors <- c( # TODO: We must try to keep the description as short as possible; I would propose to have the questions of the questionnare as text file appended
					"Disease Stage (Hoehn and Yahr)", #**
					"Presence of Regular Caregiver", #*
	                "Disease duration [in years]", #**
					"Sum of Comorbidities", #*
					"Elixhauser Comorbidity Index", #**(*) # TODO seems to be named difefrently throughout the script, please double check
					"Educational Level (according to ISCED)", #**
					"Annual income [in €]", #****
					"Perceived PD-Expertise of General Practitioner", #**
					"Perceived PD-Expertise of Neurologist", #**
						"Reasons for Communication Challenges before the COVID-19 Pandemic", #* # TODO: Not quite clear. Is it a number? A sum? A rate?
	                "PDQ-8 score", #**(*)
					"Available Ressources to Overcome Geographical Barriers before Pandemic", #**** # TODO: sum?
					"Ability to Access PD-related Care before Pandemic", #*
					"Shifted Healthcare Appointments due to financial resaons 12 Months before pandemic", #* 
	                "Extended Healthcare Insurance covering PD-related expenses", #*
					"Financial Problems due to PD-related Expenses 12 Months before pandemic", #*
                    "Financial Stability", #**
                    "Confidence Accessing PD-related Healthcare Remotely", #* TODO: What does this mean?!
					"Perceived Cooperation between Healthcare Providers", #**
					"Healthcare Providers Consulted 12 Months before pandemic", #****
                    "Experienced Stigmatization Using Healthcare Ressources", #*
						"Possibility of Remote Sessions with PD-related Healthcare Providers during pandemic", #* #TODO: What is the difference to next line?!
					"Access to Technology for Consulting Healthcare Providers during pandemic", #*
					"Communication Challenges before pandemic", #*
                    "Negative Effects on Patients due to Healthcare Accessibility Barriers", #****
					"Existence of Negative Effects on Patients due to Healthcare Accessibility Barriers", #* # TODO: What is this?
	                "Level of Urbanization", #** #TODO: is this a number?
						"Living Situation", #* #TODO: Not very precise. Situation about what?
	                "Sum of Barriers Preventing Healthcare Access before pandemic", #**
					"Difficulty Accessing PD-related Healthcare Services before pandemic", #*	
					"Geographical Barriers Concerning Access to Healthcare Ressources before pandemic", #**
					"Population according to quantiles of German population [in sqkm]", #****
	                "Locally Available PD-related Healthcare Ressources", #**
						"Frequency of Not Receiving Needed PD-related Healthcare before the COVID-19 Pandemic", #** #TODO: not quite clear to me what this means
                    "Neurologists nearby (per sqkm)", #****
                    "Gender *" #*
				)

# ==================================================================================================
# Figure 2: Odds rations for the question of unmet PD-related needs during pandemic
# item: needed_healthcare_but_did_not_receive_it_duringCovid.C4
results_OR1$factors <- predictors # change names of predictors according to list above
results_OR1$significance <- factor(results_OR1$significance, levels = c('ns', 'p < .05', 'p < .001')) # necessary to make legend look properly

# Sort values according to groups and values of OR, irrespective of 95%CI
results_OR1 %>%
  dplyr::arrange(boxOdds) %>%
  mutate(factors=factor(factors, levels=factors)) %>%  # This trick update the factor levels
  ggplot(aes(x = boxOdds, y = factors)) +
  geom_vline(aes(xintercept = 1), size = .75, linetype = "dashed", color="grey") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(aes(x = boxOdds, y = factors, color = factors_group), size = 1.5, show.legend=TRUE) +
  guides(colour = guide_legend(reverse=T)) +
  scale_colour_manual(values=my_blue2) +
  theme_blank() +
  theme(panel.grid.minor = element_blank(), legend.position = c(0.9, 0.2), 
		legend.title=element_blank()) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 20, 40), limits=c(.08, 40)) + 
  geom_hline(yintercept=hlines[1:length(hlines)-1], linetype="dotted", size = .01) +
  ylab("") +
  xlab("Odds ratio (log scale)")

# Change the way significance is coded
results_OR1 %>%
  dplyr::arrange(group, boxOdds) %>%
  mutate(factors=factor(factors, levels=factors)) %>%  # This trick update the factor levels
  ggplot(aes(x = boxOdds, y = factors)) +
  geom_vline(aes(xintercept = 1), size = .75, linetype = "dashed", color="grey") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(aes(x = boxOdds, y = factors, color = significance), size = 2, show.legend=TRUE) +
  guides(colour = guide_legend(reverse=TRUE)) +
  scale_colour_manual(values=c("#C6DBEF", "#4292C6", "#08306B")) +
  theme_minimal() +
  theme(text = element_text(size = 12),
		panel.grid.minor = element_blank(), 
		legend.position = c(0.95, 0.1), legend.title=element_blank(), legend.box.background = element_rect(colour = "black"),
		legend.background = element_rect(colour = "black", fill="white"),
		plot.title = element_text(vjust = 4, hjust = .5, color = "black", size = 12, face = "bold"),
		axis.line.x = element_line(size = .25, colour = "black"), 
		axis.ticks.x = element_line(size = .25, colour = "black"), 
		axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
		axis.text = element_text(size = 12),
		plot.caption = element_text(hjust = 0, face="italic")) +
  coord_capped_cart(bottom='right') +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 20, 40), limits=c(.1, 40), expand=c(0.2, 0)) + 
  geom_hline(yintercept=hlines[1:length(hlines)-1], linetype="dotted", size = .01) +
  ylab("") +
  xlab("Odds ratio (log scale)") + 
  # ggtitle("iPS-patients' odds of unmet need for healthcare services during COVID-19 pandemic") + 
  labs(title = "iPS-patients' odds of unmet need for healthcare services during COVID-19 pandemic",
  caption = "*Gender was coded so that negative odds means that being female indicates a higher risk of unmet needs")
#TODO: Please double check the results, as most of them look plausible while at least one is not quite clear: Confidence Accessing PD-related Healthcare Remotely increase significantly the Odds?!?

# ==================================================================================================
# Supplementary table 1: odds and Confidence intervals
suppl_table1 			<- results_OR1 %>% select(., -any_of(c("yAxis", "significance", "dot_color", "pvalue"))) %>% dplyr::mutate_if(is.numeric, round, 2)
pvalues 				<- results_OR1 %>% select("pvalue") %>% round(3)
pvalues[pvalues<.001] 	<- 'p < .001'
suppl_table1 			<- cbind(suppl_table1, pvalues)
colnames(suppl_table1) 	<- c("Factors", "Domain", "Odds Ratio", "CI.05", "CI.95", "p-value") 
write.csv(suppl_table1, file.path(wdir, "results", "supplementary_table1.csv"), row.names = F) # csv-file may be easily imported into text processing software

#display Odds ratio table
results_OR1
# ==================================================================================================
# ==================================================================================================
## DEPRECATED VERSIONS OF VISUALISATION
# Display results from OR estimation
# a) Sort values according to OR, irrespective of 95%CI
#results_OR1 %>%
#  dplyr::arrange(boxOdds) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
#  mutate(factors=factor(factors, levels=factors)) %>%  # This trick update the factor levels
#  ggplot(aes(x = boxOdds, y = factors)) +
#  geom_vline(aes(xintercept = 1), size = .75, linetype = "dashed", color="grey") +
#  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
#  geom_point(size = 1.5, color = "brown") +
#  theme_bw() +
#  theme(panel.grid.minor = element_blank()) +
# scale_x_continuous(breaks = seq(-5,5,1) ) +
#  coord_trans(x = "log10") +
#  ylab("") +
#  xlab("Odds ratio (log scale)")

# b) volcano plot; THIS IS EXPERIMENTAL AND LOOKS BAD!
#results_OR1 %>%
#  mutate(factors=factor(factors, levels=factors)) %>%  # This trick update the factor levels
#  ggplot(aes(x = boxOdds, y = -log10(pvalue))) +
#  geom_point(size = 1.5, color = group) +
#  theme_bw() +
#  theme(panel.grid.minor = element_blank()) +
#  coord_trans(x = "log10") +
#  ylab("") +
#  xlab("Odds ratio (log scale)") #+
# ==================================================================================================
# ==================================================================================================


# ==================================================================================================
# Variable factor selection using GLM with the dependent variable being:
# needed_healthcare_but_did_not_receive_it_duringCovid.C4, which ranges from 1 to 5 (and NA)
# Method inspired from http://rstudio-pubs-static.s3.amazonaws.com/448536_221fe7b85ca1471d8f4a53c05fcbe95b.html among other sources

# General functions
pMiss <- function(x){sum(is.na(x))/length(x)*100} # function to find missing (NA) values

data_full_glm <- df_OR1_complete %>% dplyr::select(factorsOR1, dv) %>% mutate(across(c(2,9,12,13, 15, 20, 21, 22, 26, 27), as.factor)) # getting data for the full-GLM
data_full_glm$pdq8_total.A3 <- as.numeric(data_full_glm$pdq8_total.A3)
data_full_glm$educational_level.D8[data_full_glm$educational_level.D8==13] <- NA

# Sanity checks
apply(data_full_glm,2,pMiss) # percentage of missing values per column
apply(data_full_glm,1,pMiss) # percentage of missing values per row

aggr_plot 			<- aggr(data_full_glm[-1], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
inlist 				<- c("disease_stage.A2", "gender.D2", "vWEI", "dv") # names of the variables that should be included as covariates in every imputation model

# 1. Data imputation using multivariate routines (MICE package)
pred 				<- quickpred(data_full_glm, minpuc = 0.5, include = inlist, mincor=.1) # predictor matrix
generate_imputation <- mice(data=data_full_glm,
							 predictorMatrix = pred, #predictormatrix,
							 m=10,
							 maxit=5,            
							 diagnostics=TRUE)
							 #MaxNWts=3000)				 

imputed_data_full = data.frame(complete(generate_imputation)) %>% drop_na()
imputed_data_full$visit_healthcare_providers_sum.B6[table(imputed_data_full$visit_healthcare_providers_sum.B6==5] <-4
#imputed_data_full <- imputed_data_full %>% mutate_if(is.factor, as.integer) # converts factors into integers
# ==================================================================================================
# Regression models w/ stepwise reduction using glmStepAIC {caret package}-algorithm with CV
# Separate data into train and test dataset
index 		<- createDataPartition(imputed_data_full$dv, p = 0.8, list = FALSE) # split data with balanced values for dv
train_data 	<- imputed_data_full[index,]
test_data 	<- imputed_data_full[-index,]

# Define model
train_data <- model.frame(dv ~ ., data = train_data, drop.unused.levels = TRUE) # drop unused factors
objControl <- trainControl(method = "cv", number = 10, #returnResamp = 'final',
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           repeats = 5)

train_control <- trainControl(
  method = "cv", number = 10,
  savePredictions = "final",
  classProbs = TRUE)

mdl_full <- train(train_data %>% select(-dv),
					train_data$dv,
					method = "glm",
					family = "binomial",
					trControl = train_control,
					metric = "Accuracy")
 
predicted_classes <- predict(mdl_full, newdata = train_data) 
predicted_probs <- predict(mdl_full, newdata = train_data, type = "prob")
confusionMatrix(predicted_classes, train_data$dv, positive = "yes") 
 
mdl_full_preds <- predict(mdl_full, newdata = train_data, type = "prob")
(mdl_full_eval <- evalmod(
  scores = mdl_full_preds$yes,
  labels = train_data$dv
))

options(yardstick.event_first = FALSE)  # set the second level as success
data.frame(
  pred = mdl_full_preds$yes, 
  obs = train_data$dv
) %>%
  yardstick::roc_curve(obs, pred) %>%
  autoplot() +
  labs(
    title = "Full Model ROC Curve, Test Data",
    subtitle = "AUC = ??"
)

reg_caret_model <- train(train_data %>% select(-dv),
                      train_data$dv, #train_data[,dim(train_data)[2]],
                      method = 'glmStepAIC', #lmStepAIC, BstLm, 
                      trControl = objControl,
					  #metric = "Accuracy",
					  preProcess = c("center", "scale"),
					  tuneLength = 5, metric = "Accuracy", #"ROC",
                      family = binomial, 
					  link="logit")

predicted_classes <- predict(reg_caret_model, newdata = train_data) 
predicted_probs <- predict(reg_caret_model, newdata = train_data, type = "prob")
confusionMatrix(predicted_classes, train_data$dv, positive = "yes")


scores_list <- join_scores(
  predict(mdl_full, newdata = train_data, type = "prob")$yes,
  predict(reg_caret_model, newdata = train_data, type = "prob")$yes)

labels_list <- join_labels(
  train_data$dv,
  train_data$dv)

pe <- evalmod(
  scores = scores_list, 
  labels = labels_list,
  modnames = c("Full", "glmStepAIC"), #, "Final (Training)"),
  posclass = "yes")

autoplot(pe, "ROC")

resamps <- resamples(list('Full' = mdl_full, 
                          'glmStepAIC' = reg_caret_model))#,
                          #'Final' = mdl_final_train))
summary(resamps)
bwplot(resamps, layout = c(2, 1))


predictTest = predict(reg_caret_model, newdata = test_data, type = "raw")
head(predict(reg_caret_model, newdata = test_data))


calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

calc_acc(actual = test_data[,34],
         predicted = predict(reg_caret_model, newdata = test_data))

# Stepwise regression
fit_temp_complete <- glm("I(dv=='yes') ~ .", data=td, family=binomial)
step.model <- stepAIC(fit_temp_complete, direction = "both", 
                      trace = TRUE)
summary(step.model)



step(fit_temp_complete, direction="backward")
summary(fit_temp_complete)



#results: @Marlena, this has changed as some more categories are included now!
# =====> @David: I will update this table as after the analysis
#+--------------+----------------------------------------------------------------------+------------+---------------------+
#  | Factor Group |                               Variable                               |  box_odds  |         CI          |
#  +--------------+----------------------------------------------------------------------+------------+---------------------+



#create g-plots for factors w. CI>1 from previous step (variables marked with "x")
# =====> @David: I will update this table as after the analysis

dfGER_plot <- dfGER
dfGER_plot[dfGER_plot<0] <- NA
#pdq8_total
#dfGER_plot$pdq8_total.A3[dfGER_plot$pdq8_total.A3<0] = NA
#p_test <- ggplot(aes(x=pdq8_total.A3, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
#p_test
