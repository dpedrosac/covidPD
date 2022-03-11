# This is code to run analyses for the covidPD project;
# Code developed by Marlena van Munster, Marcel Printz and David Pedrosa

# Version 1.6 # 2022-02-19, recoded all values, refined analyses

flag_check=FALSE
## First specify the packages of interest
packages = c(	"readxl", "tableone", "dplyr", "tidyverse", "coin", "lemon", "precrec", "mctest", "magrittr",
				"rstatix", "RColorBrewer", "mice", "caret", "VIM", "doParallel", "fastDummies", "car",
				"MLmetrics", "gtsummary", "xfun")

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
} else if (username == "Marcel Printz") {
	wdir = "C:/Users/Marcel Printz/Desktop/covidPD"
	}
setwd(wdir)

# ==================================================================================================
# Load and tidy up raw data with the majority of columns of interest (further data to be added below)

df_total 		<- read.csv(file.path(wdir, "data", "raw_data.csv")) # read data frama from raw (csv/xlsx-file)
cols			<- read.csv(file.path(wdir, "colnames.txt"), header=FALSE, stringsAsFactors=FALSE)$V1 # read column names to frename in next line
colnames(df_total) 	<- cols[1:length(colnames(df_total))]	# replace column names with cleaned version in 'colnames.txt'
df_total[(df_total>=93 & df_total<=99)] <- NA 	# convert values that are not meaningful into NAs
df_total[df_total<0]	<- NA 	# convert values that are not meaningful into NAs

# ==================================================================================================
# Comorbidity index
filename_comorbidities  <- file.path(wdir, "data", "vanWalravenElixhauserIndex.csv")
df_comorbidities 	<- read.csv2(filename_comorbidities)
df_comorbidities 	<- data.frame(vWEI=df_comorbidities$value)

# ==================================================================================================
# Neurologists per 100.000 inh. & average population (GER) (cf. preprocess_geospatial_data.r)
df_demographics <-read.csv(file.path(wdir, "data", "demographics_per_postal_code.csv"))
for (item in 1:length(df_demographics$plz)){ # dodgy way to enter 0's when postal codes < 5 digits
	if (!is.na(df_demographics$plz[item]) & nchar(df_demographics$plz[item]) < 3){
		df_demographics$plz[item] <- paste0("0", df_demographics$plz[item])
	} else {
	df_demographics$plz[item] <- df_demographics$plz[item]
	}
}	

population_data <- data.frame( # create empty dataframe of the size of data_raw
				  populationGER=rep(NA, dim(df_total)[1]), 
				  neurologistsGER=rep(NA, dim(df_total)[1]),
				  physiciansGER=rep(NA, dim(df_total)[1]))

# ==================================================================================================
# only use regional data of interest, that is German data
idx_GER 		<- which(df_total$country=="GE") # index of German data
plzGER 			<- unique(df_total$postal_code[idx_GER]) # postal codes available in {plzGER}

for ( {in plzGER) { # for loop to extract population, neurologists and total physician density
  idx_demographics 	<- which(df_demographics$plz==vals)
  idx_df 		<- which(df_total$postal_code==vals)
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
df_total 	<- data.frame(cbind(df_total, population_data, df_comorbidities)) # add data to working data matrix
dfGER 		<- df_total[idx_GER,]

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

# ==================================================================================================
## Create TableOne for specific values

Vars 			<- c(	"age.D1", "gender.D2", "disease_duration_cat", "disease_stage_name", "populationGER", "neurologistsGER", 
				"physiciansGER", "education_cat", "pdq8_total.A3", "vWEI")
df_tableOne 		<- df_total %>% select(all_of(Vars))
colnames_Vars		<- c("Age", "Gender", "Disease duration", "Disease stage", "Inhabiltants per sqkm", "Neurologists per sqkm", 
				"General practitioners per sqkm", "Education level according to ISCED", "PDQ-8 scores [in %]", 
			     	"Van-Walraven-Elixhauser Comorbidity Index")
colnames(df_tableOne) 	<- colnames_Vars
factVars 		<- c("Gender", "Education level according to ISCED", "Disease duration", "Disease stage") # Here only values with categorial (ordinal distribution should be added) 
tableOne 		<- CreateTableOne(vars=colnames_Vars, factorVars=factVars, data=df_tableOne) # @Marcel, here the vars of interest should be renamed
print(tableOne, nonnormal=c("Disease stage", "Education level according to ISCED"))

# ==================================================================================================
# Start analysing major questions 
# Extract values of interest pre and during (cf. ...B17 vs. ...C6) 
df_careCovid 		<- df_total %>% select(satisfaction_PDcare_priorCovid.B17, satisfaction_with_care_duringCovid.C6) %>% drop_na()
colnames(df_careCovid) 	<- c("before", "during") # change colnames in meaningful way 
df_careCovid 		<- dplyr::mutate(df_careCovid, ID = row_number()) %>% 
							pivot_longer(cols = c("before", "during"), 
									names_to = "timing", 
									values_to = "ratings")

df_careCovid$ratings[(df_careCovid$ratings==-999 | df_careCovid$ratings ==5)] 	<- NA # -999 means "not answered because question not applicable based on a previous answer", a bit redundant after lines 41f.
df_careCovid$ID 									<- as.factor(df_careCovid$ID) # ID needs to be coded as factor

df_careCovid %>%
  group_by(timing) %>%
  get_summary_stats(ratings, show = c("mean", "sd", "median", "iqr"))  

# Create a summary of available results
summary_care 		<- df_careCovid %>% filter(timing=="before") %>% drop_na() %>% group_by(ratings)  %>% summarize(before=sum(ratings))
temp_data 		<- df_careCovid %>% filter(timing=="during") %>% drop_na()%>% group_by(ratings)  %>% summarize(during=sum(ratings))
summary_care 		<- merge(summary_care, temp_data, by="ratings", all = T)
summary_care[is.na(summary_care)]=0.001

colnames(summary_care) 	<- c("ratings", "before", "during")
summary_care$before 	<- summary_care$before/sum(summary_care$before)
summary_care$during 	<- summary_care$during/sum(summary_care$during)
summary_care 			<- dplyr::mutate(summary_care, ID = row_number()) %>% 
								pivot_longer(cols = c("before", "during"), 
										names_to = "timing", 
										values_to = "percentage")

summary_care		<- summary_care %>% filter(ratings>0)
symmetry_test(ratings~as.factor(timing) | ID, data=df_careCovid, distribution="exact")
stat.test 		<- df_careCovid  %>%
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
						# ============================================================== 1->#
						"disease_stage.A2", 
						"regular_caregiver_categorial.B1a", 
						# ============================================================== 2->#
						"disease_duration.A1", 
						"comorbidities_sum.A4", 
						"vWEI", 	
						# ============================================================== 3->#						
						"educational_level.D8", 
						"income.D9",  					
						# ============================================================== 4->#						
						"GP_expertise.B3", 	
						"neurologists_expertise.B5", 
						# ============================================================== 5->#												
						#"reason_for_communication_challenges_sum_categorized.B14a",
						"communication_challenges_priorCovid.B14",
						# ============================================================== 6->#						
						"pdq8_total.A3", 
						# ============================================================== 7->#												
						"overcoming_barriers_sum.B7a", 
						"inability_to_access_care_sum_categorized_priorCovid.B9a", 
						# ============================================================== 8->#						
						"forgot_treatment_due_to_cost_priorCovid_categorized.B11", 
						"extended_health_insurance_due_to_PD.B12", 	
						"financial_difficulties_from_treatment_costs_categorized.B13", 
						"extent_of_financial_stability_categorized.D10", 
						# ============================================================== 9->#						
						# 9 not applicable to our dataset
						# ============================================================== 10->#						
						"confidence_in_accessing_necessary_services_remotely_categorized.C3_3", 
						"cooperation_healthcare_providers_yesorno.B6a", 
						"visit_healthcare_providers_sum.B6", 
						# ============================================================== 11->#												
						"reason_for_experiencing_stigmatisation_sum_categorized_RC1.B15", 
						"received_remote_sessions_duringCovid.C2", 
						"access_to_technology_categorized.C2c2", 
						#"communication_challenges_priorCovid.B14", 
						# ============================================================== 12->#												
						"personal_accessibility_barriers_sum.B16a", 
						#"personal_accessibility_barriers_sum_categorized.B16a", 
						#"type_of_community_categorized.D6", 
						"living_situation_categorized.D7",
						"barriers_to_care_sum_categorized.B9b", 
						"ease_obtaining_healthcare_priorCovid_categorized.B10",
						# ============================================================== 13->#						
						"geographical_barriers_healthcare_providers.B7", 
						"quantile_population", 				
						#"local_availability_sum_categorized.B8", 
						"ability_to_access_care_priorCovid.B9", 
						"neurologistsGER", 
						# ============================================================== 14->#							
						"gender.D2") 

group 				<- c(	1,1, # grouping is needed for later and corresponds to factorsOR1
							2,2, # Health status
							3,3,3, # Health Literacy
							4,4, # ... see below for further categories ...
							5,
							6,
							7,7,
							8,8,8,8,
							10,10,10,
							11,11,11, 
							12,12,12,12,
							13,13,13,13,
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
					"A2 - Disease Stage [H&Y]", #**
 					"B1a - Regular caregiver present", #*
	                		
					"A1 - Disease duration [Years]", #**
  					"A4 - No. of comorbidities", #*
 					"vWEI - Comorbidity Index [vWEI]", #**(*)
					
					"D8 - Education [ISCED]", #**
  					"D9 - Net Householdincome [€/year]", #****
					
					"B3 - Perceived GP expertise", #**
  					"B5 - Perceived Neurologist expertise", #**
					
					#"B14a - Sum of Reasons for Communication Challenges before the COVID-19 Pandemic", #* # TODO: Not quite clear. Is it a number? A sum? A rate? -> Answer: a sum // Besides problem with multicollinearity (see below "Communication challanges ...")
	                		"B14 - Communication challenges pre COVID",
	
					"A3 - PDQ-8 score", #**(*)
					
					"B7a - No. of ressources against geographical barriers pre COVID", #**** 
  					"B9a - Ability to access PD-care pre COVID", #*
					
					"B11 - Rescheduled healthcare due to financial burden pre COVID", #* 
  					"B12 - Extended healthcare insurance", #* 
  					"B13 - Financial burden related to PD pre COVID", #*
 				 	"D10 - Financial stability", #**
                   			
	
					"C3_3 - Confidence accessing healthcare remotely", #* 
  					"B6a - Perceived cooperation between healthcare providers", #**
  					"B6 - No. of healthcare providers consulted pre COVID", #****
                   			
					"B15 - Experienced stigmatization in healthcare", #*
  					"C2 - Availability of remote healthcare during COVID", #* #TODO: What is the difference to next line?!
  					"C2c2 - Access to telehealth technologies during COVID", #*
  					#B14 - "Communication Challenges before pandemic", #* # TODO: produces isues with collinearity in the dataset together with "Reasons for Communication Challenges..."
                   			
					"B16a - No. of negative consequences from barriers to healthcare", #****
  					#B16a.c - Existence of Negative Effects on Patients due to Healthcare Accessibility Barriers", #* 
					#D6 - Level of Urbanization", #** #TODO: This is very similar to "Population according to ..." below. I would prefer the other one as it is more reliable and we only have 3/6 levels available here; besides resuöts are almost identical
  					"D7 - Living independently", #* 
  					"B9b - No. of barriers in acces to healthcare pre COVID", #**
  					"B10 - Perceived difficulty of accessing healthcare pre COVID", #*	
					
					"B7 - No. of geographical barriers in access to healthcare pre COVID", #**
  					"qp - Population according to quantiles of German population [in sqkm]", #****
	                		#"B8 - Locally Available PD-related Healthcare Ressources", #**
					"B9 - Not received needed healthcare pre COVID", #** 
  					"nGER - Neurologists nearby (per sqkm)", #****
                    
					"D2 - Gender *" #*
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

#TODO 1: Please double check the results, as most of them look plausible while at least one is not quite clear: Confidence Accessing PD-related Healthcare Remotely increase significantly the Odds?!?
pMiss <- function(x){sum(is.na(x))/length(x)*100} # function to find missing (NA) values
sort(apply(df_OR1_complete,2,pMiss), decreasing=TRUE)[1:5]

# TODO 2: Run the last two lines. There are some predictors with lots of missing entries which is problematic for regression later. Besides, there is some redundancy among some of them:
#  personal_accessibility_barriers_sum.B16a vs.  personal_accessibility_barriers_sum_categorized.B16a
# Please double-check if there are some of the five that we can replace and if so try to change the code. The lines to change if you remove (or add) something are;
# line 293f, line 330f, line401f.

# TODO 3: We have problems with multicollinearity in the data. The problem is an innate condition for GLM, which means  a little oversimplified that values that are perfectly correlated with each other
# make the estimation of residuals complicated so that the model becomes bad. The problem arises with:

flag_check = TRUE
if (flag_check){ # need to run the part with the stepwise regression first to make this work ...
full_model_test = glm(I(dv=='yes') ~ ., data = train_data)
mctest::imcdiag(full_model_test)
XX <- data_full_glm %>% select(c("local_avail
_sum_categorized.B8", "ability_to_access_care_priorCovid.B9"), contains("communication_challenges")) %>% ggpairs(upper = list(continuous = wrap("cor", method = "spearman")))
XX
}

# TODO 4: What is ""ability_to_access_care_priorCovid.B9", that does appear somehow and looks a bit odd. It's very related to "local_availability_sum_categorized.B8". We should decide for one (if first actually exists). 
# -> B9: "felt that healthcare was needed but not available pre covid (never/rarely/sometimes/often/always) and is now named "Frequency of the Impression that Needed PD-related Healthcare was not Received before the COVID-19 Pandemic" for plotting
# --> the strong relation between B8 and B9 seems logical because if there isn´t a healthcare ressource locally available (B8) it´s more likely that people there more often perceive a lack of healthcare (B9)


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
# ==================================================================================================
# Stepwise linear regression in order to reduce dimensionality/extract the most meaningful predictors
# needed_healthcare_but_did_not_receive_it_duringCovid.C4, which ranges from 1 to 5 (and NA)
# Method inspired from http://rstudio-pubs-static.s3.amazonaws.com/448536_221fe7b85ca1471d8f4a53c05fcbe95b.html among other sources

# General functions
pMiss <- function(x){sum(is.na(x))/length(x)*100} # function to find missing (NA) values

data_full_glm <- df_OR1_complete %>% dplyr::select(factorsOR1, dv) %>% mutate(across(c(2,9,12,13, 15, 20, 21, 22, 26, 27), as.factor)) # getting data for the full-GLM
data_full_glm$pdq8_total.A3 <- as.numeric(data_full_glm$pdq8_total.A3)

flag_check = TRUE
if (flag_check) { # Sanity checks; looks for incomplete columns within the entire dataset
	apply(data_full_glm,2,pMiss) # percentage of missing values per column
	apply(data_full_glm,1,pMiss) # percentage of missing values per row
	aggr_plot1 			<- aggr(data_full_glm[1:16], col = c('navyblue','red'), numbers = TRUE, sortVars = TRUE, 
													labels = names(data), cex.axis = .7, gap = 3, 
													ylab = c("Histogram of missing data","Pattern")) # Display missing values graphically
	aggr_plot2 			<- aggr(data_full_glm[17:33], col = c('navyblue','red'), numbers = TRUE, sortVars = TRUE, 
												labels = names(data), cex.axis = .7, gap = 3, 
												ylab = c("Histogram of missing data","Pattern")) # Display missing values graphically
}

# ==================================================================================================
# Data imputation using the MICE package with a multivariate approach
inlist 				<- c("disease_stage.A2", "gender.D2", "vWEI", "dv") # names of the variables that should be included as covariates in every imputation model
pred 				<- quickpred(data_full_glm, minpuc = 0.5, include = inlist) # predictor matrix
generate_imputation <- mice(data=data_full_glm,
							 predictorMatrix = pred, #predictormatrix,
							 m=10,
							 maxit=5,            
							 diagnostics=TRUE)		 

# Sanity checks
if (flag_check){ # Generates a density plot with the observed vs. the imputed variables
	densityplot(generate_imputation, xlim = c(0, 5), ylim = c(0, 0.4))
}

# Prepare "imputed" data for GLM with dummy variables and by converting columns integers where appropiate:
imputed_data_full 									<- generate_imputation
imputed_data_full$visit_healthcare_providers_sum.B6[imputed_data_full$visit_healthcare_providers_sum.B6==5] <-4
imputed_data_full 									<- data.frame(complete(generate_imputation)) %>% drop_na()
imputed_data_full$neurologists_expertise.B5 		<- as.integer(imputed_data_full$neurologists_expertise.B5)
imputed_data_full$visit_healthcare_providers_sum.B6 <- as.integer(imputed_data_full$visit_healthcare_providers_sum.B6)  

imputed_data_full <- dummy_cols(imputed_data_full, select_columns = c(	'gender.D2', 'received_remote_sessions_duringCovid.C2', 
															'regular_caregiver_categorial.B1a',# 'type_of_community_categorized.D6',
															'overcoming_barriers_sum.B7a', 'personal_accessibility_barriers_sum_categorized.B16a', 
															'reason_for_experiencing_stigmatisation_sum_categorized_RC1.B15', 'extended_health_insurance_due_to_PD.B12',
															'inability_to_access_care_sum_categorized_priorCovid.B9a'), remove_selected_columns = TRUE)
str(imputed_data_full)

# ==================================================================================================
## GLM analyses, that is full model vs. model w/ stepwise reduction w/ glmStepAIC from {caret} package
# Analyses adapted from: https://rpubs.com/mpfoley73/625323

# Separate data into train and test dataset
index 		<- createDataPartition(imputed_data_full$dv, p = 0.8, list = FALSE) # split data with balanced values for dv
train_data 	<- imputed_data_full[index,]
test_data 	<- imputed_data_full[-index,]
model_est 	<- data.frame(model_name=c("Full GLM", "Stepwise reduced GLM"), AUC=c(NA,NA), LogLoss=c(NA, NA), Accuracy=c(NA,NA))
# ==================================================================================================
# a) FULL GLM and save results to workspace {mdl_full}
train_data <- model.frame(dv ~ ., data = train_data, drop.unused.levels = TRUE) # drop unused factors
train_control <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE)
mdl_full <- train(train_data %>% select(-dv),
					train_data$dv,
					method = "glm",
					preProcess = c("center", "scale"),
					family = "binomial",
					trControl = train_control,
					metric = "Accuracy")

# Interpreting results {mdl_full}:
mdl_full # Accuracy of full model results in ~84.1%
varImp(mdl_full) # Factors most contributing to data are listed here  

predicted_classes 	<- predict(mdl_full, newdata = test_data) 
predicted_probs 	<- predict(mdl_full, newdata = test_data, type = "prob")
mdl_full_matrix 	<- confusionMatrix(predicted_classes, test_data$dv, positive = "yes") # predictions on (independent) test data
  
mdl_full_preds <- predict(mdl_full, newdata = test_data, type = "prob")
(mdl_full_eval <- evalmod(
  scores = mdl_full_preds$yes,
  labels = test_data$dv
))

mdl_full_prob <- predict(mdl_full, newdata = test_data, type = "prob") %>%
        bind_cols(predict(mdl_full, test_data)) %>%
        bind_cols(select(test_data, dv))

trueLabelsBinary <- ifelse(mdl_full_prob$dv=="yes", 1, 0)
predictedLabelsBinary <- ifelse(mdl_full_prob$`...3`=="yes", 1, 0)
model_est$LogLoss[1] = LogLoss(mdl_full_prob$yes, trueLabelsBinary)
model_est$AUC[1] = AUC(mdl_full_prob$yes, trueLabelsBinary)
model_est$Accuracy[1] = mdl_full$results[[2]]

annotation <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]", mdl_full_matrix$overall[[1]],  mdl_full_matrix$overall[[3]], mdl_full_matrix$overall[[4]]))

# Print AUC for the FULL model
options(yardstick.event_first = FALSE)  # set the second level as success
fig3a <- data.frame(pred = mdl_full_preds$yes, obs = test_data$dv) %>%
	yardstick::roc_curve(obs, pred) %>%
	autoplot() +
	theme_bw() +
	labs(title = "Prediction zthat healthcare was needed but wasn't received", 
		subtitle = "Full model GLM including all predictors") + 
	geom_text(data=annotation, aes(x=x, y=y, label=label), color="black", fontface="bold") + 
	coord_equal() +
	xlab("1 - specificity") + ylab("sensitivity")

# ==================================================================================================
# b) stepwise regression using {caret}-package
mdl_step 	<- train(train_data %>% select(-dv),
                      train_data$dv,
                      method = 'glmStepAIC', 
					  preProcess = c("center", "scale"),
					  tuneLength = 10, #"ROC",
                      family = binomial,
                      trControl = train_control,
					  metric = "Accuracy")
mdl_step
varImp(mdl_step$finalModel) # Factors most contributing to data are listed here  

# Interpreting results of stepwise reduced GLM:
predicted_classes 	<- predict(mdl_step, newdata = test_data) 
predicted_probs	 	<- predict(mdl_step, newdata = test_data, type = "prob")
mdl_step_matrix 	<- confusionMatrix(predicted_classes, test_data$dv, positive = "yes") # predictions on (independent) test data

mdl_step_preds <- predict(mdl_step, newdata = test_data, type = "prob")
(mdl_step_eval <- evalmod(
  scores = mdl_step_preds$yes,
  labels = test_data$dv
))

mdl_step_prob <- predict(mdl_step, newdata = test_data, type = "prob") %>%
        bind_cols(predict(mdl_step, test_data)) %>%
        bind_cols(select(test_data, dv))

trueLabelsBinary <- ifelse(mdl_step_prob$dv=="yes", 1, 0)
predictedLabelsBinary <- ifelse(mdl_step_prob$`...3`=="yes", 1, 0)
model_est$LogLoss[2] = LogLoss(mdl_step_prob$yes, trueLabelsBinary)
model_est$AUC[2] = AUC(mdl_step_prob$yes, trueLabelsBinary)
model_est$Accuracy[2] = mdl_step$results[[2]]

# Print AUC for the FULL model
annotation <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]", mdl_step_matrix$overall[[1]],  mdl_step_matrix$overall[[3]], mdl_step_matrix$overall[[4]]))
options(yardstick.event_first = FALSE)  # set the second level as success
fig3b <- data.frame(pred = mdl_step_preds$yes, obs = test_data$dv) %>%
	yardstick::roc_curve(obs, pred) %>%
	autoplot() +
	theme_bw() +
	labs(title = "Prediction zthat healthcare was needed but wasn't received", 
		subtitle = "GLM model after AIC-based stepwise reduction ") + 
	geom_text(data=annotation, aes(x=x, y=y, label=label), color="black", fontface="bold") + 
	coord_equal() +
	xlab("1 - specificity") + ylab("sensitivity")


# Compare models
p_comparison_models <- model_est %>% pivot_longer(!model_name, names_to="metric") %>%
  ggplot(aes(fill = model_name, y = value, x = metric)) + 
  geom_bar(position = "dodge", stat = "identity") +
  #scale_fill_manual(values = c("#7A8B99", "#A9DDD6")) +
	theme_minimal() +
	theme(text = element_text(size = 12),
		  plot.caption = element_text(hjust = .7, face="italic"), 
		  legend.title = element_text(hjust = .5, color = "black", size = 12, face = "bold"),
		  axis.text = element_text(size = 12), 
		  legend.position = c(0.86, 0.9), 
		  plot.margin = margin(t = 10, unit = "pt")) +
	ylim(0,1) +
	scale_fill_brewer(palette = 1) + 
	labs(
		y = "Value",
		x = "",
		fill = NULL,
		title = "Comparing the full model with the reduced model using distinct metrics",
		caption = "Higher values indicate better performance: Accuracy and AUC\nLower values indicate better performance: Log Loss") + 
  geom_text(aes(label = round(value, 3)), vjust = -0.5, size = 3, position = position_dodge(width= 0.9)) #+
  #coord_capped_cart()
p_comparison_models

# ==================================================================================================
# Summary of all results from the stepwise reduced regression and plots to show the results for the 
# identified predictors
summary(mdl_step) # No function was traceable to get that into a table, so it has to be done manually!

#TODO: Supplementary data which displeays the levels of the answers on the x-Axis and the means for 'yes' and 'no' on the y axis for the
# supplementary data. For that we need a list of factors (-> just copy the entore list and remove what is not needed) and the ggplot routines in a loop ; ) 



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
