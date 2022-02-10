# This is code to run analyses for the covidPD project;
# Code developed by Marlena van Munster, Marcel Printz and David Pedrosa

# Version 1.5 # 2022-01-29, recoded values and updated code

## First specify the packages of interest
packages = c(	"readxl", "tableone", "dplyr", "tidyverse", "coin", 
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
filename_comorbidities  <- file.path(wdir, "data", "vanWalravenElixhauserIndex.csv")
df_comorbidities 			  <- read.csv2(filename_comorbidities)
df_comorbidities 			  <- data.frame(vWEI=df_comorbidities$value)

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
## Tidy up factors before analyses

# Adapt the factors of the dataframe with corresponding levels
df_total$gender.D2 			<- as.factor(df_total$gender.D2)
levels(df_total$gender.D2) 	<- c("female", "male", "diverse") # have added third gender, as there was one person answering 3. @ Marlena is that correct?

df_total$educational_level.D8 <- as.factor(df_total$educational_level.D8)
df_total$educational_level.D8[df_total$educational_level.D8==13] <- NA

df_total <- df_total %>% droplevels() %>%  mutate(educational_level.D8 = fct_relevel(educational_level.D8, c("4", "1", "2", "3"))) # Is that correct now or do we need to drop factor=4?!
df_total$educational_level.D8[df_total$educational_level.D8==4] <- NA
df_total$educational_level.D8 <- as.integer(droplevels(df_total$educational_level.D8))

df_total$disease_stage_name <- as.factor(df_total$disease_stage.A2)
#df_total <- df_total %>%  mutate(disease_stage_name = fct_relevel(disease_stage_name, paste("Hoehn & Yahr", as.roman(c(1:5))))) # Is that correct now or do we need to drop factor=4?!
df_total$disease_stage.A2 <- as.integer(df_total$disease_stage.A2)

df_total$disease_duration_cat <- as.factor(df_total$disease_duration.A1)
levels(df_total$disease_duration_cat) <- c(">2 years", "2-5 years", "5-10 years", "10-15 years", ">15 years") # disease duration

df_total$overcoming_barriers_sum.B7a <- df_total$overcoming_barriers_transportz.B7a + 
										df_total$overcoming_barriers_financialsupport.B7a + 
										df_total$overcoming_barriers_telemedicine.B7a # TODO: Apparently there has been a mistake in the way this was summed up, so repeated it manually!

df_total <- df_total %>% mutate(quantile_population = ntile(populationGER, 5)) # converts the population data to categories according to the quantiles

# B3
df_total$GP_expertise.B3 <- as.factor(df_total$GP_expertise.B3)
df_total <- df_total %>%  mutate(GP_expertise.B3 = fct_relevel(GP_expertise.B3, c("5","4", "3", "2", "1","98 = NA")))
df_total$GP_expertise.B3 <- as.integer(df_total$GP_expertise.B3)
#B5
df_total$neurologists_expertise.B5 <- as.factor(df_total$neurologists_expertise.B5)
df_total <- df_total %>%  mutate(neurologists_expertise.B5 = fct_relevel(neurologists_expertise.B5, c("5","4", "3", "2", "1","98 = NA", "99 = NA")))
df_total$neurologists_expertise.B5 <- as.integer(df_total$neurologists_expertise.B5)
#B6a
df_total$cooperation_healthcare_providers_yesorno.B6a <- as.factor(df_total$cooperation_healthcare_providers_yesorno.B6a)
df_total <- df_total %>%  mutate(cooperation_healthcare_providers_yesorno.B6a = fct_relevel(cooperation_healthcare_providers_yesorno.B6a, c("5","4", "3", "2", "1","97 = NA", "98 = NA", "99=NA")))
df_total$cooperation_healthcare_providers_yesorno.B6a <- as.integer(cooperation_healthcare_providers_yesorno.B6a)
#B10: ease_obtaining_healthcare_preCovid_categorized.B10 not adapted, because it is already categorized. @Marlena: Which values does categorym"1" and "2" include?
#B17: satisfaction_PDcare_preCovid_categorized.B17 not adapted, because it is already categorized. @Marlena: Which values does category "1" and "2" include?
#C2c2: access_to_techniology_categorized.C2c2 not adapted, because it is already categorized. @Marlena: Which values does category "1" and "2" include?
#C3_3: confidence_in_accessing_necessary_services_remotely_categorized.C3_3 not adapted, because it is already categorized. @Marlena: Which values does category "1" and "2" include?
#C6: satisfaction_with_care_duringCovid_categorized.C6 not adapted, because it is already categorized. @Marlena: Which values does category "1" and "2" include?
#D6: type_of_community.D6 not adapted, because it is already categorized. @Marlena: Which values does category "1", "2" and "3" include?
#D10: extent_of_financial_stability.D10 not adapted, because we probably will use extent_of_financial_stability_categorized.D10, which is already categorized and contains an inverted code.

# ___________________________________________________________________________________________________________________________________________________________________________________________
# Adapting using remode() from car package could be:
df_total$GP_expertise.B3_recode <- recode(df_total$GP_expertise.B3, "1=5; 2=4; 3=3; 4=2; 5=1, 98=NA")
table(Canadian_and_German_data_16092021$B3_ratedGPexpertise)
1   2   3   4   5  98
9  57 126 163  42  52

# ==================================================================================================
## Create TableOne for specific values

Vars 			<- c(	"age.D1", "gender.D2", "disease_duration_cat", "disease_stage.name", "populationGER", "neurologistsGER", 
						"physiciansGER", "educational_level.D8", "pdq8_total.A3", "vWEI")
#nonnormalVars 	<- c("disease_stage.A2", "disease_duration.A1") # avoids returning mean in favor of median 
factVars 		<- c("gender.D2", "educational_level.D8", "disease_duration_cat", "disease_stage.name") # Here only values with categorial (ordinal distribution should be added) 
tableOne 		<- CreateTableOne(vars=Vars, factorVars=factVars, data=dfGER) # @Marcel, here the vars of interest should be renamed
print(tableOne, nonnormal=c("disease_Stage.A2"))

# ==================================================================================================
# Start analysing major questions 
# Extract values of interest pre and post (cf. ...B17 vs. ...C6) 
df_careCovid 			<- df_total %>% select(satisfaction_PDcare_preCovid.B17, satisfaction_with_care_duringCovid.C6) %>% drop_na()
colnames(df_careCovid) 	<- c("pre", "post") # change colnames in meaningful way 
df_careCovid 			<- dplyr::mutate(df_careCovid, ID = row_number()) %>% 
							pivot_longer(
											cols = c("pre", "post"), 
											names_to = "timing", 
											values_to = "ratings")

df_careCovid$ratings[df_careCovid$ratings==-999] 	<- NA # -999 means "not answered because question not applicable based on a previous answer", a bit redundant after lines 41f.
df_careCovid$ID 									<- as.factor(df_careCovid$ID) # ID needs to be coded as factor

df_careCovid %>%
  group_by(timing) %>%
  get_summary_stats(ratings, show = c("mean", "sd", "median", "iqr"))  
# ======> @Marlena, not quite clear to me, why lower values at post! What is the coding here? We should refactor alle used codes from bad to good in ascending order (see line 100)
#M: currently 1 = very satisfied - if we´d like to change to 1 = very unsatisfied it should look like this, right?
df_total <- df_total %>%  mutate(satisfaction_PDcare_preCovid.B17 = fct_relevel(satisfaction_PDcare_preCovid.B17, c("4", "3", "2", "1"))
df_total <- df_total %>%  mutate(satisfaction_with_care_duringCovid.C6 = fct_relevel(satisfaction_with_care_duringCovid.C6, c("4", "3", "2", "1"))


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
y_start 	<- summary_care %>% filter(timing=="pre") %>% summarize(y_start=cumsum(percentage))
y_end 		<- summary_care %>% filter(timing=="post") %>% summarize(y_end=cumsum(percentage))
df_lines 	<- data.frame(y_start=y_start, y_end=y_end, x_start=rep(1+bar_width/1.99,4), x_end=rep(2-bar_width/1.99,4))

summary_care$timing <- as.factor(summary_care$timing)
levels(summary_care$timing) <- c("pre", "post") # explicit definition of order necessary as otherwise data is sorted alphabetically (cf. https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph)

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
	labs(x="",y="Percentage of questionnaires") + 
	geom_segment(data = df_lines, colour="black", size=.25,
			aes(x = x_start, xend = x_end, y = y_start, yend = y_end)) + 
	theme_minimal()
p_satisfaction_with_care

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
#| 9. NA          	      	  | Difficulties of Diagnosis            |
#| 10. C3, B6a, B6            | Coordination in care                 |
#| 11. B15, B14, C2c          | Communication (system)               |
#| 12. B16, D6, D7, B9b, B10  | Disparty in Health Services          |
#| 13. B7, B8, B9,            | Unavailability of Specalist Services |
#| 14. D2 	       	          | Other				     			 |
#+----------------------------+--------------------------------------+
# created with: https://ozh.github.io/ascii-tables/

# ======> @Marlena, again, we need to double check the coding for all the used variables
#M: is there a possibility to view the raw data in a table format (1 colum per variable and 1 row per participant = 1 value per box)? this would it make easier for me to check the coding
#B1a is labled differently in the dataset than in the code - is there a reason? in the dataset: B1a_regularcaregiver_yesorno

# ======> @Marlena, some variables cannot be understood such as the value of "9" in the extended_health_insurance_due_to_PD.B12

dependent_variable = "needed_healthcare_but_did_not_receive_it_duringCovid.C4"
factorsOR1 <- c( # sorted according to the barriers proposed in https://doi.org/10.3233/jpd-212735
						"disease_stage.A2", "regular_caregiver_categorial.B1a", #checked
						
						"disease_duration.A1", "comorbidities_categorial.A4", "vWEI", 	#checked - we could also use "comorbidities_sum.A4"
						
						"educational_level.D8", "income.D9",  		#checked - M: I would summarize both variables			
						
						"GP_expertise.B3", "neurologists_expertise.B5", #checked
						
						"reason_for_communication_challenges_sum_categorized.B14a", #checked
						
						"pdq8_total.A3", #checked - we could also use the variable pdq8_categorial.A3 (pdq-8 score summarized in 4 categories)
						
						"overcoming_barriers_sum.B7a", "ability_to_access_care_priorCovid_categorized.B9a", #M: need to check B9a in the dataset again

						"forgot_treatment_due_to_cost_preCovid_categorized.B11", #checked
						"extended_health_insurance_due_to_PD.B12", "financial_difficulties_from_treatment_costs_categorized.B13", #checked
						"extent_of_financial_stability.D10", #checked - we could also use extent_of_financial_stability_categorized.D10
						
						# 9 missing

						"confidence_in_accessing_necessary_services_remotely_categorized.C3_3", "cooperation_healthcare_providers_yesorno.B6a", #checked
						"visit_healthcare_providers_sum.B6", #M: need to check B6 in the dataset
						
						"reason_for_experiencing_stigmatisation_sum_categorized_RC1.B15", "received_remote_sessions_duringCovid.C2", #cheched
						"access_to_techniology_categorized.C2c2", "communication_challenges_preCovid.B14", #checked M: need to check B14 in the dataset
						
						"personal_accessibility_barriers_sum.B16a", "personal_accessibility_barriers_sum_categorized.B16a", #checked
  
						"type_of_community.D6", "living_situation.D7", # checked - we could also use type_of_community_categorized.D6 and living_situation_categorized.D7
						"barriers_to_care_sum_categorized.B9b", #checked
						"ease_obtaining_healthcare_preCovid_categorized.B10", #checked
	
						"geographical_barriers_healthcare_providers.B7", "quantile_population", # "populationGER",	#checked M: need to check B7 in the dataset					
						"local_availability_sum_categorized.B8", "ability_to_access_care_priorCovid.B9", "neurologistsGER", #checked
	
						"gender.D2") #checked

group 				<- c(	1,1, # grouping is needed for later and corresponds to factorsOR1
							2,2, # Health status
							3,3,3,
							4,4,
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
	  
results1 = c()
for (fac in factorsOR1) { # for loop to get results from all factors of interest
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

# =====> @both: we need a more precise naming of the predictors here and this list MUST be as long as results_OR1. We will use that for the next figure
predictors <- c(
					"Disease Stage (Hoehn and Yahr)", #**
					 "Presence of a Regular Caregiver", #*
	                                 "Duration of Parkinson´s Disease", #**
	                                 "Presence of Comorbidities", #*
					 "Disease Burden (Elixhauser Comorbidity Index)", #**(*)
                                          "Educational Level", #**
	                                  "Annual net income", #****
	                                  "Perceived PD-Expertise of General Practitioner", #**
                                          "Perceived PD-Expertise of Neurologist", #**
	                                  "Reasons for Communication Challenges before the COVID-19 Pandemic", #*
	                                  "PD-related Quality of Life (PDQ-8)", #**(*)
                                          "Number of Available Ressources to Overcome Geographical Barriers before the COVID-19 Pandemic", #****
                                          "Ability to Access PD-related Care before the COVID 19 Pandemic", #*
	                                  "(Number of) Shifted Healthcare Appointments due to Costs in the last 12 Months before the Covid-19 pandemic", #*
	                                  "Existence of Extended Healthcare Insurance to Cover PD-associated Costs", #*
                                          "Existence of Financial Problems due to PD-related Treatment Costs in the last 12 Months before the COVID-19 pandemic", #*
                                           "Financial Stability", #**
                                           "Confidence in Accessing PD-related Healthcare Remotely", #*
	                                   "Perceived Cooperation of Healthcare Providers", #**
                                           "Number of Healthcare Providers Consulted (in the last 12 Months before the COVID-19 Pandemic)", #****
                                           "Experiencing Stigmatization Using Healthcare Ressources", #*
	                                   "Received Remote Sessions with PD-related Healthcare Providers during the COVID-19 Pandemic", #*
                                           "Existing Access to Technology for Consulting PD-related Healthcare Providers during the COVID-19 Pandemic", #*
                                           "Presence of Communication Challenges before the COVID-19 Pandemic", #*
                                           "Number of Negative Effects on Patients Personal Life due to Healthcare Accessibility Barriers", #****
	                                   "Extent/Existance of Negative Effects on Patients Personal Life due to Healthcare Accessibility Barriers", #*
	                                   "Level of Urbanization", #**
	                                   "Living Situation", #*
	                                   "Number of Barriers Preventing from Receiving Needed PD-related Healthcare Ressources before the COVID-19 Pandemic", #**
                                           "Perceived Difficulty in Accessing PD-related Healthcare before the COVID-19 Pandemic", #*
	                                   "Level of Geographical Barrier Concerning Access to Healthcare Ressources before the COVID-19 Pandemic", #**
                                           "Quantile Population", #****
	                                   "Number of Locally Available PD-related Healthcare Ressources", #**
                                           "Frequency of Not Receiving Needed PD-related Healthcare before the COVID-19 Pandemic", #**
                                            "Density of Neurological Practices and Hospital Departments (per sqm)", #****
                                            "Gender" #*
				)

# Sort values according to groups and values of OR, irrespective of 95%CI
results_OR1 %>%
  dplyr::arrange(group, boxOdds) %>%
  mutate(factors=factor(factors, levels=factors)) %>%  # This trick update the factor levels
  ggplot(aes(x = boxOdds, y = factors)) +
  geom_vline(aes(xintercept = 1), size = .75, linetype = "dashed", color="grey") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(aes(x = boxOdds, y = factors, color = factors_group), size = 1.5, show.legend=TRUE) +
  guides(colour = guide_legend(reverse=T)) +
  scale_colour_manual(values=my_blue2) +
  theme_bw() +
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
  geom_point(aes(x = boxOdds, y = factors, color = significance), size = 1.5, show.legend=TRUE) +
  guides(colour = guide_legend(reverse=TRUE)) +
  scale_colour_manual(values=c("#08306B", "#B8A31F", "#6B340D")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = c(0.9, 0.2), 
		legend.title=element_blank()) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 20, 40), limits=c(.08, 40)) + 
  geom_hline(yintercept=hlines[1:length(hlines)-1], linetype="dotted", size = .01) +
  ylab("") +
  xlab("Odds ratio (log scale)")

#display Odds ratio table
results_OR1

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
## UNFINISHED ATTEMPT TO REDUCE DATA DIMENSIONALITY!
# ==================================================================================================
# Attempt to create an entire glm with all predictors 
# use again needed_healthcare_but_did_not_receive_it_duringCovid.C4, which ranges from 1 to 5 (and NA) as dv

# Try again with http://rstudio-pubs-static.s3.amazonaws.com/448536_221fe7b85ca1471d8f4a53c05fcbe95b.html



# ==================================================================================================
# The first step is data imputation using multivariate routines (MICE package)

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
pred 				<- quickpred(data_full_glm, minpuc = 0.5, include = inlist, mincor=.1) # predictor matrix
generate_imputation <- mice(data=data_full_glm,
							 predictorMatrix = pred, #predictormatrix,
							 m=10,
							 maxit=5,            
							 diagnostics=TRUE)
							 #MaxNWts=3000)				 
# imp_data_full_glm <- data.frame(complete(imp_gen)) #not really needed
imputed_data_full = data.frame(complete(generate_imputation)) %>% drop_na()

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
#objControl <- trainControl(method = "cv", number = 3, classProbs = TRUE,)

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
#+--------------+----------------------------------------------------------------------+------------+---------------------+
#  | Factor Group |                               Variable                               |  box_odds  |         CI          |
#  +--------------+----------------------------------------------------------------------+------------+---------------------+
#  |            1 | disease_stage.A2                                                     | 1.1297452  | 0.8668900 1.4723024 |
#  |            1 | regular_caregiver_categorial.B1a                                     | 0.7760327  | 0.4323766 1.3928293 |
#  |            2 | disease_duration.A1                                                  | 0.8987483  | 0.6935557 1.1646483 |
#  |        x   2 | pdq8_total.A3                                                        | 1.0259540  | 1.0058040 1.0465077 |
#  |            3 | educational_level.D8                                                 | 1.2006415  | 0.9696350 1.4866832 |
#  |            3 | income.D9                                                            | 0.8876343  | 0.5496578 1.4334276 |
#  |        x   4 | GP_expertise.B3                                                      | 1.4128321  | 1.0161181 1.9644316 |
#  |        x   4 | neurologists_expertise.B5                                            | 1.9112435  | 1.4167890 2.5782611 |
#  |            5 | reason_for_communication_challenges_sum_categorized.B14a             | 1.2490385  | 0.9770924 1.5966739 |
#  |            7 | overcoming_barriers_sum.B7a                                          | 2.2666667  | 0.7888392 6.5130866 |
#  |        x   7 | ability_to_access_care_priorCovid_categorized.B9a                    | 5.4264706  | 2.9462694 9.9945318 |
#  |            8 | forgot_treatment_due_to_cost_preCovid_categorized.B11                | 0.9982726  | 0.9753491 1.0217349 |
#  |        x   8 | extended_health_insurance_due_to_PD.B12                              | 2.1118587  | 1.1335885 3.9343619 |
#  |        x   8 | financial_difficulties_from_treatment_costs_categorized.B13          | 1.6007106  | 1.1801045 2.1712267 |
#  |        x   8 | extent_of_financial_stability.D10                                    | 1.5624042  | 1.0789692 2.2624435 |
#  |        x  10 | confidence_in_accessing_necessary_services_remotely_categorized.C3_3 | 2.4413327  | 1.3160985 4.5286165 |
#  |           10 | cooperation_healthcare_providers.B6a                                 | 0.9956897  | 0.4540626 2.1833950 |
#  |           10 | visit_healthcare_providers_sum.B6                                    | 1.2372238  | 0.7699351 1.9881191 |
#  |           11 | reason_for_experiencing_stigmatisation_sum_categorized_RC1.B15       | 0.9894027  | 0.9836513 0.9951877 |
#  |           11 | received_remote_sessions_duringCovid_categorized.C2a                 | 0.9398496  | 0.3646238 2.4225443 |
#  |           12 | personal_accessibility_barriers_sum.B16a                             | 1.1764888  | 0.9347349 1.4807685 |
#  |           12 | type_of_community.D6                                                 | 0.9824674  | 0.7641601 1.2631412 |
#  |           12 | living_situation.D7                                                  | 0.7505585  | 0.3903708 1.4430847 |
#  |        x  12 | barriers_to_care_sum_categorized.B9b                                 | 1.2750719  | 1.0310555 1.5768390 |
#  |        x  12 | ease_pbtaining_healthcare_preCovid.B10                               | 1.9801843  | 1.0070176 3.8938044 |
#  |        x  13 | geographical_barriers_healthcare_providers.B7                        | 1.8883553  | 1.0765294 3.3123905 |
#  |        x  13 | local_availability_sum.B8                                            | 1.0062793  | 0.9372511 1.0803914 |
#  |        x  13 | ability_to_access_care_priorCovid.B9                                | 2.5003187  | 1.8820097 3.3217650 |
#  +--------------+----------------------------------------------------------------------+------------+---------------------+


#create g-plots for factors w. CI>1 from previous step (variables marked with "x")
dfGER_plot <- dfGER
dfGER_plot[dfGER_plot<0] <- NA


#pdq8_total
dfGER_plot$pdq8_total.A3[dfGER_plot$pdq8_total.A3<0] = NA
p_test <- ggplot(aes(x=pdq8_total.A3, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
p_test
#GPexpertise
dfGER_plot$GP_expertise.B33[dfGER$GP_expertise.B3<0] =NA
p_test <- ggplot(aes(x=GP_expertise.B3, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
p_test
#neurologistsexpertise
dfGER_plot$neurologists_expertise.B5[dfGER$neurologists_expertise.B5<0] =NA
p_test <- ggplot(aes(x=neurologists_expertise.B5, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
p_test
#abilitytoaccesscarepreCOVID_9a
dfGER_plot$ability_to_access_care_priorCovid_categorized.B9a[dfGER$ability_to_access_care_priorCovid_categorized.B9a<0] =NA
p_test <- ggplot(aes(x=ability_to_access_care_priorCovid_categorized.B9a, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
p_test
#extended_health_insurance_due_to_PD.B12 
dfGER_plot$extended_health_insurance_due_to_PD.B12[dfGER$extended_health_insurance_due_to_PD.B12<0] =NA  #question: my plots look different if I skipp this line - Why?
p_test <- ggplot(aes(x=extended_health_insurance_due_to_PD.B12, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
p_test
#financialdifficulties
#I detected one factor = 0 which does not makes sense -> excluded it
dfGER_plot$financial_difficulties_from_treatment_costs_categorized.B13[dfGER$financial_difficulties_from_treatment_costs_categorized.B13<=0] =NA
p_test <- ggplot(aes(x=financial_difficulties_from_treatment_costs_categorized.B13, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
p_test
#extent_of_financial_stability.D10
dfGER_plot$extent_of_financial_stability.D10[dfGER$extent_of_financial_stability.D10<0] =NA
p_test <- ggplot(aes(x=extent_of_financial_stability.D10, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
p_test
#barriers_to_care_sum_categorized.B9b
#this barplot contained several values <0 and >3 which does not make sense (dataset only contains values from 1-3)
dfGER_plot$barriers_to_care_sum_categorized.B9b[dfGER$barriers_to_care_sum_categorized.B9b<0] =NA
p_test <- ggplot(aes(x=barriers_to_care_sum_categorized.B9b, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
p_test
#ease_pbtaining_healthcare_preCovid.B10 
#this variable is wrong in the dataset - ease_pbtaining_healthcare_preCovid_categorized.B10 is the normal B10 - and ease_pbtaining_healthcare_preCovid.B10 is the categorized variable, however there are a lot o missings where there should be values
dfGER_plot$ease_pbtaining_healthcare_preCovid_categorized.B10[dfGER$ease_pbtaining_healthcare_preCovid_categorized.B10<0] =NA
p_test <- ggplot(aes(x=ease_pbtaining_healthcare_preCovid_categorized.B10, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
p_test
#geographical_barriers_healthcare_providers.B7
dfGER_plot$geographical_barriers_healthcare_providers.B7[dfGER$geographical_barriers_healthcare_providers.B7<0] =NA
p_test <- ggplot(aes(x=geographical_barriers_healthcare_providers.B7, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
p_test
#local_availability_sum.B8 
#again I´d go with the categorized variable but I think there is some error in the dataset
#dfGER_plot$local_availability_sum_categorized.B8[dfGER$local_availability_sum_categorized.B8<0] =NA
#p_test <- ggplot(aes(x=local_availability_sum_categorized.B8, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
#p_test
dfGER_plot$local_availability_sum.B8[dfGER$local_availability_sum.B8<0] =NA
p_test <- ggplot(aes(x=local_availability_sum.B8, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
p_test
#ability_to_access_care_priorCovid.B9
#again I´d go with the categorized variable
#dfGER_plot$ability_to_access_care_priorCovid_categorized.B9[dfGER$ability_to_access_care_priorCovid_categorized.B9<0] =NA
#p_test <- ggplot(aes(x=ability_to_access_care_priorCovid_categorized.B9, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
#p_test
dfGER_plot$ability_to_access_care_priorCovid.B9[dfGER$ability_to_access_care_priorCovid.B9<0] =NA
p_test <- ggplot(aes(x=ability_to_access_care_priorCovid.B9, y=satisfaction_with_care_duringCovid.C6), data=dfGER_plot) + geom_jitter()
p_test
