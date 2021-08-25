## Install all necessary packages
install.packages("tidyverse")
install.packages("data.table")

library("tidyverse")
library("data.table") # not needed

## General settings
wdir <- setwd("/media/storage/covidPD")

## Start reading data
data = as.data.frame(read.csv(file.path(wdir, "data", "raw_data.csv")))
na_if(data, 99) # Convert values of "99" to NA, probably also needed for other pseudo values

colnames = 	[
			"subj",
			"date_complete",
			"country",
			"consent", 
			"disease_duration.A1",
			"disease_stage.A2",
			"pdq8_1.A3", 
			"pdq8_2.A3", 
			"pdq8_3.A3", 
			"pdq8_4.A3", 
			"pdq8_5.A3", 
			"pdq8_6.A3", 
			"pdq8_7.A3", 
			"pdq8_8.A3", 			
			"pdq8_total.A3", 
			"pdq8_categorial.A3", 
			"comorbidities_categorial.A4",
			"comorbidities_list.A4",
			"comorbidities_sum.A4",
			"regular_caregiver_categorial.B1a",
			"regular_caregiver_relative_or_friend.B1a",
			"regular_caregiver_professional.B1a",
			"regular_caregiver_spouse.B1a",
			"regular_caregiver_sum.B1a",
			"regular_caregiver_attends_appointments.B1b",
			"regular_caregiver_helps_to_communicate.B1c",	
			"GP_visits_frequency.B2", 
			"GP_expertise.B3",
			"neurologists_visitis_preCovid.B4",
			"neurologists_expertise.B5",	
			"visit_healthcare_providers_categorial.B6",
			"visit_healthcare_providers_mental_healthcare.B6",
			"visit_healthcare_providers_speech_therapist.B6",
			"visit_healthcare_providers_physical_therapist.B6",
			"visit_healthcare_providers_social_worker.B6",
			"visit_healthcare_providers_other.B6",
			"visit_healthcare_providers_text.B6",
			"coorperation_healthcare_providers.B6a",
			...
]
## Provide information for the levels provided
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
