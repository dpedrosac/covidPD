# This is code to display the available data at the distinct German postal codes;
# Code developed by David Pedrosa

# Version 1.2 # 2021-12-10, adding data from the "Kassenaerztliche Bundesvereinigung"

# ==================================================================================================
## Specify packages of interest and load them automatically if needed
packages = c(
			"maptools", "dplyr", "Rcpp", "RColorBrewer", "osmar", "XML", "sf", "ggmap", "tidyverse", 
			"viridis", "stringr", "spdep", "proj4", "choroplethr", "ggplot2", "R6", "readr", "maps",
			"gpclib", "rgdal", "rgeos", "terra", "mapproj", "classInt", "ggthemes", "svDialogs") # packages which may be needed over the course

## Load or install all packages defined above
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# ==================================================================================================
## In case of multiple people working on one project, this helps to create an automatic script
username = Sys.info()["login"]
if (username == "dpedr") {
wdir = "D:/covidPD/"
} else if (username == "david") {
wdir = "/media/storage/skripte/covidPD/"
}
setwd(wdir)

# ==================================================================================================
## Load all necessary data to workspace
# Load spatial data and convert to dataframe (ggplot2 routines later require dataframes)
spatial_dataGER <- readOGR(dsn = file.path(wdir, "data"), layer = "plz-3stellig") # variable formerly ger_plz
gpclibPermit() # enable gpclib, that is needed by maptools (is that needed at all?)
spatial_dataGER@data$id <- rownames(spatial_dataGER@data)
spatial_dataGER.point <- fortify(spatial_dataGER, region="id")
spatial_dataGER.df <- inner_join(spatial_dataGER.point, spatial_dataGER@data, by="id")
spatial_dataGER$area_sqkm <- expanse(vect(file.path(wdir, "data", "plz-3stellig.shp"))) / 1000000

# Load KBV data ("Aerztedichte") (source: https://gesundheitsdaten.kbv.de/cms/html/16402.php)
# csv-data from xlsx modified so that columns have meaningful names (see below) and data is without German "Umlaute"
df_kbv <- read.csv2(file.path(wdir, "data", "aerztedichte_kbv_mod2.csv"))
colnames(df_kbv)[1] = "Regionalschluessel" # no idea why this is necessary!
cols_of_interest = c("Regionalschluessel", "PLZ", "Name", "Regionstyp", "density_physicians_total_by100K_pop", "density_physicians_neurologist_by100K_pop")
df_kbv_all = df_kbv %>% select(cols_of_interest)
df_kbv = df_kbv_all[which(df_kbv_all$Regionstyp==c("Kreise")),] 
df_kbv_reg = df_kbv_all[which(df_kbv_all$Regionstyp==c("Raumordnungsregionen")),] 

for (item in 1:length(df_kbv$PLZ)){ # dodgy way of entering zeros when postal codes have less than 5 digits
	if (!is.na(df_kbv$PLZ[item]) & nchar(df_kbv$PLZ[item]) < 3){
		df_kbv$PLZ[item] <- paste0("0", df_kbv$PLZ[item])
	} else {
	df_kbv$PLZ[item] <- df_kbv$PLZ[item]
	}
}	

# Load "Raumordnungsregionen" (source: https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjKi7L85dj0AhVB_rsIHT9YChgQFnoECAIQAQ&url=https%3A%2F%2Fwww.bbsr.bund.de%2FBBSR%2FDE%2Fforschung%2Fraumbeobachtung%2FRaumabgrenzungen%2Fdeutschland%2Fregionen%2FRaumordnungsregionen%2Fraumordnungsregionen-2017.xlsx%3F__blob%3DpublicationFile%26v%3D3&usg=AOvVaw3-EcnHoYwbArJyRCivHcqK)
# csv-data from xlsx modified so that no German "Umlaute" are present
df_raumordnungsregionen <- read.csv2(file.path(wdir, "data", "raumordnungsregionen_mod.csv"))

# Load PLZ data (source: https://www.suche-postleitzahl.org/downloads)
# csv-data from xlsx modified so that columns have meaningful names (see below) and data is without German "Umlaute"
df_plz2kreis <- read.csv2(file.path(wdir, "data", "zuordnung_plz_ort_landkreis.csv"))
for (item in 1:length(df_plz2kreis$plz3)){ # dodgy way of entering zeros when postal codes have less than 5 digits
	if (!is.na(df_plz2kreis$plz3[item]) & nchar(df_plz2kreis$plz3[item]) < 3){
		df_plz2kreis$plz3[item] <- paste0("0", df_plz2kreis$plz3[item])
	} else {
	df_plz2kreis$plz3[item] <- df_plz2kreis$plz3[item]
	}
}	

## Start merging all results to one dataframe
# Create csv-file with all information (inhabitants, size, etc.) and remove empty rows
if (file.exists(file.path(wdir, "data", "demographics_per_postal_code.csv"))){
	demographics_df <- read.csv(file.path(wdir, "data", "demographics_per_postal_code.csv"))
} else {
	len_dataframe <- length(unique(plz_from_source)) 
	demographics_df <- data.frame( # create empty dataframe
	plz=rep(NA,len_dataframe),#as.character(rep(NA,length(spatial_dataGER$plz)),
	size=rep(NA,len_dataframe),#rep(NA,length(spatial_dataGER$plz)),
	inhabitants=rep(NA,len_dataframe),#rep(NA,length(spatial_dataGER$plz)), 
	sqm=rep(NA,len_dataframe),#rep(NA,length(spatial_dataGER$plz)))
	physicians=rep(NA,len_dataframe),
	neurologists=rep(NA,len_dataframe))
	
	# Geospatial data and further information from https://www.suche-postleitzahl.org/downloads
	# No bigger modifications needes besides replacing Umlauts and "ß"
	df_temp = read.csv(file.path(wdir, "data", "plz_einwohner.csv"))
	df_temp$plz <- as.character(df_temp$plz)
	for (item in 1:length(df_temp$plz)){ # dodgy way of entering zeros when postal codes have less than 5 digits
		if (nchar(df_temp$plz[item]) < 5){
			df_temp$plz[item] <- paste0("0", df_temp$plz[item])
		} else {
			df_temp$plz[item] <- df_temp$plz[item]
		}
	}	
	
	plz_from_source <- spatial_dataGER$plz
	plz_from_kbv <- df_kbv$PLZ
	iter <- 0
	
	# Start for-loop to assign data
	cat("Assigning data from source to geospatial data ...\n")
	pb = txtProgressBar(min = 0, max = length(unique(plz_from_source)), initial = 0, style=3)
	for (value in unique(plz_from_source)) {
		iter <- iter + 1
		setTxtProgressBar(pb, iter)
		ind_plz1 <- which(substr(df_temp$plz, 1,3)==value)
		ind_plz2 <- which(plz_from_source==value)
		ind_plz3 <- which(df_kbv$PLZ==value)
		
		demographics_df[iter, 1] = as.character(value)
		if (nchar(value) < 3){
			demographics_df[iter, 1] = as.character(paste0("0", value))
		}
		demographics_df[iter, 2] = sum(spatial_dataGER$area_sqkm[ind_plz2])
		demographics_df[iter, 3] = sum(df_temp$einwohner[ind_plz1])
		demographics_df[iter, 4] = demographics_df[iter, 3] / demographics_df[iter, 2]
		if (length(ind_plz3)>1){
			demographics_df[iter, 5] = mean(df_kbv$density_physicians_total_by100K_pop[ind_plz3])
			demographics_df[iter, 6] = mean(df_kbv$density_physicians_neurologist_by100K_pop[ind_plz3]) 
		} else if (identical(ind_plz3, integer(0))) {
			if (identical(df_plz2kreis$landkreis[which(df_plz2kreis$plz3==value)][1], "")){
				next
			}
			#break
			
			found_kreise <- unique(df_plz2kreis$landkreis[which(df_plz2kreis$plz3==value)])
			found_kreise = found_kreise[found_kreise != ""][1]
			region = df_raumordnungsregionen$ROR11name[which(df_raumordnungsregionen$krs17name==found_kreise)]
			if (identical(region, character(0))){
				region = df_raumordnungsregionen$ROR11name[which(df_raumordnungsregionen$krs17name==paste0(found_kreise, ", Stadt"))]
			}

			if (identical(region, character(0))){
				region = df_raumordnungsregionen$ROR11name[which(df_raumordnungsregionen$krs17name==strsplit(found_kreise, split=" ")[[1]][2] )]
			}

			
			demographics_df[iter, 5] = df_kbv$density_physicians_total_by100K_pop[which(df_kbv_reg$Name==region)] 
			demographics_df[iter, 6] = df_kbv$density_physicians_neurologist_by100K_pop[which(df_kbv_reg$Name==region)] 
		} else {
			demographics_df[iter, 5] = df_kbv$density_physicians_total_by100K_pop[ind_plz3]
			demographics_df[iter, 6] = df_kbv$density_physicians_neurologist_by100K_pop[ind_plz3]
		}
		if (iter==length(plz_from_source)) cat("\nDone!\n")
	}
	close(pb)
	# write.csv(demographics_df,file.path(wdir, "data", "demographics_per_postal_code.csv"), row.names = FALSE) # not working until fix for missing "0" is found
}
demographics_df <- demographics_df[!duplicated(demographics_df), ] # remove duplicates

# Some PLZ need manual refinement, especially with major cities
# Berlin
ind_BRL <- which(	demographics_df$plz=="101" | 
					demographics_df$plz=="102" |
					demographics_df$plz=="103" | 
					demographics_df$plz=="104" |
					demographics_df$plz=="105" | 
					demographics_df$plz=="106" |
					demographics_df$plz=="107" | 
					demographics_df$plz=="108" |
					demographics_df$plz=="109" |
					demographics_df$plz=="120" | 
					demographics_df$plz=="121" |
					demographics_df$plz=="122" | 
					demographics_df$plz=="123" |
					demographics_df$plz=="124" | 
					demographics_df$plz=="125" |
					demographics_df$plz=="126" | 
					demographics_df$plz=="130" |
					demographics_df$plz=="131" |
					demographics_df$plz=="133" |
					demographics_df$plz=="134" | 
					demographics_df$plz=="135" |
					demographics_df$plz=="136" | 
					demographics_df$plz=="140" |
					demographics_df$plz=="141"
					)
demographics_df$physicians[ind_BRL] = df_kbv$density_physicians_total_by100K_pop[which(df_kbv$Name=="Berlin, Stadt")]				
demographics_df$neurologists[ind_BRL] = df_kbv$density_physicians_neurologist_by100K_pop[which(df_kbv$Name=="Berlin, Stadt")]				

# Dresden
ind_DRE <- which(	demographics_df$plz=="010" | 
					demographics_df$plz=="011" |
					demographics_df$plz=="012" | 
					demographics_df$plz=="013")
demographics_df$physicians[ind_DRE] = df_kbv$density_physicians_total_by100K_pop[which(df_kbv$Name=="Dresden, Stadt")]				
demographics_df$neurologists[ind_DRE] = df_kbv$density_physicians_neurologist_by100K_pop[which(df_kbv$Name=="Dresden, Stadt")]				

# Hamburg
ind_HH <- which(	demographics_df$plz=="200" | 
					demographics_df$plz=="201" |
					demographics_df$plz=="202" | 
					demographics_df$plz=="203" |
					demographics_df$plz=="204" |
					demographics_df$plz=="205" |
					demographics_df$plz=="210" |
					demographics_df$plz=="211" |
					demographics_df$plz=="212" |
					demographics_df$plz=="223" |
					demographics_df$plz=="224" |
					demographics_df$plz=="225" |
					demographics_df$plz=="226" |
					demographics_df$plz=="227" |
					demographics_df$plz=="228" )
demographics_df$physicians[ind_HH] = df_kbv$density_physicians_total_by100K_pop[which(df_kbv$Name=="Hamburg, Stadt")]				
demographics_df$neurologists[ind_HH] = df_kbv$density_physicians_neurologist_by100K_pop[which(df_kbv$Name=="Hamburg, Stadt")]				

# Hannover
ind_HAN <- which(	demographics_df$plz=="301" | 
					demographics_df$plz=="304" |
					demographics_df$plz=="305" |
					demographics_df$plz=="306" | 
					demographics_df$plz=="308" | 
					demographics_df$plz=="309" )
demographics_df$physicians[ind_HAN] = df_kbv$density_physicians_total_by100K_pop[which(df_kbv$Name=="Hannover, Stadt")]				
demographics_df$neurologists[ind_HAN] = df_kbv$density_physicians_neurologist_by100K_pop[which(df_kbv$Name=="Hannover, Stadt")]				

# Koeln
ind_COL <- which(	demographics_df$plz=="506" | 
					demographics_df$plz=="507" |
					demographics_df$plz=="508" | 
					demographics_df$plz=="509" |
					demographics_df$plz=="510" |
					demographics_df$plz=="511")
demographics_df$physicians[ind_COL] = df_kbv$density_physicians_total_by100K_pop[which(df_kbv$Name=="Koeln, Stadt")]				
demographics_df$neurologists[ind_COL] = df_kbv$density_physicians_neurologist_by100K_pop[which(df_kbv$Name=="Koeln, Stadt")]				

# Stuttgart
ind_STU <- which(	demographics_df$plz=="701" | 
					demographics_df$plz=="703" |
					demographics_df$plz=="704" |
					demographics_df$plz=="705" | 
					demographics_df$plz=="706" | 
					demographics_df$plz=="707" | 
					demographics_df$plz=="708" )
demographics_df$physicians[ind_STU] = df_kbv$density_physicians_total_by100K_pop[which(df_kbv$Name=="Stuttgart, Stadt")]				
demographics_df$neurologists[ind_STU] = df_kbv$density_physicians_neurologist_by100K_pop[which(df_kbv$Name=="Stuttgart, Stadt")]		

# Muenchen
ind_MUN <- which(	demographics_df$plz=="803" | 
					demographics_df$plz=="805" |
					demographics_df$plz=="806" |
					demographics_df$plz=="808" | 
					demographics_df$plz=="809" | 
					demographics_df$plz=="812" |
					demographics_df$plz=="813" |
					demographics_df$plz=="814" |
					demographics_df$plz=="815" |
					demographics_df$plz=="816" |
					demographics_df$plz=="817" |
					demographics_df$plz=="818" |
					demographics_df$plz=="819" |
					demographics_df$plz=="856" )
demographics_df$physicians[ind_MUN] = df_kbv$density_physicians_total_by100K_pop[which(df_kbv$Name=="Muenchen, Stadt")]				
demographics_df$neurologists[ind_MUN] = df_kbv$density_physicians_neurologist_by100K_pop[which(df_kbv$Name=="Muenchen, Stadt")]				



# ==================================================================================================
## Prepare data for plotting population per skm
merge.shp <- merge(spatial_dataGER.df, demographics_df, by = "plz") 
merge.shp <- merge.shp[order(merge.shp$order), ]

# Get some meaningful breaks (manually)
manual_breaks <- c(100,150,300,1000,4000)
minVal <- min(merge.shp$sqm, na.rm = T)
maxVal <- max(merge.shp$sqm, na.rm = T)

labels <- c()
brks <- c(minVal, manual_breaks, maxVal)

for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
# define a new variable on the data set just as above
merge.shp$brks <- cut(merge.shp$sqm, 
                     breaks = brks, 
                     include.lowest = TRUE, 
                     labels = labels)

brks_scale <- levels(merge.shp$brks)
labels_scale <- rev(brks_scale)

fig <- ggplot(data=merge.shp, aes(x = long, 
						  y = lat, 
						  group = group)) +
	geom_polygon(aes(fill = brks), na.rm=FALSE, rule="evenodd", position="identity") +
	#geom_path(data = merge.shp, aes(x = long, 
    #                               y = lat, 
    #                               group = group), 
    #          color = "white", size = 0.001) +
    coord_map() +
	scale_fill_brewer("", drop = FALSE, na.value = "black") + 
	ggplot2::theme_void() +
	ggtitle("People per square kilometers") +
	theme_map() +
    theme(
      legend.position = c(0.5, 0.03),
      legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.0)),
      legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
      plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                                   margin = margin(b = -0.1, 
                                                   t = -0.1, 
                                                   l = 2, 
                                                   unit = "cm"), 
                                   debug = F),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 6, 
                                  hjust = 0.92, 
                                  margin = margin(t = 0.2, 
                                                  b = 0, 
                                                  unit = "cm"), 
                                  color = "#939184")) +
	
	#theme(legend.position = "bottom")+
	labs(x = NULL, 
         y = NULL, 
         title = "Germany's regional demographics", 
         subtitle = "Average population per square kilometer in German municipalities, 2019", 
         caption = "Map CC-BY-SA; Author: AG Bewegungsstoerungen und Neuromodulation, UKGM; Geometries: plz-suche.org, 2021; Data: Zensus, 2011") +
	scale_fill_manual(
          values = rev(brewer.pal(7, "Blues")),
          breaks = rev(brks_scale),
          name = "Population [inhabitants per km^2]",
          drop = FALSE,
          labels = labels_scale,
          guide = guide_legend(
            direction = "horizontal",
            keyheight = unit(2, units = "mm"),
            keywidth = unit(70 / length(labels), units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 1,
            nrow = 1,
            byrow = T,
            reverse = T,
            label.position = "bottom"
          )
      )
fig 

# ==================================================================================================
## Start plotting available number of questionnaires per postal code
df_questionnaire <- read.csv(file.path(wdir, "data", "raw_data.csv"))
df_questionnaire <- df_questionnaire[df_questionnaire$Country=="GE", ]

df_data = data.frame(plz=demographics_df$plz, 
					 count_questionnaires=rep(0, length(demographics_df$plz)))
iter <- 0
for (value in df_data$plz) {
	iter <- iter + 1
	if (length(which(df_questionnaire$Postalcode==value))>0) { 
		df_data$count_questionnaires[iter] <- length(which(df_questionnaire$Postalcode==value))
	}
}

merge.shp <- merge(spatial_dataGER.df, df_data, by = "plz")
merge.shp <- merge.shp[order(merge.shp$order), ]

# Get some meaningful breaks (manually)
manual_breaks <- c(0,1,2,3,4,5)
minVal <- min(merge.shp$count_questionnaires, na.rm = T)
maxVal <- max(merge.shp$count_questionnaires, na.rm = T)

labels <- c()
brks2 <- c(-1, manual_breaks, maxVal)

for(idx in 1:length(brks2)){
  labels <- c(labels,round(brks2[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
# define a new variable on the data set just as above
merge.shp$brks2 <- cut(merge.shp$count_questionnaires, 
                     breaks = brks2, 
                     include.lowest = TRUE, 
                     labels = labels)

brks_scale <- levels(merge.shp$brks2)
labels_scale <- rev(brks_scale)

fig2 <- ggplot(data=merge.shp, aes(x = long, 
						  y = lat, 
						  group = group)) +
	geom_polygon(aes(fill = brks2), na.rm=FALSE, rule="evenodd", position="identity") +
    coord_map() +
	scale_fill_brewer("", drop = FALSE, na.value = "black") + 
	ggplot2::theme_void() +
	ggtitle("Available questionnaires") +
	theme_map() +
    theme(
      legend.position = c(0.5, 0.03),
      legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.0)),
      legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
      plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                                   margin = margin(b = -0.1, 
                                                   t = -0.1, 
                                                   l = 2, 
                                                   unit = "cm"), 
                                   debug = F),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 6, 
                                  hjust = 0.92, 
                                  margin = margin(t = 0.2, 
                                                  b = 0, 
                                                  unit = "cm"), 
                                  color = "#939184")) +
	
	#theme(legend.position = "bottom")+
	labs(x = NULL, 
         y = NULL, 
         title = "Origin of available questionnaires", 
         subtitle = "Number of questionnaires per postal code, 2021", 
         caption = "Map CC-BY-SA; Author: AG Bewegungsstoerungen und Neuromodulation, UKGM; Geometries: plz-suche.org, 2021; Data: own questionnaire") +
	scale_fill_manual(
          values = rev(brewer.pal(7, "Blues")),
          breaks = rev(brks_scale),
          name = "No. of questionnaires",
          drop = FALSE,
          labels = labels_scale,
          guide = guide_legend(
            direction = "horizontal",
            keyheight = unit(2, units = "mm"),
            keywidth = unit(70 / length(labels), units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 1,
            nrow = 1,
            byrow = T,
            reverse = T,
            label.position = "bottom"
          )
      )
fig2

# ==================================================================================================
## Start plotting number of neurologist per postal code
merge.shp <- merge(spatial_dataGER.df, demographics_df, by = "plz") 
merge.shp <- merge.shp[order(merge.shp$order), ]

# Get some meaningful breaks (manually)
manual_breaks <- c(0.1,4.5,5.7,6.8,10.2)
minVal <- min(merge.shp$neurologists, na.rm = T)
maxVal <- max(merge.shp$neurologists, na.rm = T)

labels <- c()
brks <- c(minVal, manual_breaks, maxVal)

for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
# define a new variable on the data set just as above
merge.shp$brks <- cut(merge.shp$neurologists, 
                     breaks = brks, 
                     include.lowest = TRUE, 
                     labels = labels)

brks_scale <- levels(merge.shp$brks)
labels_scale <- rev(brks_scale)

fig3 <- ggplot(data=merge.shp, aes(x = long, 
						  y = lat, 
						  group = group)) +
	geom_polygon(aes(fill = brks), na.rm=FALSE, rule="evenodd", position="identity") +
	#geom_path(data = merge.shp, aes(x = long, 
    #                               y = lat, 
    #                               group = group), 
    #          color = "white", size = 0.001) +
    coord_map() +
	scale_fill_brewer("", drop = FALSE, na.value = "black") + 
	ggplot2::theme_void() +
	ggtitle("Neurologists per 100.000 inhabitants") +
	theme_map() +
    theme(
      legend.position = c(0.5, 0.03),
      legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.0)),
      legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
      plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                                   margin = margin(b = -0.1, 
                                                   t = -0.1, 
                                                   l = 2, 
                                                   unit = "cm"), 
                                   debug = F),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 6, 
                                  hjust = 0.92, 
                                  margin = margin(t = 0.2, 
                                                  b = 0, 
                                                  unit = "cm"), 
                                  color = "#939184")) +
	
	#theme(legend.position = "bottom")+
	labs(x = NULL, 
         y = NULL, 
         title = "Germany's regional differences for neurologists", 
         subtitle = "Average density of neurologists per 100.000 inhabitants in German municipalities, 2019", 
         caption = "Map CC-BY-SA; Author: AG Bewegungsstoerungen und Neuromodulation, UKGM; Geometries: plz-suche.org, 2021; Data: KBV, 2019") +
	scale_fill_manual(
          values = rev(brewer.pal(7, "Blues")),
          breaks = rev(brks_scale),
          name = "Neurologists [per 100.000 inhabitants]",
          drop = FALSE,
          labels = labels_scale,
          guide = guide_legend(
            direction = "horizontal",
            keyheight = unit(2, units = "mm"),
            keywidth = unit(70 / length(labels), units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 1,
            nrow = 1,
            byrow = T,
            reverse = T,
            label.position = "bottom"
          )
      )
fig3
