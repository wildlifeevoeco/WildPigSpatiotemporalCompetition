#Script 1: Contains code for proportional hazard models + plotting coef's

#packages
library(dplyr)
library(readr)
library(camtrapR)
library(evaluate)
library(gdata)
library(janitor)
library(lubridate)
library(maptools)
library(data.table)
library(janitor)
library(stats)

#data import
BoarDATA <- read_csv("Data/Processed/BoarDATA.csv", 
                     col_types = cols(posixdate = col_datetime(format = "%m/%d/%Y %H:%M")))

########################################################################################################
#this section will identify the species of interest, and filter out the species recorded 

#convert to data table to work with
BoarDATA2 <- as.data.table(BoarDATA)

#converting boar --> invasive species and mammals of interest to native
species_interest <- c("Native", "Invasive", "Human")

#new species column to work off as reference 
BoarDATA2$Native <- BoarDATA2$Animal

#then renaming the species to native / invasive 
BoarDATA2$Native[BoarDATA2$Native == "Boar"] <- "Invasive"
BoarDATA2$Native[BoarDATA2$Native == "Moose"] <- "Native"
BoarDATA2$Native[BoarDATA2$Native == "White Tailed Deer"] <- "Native"
BoarDATA2$Native[BoarDATA2$Native == "Coyote"] <- "Native"
BoarDATA2$Native[BoarDATA2$Native == "Elk"] <- "Native"
BoarDATA2$Native[BoarDATA2$Native == "Mule Deer"] <- "Native"

#importnat to include us at cameras for censoring  
BoarDATA2$Native[BoarDATA2$Native == "Human"] <- "Human"

#sorting by species of interest 
BoarDATA2[, interest := Native %in% species_interest]

#only species of interest
BoarDATA3 <- filter(BoarDATA2, interest == TRUE)
BoarDATA3 <- as.data.frame(BoarDATA3)

########################################################################################################
##code adapted from Louvrier et al. 2021
#I wasnt able to get the function (below) to run without re-naming my variables to the same names they had.
#so just aligning our data so the function can run correctly.
CT_act_MN <- BoarDATA3

d1_interest <- CT_act_MN
str(d1_interest)
d1_interest$Species <- as.factor(d1_interest$Native)
class(d1_interest$Species)

d1_interest$Station <- as.factor(d1_interest$Camera)
levels(d1_interest$Station) <- c(1:17)
d1_interest$Station <- as.integer(d1_interest$Station)
class(d1_interest$Station)

d1_interest <- as.data.frame(d1_interest)

d1_interest$DateTimeOriginal <- d1_interest$posixdate
class(d1_interest$DateTimeOriginal)

#extracting the ids of the camera traps
id_CT <- unique(d1_interest$Station)

########################################################################################################
#Louvrier et al. 2021 function which calculates time between sequential events for a given camera trap. 

calculate_delta <- function(species)
{
  #empty list 
  delta_species <- list(0)
  #loop starts here
  
  for(j in id_CT)
  {
    k = which(id_CT == j)
    #take the CT by its id 
    B <- d1_interest %>% filter(Station == j)
    
    #order it by date and time
    B_ordered <- B %>% arrange(DateTimeOriginal)
    
    #delta is going to contain the calculated difference of time between species A and B for each row of the detections of the CT
    #but only if the two species from row i and i+1 are in the specific order
    #if the combination of the species names between the row i and i + 1 is equal to the names of the species of interest     stuck together then we calculate difftime between the two lines,  otherwise, no
    
    delta <- rep(NA, nrow(B_ordered))
    for(i in 1: (nrow(B_ordered)))
    {
      if (paste(B_ordered[c(i:(i+1)),"Species"], collapse ="") == paste(species, collapse=""))
      { 
        diff <- difftime(B_ordered[i,"DateTimeOriginal"], B_ordered[i+1,"DateTimeOriginal"])
        if (diff < - 10000) 
          delta[i] <- 0 
        }
        if (diff >= -10000)
        {
          delta[i] <- diff
        }
      }
    }
    
    delta_species[[k]] <- delta
    names(delta_species)[k] <- as.character(j)
    
  }
  return(delta_species)
  
}
#################################################################################
#selecting the combo of invasive wild pig --> any native species 
species = c(species_interest[2],species_interest[1])
delata_boar_any <- calculate_delta(species)
#putting this in a dataframe 
df_time_diff_boar_any = plyr::ldply(delata_boar_any, rbind)

##next interactions.... combo of any native --> any native sp of interest 
species = c(species_interest[1],species_interest[1])
delata_any_any <- calculate_delta(species) 
#putting this in a dataframe 
df_time_diff_any_any = plyr::ldply(delata_any_any, rbind)

##final interaction of interest... wild pig --> wild pig 
species = c(species_interest[2],species_interest[2])
delata_boar_boar <- calculate_delta(species) 
#putting this in a dataframe 
df_time_diff_boar_boar = plyr::ldply(delata_boar_boar, rbind)

##extracting events that were interupted by humans (censor). First human --> native species
species = c(species_interest[3],species_interest[1])
delata_human_native <- calculate_delta(species) 
#putting this in a dataframe 
df_time_diff_human_native = plyr::ldply(delata_human_native, rbind)

##and now human to boar
species = c(species_interest[3],species_interest[2])
delata_human_boar <- calculate_delta(species) 
#putting this in a dataframe 
df_time_diff_human_boar = plyr::ldply(delata_human_boar, rbind)

#################################################################################
#need to extract time-to-event values from dataframes 

#pivot longer,  last two columns in the DF = TTE's + the camera associated with 
#wild pig --> native species combo
boar_any_extract <- tidyr::pivot_longer(df_time_diff_boar_any, c(2:50), values_drop_na = TRUE)
# extracting the two columns
boar_any_extracted <- cbind.data.frame(boar_any_extract$.id, boar_any_extract$value)
#create a new column labelling that this is the interaction
boar_any_extracted$Type <- rep(c("Boar_native"), times = 7)
boar_any_extracted$ID <- boar_any_extracted$`boar_any_extract$.id`

#repeat for wild pig to wild pig
boar_boar_extract <- tidyr::pivot_longer(df_time_diff_boar_boar, c(2:50), values_drop_na = TRUE)
boar_boar_extracted <- cbind.data.frame(boar_boar_extract$.id, boar_boar_extract$value)
boar_boar_extracted$Type <- rep(c("Boar_boar"), times = 11)
boar_boar_extracted$ID <- boar_boar_extracted$`boar_boar_extract$.id`

#repeat for native to native
any_any_extract <- tidyr::pivot_longer(df_time_diff_any_any, c(2:50), values_drop_na = TRUE)
any_any_extracted <- cbind.data.frame(any_any_extract$.id, any_any_extract$value)
any_any_extracted$Type <- rep(c("Native_native"), times = 517)
any_any_extracted$ID <- any_any_extracted$`any_any_extract$.id`

#repeat for human to native species  
human_native_extract <- tidyr::pivot_longer(df_time_diff_human_native, c(2:50), values_drop_na = TRUE)
human_native_extracted <- cbind.data.frame(human_native_extract$.id, human_native_extract$value)
human_native_extracted$Type <- rep(c("Human_native"), times = 27)
human_native_extracted$ID <- human_native_extracted$`human_native_extract$.id`

#repeat for human to wild pig
human_boar_extract <- tidyr::pivot_longer(df_time_diff_human_boar, c(2:50), values_drop_na = TRUE)
human_boar_extracted <- cbind.data.frame(human_boar_extract$.id, human_boar_extract$value)
human_boar_extracted$Type <- rep(c("Human_boar"), times = 1)
human_boar_extracted$ID <- human_boar_extracted$`human_boar_extract$.id`

#################################################################################
#clean up and merge 3 dataframes 
boar_boar_extracted$TTE <- boar_boar_extracted$`boar_boar_extract$value`
boar_any_extracted$TTE <- boar_any_extracted$`boar_any_extract$value`
any_any_extracted$TTE <- any_any_extracted$`any_any_extract$value`

human_boar_extracted$TTE <- human_boar_extracted$`human_boar_extract$value`
human_native_extracted$TTE <- human_native_extracted$`human_native_extract$value`

#clean up unnecessary columns 
boar_boar_clean <- dplyr::select(boar_boar_extracted, -1,-2)
boar_any_clean <- dplyr::select(boar_any_extracted, -1,-2)
any_any_clean <- dplyr::select(any_any_extracted, -1,-2)

human_boar_clean <- dplyr::select(human_boar_extracted, -1,-2)
human_native_clean <- dplyr::select(human_native_extracted, -1,-2)

#################################################################################
#merge to an analysis dataframe
cox_1.0 <- rbind(boar_any_clean, boar_boar_clean, any_any_clean, 
                 human_boar_clean, human_native_clean)

#save as a CSV
cox_1.0 <- write_csv(cox_1.0, "cox_1.0.csv")
##############################################################################
# attaching habitat covariates to TTE's

#load habitat covar data 
BoarDATA <- read_csv("Data/Processed/BoarDATA.csv", 
                     col_types = cols(posixdate = col_datetime(format = "%m/%d/%Y %H:%M")))

#TTE data
cox_1.0 <- read_csv("Data/Processed/cox_1.0.csv")

#subset habitat data to covars of intetest 
sub <- BoarDATA[c("Camera", "posixdate", "Habitat_Ty" , "ElevationC", "DIST_WATER", "DIST_ROAD")]

#rename 
colnames(cox_1.0)[2] <- "Camera"

#creating a column of unique values in cox_1.0 to sort out later after merge. 
cox_1.0$Unique <- c(1:530)

#renaming numerical to camera_id names
cox_1.0$Camera[cox_1.0$Camera == "1"] <- "Cam01"
cox_1.0$Camera[cox_1.0$Camera == "2"] <- "Cam02"
cox_1.0$Camera[cox_1.0$Camera == "3"] <- "Cam03"
cox_1.0$Camera[cox_1.0$Camera == "4"] <- "Cam04"
cox_1.0$Camera[cox_1.0$Camera == "5"] <- "Cam05"
cox_1.0$Camera[cox_1.0$Camera == "6"] <- "Cam07"
cox_1.0$Camera[cox_1.0$Camera == "7"] <- "Cam08"
cox_1.0$Camera[cox_1.0$Camera == "8"] <- "Cam09"
cox_1.0$Camera[cox_1.0$Camera == "9"] <- "Cam10"
cox_1.0$Camera[cox_1.0$Camera == "10"] <- "Cam11"
cox_1.0$Camera[cox_1.0$Camera == "11"] <- "Cam12"
cox_1.0$Camera[cox_1.0$Camera == "12"] <- "Cam13"
cox_1.0$Camera[cox_1.0$Camera == "13"] <- "Cam14"
cox_1.0$Camera[cox_1.0$Camera == "14"] <- "Cam15"
cox_1.0$Camera[cox_1.0$Camera == "15"] <- "Cam16"
cox_1.0$Camera[cox_1.0$Camera == "16"] <- "Cam17"
cox_1.0$Camera[cox_1.0$Camera == "17"] <- "Cam18"

#merge 
intermediate <- merge(cox_1.0, sub, by = "Camera", all.x = FALSE, all.y = FALSE, no.dups = TRUE)

#filter by unique values
almostthere <-  intermediate %>% 
  group_by(Unique) %>%
  filter(row_number()==1)

#fix habitat names for plotting later 
almostthere$Habitat_Ty[almostthere$Habitat_Ty == "Cropland/?"] <- "Cropland"
almostthere$Habitat_Ty[almostthere$Habitat_Ty == "Pasture"] <- "Apasture"

# save analysis dataframe
cox_analyze <- write_csv(almostthere, "cox_analyze.csv")

######################################################################################################################
# Hazard models
#packages req'd for hazard models
library(readr)
library(data.table)
library(coxme)
library(survival)
library(survminer)
library(dplyr)
library(MuMIn)
library(plotrix)

#data 
cox_analyze <- read_csv("Data/Processed/cox_analyze.csv")

#remove the human inturrupted observations  
cox_analyze <- subset(cox_analyze$Type, select = -c("Human_native", "Human_boar"))  

#structuring
cox_analyze$Camera <- as.factor(cox_analyze$Camera)
cox_analyze$Type <- as.factor(cox_analyze$Type)
cox_analyze$Habitat_Ty <- as.factor(cox_analyze$Habitat_Ty)

#TTE's as positive numbers  
cox_analyze$TTE <- abs(cox_analyze$TTE)
#####################################################################################################
#model set for ranking based on the few important covars

#just a test model to make sure structure works. 
test <- coxph(Surv(TTE) ~ 1, data = cox_analyze)
summary(test)
AIC(test)

#base model with just cams as a random intercept. 
base <- coxme(Surv(TTE) ~ (1|Camera), data = cox_analyze)
summary(base)
AICbase <- AICc(base)

#next model with just interaction type 
model1 <- coxme(Surv(TTE) ~ Type + (1|Camera), data = cox_analyze)
summary(model1)
AIC1 <- AICc(model1)

#habitat type 
model2 <- coxme(Surv(TTE) ~ Habitat_Ty + (1|Camera), data = cox_analyze)
summary(model2)
AIC2 <- AICc(model2)

#habitat + interaction type 
model <- coxme(Surv(TTE) ~ Habitat_Ty + Type  + (1|Camera), data = cox_analyze)
summary(model)
AIC3 <- AICc(model)


#creating an AICc table for model ranking
#putting AICc values into a vector with models attached
Model_names <- c("Base", "Type", "Habitat", "Habitat + Type")
AICvector <- c(AICbase, AIC1, AIC2, AIC3)

#generating weights 
AICw <- Weights(AICvector)
AICw

#joining names + AICc + Weights 
AICtable <- cbind.data.frame(Model_names, AICvector, AICw)

#summary stats 
exp(mean(log(cox_analyze$TTE[cox_analyze$Type == "Boar_boar"])))    
# mean = 13.1774 hrs     median = 11 hrs

exp(mean(log(cox_analyze$TTE[cox_analyze$Type == "Boar_native"])))
# mean = 19.2628 hrs    median = 14.86667 hrs

exp(mean(log(cox_analyze$TTE[cox_analyze$Type == "Anative_native"])))
# mean = 11.18876 hrs   median = 6.471181

################################################################################################
#Below is predicting risk based on the top suppored model. 

#creating new column of predicted risk from model
cox_analyze$risk <- coxme:::predict.coxme(model, type="risk")

#subsetting the predicted risk by the interaction type and then generating summary stats 
b_b <- filter(cox_analyze, Type == "Boar_boar")
mean(b_b$risk)  
median(b_b$risk)
std.error(b_b$risk) 

b_n <- filter(cox_analyze, Type == "Boar_native")
mean(b_n$risk) 
median(b_n$risk) 
std.error(b_n$risk)

n_n <- filter(cox_analyze, Type == "Anative_native")
mean(n_n$risk) 
median(n_n$risk) 
std.error(n_n$risk) 

#######################################################################
#making figures for the predicted risk values

#loading packages 
library(ggplot2)
library(ggpubr)

#new data just to plot 
boxplot_data <- cox_analyze

boxplot_data$Type <- as.character(boxplot_data$Type)

#consistent names 
boxplot_data$Type[boxplot_data$Type == "Boar_boar"] <- "wild pig:wild pig"
boxplot_data$Type[boxplot_data$Type == "Boar_native"] <- "wild pig:native"
boxplot_data$Type[boxplot_data$Type == "Anative_native"] <- "native:native species"

boxplot_data$Type <- as.factor(boxplot_data$Type)

#loading colour palatte to keep consistent with activity plots from other script
col <- c("#E69F00", "#993300", "#009E73")

#violin plot for predicted hazard values. 
ggplot(data = boxplot_data, mapping = aes(x = Type, y = risk, fill=Type)) +
  geom_violin() +
  scale_fill_manual(values = c("#E69F00", "#993300", "#009E73")) +
  geom_jitter(alpha = 0.2) +
  theme_classic() +
  theme(legend.title= element_blank()) +
  theme(legend.position = "none") +
  xlab("Type of species to species event") + ylab("Predicted hazard value") +
  theme(axis.title=element_text(size=16, colour = "black")) +
  theme(axis.text=element_text(size=16, colour = "black")) +
  geom_hline(yintercept = 1, linetype="dashed", alpha=0.3)


#violin plot for all species events and habitat combinations 
  ggplot(data = boxplot_data, mapping = aes(x = interaction(Type,Habitat_Ty), y = risk, fill=Type)) +
  geom_violin() +
  scale_fill_manual(values = c("#E69F00", "#993300", "#009E73")) +
  geom_jitter(alpha = 0.2) +
  theme_classic() +
  theme(legend.title= element_blank()) +
  theme(legend.position = "none") +
  xlab("Type of species to species event per habitat") + ylab("Predicted hazard values") +
  theme(axis.title=element_text(size=10, colour = "black"),  plot.margin = margin(, 0.7, , , "cm")) +
  theme(axis.text=element_text(size=6, colour = "black")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  geom_hline(yintercept = 1, linetype="dashed", alpha=0.3)

###############################################################################
#plotting the coefficient values from top supported hazard model.

#package call  
library(tibble)

#coe'f names, values, and error
Coefs <- c("Cropland", "Deciduous forest", "Wetland", "Woodland", "Wild pig:wild pig", "Wild pig:native")
Values <- c(0.06989569, -0.09961192, 0.05139819, -1.03960363, -0.18228521,  -0.54251537)
SE <- c(0.1702148, 0.1540387, 0.1624072, 0.5294666, 0.3152767,  0.3897565)

#new DF
model.summary <- cbind.data.frame(Coefs, Values, SE)

#To create 95% CI intervals
model.summary$Confid <- (model.summary$SE * 1.96)

# bolding for significance.
bold.hab <- c("Wild pig:wild pig")
bold.labels <- ifelse(levels(model.summary$Coefs) %in% bold.hab, yes = "bold", no = "plain")

#the plot of coef values for the suppliment
ggplot(model.summary, aes(x=Coefs, y=Values)) +
  geom_point(col="black", size=3) +
  xlab("Coefficients from top supported model") + ylab("Standardized coefficient value") +
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.3) +
  geom_errorbar(aes(ymin=Values-Confid, ymax=Values+Confid), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(face = bold.labels)) +
  scale_x_discrete(limits = c("Cropland", "Deciduous forest", "Wetland", "Woodland", "Wild pig:wild pig", "Wild pig:native")) +
  theme_classic() +
  theme(axis.title=element_text(size=16, colour = "black")) +
  theme(axis.text=element_text(size=16, colour = "black")) +
  annotate("text", x = 4, y = 0.5, size = 12,
           label = "*",
           fontface = "bold")

################################################################################
#Bootstrapping distributions of each event type to show we have confidence in trends exhibited (Fig 4)
#packages required                
library(png)
library(jpeg)
library(grid)

#calculating the bounds that the boxplot data will be held by
n_n_sub <- subset(cox_analyze, Type == "Native_native")
b_n_sub <- subset(cox_analyze, Type == "Boar_native")
max(b_n_sub$TTE) #63.29236
min(b_n_sub$TTE) #1.043056
exp(mean(log(b_n_sub$TTE))) #11.28308
mean(b_n_sub$TTE) #19.2628
median(b_n_sub$TTE) #14.86667


b_b_sub <- subset(cox_analyze, Type == "Boar_boar")
max(b_b_sub$TTE) #30.86597
min(b_b_sub$TTE) #1.133333
exp(mean(log(b_b_sub$TTE))) #7.745591
mean(b_b_sub$TTE) #13.1774
median(b_b_sub$TTE) #11

#subsetting data types to not include native native for boxplot
box_sub <- subset(cox_analyze, !Type == "Native_native")

#bootstrapping native-to-native species dist'n to n=7 (smallest sample in data), and reiterating 1000x
set.seed(08092023)
TTE <- replicate(1000, median(sample(n_n_sub$TTE, 7, replace = TRUE)))

n_n_boot_sample <- as.data.frame(seq(1:1000))
n_n_boot_sample$TTE <- unlist(TTE)
n_n_boot_sample$Type <- rep("Native_native") 
n_n_boot_sample$Type <- as.factor(n_n_boot_sample$Type)
n_n_boot_sample <- as.data.frame(n_n_boot_sample)

#generating a boxplot of mean + range TTE for small sample size Types
b_n_boxplot_img <- ggplot(box_sub) +
  geom_boxplot(aes(TTE, colour=Type), data =box_sub, lwd=1) +
  scale_fill_manual(values = c("#E69F00", "#993300")) +
  scale_colour_manual(values = c("#E69F00", "#993300")) +
  #NOTE HERE: I expanded X_axis by 0.5 units so outlier would fit
  #need to expland placement below by 0.5 
  scale_x_continuous(expand = c(0, 0.5)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic()  +
  theme(legend.position = "none") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank())

#saving blank version of the boxplot to be imposed with probabilty distribution
ggsave("/Output/Box_insert.png",
       plot = b_n_boxplot_img, 
       device = "png")

#calling back image
path <- "/Output/Box_insert.png"
img <- readPNG(path, native = TRUE)
img <- rasterGrob(img, interpolate=TRUE)

#plotting image of boxplot of pig-pig and pig-native categories on top of 
#a probability distribution of native-native 
ggplot() +
  #because i extended the values above for the image to fit, need to call same 
  #x-dimensions so the .png is accurately on the PDF x-axis.
  #set annotation values to values of raw data (with a 0.5 extension, see above)
  annotation_custom(img, xmin=0, xmax=63, ymin=0, ymax=) +
  #adding density of bootstrapped (n=7) median values x 1000
  geom_density(aes(TTE, fill=Type, colour=Type), alpha = 0.15, data=n_n_boot_sample) +
  scale_colour_manual(name = "Event type",
                      guide = "legend",
                      labels = c("Wild pig to native","Wild pig to wild pig","Native to native species"),
                      values = c("#993300","#E69F00", "#009E73"),
                      drop = FALSE) +
  scale_fill_manual(name = "Event type",
                    guide = "legend",
                    labels = c("Wild pig to native","Wild pig to wild pig","Native to native species"),
                    values = c("#993300", "#E69F00", "#009E73"),
                    drop = FALSE) +
  scale_x_continuous(limits = c(0,64), expand = expansion(mult = c(0, 0)) ) + 
  scale_y_continuous(limits = c(0,0.13), expand = expansion(mult = c(0, 0))) +
  ylab("Relative density ") + 
  xlab("Time between events (hrs)") +
  theme_classic() + 
  theme(legend.position = c(0.67,0.82)) +  
  theme(axis.title=element_text(size=10, colour = "black")) +
  theme(axis.text=element_text(size=10, colour = "black")) +
  theme(legend.title=element_text(size=10.5, colour = "black")) +
  theme(legend.text=element_text(size=10, colour = "black")) +
  #pig:pig line not needed here, box almost touches 0
  #pig to native line
  geom_segment(aes(x=14.8,y=0,xend=14.85,yend=0.069),
               linetype="dashed", color = "#993300", linewidth = 1.5) 

#percentage of values that fall within dist'n of native-native
# pig to native species MEDIAN: 14.86667
(sum(n_n_boot_sample$TTE<14.86667)) / (length(n_n_boot_sample$TTE))
#median lies above 92% of resampled n-n

# pig to pig mean: MEDIAN: 11
(sum(n_n_boot_sample$TTE<11)) / (length(n_n_boot_sample$TTE))
#median lies above 84% of resampled n-n



