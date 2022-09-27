###############################################################################
#Code for Carswell et al. wild pigs paper
#Script 1: Contains code for proportional hazard models + plotting coef's

#laoding 1st set of req. packages
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

#loading OG boar data from. 
BoarDATA <- read_csv("Data/Processed/BoarDATA.csv", 
                     col_types = cols(posixdate = col_datetime(format = "%m/%d/%Y %H:%M")))

########################################################################################################
#this section will identify the species of interest, and filter out the species recorded 
## that are unliekly to be ecologically affected (or at least managers wont care by many interactions)

#convert to data table to work with
BoarDATA2 <- as.data.table(BoarDATA)

#coverting boar --> invasive species and mammals of interest to native b/c we need to group to have enough 
#observations 
species_interest <- c("Native", "Invasive", "Human")

#creating a new column so I have an OG reference to bounce off of 
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

#sorting DT by species of interest 
BoarDATA2[, interest := Native %in% species_interest]

# and creating a new DF of only species of interest
BoarDATA3 <- filter(BoarDATA2, interest == TRUE)
BoarDATA3 <- as.data.frame(BoarDATA3)

########################################################################################################
## Much of this code is adapted from Louvrier et al. 2021
#I wasnt able to get the function (below) to run wihtout re-naming my variables to the same names they had.
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
## this is the function published in Louvrier et al. 2021 which calculates time 
## between desired sequential events for a given camera trap. 

calculate_delta <- function(species)
{
  #empty list 
  delta_species <- list(0)
  ###loop starts here
  
  for(j in id_CT)
  {
    k = which(id_CT == j)
    #take the CT by its id 
    B <- d1_interest %>% filter(Station == j)
    
    #order it by date and time
    B_ordered <- B %>% arrange(DateTimeOriginal)
    
    #delta is going to contain the calculated difference of time between species A and B for each row of the detections of the CT
    #but only if the two species from row i and i+1 are in the specific order
    #if the combination of the species names between the row i and i + 1 is equal to the names of the species of interest     stuck togetehr then we calcutalte difftime between the two lines,  otherwise, no
    
    delta <- rep(NA, nrow(B_ordered))
    for(i in 1: (nrow(B_ordered)))
    {
      if (paste(B_ordered[c(i:(i+1)),"Species"], collapse ="") == paste(species, collapse=""))
      { 
        diff <- difftime(B_ordered[i,"DateTimeOriginal"], B_ordered[i+1,"DateTimeOriginal"])
        if (diff < - 10000) #the only reason i left these in is b/c the f(x)n wouldn't run without it
        {                   #and i dont know enough to re-write. I just extended the time to a massive # to make it useless tho.
                            # you can adjust this time depending on your desired study
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
##selecting the combo of invasive boar --> any native species 
species = c(species_interest[2],species_interest[1])
delata_boar_any <- calculate_delta(species)
#putting this in a dataframe 
df_time_diff_boar_any = plyr::ldply(delata_boar_any, rbind)

##next interactions.... selecting the combo of any native --> any native sp of interest 
species = c(species_interest[1],species_interest[1])
delata_any_any <- calculate_delta(species) 
#putting this in a dataframe 
df_time_diff_any_any = plyr::ldply(delata_any_any, rbind)

##final interaction of interest... boar --> boar 
species = c(species_interest[2],species_interest[2])
delata_boar_boar <- calculate_delta(species) 
#putting this in a dataframe 
df_time_diff_boar_boar = plyr::ldply(delata_boar_boar, rbind)

##extracting events that were interuppted by humans (censor). First human --> native species
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
#The 'df_time_diff' dataframes are large and very jumbled 
# below i extract the TTE's in a weird way... but it works 

#first step is to pivot longer... then last two columns in the DF end up the TTE's + the camera associated with 
#boar --> native species combo
boar_any_extract <- tidyr::pivot_longer(df_time_diff_boar_any, c(2:50), values_drop_na = TRUE)
#and then extracting the two columns at the end..
boar_any_extracted <- cbind.data.frame(boar_any_extract$.id, boar_any_extract$value)
#create a new column labelling that this is the interaction
boar_any_extracted$Type <- rep(c("Boar_native"), times = 7)
#also need to fix the names. ARGH.There must be an easier way to do this but I'm not sure how. 
boar_any_extracted$ID <- boar_any_extracted$`boar_any_extract$.id`

#repeating for the next interaction (boar --> boar). same steps as above.
boar_boar_extract <- tidyr::pivot_longer(df_time_diff_boar_boar, c(2:50), values_drop_na = TRUE)
boar_boar_extracted <- cbind.data.frame(boar_boar_extract$.id, boar_boar_extract$value)
boar_boar_extracted$Type <- rep(c("Boar_boar"), times = 11)
boar_boar_extracted$ID <- boar_boar_extracted$`boar_boar_extract$.id`

#repeating for the next interaction (native --> native). same steps as above.
any_any_extract <- tidyr::pivot_longer(df_time_diff_any_any, c(2:50), values_drop_na = TRUE)
any_any_extracted <- cbind.data.frame(any_any_extract$.id, any_any_extract$value)
any_any_extracted$Type <- rep(c("Native_native"), times = 517)
any_any_extracted$ID <- any_any_extracted$`any_any_extract$.id`

#repeating for the next interaction (human --> native). same steps as above. CENSOR
human_native_extract <- tidyr::pivot_longer(df_time_diff_human_native, c(2:50), values_drop_na = TRUE)
human_native_extracted <- cbind.data.frame(human_native_extract$.id, human_native_extract$value)
human_native_extracted$Type <- rep(c("Human_native"), times = 27)
human_native_extracted$ID <- human_native_extracted$`human_native_extract$.id`

#repeating for the final interaction (human --> native). same steps as above. CENSOR
human_boar_extract <- tidyr::pivot_longer(df_time_diff_human_boar, c(2:50), values_drop_na = TRUE)
human_boar_extracted <- cbind.data.frame(human_boar_extract$.id, human_boar_extract$value)
human_boar_extracted$Type <- rep(c("Human_boar"), times = 1)
human_boar_extracted$ID <- human_boar_extracted$`human_boar_extract$.id`

#################################################################################
#now i need to merge these three TTE dataframes to I can analyze it
#but first, correcting the columns so all names line up
boar_boar_extracted$TTE <- boar_boar_extracted$`boar_boar_extract$value`
boar_any_extracted$TTE <- boar_any_extracted$`boar_any_extract$value`
any_any_extracted$TTE <- any_any_extracted$`any_any_extract$value`

human_boar_extracted$TTE <- human_boar_extracted$`human_boar_extract$value`
human_native_extracted$TTE <- human_native_extracted$`human_native_extract$value`

#removing the old columns with the bad names. Sorry, this is not efficient but.. worked for me
boar_boar_clean <- dplyr::select(boar_boar_extracted, -1,-2)
boar_any_clean <- dplyr::select(boar_any_extracted, -1,-2)
any_any_clean <- dplyr::select(any_any_extracted, -1,-2)

human_boar_clean <- dplyr::select(human_boar_extracted, -1,-2)
human_native_clean <- dplyr::select(human_native_extracted, -1,-2)

#################################################################################
#merging the DF's into one to analyze
cox_1.0 <- rbind(boar_any_clean, boar_boar_clean, any_any_clean, 
                 human_boar_clean, human_native_clean)

#save as a CSV
cox_1.0 <- write_csv(cox_1.0, "cox_1.0.csv")
##############################################################################
# okay now that I have my DF set up... I need to attach the habitat covars +  time etc. 

#re-loading the dataset where the co vars are attached for each camera location
BoarDATA <- read_csv("Data/Processed/BoarDATA.csv", 
                     col_types = cols(posixdate = col_datetime(format = "%m/%d/%Y %H:%M")))

#re-loading the processed TTE dataframe
cox_1.0 <- read_csv("Data/Processed/cox_1.0.csv")


#sub set the first data set so that i have what i need from big DF
sub <- BoarDATA[c("Camera", "posixdate", "Habitat_Ty" , "ElevationC", "DIST_WATER", "DIST_ROAD")]

#rename ID --> camera in the other DF
colnames(cox_1.0)[2] <- "Camera"

#creating a column of unique values in cox_1.0 to sort out later after merge. 
cox_1.0$Unique <- c(1:530)

#changing names in column to match up for merge. b/c of the weird names this is the best 
## way i know how to do it. in hindsight.. would have purely numeric names. 
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

#okay cool. now i can finally merge all cox_1.0 + sub set covars. b/c this dont work yet 
intermediate <- merge(cox_1.0, sub, by = "Camera", all.x = FALSE, all.y = FALSE, no.dups = TRUE)

#now can i filter out by unique values?
almostthere <-  intermediate %>% 
  group_by(Unique) %>%
  filter(row_number()==1)

#final step is to rename that weird cropland/? to normal cropland so it can be a category.
almostthere$Habitat_Ty[almostthere$Habitat_Ty == "Cropland/?"] <- "Cropland"
almostthere$Habitat_Ty[almostthere$Habitat_Ty == "Pasture"] <- "Apasture"

#export this as a df ready to analyze. 

cox_analyze <- write_csv(almostthere, "cox_analyze.csv"
######################################################################################################################
# Great, now we can actually run these hazard models
######################################################################################################################
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

#labeling the human interuppted interactions for censoring. 
cox_analyze$Type[cox_analyze$Type == "Human_native"] <- "Censor"
cox_analyze$Type[cox_analyze$Type == "Human_boar"] <- "Censor"

#looking at structure 
str(cox_analyze)

#all characters as factors
cox_analyze$Camera <- as.factor(cox_analyze$Camera)
cox_analyze$Type <- as.factor(cox_analyze$Type)
cox_analyze$Habitat_Ty <- as.factor(cox_analyze$Habitat_Ty)

#need TTE's as positive not negative numbers. Function calculates TTE as a negative 
cox_analyze$TTE <- abs(cox_analyze$TTE)

#Cox ph models take censor as 0's and data as 1's
cox_analyze$Censor <- ifelse(cox_analyze$Type == "Censor", 0, 1)
_c_analyze <- as.data.frame(moose_c_analyze)
#####################################################################################################
#model set for ranking based on the few imp. covars were interested in

#just a test model to make sure structure works. 
test <- coxph(Surv(TTE, Censor) ~ 1, data = cox_analyze)
summary(test)
AIC(test)

#base model with just cams as a random intercept. 
base <- coxme(Surv(TTE, Censor) ~ (1|Camera), data = cox_analyze)
summary(base)
AICbase <- AICc(base)

#next model with just interaction type 
model1 <- coxme(Surv(TTE, Censor) ~ Type + (1|Camera), data = cox_analyze)
summary(model1)
AIC1 <- AICc(model1)

#habitat type 
model2 <- coxme(Surv(TTE, Censor) ~ Habitat_Ty + (1|Camera), data = cox_analyze)
summary(model2)
AIC2 <- AICc(model2)

#habitat + interaction type 
model <- coxme(Surv(TTE, Censor) ~ Habitat_Ty + Type  + (1|Camera), data = cox_analyze)
summary(model)
AIC3 <- AICc(model)

##############3
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

#sub setting the predicted risk by the interaction type and then generating summary stats 
b_b <- filter(cox_analyze, Type == "Boar_boar")
mean(b_b$risk)  #0.8269305
median(b_b$risk)#0.845022
std.error(b_b$risk) #0.007568203

b_n <- filter(cox_analyze, Type == "Boar_native")
mean(b_n$risk) #0.595333
median(b_n$risk) #0.5894162
std.error(b_n$risk) #0.01822909

n_n <- filter(cox_analyze, Type == "Anative_native")
mean(n_n$risk) #1.004322
median(n_n$risk) #1.006868
std.error(n_n$risk) #0.004615157

#######################################################################
#making figures for the predicted risk values

#loading packages 
library(ggplot2)
library(ggpubr)

#create a new DF dropping the censor data so I can make a boxplot of predicted risk. 
boxplot_data <- subset(cox_analyze, Type!="Censor")

boxplot_data$Type <- as.character(boxplot_data$Type)

#just getting names consistent with MS
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

###############################################################################
#plotting the coefficient values from top supported hazard model.

#only new req'd package. 
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

#the plot of coef values for the appendix.
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




