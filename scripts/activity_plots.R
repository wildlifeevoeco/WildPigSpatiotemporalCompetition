### This script is for the 24-hour activity pattern graphs. 
#most of this code was adapted from Louvrier et al. 2021


##packages req'd for figures 
library(camtrapR)
library(car)
library(maptools)
library(circular)
library(plyr)
library(dplyr)
library(readr)
library(camtrapR)
library(evaluate)
library(gdata)
library(janitor)
library(lubridate)
library(maptools)
library(data.table)
library(ggplot2)

# Data import 
BoarDATA <- read_csv("Data/Processed/BoarDATA.csv", 
                     col_types = cols(posixdate = col_datetime(format = "%m/%d/%Y %H:%M")))

#species event renames
unique(BoarDATA$Animal)
BoarDATA$Animal[BoarDATA$Animal == "Boar"] <- "wild pig"
BoarDATA$Animal[BoarDATA$Animal == "White Tailed Deer"] <- "white tailed deer"
BoarDATA$Animal[BoarDATA$Animal == "Coyote"] <- "coyote"
BoarDATA$Animal[BoarDATA$Animal == "Moose"] <- "moose"
BoarDATA$Animal[BoarDATA$Animal == "Mule Deer"] <- "mule deer"
BoarDATA$Animal[BoarDATA$Animal == "Elk"] <- "elk"

#data structuring
class(BoarDATA$Camera) 
class(BoarDATA$posixdate) 
BoarDATA$posixdate <- as.character(BoarDATA$posixdate)

##############################################################################
# create activity overlap plots
#define species of interest
species_interest <- c("wild pig", "moose", "white tailed deer", "coyote", "elk", "mule deer", "native")
Native <- c("white tailed deer", "coyote", "elk", "mule deer")

#colour palate
color_list <- c("#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#29AF7FFF", "#56B4E9")

###############################################################################
#fix habitat name
unique(BoarDATA$Habitat_Ty)
BoarDATA$Habitat_Ty[BoarDATA$Habitat_Ty == "Cropland/?"] <- "Cropland"

habs <- unique(BoarDATA$Habitat_Ty)

#first make a new column of "native" or wild pig
BoarDATA$native <- BoarDATA$Animal
BoarDATA$native[BoarDATA$native == "Boar"] <- "wild pig"
BoarDATA$Animal[BoarDATA$native == "White Tailed Deer"] <- "white tailed deer"
BoarDATA$Animal[BoarDATA$native == "Coyote"] <- "coyote"
BoarDATA$Animal[BoarDATA$native == "Moose"] <- "moose"
BoarDATA$Animal[BoarDATA$native == "Mule Deer"] <- "mule deer"
BoarDATA$Animal[BoarDATA$native == "Elk"] <- "elk"

BoarDATA$native <-  mapvalues(BoarDATA$native, from = 
                                c("white tailed deer", "coyote", "elk",
                                  "mule deer", "moose"), to = c("native",
                                                                "native", "native", "native", "native"))

# 2D comparison of temporal overlap between wild pigs + all native species 
speciesA_for_activity <- "wild pig"    
speciesB_for_activity <- "native"    
dataf <- BoarDATA

d_overlap_boarnative <- activityOverlap(recordTable = dataf,
                                        speciesA    = speciesA_for_activity,
                                        speciesB    = speciesB_for_activity,
                                        speciesCol = "native",
                                        recordDateTimeCol = "posixdate",
                                        plotR       = TRUE,
                                        add.rug     = FALSE
                                        #writePNG    = TRUE,
                                        #addLegend   = TRUE,
                                        #plotDirectory = getwd()
)

dhat <- "0.72"  #coef of activtiy overlap. See Ridout and Linkie 2009.

plot_overlap <- function(speciesA_for_activity, speciesB_for_activity, d_overlap)
{
  col1 <- color_list[which(species_interest == speciesA_for_activity)]
  col2 <- color_list[which(species_interest == speciesB_for_activity)]
  
  speciesA <- species_interest[which(species_interest == speciesA_for_activity)]
  speciesB <- species_interest[which(species_interest == speciesB_for_activity)]
  
  #cat and cat Dhat1 = 0.89
  A <- cbind(d_overlap[,c(1:2)], Animal = speciesA_for_activity)
  B <- cbind (d_overlap[,c(1,3)], Animal = speciesB_for_activity)
  
  colnames(A)[2] <- "value"
  colnames(B)[2] <- "value"
  
  # A dataframe for annotations
  annot <- data.frame(
    text = c(speciesA, speciesB),
    x = c(4, 4),
    y = c(0.18, 0.16))
  
  line_A <- data.frame(x = c(0,1,3.5),
                       y = c(0.18,0.18,0.18))
  
  line_B <- data.frame(x = c(0,1,3.5),
                       y = c(0.16,0.16,0.16))
  
  plot <- ggplot() +
    # first species
    geom_line(data=A, aes(x=x, y=value, linetype = "longdash"), color = col1) + 
    # scale_color_manual(values = col1) +
    # labs(color = 'Y series') +
    geom_line(data=line_A, aes(x = x, y = y, linetype = "longdash"), col = col1,
              alpha = 0.8, size = 10) +
    geom_area(data=A, aes(x=x, y=value, linetype = "longdash"), fill = col1, alpha = 0.4) +
    
    # second
    geom_line(data=B, aes(x=x, y=value, linetype = "solid"), color = col2) +
    geom_line(data=line_B, aes(x = x, y = y, linetype = "solid"), col = col2,
              alpha = 0.8, size = 10) +
    geom_area(data=B, aes(x=x, y=value, linetype = "solid"), fill = col2, alpha = 0.4) +
    
    theme_classic() +
    xlab("Time of day (24-hour time)") +
    ylab("Density of observations")+
    theme(axis.title=element_text(size=16, colour = "black")) +
    theme(axis.text=element_text(size=16, colour = "black")) +
    geom_text(data=annot, aes(x=x, y=y, label=text), colour="black", hjust=0, size=7) +
    scale_x_continuous(breaks = seq(0,24, by= 4)) +
    theme(legend.position = "none") +
    return(plot)
}

boarnative <- plot_overlap(speciesA_for_activity,speciesB_for_activity, d_overlap_boarnative)
boarnative
##############################################################################################################################################################
#Circular diel plots of wild pig to each native species 
#wild pig / moose

# create activity overlap plot
# define species of interest
species_interest2 <- c("wild pig", "moose")

speciesA_for_activity <- "wild pig"   
speciesB_for_activity <- "moose"    

speciesA <- species_interest2[which(species_interest2 == speciesA_for_activity)]
speciesB <- species_interest2[which(species_interest2 == speciesB_for_activity)]

dataf <- BoarDATA

d_overlap_AB <- activityOverlap(recordTable = BoarDATA,
                                speciesA    = speciesA_for_activity,
                                speciesB    = speciesB_for_activity,
                                speciesCol = "Animal",
                                recordDateTimeCol = "posixdate",
                                plotR       = TRUE,
                                add.rug     = FALSE
                                #writePNG    = TRUE,
                                #addLegend   = TRUE,
                                #plotDirectory = getwd()
)

#
A <- cbind(d_overlap_AB[,c(1:2)], species = speciesA)
B <- cbind (d_overlap_AB[,c(1,3)], species = speciesB)

colnames(A)[2] <- "value"
colnames(B)[2] <- "value"

ABCDEF <- rbind(A,B)

B_M <-  ggplot(data = ABCDEF, aes(x = x, y = value, colour = species)) +
  geom_line() +
  geom_area(aes(fill = species, group = species),
            alpha = 0.4, position = 'identity', outline.type = "full") +
  
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 2)) +
  geom_vline(xintercept = 6, linetype="dashed", color = "grey") +
  geom_vline(xintercept = 18, linetype="dashed", color = "grey") +
  #scale_color_viridis_d() +
  coord_polar() +
  scale_color_manual(values = c("#56B4E9", "#E69F00")) +
  scale_fill_manual(values = c("#56B4E9","#E69F00")) +
  
  theme_minimal(base_size = 14) +
  labs(title="") +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom") +
  theme(legend.title= element_blank()) 

plot(B_M)
##############################################################################################################################################################
#wild pig / white tail

# create activity overlap plot
# define species of interest
species_interest3 <- c("wild pig", "white tailed deer")

speciesA_for_activity <- "wild pig"   
speciesB_for_activity <- "white tailed deer"    

speciesA <- species_interest3[which(species_interest3 == speciesA_for_activity)]
speciesB <- species_interest3[which(species_interest3 == speciesB_for_activity)]

dataf <- BoarDATA

d_overlap_AB <- activityOverlap(recordTable = BoarDATA,
                                speciesA    = speciesA_for_activity,
                                speciesB    = speciesB_for_activity,
                                speciesCol = "Animal",
                                recordDateTimeCol = "posixdate",
                                plotR       = TRUE,
                                add.rug     = FALSE
                                #writePNG    = TRUE,
                                #addLegend   = TRUE,
                                #plotDirectory = getwd()
)

#
A <- cbind(d_overlap_AB[,c(1:2)], species = speciesA)
B <- cbind (d_overlap_AB[,c(1,3)], species = speciesB)

colnames(A)[2] <- "value"
colnames(B)[2] <- "value"

ABCDEF <- rbind(A,B)

B_WTD <-  ggplot(data = ABCDEF, aes(x = x, y = value, colour = species)) +
  geom_line() +
  geom_area(aes(fill = species, group = species),
            alpha = 0.4, position = 'identity', outline.type = "full") +
  
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 2)) +
  geom_vline(xintercept = 6, linetype="dashed", color = "grey") +
  geom_vline(xintercept = 18, linetype="dashed", color = "grey") +
  #scale_color_viridis_d() +
  coord_polar() +
  scale_color_manual(values = c("#999900", "#E69F00")) +
  scale_fill_manual(values = c("#999900", "#E69F00")) +
  
  theme_minimal(base_size = 14) +
  xlab("") +
  ylab("") +
  labs(title="") +
  theme(legend.position = "bottom") +
  theme(legend.title= element_blank())

plot(B_WTD)
##############################################################################################################################################################
#wild pig / Mule Deer

# create activity overlap plot
# define species of interest
species_interest4 <- c("wild pig", "mule deer")

speciesA_for_activity <- "wild pig"   
speciesB_for_activity <- "mule deer"    

speciesA <- species_interest4[which(species_interest4 == speciesA_for_activity)]
speciesB <- species_interest4[which(species_interest4 == speciesB_for_activity)]

dataf <- BoarDATA

d_overlap_AB <- activityOverlap(recordTable = BoarDATA,
                                speciesA    = speciesA_for_activity,
                                speciesB    = speciesB_for_activity,
                                speciesCol = "Animal",
                                recordDateTimeCol = "posixdate",
                                plotR       = TRUE,
                                add.rug     = FALSE
                                #writePNG    = TRUE,
                                #addLegend   = TRUE,
                                #plotDirectory = getwd()
)

#
A <- cbind(d_overlap_AB[,c(1:2)], species = speciesA)
B <- cbind (d_overlap_AB[,c(1,3)], species = speciesB)

colnames(A)[2] <- "value"
colnames(B)[2] <- "value"

ABCDEF <- rbind(A,B)

B_MD <-  ggplot(data = ABCDEF, aes(x = x, y = value, colour = species)) +
  geom_line() +
  geom_area(aes(fill = species, group = species),
            alpha = 0.4, position = 'identity', outline.type = "full") +
  
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 2)) +
  geom_vline(xintercept = 6, linetype="dashed", color = "grey") +
  geom_vline(xintercept = 18, linetype="dashed", color = "grey") +
  #scale_color_viridis_d() +
  coord_polar() +
  scale_color_manual(values = c("#BBDF27FF","#E69F00")) +
  scale_fill_manual(values = c("#BBDF27FF","#E69F00")) +
  
  theme_minimal(base_size = 14) +
  xlab("") +
  ylab("") +
  labs(title="") +
  theme(legend.position = "bottom") +
  theme(legend.title= element_blank()) 

plot(B_MD)
##############################################################################################################################################################
#wild pig / Elk

# create activity overlap plot
# define species of interest
species_interest5 <- c("wild pig", "elk")

speciesA_for_activity <- "wild pig"   
speciesB_for_activity <- "elk"    

speciesA <- species_interest5[which(species_interest5 == speciesA_for_activity)]
speciesB <- species_interest5[which(species_interest5 == speciesB_for_activity)]

dataf <- BoarDATA

d_overlap_AB <- activityOverlap(recordTable = BoarDATA,
                                speciesA    = speciesA_for_activity,
                                speciesB    = speciesB_for_activity,
                                speciesCol = "Animal",
                                recordDateTimeCol = "posixdate",
                                plotR       = TRUE,
                                add.rug     = FALSE
                                #writePNG    = TRUE,
                                #addLegend   = TRUE,
                                #plotDirectory = getwd()
)

#
A <- cbind(d_overlap_AB[,c(1:2)], species = speciesA)
B <- cbind (d_overlap_AB[,c(1,3)], species = speciesB)

colnames(A)[2] <- "value"
colnames(B)[2] <- "value"

ABCDEF <- rbind(A,B)

B_E <-  ggplot(data = ABCDEF, aes(x = x, y = value, colour = species)) +
  geom_line() +
  geom_area(aes(fill = species, group = species),
            alpha = 0.4, position = 'identity', outline.type = "full") +
  
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 2)) +
  geom_vline(xintercept = 6, linetype="dashed", color = "grey") +
  geom_vline(xintercept = 18, linetype="dashed", color = "grey") +
  #scale_color_viridis_d() +
  coord_polar() +
  scale_color_manual(values = c("#3B528BFF","#E69F00")) +
  scale_fill_manual(values = c("#3B528BFF","#E69F00")) +
  
  theme_minimal(base_size = 14) +
  xlab("") +
  ylab("") +
  labs(title="") +
  theme(legend.position = "bottom") +
  theme(legend.title= element_blank()) 

plot(B_E)
##############################################################################################################################################################
#wild pig / Coyote

# create activity overlap plot
# define species of interest
species_interest6 <- c("wild pig", "coyote")

speciesA_for_activity <- "wild pig"   
speciesB_for_activity <- "coyote"    

speciesA <- species_interest6[which(species_interest6 == speciesA_for_activity)]
speciesB <- species_interest6[which(species_interest6 == speciesB_for_activity)]

dataf <- BoarDATA

d_overlap_AB <- activityOverlap(recordTable = BoarDATA,
                                speciesA    = speciesA_for_activity,
                                speciesB    = speciesB_for_activity,
                                speciesCol = "Animal",
                                recordDateTimeCol = "posixdate",
                                plotR       = TRUE,
                                add.rug     = FALSE
                                #writePNG    = TRUE,
                                #addLegend   = TRUE,
                                #plotDirectory = getwd()
)

#
A <- cbind(d_overlap_AB[,c(1:2)], species = speciesA)
B <- cbind (d_overlap_AB[,c(1,3)], species = speciesB)

colnames(A)[2] <- "value"
colnames(B)[2] <- "value"

ABCDEF <- rbind(A,B)

B_C <-  ggplot(data = ABCDEF, aes(x = x, y = value, colour = species)) +
  geom_line() +
  geom_area(aes(fill = species, group = species),
            alpha = 0.4, position = 'identity', outline.type = "full") +
  
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 2)) +
  geom_vline(xintercept = 6, linetype="dashed", color = "grey") +
  geom_vline(xintercept = 18, linetype="dashed", color = "grey") +
  #scale_color_viridis_d() +
  coord_polar() +
  scale_color_manual(values = c("#440154FF", "#E69F00")) +
  scale_fill_manual(values = c("#440154FF", "#E69F00")) +
  
  theme_minimal(base_size = 14) +
  xlab("") +
  ylab("") +
  labs(title="") +
  theme(legend.position = "bottom") +
  theme(legend.title= element_blank()) 

plot(B_C)

##############################################################################################################################################################
#now, taking the 5 circular plots from above and making a multipanel figure
library(cowplot)
library(grid)
library(ggplotify)
library(gridExtra)
library(ggpubr)
#my five circular objects. 
B_M
B_WTD
B_MD
B_E
B_C

#combing them into one multi figure pannel

lay <- rbind(c(1,1,2,2,3,3),
             c(NA,4,4,5,5,NA))

figure <- grid.arrange(B_M, B_WTD, B_MD, B_E, B_C, layout_matrix = lay)
figure

annotate_figure(figure, left = text_grob("Density of observations", color = "black", size = 16, rot = 90), 
                bottom = text_grob("Time of day (24-hour time)", color = "black", size = 16),
                right = text_grob("maybe", color = "white"))





