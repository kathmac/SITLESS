#SITless ERPs
#Rversion 1.4.1717 
#Created by Katherine McDonald 8.17.2022
library(readxl)
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(haven)
library(data.table)
library(janitor)
library(funModeling)
library(scales)
library(reshape)

#Import Data for 1B
PA_CON_PRE_1B_Targets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/PRE/1B/Combined_Targets/Export_Excel/GrandAVG/SITlessPACONPRE1BTargets.xlsm")
PA_CON_PRE_1B_NonTargets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/PRE/1B/Combined_Targets/Export_Excel/GrandAVG/SITlessPACONPRE1BNonTargets.xlsm")
PA_CON_PRE_1B_Lures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/PRE/1B/Combined_Lures/Export_Excel/GrandAVG/SITlessPACONPRE1BLures.xlsm")
PA_CON_PRE_1B_NonLures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/PRE/1B/Combined_Lures/Export_Excel/GrandAVG/SITlessPACONPRE1BNonLures.xlsm")

PA_CON_POST_1B_Targets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/POST/1B/Combined_Targets/Export_Excel/GrandAVG/SITlessPACONPOST1BTargets.xlsm")
PA_CON_POST_1B_NonTargets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/POST/1B/Combined_Targets/Export_Excel/GrandAVG/SITlessPACONPOST1BNonTargets.xlsm")
PA_CON_POST_1B_Lures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/POST/1B/Combined_Lures/Export_Excel/GrandAVG/SITlessPACONPOST1BLures.xlsm")
PA_CON_POST_1B_NonLures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/POST/1B/Combined_Lures/Export_Excel/GrandAVG/SITlessPACONPOST1BNonLures.xlsm")

SB_CON_PRE_1B_Targets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/PRE/1B/Combined_Targets/Export_Excel/GrandAVG/SITlessSBCONPRE1BTargets.xlsm")
SB_CON_PRE_1B_NonTargets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/PRE/1B/Combined_Targets/Export_Excel/GrandAVG/SITlessSBCONPRE1BNonTargets.xlsm")
SB_CON_PRE_1B_Lures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/PRE/1B/Combined_Lures/Export_Excel/GrandAVG/SITlessSBCONPRE1BLures.xlsm")
SB_CON_PRE_1B_NonLures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/PRE/1B/Combined_Lures/Export_Excel/GrandAVG/SITlessSBCONPRE1BNonLures.xlsm")

SB_CON_POST_1B_Targets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/POST/1B/Combined_Targets/Export_Excel/GrandAVG/SITlessSBCONPOST1BTargets.xlsm")
SB_CON_POST_1B_NonTargets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/POST/1B/Combined_Targets/Export_Excel/GrandAVG/SITlessSBCONPOST1BNonTargets.xlsm")
SB_CON_POST_1B_Lures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/POST/1B/Combined_Lures/Export_Excel/GrandAVG/SITlessSBCONPOST1BLures.xlsm")
SB_CON_POST_1B_NonLures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/POST/1B/Combined_Lures/Export_Excel/GrandAVG/SITlessSBCONPOST1BNonLures.xlsm")

#Call Variables for 1B
#time 
#Call Variables
time <- (PA_CON_PRE_1B_Targets$time)
time <-as.numeric(time) 

#PA_CON_PRE_1B_Targets 
CZ_PA_CON_PRE_1B_Targets <- (PA_CON_PRE_1B_Targets$CZ)
CPZ_PA_CON_PRE_1B_Targets <- (PA_CON_PRE_1B_Targets$CPZ)
PZ_PA_CON_PRE_1B_Targets <- (PA_CON_PRE_1B_Targets$PZ)

#PA_CON_PRE_1B_NonTargets
CZ_PA_CON_PRE_1B_NonTargets <- (PA_CON_PRE_1B_NonTargets$CZ)
CPZ_PA_CON_PRE_1B_NonTargets <- (PA_CON_PRE_1B_NonTargets$CPZ)
PZ_PA_CON_PRE_1B_NonTargets <- (PA_CON_PRE_1B_NonTargets$PZ)

#PA_CON_PRE_1B_Lures
CZ_PA_CON_PRE_1B_Lures <- (PA_CON_PRE_1B_Lures$CZ)
CPZ_PA_CON_PRE_1B_Lures <- (PA_CON_PRE_1B_Lures$CPZ)
PZ_PA_CON_PRE_1B_Lures <- (PA_CON_PRE_1B_Lures$PZ)

#PA_CON_PRE_1B_NonLures
CZ_PA_CON_PRE_1B_NonLures <- (PA_CON_PRE_1B_NonLures$CZ)
CPZ_PA_CON_PRE_1B_NonLures <- (PA_CON_PRE_1B_NonLures$CPZ)
PZ_PA_CON_PRE_1B_NonLures <- (PA_CON_PRE_1B_NonLures$PZ)

#PA_CON_POST_1B_Targets 
CZ_PA_CON_POST_1B_Targets <- (PA_CON_POST_1B_Targets$CZ)
CPZ_PA_CON_POST_1B_Targets <- (PA_CON_POST_1B_Targets$CPZ)
PZ_PA_CON_POST_1B_Targets <- (PA_CON_POST_1B_Targets$PZ)

#PA_CON_POST_1B_NonTargets
CZ_PA_CON_POST_1B_NonTargets <- (PA_CON_POST_1B_NonTargets$CZ)
CPZ_PA_CON_POST_1B_NonTargets <- (PA_CON_POST_1B_NonTargets$CPZ)
PZ_PA_CON_POST_1B_NonTargets <- (PA_CON_POST_1B_NonTargets$PZ)

#PA_CON_POST_1B_Lures
CZ_PA_CON_POST_1B_Lures <- (PA_CON_POST_1B_Lures$CZ)
CPZ_PA_CON_POST_1B_Lures <- (PA_CON_POST_1B_Lures$CPZ)
PZ_PA_CON_POST_1B_Lures <- (PA_CON_POST_1B_Lures$PZ)

#PA_CON_POST_1B_NonLures
CZ_PA_CON_POST_1B_NonLures <- (PA_CON_POST_1B_NonLures$CZ)
CPZ_PA_CON_POST_1B_NonLures <- (PA_CON_POST_1B_NonLures$CPZ)
PZ_PA_CON_POST_1B_NonLures <- (PA_CON_POST_1B_NonLures$PZ)

#SB_CON_PRE_1B_Targets 
CZ_SB_CON_PRE_1B_Targets <- (SB_CON_PRE_1B_Targets$CZ)
CPZ_SB_CON_PRE_1B_Targets <- (SB_CON_PRE_1B_Targets$CPZ)
PZ_SB_CON_PRE_1B_Targets <- (SB_CON_PRE_1B_Targets$PZ)

#SB_CON_PRE_1B_NonTargets
CZ_SB_CON_PRE_1B_NonTargets <- (SB_CON_PRE_1B_NonTargets$CZ)
CPZ_SB_CON_PRE_1B_NonTargets <- (SB_CON_PRE_1B_NonTargets$CPZ)
PZ_SB_CON_PRE_1B_NonTargets <- (SB_CON_PRE_1B_NonTargets$PZ)

#SB_CON_PRE_1B_Lures
CZ_SB_CON_PRE_1B_Lures <- (SB_CON_PRE_1B_Lures$CZ)
CPZ_SB_CON_PRE_1B_Lures <- (SB_CON_PRE_1B_Lures$CPZ)
PZ_SB_CON_PRE_1B_Lures <- (SB_CON_PRE_1B_Lures$PZ)

#SB_CON_PRE_1B_NonLures
CZ_SB_CON_PRE_1B_NonLures <- (SB_CON_PRE_1B_NonLures$CZ)
CPZ_SB_CON_PRE_1B_NonLures <- (SB_CON_PRE_1B_NonLures$CPZ)
PZ_SB_CON_PRE_1B_NonLures <- (SB_CON_PRE_1B_NonLures$PZ)

#SB_CON_POST_1B_Targets 
CZ_SB_CON_POST_1B_Targets <- (SB_CON_POST_1B_Targets$CZ)
CPZ_SB_CON_POST_1B_Targets <- (SB_CON_POST_1B_Targets$CPZ)
PZ_SB_CON_POST_1B_Targets <- (SB_CON_POST_1B_Targets$PZ)

#SB_CON_POST_1B_NonTargets
CZ_SB_CON_POST_1B_NonTargets <- (SB_CON_POST_1B_NonTargets$CZ)
CPZ_SB_CON_POST_1B_NonTargets <- (SB_CON_POST_1B_NonTargets$CPZ)
PZ_SB_CON_POST_1B_NonTargets <- (SB_CON_POST_1B_NonTargets$PZ)

#SB_CON_POST_1B_Lures
CZ_SB_CON_POST_1B_Lures <- (SB_CON_POST_1B_Lures$CZ)
CPZ_SB_CON_POST_1B_Lures <- (SB_CON_POST_1B_Lures$CPZ)
PZ_SB_CON_POST_1B_Lures <- (SB_CON_POST_1B_Lures$PZ)

#SB_CON_POST_1B_NonLures
CZ_SB_CON_POST_1B_NonLures <- (SB_CON_POST_1B_NonLures$CZ)
CPZ_SB_CON_POST_1B_NonLures <- (SB_CON_POST_1B_NonLures$CPZ)
PZ_SB_CON_POST_1B_NonLures <- (SB_CON_POST_1B_NonLures$PZ)



#Pre Non-Target and Target waveforms for SB and PA on the same graph - CZ 
PreTargetNonTargetPASB_CZ.df <-data.frame(time, CZ_PA_CON_PRE_1B_Targets, CZ_PA_CON_PRE_1B_NonTargets, CZ_SB_CON_PRE_1B_Targets, CZ_SB_CON_PRE_1B_NonTargets)
longformat.PreTargetNonTargetPASB_CZ.df <- melt(PreTargetNonTargetPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetNonTargetPASB_CZ.df$value<-as.numeric(as.character(longformat.PreTargetNonTargetPASB_CZ.df$value))

ggplot(longformat.PreTargetNonTargetPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Pre Target and NonTarget Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Non-Target and Target waveforms for SB and PA on the same graph - CPZ
PreTargetNonTargetPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_PRE_1B_Targets, CPZ_PA_CON_PRE_1B_NonTargets, CPZ_SB_CON_PRE_1B_Targets, CPZ_SB_CON_PRE_1B_NonTargets)
longformat.PreTargetNonTargetPASB_CPZ.df <- melt(PreTargetNonTargetPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetNonTargetPASB_CPZ.df$value<-as.numeric(as.character(longformat.PreTargetNonTargetPASB_CPZ.df$value))

ggplot(longformat.PreTargetNonTargetPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Pre Target and NonTarget Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Non-Target and Target waveforms for SB and PA on the same graph - PZ 
PreTargetNonTargetPASB_PZ.df <-data.frame(time, PZ_PA_CON_PRE_1B_Targets, PZ_PA_CON_PRE_1B_NonTargets, PZ_SB_CON_PRE_1B_Targets, PZ_SB_CON_PRE_1B_NonTargets)
longformat.PreTargetNonTargetPASB_PZ.df <- melt(PreTargetNonTargetPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetNonTargetPASB_PZ.df$value<-as.numeric(as.character(longformat.PreTargetNonTargetPASB_PZ.df$value))

ggplot(longformat.PreTargetNonTargetPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Pre Target and NonTarget Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Non-Target and Target waveforms for SB and PA on the same graph - CZ, CPZ, PZ 
PreTargetNonTargetPASB.df <-data.frame(time, CZ_PA_CON_PRE_1B_Targets, CPZ_PA_CON_PRE_1B_Targets, PZ_PA_CON_PRE_1B_Targets,  
                                             CZ_PA_CON_PRE_1B_NonTargets, CPZ_PA_CON_PRE_1B_NonTargets, PZ_PA_CON_PRE_1B_NonTargets, 
                                             CZ_SB_CON_PRE_1B_Targets, CPZ_SB_CON_PRE_1B_Targets,PZ_SB_CON_PRE_1B_Targets, 
                                             CZ_SB_CON_PRE_1B_NonTargets, CPZ_SB_CON_PRE_1B_NonTargets, PZ_SB_CON_PRE_1B_NonTargets)
longformat.PreTargetNonTargetPASB.df <- melt(PreTargetNonTargetPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetNonTargetPASB.df$value<-as.numeric(as.character(longformat.PreTargetNonTargetPASB.df$value))

ggplot(longformat.PreTargetNonTargetPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Pre Target and NonTarget Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Pre Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - CZ
PreNonTargetPASB_CZ.df <-data.frame(time, CZ_PA_CON_PRE_1B_NonTargets, CZ_PA_CON_PRE_1B_Lures, CZ_PA_CON_PRE_1B_NonLures, 
                                    CZ_SB_CON_PRE_1B_NonTargets, CZ_SB_CON_PRE_1B_Lures, CZ_SB_CON_PRE_1B_NonLures)
longformat.PreNonTargetPASB_CZ.df <- melt(PreNonTargetPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreNonTargetPASB_CZ.df$value<-as.numeric(as.character(longformat.PreNonTargetPASB_CZ.df$value))

ggplot(longformat.PreNonTargetPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Pre NonTarget Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - CPZ
PreNonTargetPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_PRE_1B_NonTargets, CPZ_PA_CON_PRE_1B_Lures, CPZ_PA_CON_PRE_1B_NonLures, 
                                     CPZ_SB_CON_PRE_1B_NonTargets, CPZ_SB_CON_PRE_1B_Lures, CPZ_SB_CON_PRE_1B_NonLures)
longformat.PreNonTargetPASB_CPZ.df <- melt(PreNonTargetPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreNonTargetPASB_CPZ.df$value<-as.numeric(as.character(longformat.PreNonTargetPASB_CPZ.df$value))

ggplot(longformat.PreNonTargetPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Pre NonTarget Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - PZ
PreNonTargetPASB_PZ.df <-data.frame(time, PZ_PA_CON_PRE_1B_NonTargets, PZ_PA_CON_PRE_1B_Lures, PZ_PA_CON_PRE_1B_NonLures, 
                                    PZ_SB_CON_PRE_1B_NonTargets, PZ_SB_CON_PRE_1B_Lures, PZ_SB_CON_PRE_1B_NonLures)
longformat.PreNonTargetPASB_PZ.df <- melt(PreNonTargetPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreNonTargetPASB_PZ.df$value<-as.numeric(as.character(longformat.PreNonTargetPASB_PZ.df$value))

ggplot(longformat.PreNonTargetPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Pre NonTarget Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - CZ, CPZ, PZ 
PreNonTargetPASB.df <-data.frame(time, CZ_PA_CON_PRE_1B_NonTargets, CPZ_PA_CON_PRE_1B_NonTargets, PZ_PA_CON_PRE_1B_NonTargets, 
                                       CZ_SB_CON_PRE_1B_NonTargets, CPZ_SB_CON_PRE_1B_NonTargets, PZ_SB_CON_PRE_1B_NonTargets, 
                                       CZ_PA_CON_PRE_1B_Lures, CPZ_PA_CON_PRE_1B_Lures, PZ_PA_CON_PRE_1B_Lures, 
                                       CZ_SB_CON_PRE_1B_Lures, CPZ_SB_CON_PRE_1B_Lures, PZ_SB_CON_PRE_1B_Lures,
                                       CZ_PA_CON_PRE_1B_NonLures, CPZ_PA_CON_PRE_1B_NonLures, PZ_PA_CON_PRE_1B_NonLures, 
                                       CZ_SB_CON_PRE_1B_NonLures, CPZ_SB_CON_PRE_1B_NonLures, PZ_SB_CON_PRE_1B_NonLures)
longformat.PreNonTargetPASB.df <- melt(PreNonTargetPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreNonTargetPASB.df$value<-as.numeric(as.character(longformat.PreNonTargetPASB.df$value))

ggplot(longformat.PreNonTargetPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Pre NonTarget Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Pre Target waveforms for SB and PA - CZ 
PreTargetPASB_CZ.df <-data.frame(time, CZ_PA_CON_PRE_1B_Targets, CZ_SB_CON_PRE_1B_Targets)
longformat.PreTargetPASB_CZ.df <- melt(PreTargetPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetPASB_CZ.df$value<-as.numeric(as.character(longformat.PreTargetPASB_CZ.df$value))

ggplot(longformat.PreTargetPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Pre Target Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Target waveforms for SB and PA - CPZ
PreTargetPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_PRE_1B_Targets, CPZ_SB_CON_PRE_1B_Targets)
longformat.PreTargetPASB_CPZ.df <- melt(PreTargetPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetPASB_CPZ.df$value<-as.numeric(as.character(longformat.PreTargetPASB_CPZ.df$value))

ggplot(longformat.PreTargetPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Pre Target Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Target waveforms for SB and PA - PZ 
PreTargetPASB_PZ.df <-data.frame(time, PZ_PA_CON_PRE_1B_Targets, PZ_SB_CON_PRE_1B_Targets)
longformat.PreTargetPASB_PZ.df <- melt(PreTargetPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetPASB_PZ.df$value<-as.numeric(as.character(longformat.PreTargetPASB_PZ.df$value))

ggplot(longformat.PreTargetPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Pre Target Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Target waveforms for SB and PA - CZ, CPZ, PZ
PreTargetPASB.df <-data.frame(time, CZ_PA_CON_PRE_1B_Targets, CPZ_PA_CON_PRE_1B_Targets, PZ_PA_CON_PRE_1B_Targets,  
                                       CZ_SB_CON_PRE_1B_Targets, CPZ_SB_CON_PRE_1B_Targets,PZ_SB_CON_PRE_1B_Targets)
longformat.PreTargetPASB.df <- melt(PreTargetPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetPASB.df$value<-as.numeric(as.character(longformat.PreTargetPASB.df$value))

ggplot(longformat.PreTargetPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Pre Target Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target and Target waveforms for SB and PA on the same graph - CZ 
PostTargetNonTargetPASB_CZ.df <-data.frame(time, CZ_PA_CON_POST_1B_Targets, CZ_PA_CON_POST_1B_NonTargets, CZ_SB_CON_POST_1B_Targets, CZ_SB_CON_POST_1B_NonTargets)
longformat.PostTargetNonTargetPASB_CZ.df <- melt(PostTargetNonTargetPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetNonTargetPASB_CZ.df$value<-as.numeric(as.character(longformat.PostTargetNonTargetPASB_CZ.df$value))

ggplot(longformat.PostTargetNonTargetPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Post Target and NonTarget Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target and Target waveforms for SB and PA on the same graph - CPZ
PostTargetNonTargetPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_POST_1B_Targets, CPZ_PA_CON_POST_1B_NonTargets, CPZ_SB_CON_POST_1B_Targets, CPZ_SB_CON_POST_1B_NonTargets)
longformat.PostTargetNonTargetPASB_CPZ.df <- melt(PostTargetNonTargetPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetNonTargetPASB_CPZ.df$value<-as.numeric(as.character(longformat.PostTargetNonTargetPASB_CPZ.df$value))

ggplot(longformat.PostTargetNonTargetPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Post Target and NonTarget Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target and Target waveforms for SB and PA on the same graph - PZ 
PostTargetNonTargetPASB_PZ.df <-data.frame(time, PZ_PA_CON_POST_1B_Targets, PZ_PA_CON_POST_1B_NonTargets, PZ_SB_CON_POST_1B_Targets, PZ_SB_CON_POST_1B_NonTargets)
longformat.PostTargetNonTargetPASB_PZ.df <- melt(PostTargetNonTargetPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetNonTargetPASB_PZ.df$value<-as.numeric(as.character(longformat.PostTargetNonTargetPASB_PZ.df$value))

ggplot(longformat.PostTargetNonTargetPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Post Target and NonTarget Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target and Target waveforms for SB and PA on the same graph - CZ, CPZ, PZ 
PostTargetNonTargetPASB.df <-data.frame(time, CZ_PA_CON_POST_1B_Targets, CPZ_PA_CON_POST_1B_Targets, PZ_PA_CON_POST_1B_Targets,  
                                        CZ_PA_CON_POST_1B_NonTargets, CPZ_PA_CON_POST_1B_NonTargets, PZ_PA_CON_POST_1B_NonTargets, 
                                        CZ_SB_CON_POST_1B_Targets, CPZ_SB_CON_POST_1B_Targets,PZ_SB_CON_POST_1B_Targets, 
                                        CZ_SB_CON_POST_1B_NonTargets, CPZ_SB_CON_POST_1B_NonTargets, PZ_SB_CON_POST_1B_NonTargets)
longformat.PostTargetNonTargetPASB.df <- melt(PostTargetNonTargetPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetNonTargetPASB.df$value<-as.numeric(as.character(longformat.PostTargetNonTargetPASB.df$value))

ggplot(longformat.PostTargetNonTargetPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Post Target and NonTarget Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Post Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - CZ
PostNonTargetPASB_CZ.df <-data.frame(time, CZ_PA_CON_POST_1B_NonTargets, CZ_PA_CON_POST_1B_Lures, CZ_PA_CON_POST_1B_NonLures, 
                                     CZ_SB_CON_POST_1B_NonTargets, CZ_SB_CON_POST_1B_Lures, CZ_SB_CON_POST_1B_NonLures)
longformat.PostNonTargetPASB_CZ.df <- melt(PostNonTargetPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostNonTargetPASB_CZ.df$value<-as.numeric(as.character(longformat.PostNonTargetPASB_CZ.df$value))

ggplot(longformat.PostNonTargetPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Post NonTarget Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - CPZ
PostNonTargetPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_POST_1B_NonTargets, CPZ_PA_CON_POST_1B_Lures, CPZ_PA_CON_POST_1B_NonLures, 
                                      CPZ_SB_CON_POST_1B_NonTargets, CPZ_SB_CON_POST_1B_Lures, CPZ_SB_CON_POST_1B_NonLures)
longformat.PostNonTargetPASB_CPZ.df <- melt(PostNonTargetPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostNonTargetPASB_CPZ.df$value<-as.numeric(as.character(longformat.PostNonTargetPASB_CPZ.df$value))

ggplot(longformat.PostNonTargetPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Post NonTarget Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - PZ
PostNonTargetPASB_PZ.df <-data.frame(time, PZ_PA_CON_POST_1B_NonTargets, PZ_PA_CON_POST_1B_Lures, PZ_PA_CON_POST_1B_NonLures, 
                                     PZ_SB_CON_POST_1B_NonTargets, PZ_SB_CON_POST_1B_Lures, PZ_SB_CON_POST_1B_NonLures)
longformat.PostNonTargetPASB_PZ.df <- melt(PostNonTargetPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostNonTargetPASB_PZ.df$value<-as.numeric(as.character(longformat.PostNonTargetPASB_PZ.df$value))

ggplot(longformat.PostNonTargetPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Post NonTarget Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - CZ, CPZ, PZ 
PostNonTargetPASB.df <-data.frame(time, CZ_PA_CON_POST_1B_NonTargets, CPZ_PA_CON_POST_1B_NonTargets, PZ_PA_CON_POST_1B_NonTargets, 
                                  CZ_SB_CON_POST_1B_NonTargets, CPZ_SB_CON_POST_1B_NonTargets, PZ_SB_CON_POST_1B_NonTargets, 
                                  CZ_PA_CON_POST_1B_Lures, CPZ_PA_CON_POST_1B_Lures, PZ_PA_CON_POST_1B_Lures, 
                                  CZ_SB_CON_POST_1B_Lures, CPZ_SB_CON_POST_1B_Lures, PZ_SB_CON_POST_1B_Lures,
                                  CZ_PA_CON_POST_1B_NonLures, CPZ_PA_CON_POST_1B_NonLures, PZ_PA_CON_POST_1B_NonLures, 
                                  CZ_SB_CON_POST_1B_NonLures, CPZ_SB_CON_POST_1B_NonLures, PZ_SB_CON_POST_1B_NonLures)
longformat.PostNonTargetPASB.df <- melt(PostNonTargetPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostNonTargetPASB.df$value<-as.numeric(as.character(longformat.PostNonTargetPASB.df$value))

ggplot(longformat.PostNonTargetPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Post NonTarget Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Post Target waveforms for SB and PA - CZ 
PostTargetPASB_CZ.df <-data.frame(time, CZ_PA_CON_POST_1B_Targets, CZ_SB_CON_POST_1B_Targets)
longformat.PostTargetPASB_CZ.df <- melt(PostTargetPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetPASB_CZ.df$value<-as.numeric(as.character(longformat.PostTargetPASB_CZ.df$value))

ggplot(longformat.PostTargetPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Post Target Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Target waveforms for SB and PA - CPZ
PostTargetPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_POST_1B_Targets, CPZ_SB_CON_POST_1B_Targets)
longformat.PostTargetPASB_CPZ.df <- melt(PostTargetPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetPASB_CPZ.df$value<-as.numeric(as.character(longformat.PostTargetPASB_CPZ.df$value))

ggplot(longformat.PostTargetPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Post Target Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Target waveforms for SB and PA - PZ 
PostTargetPASB_PZ.df <-data.frame(time, PZ_PA_CON_POST_1B_Targets, PZ_SB_CON_POST_1B_Targets)
longformat.PostTargetPASB_PZ.df <- melt(PostTargetPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetPASB_PZ.df$value<-as.numeric(as.character(longformat.PostTargetPASB_PZ.df$value))

ggplot(longformat.PostTargetPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Post Target Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Target waveforms for SB and PA - CZ, CPZ, PZ
PostTargetPASB.df <-data.frame(time, CZ_PA_CON_POST_1B_Targets, CPZ_PA_CON_POST_1B_Targets, PZ_PA_CON_POST_1B_Targets,  
                               CZ_SB_CON_POST_1B_Targets, CPZ_SB_CON_POST_1B_Targets,PZ_SB_CON_POST_1B_Targets)
longformat.PostTargetPASB.df <- melt(PostTargetPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetPASB.df$value<-as.numeric(as.character(longformat.PostTargetPASB.df$value))

ggplot(longformat.PostTargetPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -7)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("1B Post Target Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Import Data for 2B
PA_CON_PRE_2B_Targets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/PRE/2B/Combined_Targets/Export_Excel/GrandAVG/SITlessPACONPRE2BTargets.xlsm")
PA_CON_PRE_2B_NonTargets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/PRE/2B/Combined_Targets/Export_Excel/GrandAVG/SITlessPACONPRE2BNonTargets.xlsm")
PA_CON_PRE_2B_Lures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/PRE/2B/Combined_Lures/Export_Excel/GrandAVG/SITlessPACONPRE2BLures.xlsm")
PA_CON_PRE_2B_NonLures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/PRE/2B/Combined_Lures/Export_Excel/GrandAVG/SITlessPACONPRE2BNonLures.xlsm")

PA_CON_POST_2B_Targets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/POST/2B/Combined_Targets/Export_Excel/GrandAVG/SITlessPACONPOST2BTargets.xlsm")
PA_CON_POST_2B_NonTargets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/POST/2B/Combined_Targets/Export_Excel/GrandAVG/SITlessPACONPOST2BNonTargets.xlsm")
PA_CON_POST_2B_Lures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/POST/2B/Combined_Lures/Export_Excel/GrandAVG/SITlessPACONPOST2BLures.xlsm")
PA_CON_POST_2B_NonLures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/POST/2B/Combined_Lures/Export_Excel/GrandAVG/SITlessPACONPOST2BNonLures.xlsm")

SB_CON_PRE_2B_Targets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/PRE/2B/Combined_Targets/Export_Excel/GrandAVG/SITlessSBCONPRE2BTargets.xlsm")
SB_CON_PRE_2B_NonTargets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/PRE/2B/Combined_Targets/Export_Excel/GrandAVG/SITlessSBCONPRE2BNonTargets.xlsm")
SB_CON_PRE_2B_Lures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/PRE/2B/Combined_Lures/Export_Excel/GrandAVG/SITlessSBCONPRE2BLures.xlsm")
SB_CON_PRE_2B_NonLures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/PRE/2B/Combined_Lures/Export_Excel/GrandAVG/SITlessSBCONPRE2BNonLures.xlsm")

SB_CON_POST_2B_Targets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/POST/2B/Combined_Targets/Export_Excel/GrandAVG/SITlessSBCONPOST2BTargets.xlsm")
SB_CON_POST_2B_NonTargets <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/POST/2B/Combined_Targets/Export_Excel/GrandAVG/SITlessSBCONPOST2BNonTargets.xlsm")
SB_CON_POST_2B_Lures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/POST/2B/Combined_Lures/Export_Excel/GrandAVG/SITlessSBCONPOST2BLures.xlsm")
SB_CON_POST_2B_NonLures <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/POST/2B/Combined_Lures/Export_Excel/GrandAVG/SITlessSBCONPOST2BNonLures.xlsm")

#Call Variables for 2B
#time 
#Call Variables
time <- (PA_CON_PRE_2B_Targets$time)
time <-as.numeric(time) 

#PA_CON_PRE_2B_Targets 
CZ_PA_CON_PRE_2B_Targets <- (PA_CON_PRE_2B_Targets$CZ)
CPZ_PA_CON_PRE_2B_Targets <- (PA_CON_PRE_2B_Targets$CPZ)
PZ_PA_CON_PRE_2B_Targets <- (PA_CON_PRE_2B_Targets$PZ)

#PA_CON_PRE_2B_NonTargets
CZ_PA_CON_PRE_2B_NonTargets <- (PA_CON_PRE_2B_NonTargets$CZ)
CPZ_PA_CON_PRE_2B_NonTargets <- (PA_CON_PRE_2B_NonTargets$CPZ)
PZ_PA_CON_PRE_2B_NonTargets <- (PA_CON_PRE_2B_NonTargets$PZ)

#PA_CON_PRE_2B_Lures
CZ_PA_CON_PRE_2B_Lures <- (PA_CON_PRE_2B_Lures$CZ)
CPZ_PA_CON_PRE_2B_Lures <- (PA_CON_PRE_2B_Lures$CPZ)
PZ_PA_CON_PRE_2B_Lures <- (PA_CON_PRE_2B_Lures$PZ)

#PA_CON_PRE_2B_NonLures
CZ_PA_CON_PRE_2B_NonLures <- (PA_CON_PRE_2B_NonLures$CZ)
CPZ_PA_CON_PRE_2B_NonLures <- (PA_CON_PRE_2B_NonLures$CPZ)
PZ_PA_CON_PRE_2B_NonLures <- (PA_CON_PRE_2B_NonLures$PZ)

#PA_CON_POST_2B_Targets 
CZ_PA_CON_POST_2B_Targets <- (PA_CON_POST_2B_Targets$CZ)
CPZ_PA_CON_POST_2B_Targets <- (PA_CON_POST_2B_Targets$CPZ)
PZ_PA_CON_POST_2B_Targets <- (PA_CON_POST_2B_Targets$PZ)

#PA_CON_POST_2B_NonTargets
CZ_PA_CON_POST_2B_NonTargets <- (PA_CON_POST_2B_NonTargets$CZ)
CPZ_PA_CON_POST_2B_NonTargets <- (PA_CON_POST_2B_NonTargets$CPZ)
PZ_PA_CON_POST_2B_NonTargets <- (PA_CON_POST_2B_NonTargets$PZ)

#PA_CON_POST_2B_Lures
CZ_PA_CON_POST_2B_Lures <- (PA_CON_POST_2B_Lures$CZ)
CPZ_PA_CON_POST_2B_Lures <- (PA_CON_POST_2B_Lures$CPZ)
PZ_PA_CON_POST_2B_Lures <- (PA_CON_POST_2B_Lures$PZ)

#PA_CON_POST_2B_NonLures
CZ_PA_CON_POST_2B_NonLures <- (PA_CON_POST_2B_NonLures$CZ)
CPZ_PA_CON_POST_2B_NonLures <- (PA_CON_POST_2B_NonLures$CPZ)
PZ_PA_CON_POST_2B_NonLures <- (PA_CON_POST_2B_NonLures$PZ)

#SB_CON_PRE_2B_Targets 
CZ_SB_CON_PRE_2B_Targets <- (SB_CON_PRE_2B_Targets$CZ)
CPZ_SB_CON_PRE_2B_Targets <- (SB_CON_PRE_2B_Targets$CPZ)
PZ_SB_CON_PRE_2B_Targets <- (SB_CON_PRE_2B_Targets$PZ)

#SB_CON_PRE_2B_NonTargets
CZ_SB_CON_PRE_2B_NonTargets <- (SB_CON_PRE_2B_NonTargets$CZ)
CPZ_SB_CON_PRE_2B_NonTargets <- (SB_CON_PRE_2B_NonTargets$CPZ)
PZ_SB_CON_PRE_2B_NonTargets <- (SB_CON_PRE_2B_NonTargets$PZ)

#SB_CON_PRE_2B_Lures
CZ_SB_CON_PRE_2B_Lures <- (SB_CON_PRE_2B_Lures$CZ)
CPZ_SB_CON_PRE_2B_Lures <- (SB_CON_PRE_2B_Lures$CPZ)
PZ_SB_CON_PRE_2B_Lures <- (SB_CON_PRE_2B_Lures$PZ)

#SB_CON_PRE_2B_NonLures
CZ_SB_CON_PRE_2B_NonLures <- (SB_CON_PRE_2B_NonLures$CZ)
CPZ_SB_CON_PRE_2B_NonLures <- (SB_CON_PRE_2B_NonLures$CPZ)
PZ_SB_CON_PRE_2B_NonLures <- (SB_CON_PRE_2B_NonLures$PZ)

#SB_CON_POST_2B_Targets 
CZ_SB_CON_POST_2B_Targets <- (SB_CON_POST_2B_Targets$CZ)
CPZ_SB_CON_POST_2B_Targets <- (SB_CON_POST_2B_Targets$CPZ)
PZ_SB_CON_POST_2B_Targets <- (SB_CON_POST_2B_Targets$PZ)

#SB_CON_POST_2B_NonTargets
CZ_SB_CON_POST_2B_NonTargets <- (SB_CON_POST_2B_NonTargets$CZ)
CPZ_SB_CON_POST_2B_NonTargets <- (SB_CON_POST_2B_NonTargets$CPZ)
PZ_SB_CON_POST_2B_NonTargets <- (SB_CON_POST_2B_NonTargets$PZ)

#SB_CON_POST_2B_Lures
CZ_SB_CON_POST_2B_Lures <- (SB_CON_POST_2B_Lures$CZ)
CPZ_SB_CON_POST_2B_Lures <- (SB_CON_POST_2B_Lures$CPZ)
PZ_SB_CON_POST_2B_Lures <- (SB_CON_POST_2B_Lures$PZ)

#SB_CON_POST_2B_NonLures
CZ_SB_CON_POST_2B_NonLures <- (SB_CON_POST_2B_NonLures$CZ)
CPZ_SB_CON_POST_2B_NonLures <- (SB_CON_POST_2B_NonLures$CPZ)
PZ_SB_CON_POST_2B_NonLures <- (SB_CON_POST_2B_NonLures$PZ)



#Pre Non-Target and Target waveforms for SB and PA on the same graph - CZ 
PreTargetNonTargetPASB_CZ.df <-data.frame(time, CZ_PA_CON_PRE_2B_Targets, CZ_PA_CON_PRE_2B_NonTargets, CZ_SB_CON_PRE_2B_Targets, CZ_SB_CON_PRE_2B_NonTargets)
longformat.PreTargetNonTargetPASB_CZ.df <- melt(PreTargetNonTargetPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetNonTargetPASB_CZ.df$value<-as.numeric(as.character(longformat.PreTargetNonTargetPASB_CZ.df$value))

ggplot(longformat.PreTargetNonTargetPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Pre Target and NonTarget Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Non-Target and Target waveforms for SB and PA on the same graph - CPZ
PreTargetNonTargetPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_PRE_2B_Targets, CPZ_PA_CON_PRE_2B_NonTargets, CPZ_SB_CON_PRE_2B_Targets, CPZ_SB_CON_PRE_2B_NonTargets)
longformat.PreTargetNonTargetPASB_CPZ.df <- melt(PreTargetNonTargetPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetNonTargetPASB_CPZ.df$value<-as.numeric(as.character(longformat.PreTargetNonTargetPASB_CPZ.df$value))

ggplot(longformat.PreTargetNonTargetPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Pre Target and NonTarget Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Non-Target and Target waveforms for SB and PA on the same graph - PZ 
PreTargetNonTargetPASB_PZ.df <-data.frame(time, PZ_PA_CON_PRE_2B_Targets, PZ_PA_CON_PRE_2B_NonTargets, PZ_SB_CON_PRE_2B_Targets, PZ_SB_CON_PRE_2B_NonTargets)
longformat.PreTargetNonTargetPASB_PZ.df <- melt(PreTargetNonTargetPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetNonTargetPASB_PZ.df$value<-as.numeric(as.character(longformat.PreTargetNonTargetPASB_PZ.df$value))

ggplot(longformat.PreTargetNonTargetPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Pre Target and NonTarget Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Non-Target and Target waveforms for SB and PA on the same graph - CZ, CPZ, PZ 
PreTargetNonTargetPASB.df <-data.frame(time, CZ_PA_CON_PRE_2B_Targets, CPZ_PA_CON_PRE_2B_Targets, PZ_PA_CON_PRE_2B_Targets,  
                                       CZ_PA_CON_PRE_2B_NonTargets, CPZ_PA_CON_PRE_2B_NonTargets, PZ_PA_CON_PRE_2B_NonTargets, 
                                       CZ_SB_CON_PRE_2B_Targets, CPZ_SB_CON_PRE_2B_Targets,PZ_SB_CON_PRE_2B_Targets, 
                                       CZ_SB_CON_PRE_2B_NonTargets, CPZ_SB_CON_PRE_2B_NonTargets, PZ_SB_CON_PRE_2B_NonTargets)
longformat.PreTargetNonTargetPASB.df <- melt(PreTargetNonTargetPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetNonTargetPASB.df$value<-as.numeric(as.character(longformat.PreTargetNonTargetPASB.df$value))

ggplot(longformat.PreTargetNonTargetPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Pre Target and NonTarget Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Pre Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - CZ
PreNonTargetPASB_CZ.df <-data.frame(time, CZ_PA_CON_PRE_2B_NonTargets, CZ_PA_CON_PRE_2B_Lures, CZ_PA_CON_PRE_2B_NonLures, 
                                    CZ_SB_CON_PRE_2B_NonTargets, CZ_SB_CON_PRE_2B_Lures, CZ_SB_CON_PRE_2B_NonLures)
longformat.PreNonTargetPASB_CZ.df <- melt(PreNonTargetPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreNonTargetPASB_CZ.df$value<-as.numeric(as.character(longformat.PreNonTargetPASB_CZ.df$value))

ggplot(longformat.PreNonTargetPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Pre NonTarget Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - CPZ
PreNonTargetPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_PRE_2B_NonTargets, CPZ_PA_CON_PRE_2B_Lures, CPZ_PA_CON_PRE_2B_NonLures, 
                                     CPZ_SB_CON_PRE_2B_NonTargets, CPZ_SB_CON_PRE_2B_Lures, CPZ_SB_CON_PRE_2B_NonLures)
longformat.PreNonTargetPASB_CPZ.df <- melt(PreNonTargetPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreNonTargetPASB_CPZ.df$value<-as.numeric(as.character(longformat.PreNonTargetPASB_CPZ.df$value))

ggplot(longformat.PreNonTargetPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Pre NonTarget Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - PZ
PreNonTargetPASB_PZ.df <-data.frame(time, PZ_PA_CON_PRE_2B_NonTargets, PZ_PA_CON_PRE_2B_Lures, PZ_PA_CON_PRE_2B_NonLures, 
                                    PZ_SB_CON_PRE_2B_NonTargets, PZ_SB_CON_PRE_2B_Lures, PZ_SB_CON_PRE_2B_NonLures)
longformat.PreNonTargetPASB_PZ.df <- melt(PreNonTargetPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreNonTargetPASB_PZ.df$value<-as.numeric(as.character(longformat.PreNonTargetPASB_PZ.df$value))

ggplot(longformat.PreNonTargetPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Pre NonTarget Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - CZ, CPZ, PZ 
PreNonTargetPASB.df <-data.frame(time, CZ_PA_CON_PRE_2B_NonTargets, CPZ_PA_CON_PRE_2B_NonTargets, PZ_PA_CON_PRE_2B_NonTargets, 
                                 CZ_SB_CON_PRE_2B_NonTargets, CPZ_SB_CON_PRE_2B_NonTargets, PZ_SB_CON_PRE_2B_NonTargets, 
                                 CZ_PA_CON_PRE_2B_Lures, CPZ_PA_CON_PRE_2B_Lures, PZ_PA_CON_PRE_2B_Lures, 
                                 CZ_SB_CON_PRE_2B_Lures, CPZ_SB_CON_PRE_2B_Lures, PZ_SB_CON_PRE_2B_Lures,
                                 CZ_PA_CON_PRE_2B_NonLures, CPZ_PA_CON_PRE_2B_NonLures, PZ_PA_CON_PRE_2B_NonLures, 
                                 CZ_SB_CON_PRE_2B_NonLures, CPZ_SB_CON_PRE_2B_NonLures, PZ_SB_CON_PRE_2B_NonLures)
longformat.PreNonTargetPASB.df <- melt(PreNonTargetPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreNonTargetPASB.df$value<-as.numeric(as.character(longformat.PreNonTargetPASB.df$value))

ggplot(longformat.PreNonTargetPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Pre NonTarget Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Pre Target waveforms for SB and PA - CZ 
PreTargetPASB_CZ.df <-data.frame(time, CZ_PA_CON_PRE_2B_Targets, CZ_SB_CON_PRE_2B_Targets)
longformat.PreTargetPASB_CZ.df <- melt(PreTargetPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetPASB_CZ.df$value<-as.numeric(as.character(longformat.PreTargetPASB_CZ.df$value))

ggplot(longformat.PreTargetPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Pre Target Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Target waveforms for SB and PA - CPZ
PreTargetPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_PRE_2B_Targets, CPZ_SB_CON_PRE_2B_Targets)
longformat.PreTargetPASB_CPZ.df <- melt(PreTargetPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetPASB_CPZ.df$value<-as.numeric(as.character(longformat.PreTargetPASB_CPZ.df$value))

ggplot(longformat.PreTargetPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Pre Target Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Target waveforms for SB and PA - PZ 
PreTargetPASB_PZ.df <-data.frame(time, PZ_PA_CON_PRE_2B_Targets, PZ_SB_CON_PRE_2B_Targets)
longformat.PreTargetPASB_PZ.df <- melt(PreTargetPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetPASB_PZ.df$value<-as.numeric(as.character(longformat.PreTargetPASB_PZ.df$value))

ggplot(longformat.PreTargetPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Pre Target Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Pre Target waveforms for SB and PA - CZ, CPZ, PZ
PreTargetPASB.df <-data.frame(time, CZ_PA_CON_PRE_2B_Targets, CPZ_PA_CON_PRE_2B_Targets, PZ_PA_CON_PRE_2B_Targets,  
                              CZ_SB_CON_PRE_2B_Targets, CPZ_SB_CON_PRE_2B_Targets,PZ_SB_CON_PRE_2B_Targets)
longformat.PreTargetPASB.df <- melt(PreTargetPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreTargetPASB.df$value<-as.numeric(as.character(longformat.PreTargetPASB.df$value))

ggplot(longformat.PreTargetPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Pre Target Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target and Target waveforms for SB and PA on the same graph - CZ 
PostTargetNonTargetPASB_CZ.df <-data.frame(time, CZ_PA_CON_POST_2B_Targets, CZ_PA_CON_POST_2B_NonTargets, CZ_SB_CON_POST_2B_Targets, CZ_SB_CON_POST_2B_NonTargets)
longformat.PostTargetNonTargetPASB_CZ.df <- melt(PostTargetNonTargetPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetNonTargetPASB_CZ.df$value<-as.numeric(as.character(longformat.PostTargetNonTargetPASB_CZ.df$value))

ggplot(longformat.PostTargetNonTargetPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Post Target and NonTarget Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target and Target waveforms for SB and PA on the same graph - CPZ
PostTargetNonTargetPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_POST_2B_Targets, CPZ_PA_CON_POST_2B_NonTargets, CPZ_SB_CON_POST_2B_Targets, CPZ_SB_CON_POST_2B_NonTargets)
longformat.PostTargetNonTargetPASB_CPZ.df <- melt(PostTargetNonTargetPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetNonTargetPASB_CPZ.df$value<-as.numeric(as.character(longformat.PostTargetNonTargetPASB_CPZ.df$value))

ggplot(longformat.PostTargetNonTargetPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Post Target and NonTarget Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target and Target waveforms for SB and PA on the same graph - PZ 
PostTargetNonTargetPASB_PZ.df <-data.frame(time, PZ_PA_CON_POST_2B_Targets, PZ_PA_CON_POST_2B_NonTargets, PZ_SB_CON_POST_2B_Targets, PZ_SB_CON_POST_2B_NonTargets)
longformat.PostTargetNonTargetPASB_PZ.df <- melt(PostTargetNonTargetPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetNonTargetPASB_PZ.df$value<-as.numeric(as.character(longformat.PostTargetNonTargetPASB_PZ.df$value))

ggplot(longformat.PostTargetNonTargetPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Post Target and NonTarget Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target and Target waveforms for SB and PA on the same graph - CZ, CPZ, PZ 
PostTargetNonTargetPASB.df <-data.frame(time, CZ_PA_CON_POST_2B_Targets, CPZ_PA_CON_POST_2B_Targets, PZ_PA_CON_POST_2B_Targets,  
                                        CZ_PA_CON_POST_2B_NonTargets, CPZ_PA_CON_POST_2B_NonTargets, PZ_PA_CON_POST_2B_NonTargets, 
                                        CZ_SB_CON_POST_2B_Targets, CPZ_SB_CON_POST_2B_Targets,PZ_SB_CON_POST_2B_Targets, 
                                        CZ_SB_CON_POST_2B_NonTargets, CPZ_SB_CON_POST_2B_NonTargets, PZ_SB_CON_POST_2B_NonTargets)
longformat.PostTargetNonTargetPASB.df <- melt(PostTargetNonTargetPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetNonTargetPASB.df$value<-as.numeric(as.character(longformat.PostTargetNonTargetPASB.df$value))

ggplot(longformat.PostTargetNonTargetPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Post Target and NonTarget Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Post Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - CZ
PostNonTargetPASB_CZ.df <-data.frame(time, CZ_PA_CON_POST_2B_NonTargets, CZ_PA_CON_POST_2B_Lures, CZ_PA_CON_POST_2B_NonLures, 
                                     CZ_SB_CON_POST_2B_NonTargets, CZ_SB_CON_POST_2B_Lures, CZ_SB_CON_POST_2B_NonLures)
longformat.PostNonTargetPASB_CZ.df <- melt(PostNonTargetPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostNonTargetPASB_CZ.df$value<-as.numeric(as.character(longformat.PostNonTargetPASB_CZ.df$value))

ggplot(longformat.PostNonTargetPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Post NonTarget Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - CPZ
PostNonTargetPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_POST_2B_NonTargets, CPZ_PA_CON_POST_2B_Lures, CPZ_PA_CON_POST_2B_NonLures, 
                                      CPZ_SB_CON_POST_2B_NonTargets, CPZ_SB_CON_POST_2B_Lures, CPZ_SB_CON_POST_2B_NonLures)
longformat.PostNonTargetPASB_CPZ.df <- melt(PostNonTargetPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostNonTargetPASB_CPZ.df$value<-as.numeric(as.character(longformat.PostNonTargetPASB_CPZ.df$value))

ggplot(longformat.PostNonTargetPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Post NonTarget Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - PZ
PostNonTargetPASB_PZ.df <-data.frame(time, PZ_PA_CON_POST_2B_NonTargets, PZ_PA_CON_POST_2B_Lures, PZ_PA_CON_POST_2B_NonLures, 
                                     PZ_SB_CON_POST_2B_NonTargets, PZ_SB_CON_POST_2B_Lures, PZ_SB_CON_POST_2B_NonLures)
longformat.PostNonTargetPASB_PZ.df <- melt(PostNonTargetPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostNonTargetPASB_PZ.df$value<-as.numeric(as.character(longformat.PostNonTargetPASB_PZ.df$value))

ggplot(longformat.PostNonTargetPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Post NonTarget Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Non-Target waveforms for SB and PA (include binned and Lures, NonLures) - CZ, CPZ, PZ 
PostNonTargetPASB.df <-data.frame(time, CZ_PA_CON_POST_2B_NonTargets, CPZ_PA_CON_POST_2B_NonTargets, PZ_PA_CON_POST_2B_NonTargets, 
                                  CZ_SB_CON_POST_2B_NonTargets, CPZ_SB_CON_POST_2B_NonTargets, PZ_SB_CON_POST_2B_NonTargets, 
                                  CZ_PA_CON_POST_2B_Lures, CPZ_PA_CON_POST_2B_Lures, PZ_PA_CON_POST_2B_Lures, 
                                  CZ_SB_CON_POST_2B_Lures, CPZ_SB_CON_POST_2B_Lures, PZ_SB_CON_POST_2B_Lures,
                                  CZ_PA_CON_POST_2B_NonLures, CPZ_PA_CON_POST_2B_NonLures, PZ_PA_CON_POST_2B_NonLures, 
                                  CZ_SB_CON_POST_2B_NonLures, CPZ_SB_CON_POST_2B_NonLures, PZ_SB_CON_POST_2B_NonLures)
longformat.PostNonTargetPASB.df <- melt(PostNonTargetPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostNonTargetPASB.df$value<-as.numeric(as.character(longformat.PostNonTargetPASB.df$value))

ggplot(longformat.PostNonTargetPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Post NonTarget Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Post Target waveforms for SB and PA - CZ 
PostTargetPASB_CZ.df <-data.frame(time, CZ_PA_CON_POST_2B_Targets, CZ_SB_CON_POST_2B_Targets)
longformat.PostTargetPASB_CZ.df <- melt(PostTargetPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetPASB_CZ.df$value<-as.numeric(as.character(longformat.PostTargetPASB_CZ.df$value))

ggplot(longformat.PostTargetPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Post Target Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Target waveforms for SB and PA - CPZ
PostTargetPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_POST_2B_Targets, CPZ_SB_CON_POST_2B_Targets)
longformat.PostTargetPASB_CPZ.df <- melt(PostTargetPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetPASB_CPZ.df$value<-as.numeric(as.character(longformat.PostTargetPASB_CPZ.df$value))

ggplot(longformat.PostTargetPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Post Target Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Target waveforms for SB and PA - PZ 
PostTargetPASB_PZ.df <-data.frame(time, PZ_PA_CON_POST_2B_Targets, PZ_SB_CON_POST_2B_Targets)
longformat.PostTargetPASB_PZ.df <- melt(PostTargetPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetPASB_PZ.df$value<-as.numeric(as.character(longformat.PostTargetPASB_PZ.df$value))

ggplot(longformat.PostTargetPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Post Target Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Target waveforms for SB and PA - CZ, CPZ, PZ
PostTargetPASB.df <-data.frame(time, CZ_PA_CON_POST_2B_Targets, CPZ_PA_CON_POST_2B_Targets, PZ_PA_CON_POST_2B_Targets,  
                               CZ_SB_CON_POST_2B_Targets, CPZ_SB_CON_POST_2B_Targets,PZ_SB_CON_POST_2B_Targets)
longformat.PostTargetPASB.df <- melt(PostTargetPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostTargetPASB.df$value<-as.numeric(as.character(longformat.PostTargetPASB.df$value))

ggplot(longformat.PostTargetPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(10, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("2B Post Target Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Difference wave Pre 1B Target-NonTarget PA and SB - CZ
CZ_PA_CON_PRE_1B_Targets <- as.numeric(CZ_PA_CON_PRE_1B_Targets)
CZ_PA_CON_PRE_1B_NonTargets <- as.numeric(CZ_PA_CON_PRE_1B_NonTargets)
CZ_SB_CON_PRE_1B_Targets <-as.numeric (CZ_SB_CON_PRE_1B_Targets)
CZ_SB_CON_PRE_1B_NonTargets <- as.numeric(CZ_SB_CON_PRE_1B_NonTargets)

Diff_Pre_1B_TargNonTarg_PA_CZ <- CZ_PA_CON_PRE_1B_Targets - CZ_PA_CON_PRE_1B_NonTargets
Diff_Pre_1B_TargNonTarg_SB_CZ <- CZ_SB_CON_PRE_1B_Targets - CZ_SB_CON_PRE_1B_NonTargets

Diff_Pre_1B_TargNonTargPASB_CZ.df <-data.frame(time, Diff_Pre_1B_TargNonTarg_PA_CZ, Diff_Pre_1B_TargNonTarg_SB_CZ)
longformat.Diff_Pre_1B_TargNonTargPASB_CZ.df <- melt(Diff_Pre_1B_TargNonTargPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_1B_TargNonTargPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 1B Target-NonTarget PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre 1B Target-NonTarget PA and SB - CPZ
CPZ_PA_CON_PRE_1B_Targets <- as.numeric(CPZ_PA_CON_PRE_1B_Targets)
CPZ_PA_CON_PRE_1B_NonTargets <- as.numeric(CPZ_PA_CON_PRE_1B_NonTargets)
CPZ_SB_CON_PRE_1B_Targets <-as.numeric (CPZ_SB_CON_PRE_1B_Targets)
CPZ_SB_CON_PRE_1B_NonTargets <- as.numeric(CPZ_SB_CON_PRE_1B_NonTargets)

Diff_Pre_1B_TargNonTarg_PA_CPZ <- CPZ_PA_CON_PRE_1B_Targets - CPZ_PA_CON_PRE_1B_NonTargets
Diff_Pre_1B_TargNonTarg_SB_CPZ <- CPZ_SB_CON_PRE_1B_Targets - CPZ_SB_CON_PRE_1B_NonTargets

Diff_Pre_1B_TargNonTargPASB_CPZ.df <-data.frame(time, Diff_Pre_1B_TargNonTarg_PA_CPZ, Diff_Pre_1B_TargNonTarg_SB_CPZ)
longformat.Diff_Pre_1B_TargNonTargPASB_CPZ.df <- melt(Diff_Pre_1B_TargNonTargPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_1B_TargNonTargPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 1B Target-NonTarget PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre 1B Target-NonTarget PA and SB - PZ
PZ_PA_CON_PRE_1B_Targets <- as.numeric(PZ_PA_CON_PRE_1B_Targets)
PZ_PA_CON_PRE_1B_NonTargets <- as.numeric(PZ_PA_CON_PRE_1B_NonTargets)
PZ_SB_CON_PRE_1B_Targets <-as.numeric (PZ_SB_CON_PRE_1B_Targets)
PZ_SB_CON_PRE_1B_NonTargets <- as.numeric(PZ_SB_CON_PRE_1B_NonTargets)

Diff_Pre_1B_TargNonTarg_PA_PZ <- PZ_PA_CON_PRE_1B_Targets - PZ_PA_CON_PRE_1B_NonTargets
Diff_Pre_1B_TargNonTarg_SB_PZ <- PZ_SB_CON_PRE_1B_Targets - PZ_SB_CON_PRE_1B_NonTargets

Diff_Pre_1B_TargNonTargPASB_PZ.df <-data.frame(time, Diff_Pre_1B_TargNonTarg_PA_PZ, Diff_Pre_1B_TargNonTarg_SB_PZ)
longformat.Diff_Pre_1B_TargNonTargPASB_PZ.df <- melt(Diff_Pre_1B_TargNonTargPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_1B_TargNonTargPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 1B Target-NonTarget PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre 1B Target-NonTarget PA and SB - CZ, CPZ, PZ
Diff_Pre_1B_TargNonTargPASB.df <-data.frame(time, Diff_Pre_1B_TargNonTarg_PA_CZ, Diff_Pre_1B_TargNonTarg_SB_CZ, 
                                                  Diff_Pre_1B_TargNonTarg_PA_CPZ, Diff_Pre_1B_TargNonTarg_SB_CPZ, 
                                                  Diff_Pre_1B_TargNonTarg_PA_PZ, Diff_Pre_1B_TargNonTarg_SB_PZ )
longformat.Diff_Pre_1B_TargNonTargPASB.df <- melt(Diff_Pre_1B_TargNonTargPASB.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_1B_TargNonTargPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 1B Target-NonTarget PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Difference wave Post 1B Target-NonTarget PA and SB - CZ
CZ_PA_CON_POST_1B_Targets <- as.numeric(CZ_PA_CON_POST_1B_Targets)
CZ_PA_CON_POST_1B_NonTargets <- as.numeric(CZ_PA_CON_POST_1B_NonTargets)
CZ_SB_CON_POST_1B_Targets <-as.numeric (CZ_SB_CON_POST_1B_Targets)
CZ_SB_CON_POST_1B_NonTargets <- as.numeric(CZ_SB_CON_POST_1B_NonTargets)

Diff_Post_1B_TargNonTarg_PA_CZ <- CZ_PA_CON_POST_1B_Targets - CZ_PA_CON_POST_1B_NonTargets
Diff_Post_1B_TargNonTarg_SB_CZ <- CZ_SB_CON_POST_1B_Targets - CZ_SB_CON_POST_1B_NonTargets

Diff_Post_1B_TargNonTargPASB_CZ.df <-data.frame(time, Diff_Post_1B_TargNonTarg_PA_CZ, Diff_Post_1B_TargNonTarg_SB_CZ)
longformat.Diff_Post_1B_TargNonTargPASB_CZ.df <- melt(Diff_Post_1B_TargNonTargPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_1B_TargNonTargPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 1B Target-NonTarget PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post 1B Target-NonTarget PA and SB - CPZ
CPZ_PA_CON_POST_1B_Targets <- as.numeric(CPZ_PA_CON_POST_1B_Targets)
CPZ_PA_CON_POST_1B_NonTargets <- as.numeric(CPZ_PA_CON_POST_1B_NonTargets)
CPZ_SB_CON_POST_1B_Targets <-as.numeric (CPZ_SB_CON_POST_1B_Targets)
CPZ_SB_CON_POST_1B_NonTargets <- as.numeric(CPZ_SB_CON_POST_1B_NonTargets)

Diff_Post_1B_TargNonTarg_PA_CPZ <- CPZ_PA_CON_POST_1B_Targets - CPZ_PA_CON_POST_1B_NonTargets
Diff_Post_1B_TargNonTarg_SB_CPZ <- CPZ_SB_CON_POST_1B_Targets - CPZ_SB_CON_POST_1B_NonTargets

Diff_Post_1B_TargNonTargPASB_CPZ.df <-data.frame(time, Diff_Post_1B_TargNonTarg_PA_CPZ, Diff_Post_1B_TargNonTarg_SB_CPZ)
longformat.Diff_Post_1B_TargNonTargPASB_CPZ.df <- melt(Diff_Post_1B_TargNonTargPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_1B_TargNonTargPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 1B Target-NonTarget PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post 1B Target-NonTarget PA and SB - PZ
PZ_PA_CON_POST_1B_Targets <- as.numeric(PZ_PA_CON_POST_1B_Targets)
PZ_PA_CON_POST_1B_NonTargets <- as.numeric(PZ_PA_CON_POST_1B_NonTargets)
PZ_SB_CON_POST_1B_Targets <-as.numeric (PZ_SB_CON_POST_1B_Targets)
PZ_SB_CON_POST_1B_NonTargets <- as.numeric(PZ_SB_CON_POST_1B_NonTargets)

Diff_Post_1B_TargNonTarg_PA_PZ <- PZ_PA_CON_POST_1B_Targets - PZ_PA_CON_POST_1B_NonTargets
Diff_Post_1B_TargNonTarg_SB_PZ <- PZ_SB_CON_POST_1B_Targets - PZ_SB_CON_POST_1B_NonTargets

Diff_Post_1B_TargNonTargPASB_PZ.df <-data.frame(time, Diff_Post_1B_TargNonTarg_PA_PZ, Diff_Post_1B_TargNonTarg_SB_PZ)
longformat.Diff_Post_1B_TargNonTargPASB_PZ.df <- melt(Diff_Post_1B_TargNonTargPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_1B_TargNonTargPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 1B Target-NonTarget PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post 1B Target-NonTarget PA and SB - CZ, CPZ, PZ
Diff_Post_1B_TargNonTargPASB.df <-data.frame(time, Diff_Post_1B_TargNonTarg_PA_CZ, Diff_Post_1B_TargNonTarg_SB_CZ, 
                                             Diff_Post_1B_TargNonTarg_PA_CPZ, Diff_Post_1B_TargNonTarg_SB_CPZ, 
                                             Diff_Post_1B_TargNonTarg_PA_PZ, Diff_Post_1B_TargNonTarg_SB_PZ )
longformat.Diff_Post_1B_TargNonTargPASB.df <- melt(Diff_Post_1B_TargNonTargPASB.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_1B_TargNonTargPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 1B Target-NonTarget PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre to Post 1B Target-NonTarget PA and SB - CZ, CPZ, PZ
Diff_PrePost_1B_TargNonTargPASB.df <-data.frame(time, Diff_Pre_1B_TargNonTarg_PA_CZ, Diff_Pre_1B_TargNonTarg_SB_CZ, 
                                                Diff_Pre_1B_TargNonTarg_PA_CPZ, Diff_Pre_1B_TargNonTarg_SB_CPZ, 
                                                Diff_Pre_1B_TargNonTarg_PA_PZ, Diff_Pre_1B_TargNonTarg_SB_PZ,
                                                Diff_Post_1B_TargNonTarg_PA_CZ, Diff_Post_1B_TargNonTarg_SB_CZ, 
                                                Diff_Post_1B_TargNonTarg_PA_CPZ, Diff_Post_1B_TargNonTarg_SB_CPZ, 
                                                Diff_Post_1B_TargNonTarg_PA_PZ, Diff_Post_1B_TargNonTarg_SB_PZ )
longformat.Diff_PrePost_1B_TargNonTargPASB.df <- melt(Diff_PrePost_1B_TargNonTargPASB.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_PrePost_1B_TargNonTargPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre and Post 1B Target-NonTarget PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))



#Difference wave Pre 2B Target-NonTarget PA and SB - CZ
CZ_PA_CON_PRE_2B_Targets <- as.numeric(CZ_PA_CON_PRE_2B_Targets)
CZ_PA_CON_PRE_2B_NonTargets <- as.numeric(CZ_PA_CON_PRE_2B_NonTargets)
CZ_SB_CON_PRE_2B_Targets <-as.numeric (CZ_SB_CON_PRE_2B_Targets)
CZ_SB_CON_PRE_2B_NonTargets <- as.numeric(CZ_SB_CON_PRE_2B_NonTargets)

Diff_Pre_2B_TargNonTarg_PA_CZ <- CZ_PA_CON_PRE_2B_Targets - CZ_PA_CON_PRE_2B_NonTargets
Diff_Pre_2B_TargNonTarg_SB_CZ <- CZ_SB_CON_PRE_2B_Targets - CZ_SB_CON_PRE_2B_NonTargets

Diff_Pre_2B_TargNonTargPASB_CZ.df <-data.frame(time, Diff_Pre_2B_TargNonTarg_PA_CZ, Diff_Pre_2B_TargNonTarg_SB_CZ)
longformat.Diff_Pre_2B_TargNonTargPASB_CZ.df <- melt(Diff_Pre_2B_TargNonTargPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_2B_TargNonTargPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 2B Target-NonTarget PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre 2B Target-NonTarget PA and SB - CPZ
CPZ_PA_CON_PRE_2B_Targets <- as.numeric(CPZ_PA_CON_PRE_2B_Targets)
CPZ_PA_CON_PRE_2B_NonTargets <- as.numeric(CPZ_PA_CON_PRE_2B_NonTargets)
CPZ_SB_CON_PRE_2B_Targets <-as.numeric (CPZ_SB_CON_PRE_2B_Targets)
CPZ_SB_CON_PRE_2B_NonTargets <- as.numeric(CPZ_SB_CON_PRE_2B_NonTargets)

Diff_Pre_2B_TargNonTarg_PA_CPZ <- CPZ_PA_CON_PRE_2B_Targets - CPZ_PA_CON_PRE_2B_NonTargets
Diff_Pre_2B_TargNonTarg_SB_CPZ <- CPZ_SB_CON_PRE_2B_Targets - CPZ_SB_CON_PRE_2B_NonTargets

Diff_Pre_2B_TargNonTargPASB_CPZ.df <-data.frame(time, Diff_Pre_2B_TargNonTarg_PA_CPZ, Diff_Pre_2B_TargNonTarg_SB_CPZ)
longformat.Diff_Pre_2B_TargNonTargPASB_CPZ.df <- melt(Diff_Pre_2B_TargNonTargPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_2B_TargNonTargPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 2B Target-NonTarget PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre 2B Target-NonTarget PA and SB - PZ
PZ_PA_CON_PRE_2B_Targets <- as.numeric(PZ_PA_CON_PRE_2B_Targets)
PZ_PA_CON_PRE_2B_NonTargets <- as.numeric(PZ_PA_CON_PRE_2B_NonTargets)
PZ_SB_CON_PRE_2B_Targets <-as.numeric (PZ_SB_CON_PRE_2B_Targets)
PZ_SB_CON_PRE_2B_NonTargets <- as.numeric(PZ_SB_CON_PRE_2B_NonTargets)

Diff_Pre_2B_TargNonTarg_PA_PZ <- PZ_PA_CON_PRE_2B_Targets - PZ_PA_CON_PRE_2B_NonTargets
Diff_Pre_2B_TargNonTarg_SB_PZ <- PZ_SB_CON_PRE_2B_Targets - PZ_SB_CON_PRE_2B_NonTargets

Diff_Pre_2B_TargNonTargPASB_PZ.df <-data.frame(time, Diff_Pre_2B_TargNonTarg_PA_PZ, Diff_Pre_2B_TargNonTarg_SB_PZ)
longformat.Diff_Pre_2B_TargNonTargPASB_PZ.df <- melt(Diff_Pre_2B_TargNonTargPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_2B_TargNonTargPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 2B Target-NonTarget PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre 2B Target-NonTarget PA and SB - CZ, CPZ, PZ
Diff_Pre_2B_TargNonTargPASB.df <-data.frame(time, Diff_Pre_2B_TargNonTarg_PA_CZ, Diff_Pre_2B_TargNonTarg_SB_CZ, 
                                            Diff_Pre_2B_TargNonTarg_PA_CPZ, Diff_Pre_2B_TargNonTarg_SB_CPZ, 
                                            Diff_Pre_2B_TargNonTarg_PA_PZ, Diff_Pre_2B_TargNonTarg_SB_PZ )
longformat.Diff_Pre_2B_TargNonTargPASB.df <- melt(Diff_Pre_2B_TargNonTargPASB.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_2B_TargNonTargPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 2B Target-NonTarget PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Difference wave Post 2B Target-NonTarget PA and SB - CZ
CZ_PA_CON_POST_2B_Targets <- as.numeric(CZ_PA_CON_POST_2B_Targets)
CZ_PA_CON_POST_2B_NonTargets <- as.numeric(CZ_PA_CON_POST_2B_NonTargets)
CZ_SB_CON_POST_2B_Targets <-as.numeric (CZ_SB_CON_POST_2B_Targets)
CZ_SB_CON_POST_2B_NonTargets <- as.numeric(CZ_SB_CON_POST_2B_NonTargets)

Diff_Post_2B_TargNonTarg_PA_CZ <- CZ_PA_CON_POST_2B_Targets - CZ_PA_CON_POST_2B_NonTargets
Diff_Post_2B_TargNonTarg_SB_CZ <- CZ_SB_CON_POST_2B_Targets - CZ_SB_CON_POST_2B_NonTargets

Diff_Post_2B_TargNonTargPASB_CZ.df <-data.frame(time, Diff_Post_2B_TargNonTarg_PA_CZ, Diff_Post_2B_TargNonTarg_SB_CZ)
longformat.Diff_Post_2B_TargNonTargPASB_CZ.df <- melt(Diff_Post_2B_TargNonTargPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_2B_TargNonTargPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 2B Target-NonTarget PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post 2B Target-NonTarget PA and SB - CPZ
CPZ_PA_CON_POST_2B_Targets <- as.numeric(CPZ_PA_CON_POST_2B_Targets)
CPZ_PA_CON_POST_2B_NonTargets <- as.numeric(CPZ_PA_CON_POST_2B_NonTargets)
CPZ_SB_CON_POST_2B_Targets <-as.numeric (CPZ_SB_CON_POST_2B_Targets)
CPZ_SB_CON_POST_2B_NonTargets <- as.numeric(CPZ_SB_CON_POST_2B_NonTargets)

Diff_Post_2B_TargNonTarg_PA_CPZ <- CPZ_PA_CON_POST_2B_Targets - CPZ_PA_CON_POST_2B_NonTargets
Diff_Post_2B_TargNonTarg_SB_CPZ <- CPZ_SB_CON_POST_2B_Targets - CPZ_SB_CON_POST_2B_NonTargets

Diff_Post_2B_TargNonTargPASB_CPZ.df <-data.frame(time, Diff_Post_2B_TargNonTarg_PA_CPZ, Diff_Post_2B_TargNonTarg_SB_CPZ)
longformat.Diff_Post_2B_TargNonTargPASB_CPZ.df <- melt(Diff_Post_2B_TargNonTargPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_2B_TargNonTargPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 2B Target-NonTarget PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post 2B Target-NonTarget PA and SB - PZ
PZ_PA_CON_POST_2B_Targets <- as.numeric(PZ_PA_CON_POST_2B_Targets)
PZ_PA_CON_POST_2B_NonTargets <- as.numeric(PZ_PA_CON_POST_2B_NonTargets)
PZ_SB_CON_POST_2B_Targets <-as.numeric (PZ_SB_CON_POST_2B_Targets)
PZ_SB_CON_POST_2B_NonTargets <- as.numeric(PZ_SB_CON_POST_2B_NonTargets)

Diff_Post_2B_TargNonTarg_PA_PZ <- PZ_PA_CON_POST_2B_Targets - PZ_PA_CON_POST_2B_NonTargets
Diff_Post_2B_TargNonTarg_SB_PZ <- PZ_SB_CON_POST_2B_Targets - PZ_SB_CON_POST_2B_NonTargets

Diff_Post_2B_TargNonTargPASB_PZ.df <-data.frame(time, Diff_Post_2B_TargNonTarg_PA_PZ, Diff_Post_2B_TargNonTarg_SB_PZ)
longformat.Diff_Post_2B_TargNonTargPASB_PZ.df <- melt(Diff_Post_2B_TargNonTargPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_2B_TargNonTargPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 2B Target-NonTarget PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post 2B Target-NonTarget PA and SB - CZ, CPZ, PZ
Diff_Post_2B_TargNonTargPASB.df <-data.frame(time, Diff_Post_2B_TargNonTarg_PA_CZ, Diff_Post_2B_TargNonTarg_SB_CZ, 
                                             Diff_Post_2B_TargNonTarg_PA_CPZ, Diff_Post_2B_TargNonTarg_SB_CPZ, 
                                             Diff_Post_2B_TargNonTarg_PA_PZ, Diff_Post_2B_TargNonTarg_SB_PZ )
longformat.Diff_Post_2B_TargNonTargPASB.df <- melt(Diff_Post_2B_TargNonTargPASB.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_2B_TargNonTargPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 2B Target-NonTarget PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre to Post 2B Target-NonTarget PA and SB - CZ, CPZ, PZ
Diff_PrePost_2B_TargNonTargPASB.df <-data.frame(time, Diff_Pre_2B_TargNonTarg_PA_CZ, Diff_Pre_2B_TargNonTarg_SB_CZ, 
                                                Diff_Pre_2B_TargNonTarg_PA_CPZ, Diff_Pre_2B_TargNonTarg_SB_CPZ, 
                                                Diff_Pre_2B_TargNonTarg_PA_PZ, Diff_Pre_2B_TargNonTarg_SB_PZ,
                                                Diff_Post_2B_TargNonTarg_PA_CZ, Diff_Post_2B_TargNonTarg_SB_CZ, 
                                                Diff_Post_2B_TargNonTarg_PA_CPZ, Diff_Post_2B_TargNonTarg_SB_CPZ, 
                                                Diff_Post_2B_TargNonTarg_PA_PZ, Diff_Post_2B_TargNonTarg_SB_PZ )
longformat.Diff_PrePost_2B_TargNonTargPASB.df <- melt(Diff_PrePost_2B_TargNonTargPASB.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_PrePost_2B_TargNonTargPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre and Post 2B Target-NonTarget PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Difference wave Pre 2B Target - 1B Target PA and SB- CZ 
CZ_PA_CON_PRE_2B_Targets <- as.numeric(CZ_PA_CON_PRE_2B_Targets)
CZ_SB_CON_PRE_2B_Targets <-as.numeric (CZ_SB_CON_PRE_2B_Targets)
CZ_PA_CON_PRE_1B_Targets <- as.numeric(CZ_PA_CON_PRE_1B_Targets)
CZ_SB_CON_PRE_1B_Targets <-as.numeric (CZ_SB_CON_PRE_1B_Targets)

Diff_Pre_1B2B_Targ_PA_CZ <-CZ_PA_CON_PRE_2B_Targets-CZ_PA_CON_PRE_1B_Targets
Diff_Pre_1B2B_Targ_SB_CZ <- CZ_SB_CON_PRE_2B_Targets-CZ_SB_CON_PRE_1B_Targets

Diff_Pre_1B2B_Targ_CZ.df <-data.frame(time, Diff_Pre_1B2B_Targ_PA_CZ, Diff_Pre_1B2B_Targ_SB_CZ)
longformat.Diff_Pre_1B2B_Targ_CZ.df <- melt(Diff_Pre_1B2B_Targ_CZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_1B2B_Targ_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 2B-1B Target PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre 2B Target - 1B Target PA and SB- CPZ
CPZ_PA_CON_PRE_2B_Targets <- as.numeric(CPZ_PA_CON_PRE_2B_Targets)
CPZ_SB_CON_PRE_2B_Targets <-as.numeric (CPZ_SB_CON_PRE_2B_Targets)
CPZ_PA_CON_PRE_1B_Targets <- as.numeric(CPZ_PA_CON_PRE_1B_Targets)
CPZ_SB_CON_PRE_1B_Targets <-as.numeric (CPZ_SB_CON_PRE_1B_Targets)

Diff_Pre_1B2B_Targ_PA_CPZ <-CPZ_PA_CON_PRE_2B_Targets-CPZ_PA_CON_PRE_1B_Targets
Diff_Pre_1B2B_Targ_SB_CPZ <- CPZ_SB_CON_PRE_2B_Targets-CPZ_SB_CON_PRE_1B_Targets

Diff_Pre_1B2B_Targ_CPZ.df <-data.frame(time, Diff_Pre_1B2B_Targ_PA_CPZ, Diff_Pre_1B2B_Targ_SB_CPZ)
longformat.Diff_Pre_1B2B_Targ_CPZ.df <- melt(Diff_Pre_1B2B_Targ_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_1B2B_Targ_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 2B-1B Target PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Difference wave Pre 2B Target - 1B Target PA and SB- PZ
PZ_PA_CON_PRE_2B_Targets <- as.numeric(PZ_PA_CON_PRE_2B_Targets)
PZ_SB_CON_PRE_2B_Targets <-as.numeric (PZ_SB_CON_PRE_2B_Targets)
PZ_PA_CON_PRE_1B_Targets <- as.numeric(PZ_PA_CON_PRE_1B_Targets)
PZ_SB_CON_PRE_1B_Targets <-as.numeric (PZ_SB_CON_PRE_1B_Targets)

Diff_Pre_1B2B_Targ_PA_PZ <-PZ_PA_CON_PRE_2B_Targets-PZ_PA_CON_PRE_1B_Targets
Diff_Pre_1B2B_Targ_SB_PZ <- PZ_SB_CON_PRE_2B_Targets-PZ_SB_CON_PRE_1B_Targets

Diff_Pre_1B2B_Targ_PZ.df <-data.frame(time, Diff_Pre_1B2B_Targ_PA_PZ, Diff_Pre_1B2B_Targ_SB_PZ)
longformat.Diff_Pre_1B2B_Targ_PZ.df <- melt(Diff_Pre_1B2B_Targ_PZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_1B2B_Targ_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 2B-1B Target PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre 2B Target - 1B Target PA and SB - CZ, CPZ, PZ
Diff_Pre_1B2B_Targ.df <-data.frame(time, Diff_Pre_1B2B_Targ_PA_CZ, Diff_Pre_1B2B_Targ_SB_CZ, 
                                   Diff_Pre_1B2B_Targ_PA_CPZ, Diff_Pre_1B2B_Targ_SB_CPZ, 
                                   Diff_Pre_1B2B_Targ_PA_PZ, Diff_Pre_1B2B_Targ_SB_PZ)
longformat.Diff_Pre_1B2B_Targ.df <- melt(Diff_Pre_1B2B_Targ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_1B2B_Targ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 2B-1B Target PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post 2B Target - 1B Target PA and SB- CZ 
CZ_PA_CON_POST_2B_Targets <- as.numeric(CZ_PA_CON_POST_2B_Targets)
CZ_SB_CON_POST_2B_Targets <-as.numeric (CZ_SB_CON_POST_2B_Targets)
CZ_PA_CON_POST_1B_Targets <- as.numeric(CZ_PA_CON_POST_1B_Targets)
CZ_SB_CON_POST_1B_Targets <-as.numeric (CZ_SB_CON_POST_1B_Targets)

Diff_Post_1B2B_Targ_PA_CZ <-CZ_PA_CON_POST_2B_Targets-CZ_PA_CON_POST_1B_Targets
Diff_Post_1B2B_Targ_SB_CZ <- CZ_SB_CON_POST_2B_Targets-CZ_SB_CON_POST_1B_Targets

Diff_Post_1B2B_Targ_CZ.df <-data.frame(time, Diff_Post_1B2B_Targ_PA_CZ, Diff_Post_1B2B_Targ_SB_CZ)
longformat.Diff_Post_1B2B_Targ_CZ.df <- melt(Diff_Post_1B2B_Targ_CZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_1B2B_Targ_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 2B-1B Target PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post 2B Target - 1B Target PA and SB- CPZ
CPZ_PA_CON_POST_2B_Targets <- as.numeric(CPZ_PA_CON_POST_2B_Targets)
CPZ_SB_CON_POST_2B_Targets <-as.numeric (CPZ_SB_CON_POST_2B_Targets)
CPZ_PA_CON_POST_1B_Targets <- as.numeric(CPZ_PA_CON_POST_1B_Targets)
CPZ_SB_CON_POST_1B_Targets <-as.numeric (CPZ_SB_CON_POST_1B_Targets)

Diff_Post_1B2B_Targ_PA_CPZ <-CPZ_PA_CON_POST_2B_Targets-CPZ_PA_CON_POST_1B_Targets
Diff_Post_1B2B_Targ_SB_CPZ <- CPZ_SB_CON_POST_2B_Targets-CPZ_SB_CON_POST_1B_Targets

Diff_Post_1B2B_Targ_CPZ.df <-data.frame(time, Diff_Post_1B2B_Targ_PA_CPZ, Diff_Post_1B2B_Targ_SB_CPZ)
longformat.Diff_Post_1B2B_Targ_CPZ.df <- melt(Diff_Post_1B2B_Targ_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_1B2B_Targ_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 2B-1B Target PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Difference wave Post 2B Target - 1B Target PA and SB- PZ
PZ_PA_CON_POST_2B_Targets <- as.numeric(PZ_PA_CON_POST_2B_Targets)
PZ_SB_CON_POST_2B_Targets <-as.numeric (PZ_SB_CON_POST_2B_Targets)
PZ_PA_CON_POST_1B_Targets <- as.numeric(PZ_PA_CON_POST_1B_Targets)
PZ_SB_CON_POST_1B_Targets <-as.numeric (PZ_SB_CON_POST_1B_Targets)

Diff_Post_1B2B_Targ_PA_PZ <-PZ_PA_CON_POST_2B_Targets-PZ_PA_CON_POST_1B_Targets
Diff_Post_1B2B_Targ_SB_PZ <- PZ_SB_CON_POST_2B_Targets-PZ_SB_CON_POST_1B_Targets

Diff_Post_1B2B_Targ_PZ.df <-data.frame(time, Diff_Post_1B2B_Targ_PA_PZ, Diff_Post_1B2B_Targ_SB_PZ)
longformat.Diff_Post_1B2B_Targ_PZ.df <- melt(Diff_Post_1B2B_Targ_PZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_1B2B_Targ_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 2B-1B Target PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post 2B Target - 1B Target PA and SB - CZ, CPZ, PZ
Diff_Post_1B2B_Targ.df <-data.frame(time, Diff_Post_1B2B_Targ_PA_CZ, Diff_Post_1B2B_Targ_SB_CZ, 
                                    Diff_Post_1B2B_Targ_PA_CPZ, Diff_Post_1B2B_Targ_SB_CPZ, 
                                    Diff_Post_1B2B_Targ_PA_PZ, Diff_Post_1B2B_Targ_SB_PZ)
longformat.Diff_Post_1B2B_Targ.df <- melt(Diff_Post_1B2B_Targ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_1B2B_Targ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 2B-1B Target PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre to Post 2B Target - 1B Target PA and SB - CZ, CPZ, PZ
Diff_PrePost_1B2B_TargPASB.df <-data.frame(time, Diff_Pre_1B2B_Targ_PA_CZ, Diff_Pre_1B2B_Targ_SB_CZ, 
                                           Diff_Pre_1B2B_Targ_PA_CPZ, Diff_Pre_1B2B_Targ_SB_CPZ, 
                                           Diff_Pre_1B2B_Targ_PA_PZ, Diff_Pre_1B2B_Targ_SB_PZ, 
                                           Diff_Post_1B2B_Targ_PA_CZ, Diff_Post_1B2B_Targ_SB_CZ, 
                                           Diff_Post_1B2B_Targ_PA_CPZ, Diff_Post_1B2B_Targ_SB_CPZ, 
                                           Diff_Post_1B2B_Targ_PA_PZ, Diff_Post_1B2B_Targ_SB_PZ)
Diff_PrePost_1B2B_TargPASB.df <- melt(Diff_PrePost_1B2B_TargPASB.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_PrePost_1B2B_TargPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre and Post 1B2B Target PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))




#Difference wave Pre 2B NonTarget - 1B NonTarget PA and SB- CZ 
CZ_PA_CON_PRE_2B_NonTargets <- as.numeric(CZ_PA_CON_PRE_2B_NonTargets)
CZ_SB_CON_PRE_2B_NonTargets <-as.numeric (CZ_SB_CON_PRE_2B_NonTargets)
CZ_PA_CON_PRE_1B_NonTargets <- as.numeric(CZ_PA_CON_PRE_1B_NonTargets)
CZ_SB_CON_PRE_1B_NonTargets <-as.numeric (CZ_SB_CON_PRE_1B_NonTargets)

Diff_Pre_1B2B_NonTarg_PA_CZ <-CZ_PA_CON_PRE_2B_NonTargets-CZ_PA_CON_PRE_1B_NonTargets
Diff_Pre_1B2B_NonTarg_SB_CZ <- CZ_SB_CON_PRE_2B_NonTargets-CZ_SB_CON_PRE_1B_NonTargets

Diff_Pre_1B2B_NonTarg_CZ.df <-data.frame(time, Diff_Pre_1B2B_NonTarg_PA_CZ, Diff_Pre_1B2B_NonTarg_SB_CZ)
longformat.Diff_Pre_1B2B_NonTarg_CZ.df <- melt(Diff_Pre_1B2B_NonTarg_CZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_1B2B_NonTarg_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 2B-1B NonTarget PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre 2B NonTarget - 1B NonTarget PA and SB- CPZ
CPZ_PA_CON_PRE_2B_NonTargets <- as.numeric(CPZ_PA_CON_PRE_2B_NonTargets)
CPZ_SB_CON_PRE_2B_NonTargets <-as.numeric (CPZ_SB_CON_PRE_2B_NonTargets)
CPZ_PA_CON_PRE_1B_NonTargets <- as.numeric(CPZ_PA_CON_PRE_1B_NonTargets)
CPZ_SB_CON_PRE_1B_NonTargets <-as.numeric (CPZ_SB_CON_PRE_1B_NonTargets)

Diff_Pre_1B2B_NonTarg_PA_CPZ <-CPZ_PA_CON_PRE_2B_NonTargets-CPZ_PA_CON_PRE_1B_NonTargets
Diff_Pre_1B2B_NonTarg_SB_CPZ <- CPZ_SB_CON_PRE_2B_NonTargets-CPZ_SB_CON_PRE_1B_NonTargets

Diff_Pre_1B2B_NonTarg_CPZ.df <-data.frame(time, Diff_Pre_1B2B_NonTarg_PA_CPZ, Diff_Pre_1B2B_NonTarg_SB_CPZ)
longformat.Diff_Pre_1B2B_NonTarg_CPZ.df <- melt(Diff_Pre_1B2B_NonTarg_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_1B2B_NonTarg_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 2B-1B NonTarget PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Difference wave Pre 2B NonTarget - 1B NonTarget PA and SB- PZ
PZ_PA_CON_PRE_2B_NonTargets <- as.numeric(PZ_PA_CON_PRE_2B_NonTargets)
PZ_SB_CON_PRE_2B_NonTargets <-as.numeric (PZ_SB_CON_PRE_2B_NonTargets)
PZ_PA_CON_PRE_1B_NonTargets <- as.numeric(PZ_PA_CON_PRE_1B_NonTargets)
PZ_SB_CON_PRE_1B_NonTargets <-as.numeric (PZ_SB_CON_PRE_1B_NonTargets)

Diff_Pre_1B2B_NonTarg_PA_PZ <-PZ_PA_CON_PRE_2B_NonTargets-PZ_PA_CON_PRE_1B_NonTargets
Diff_Pre_1B2B_NonTarg_SB_PZ <- PZ_SB_CON_PRE_2B_NonTargets-PZ_SB_CON_PRE_1B_NonTargets

Diff_Pre_1B2B_NonTarg_PZ.df <-data.frame(time, Diff_Pre_1B2B_NonTarg_PA_PZ, Diff_Pre_1B2B_NonTarg_SB_PZ)
longformat.Diff_Pre_1B2B_NonTarg_PZ.df <- melt(Diff_Pre_1B2B_NonTarg_PZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_1B2B_NonTarg_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 2B-1B NonTarget PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre 2B NonTarget - 1B NonTarget PA and SB - CZ, CPZ, PZ
Diff_Pre_1B2B_Targ.df <-data.frame(time, Diff_Pre_1B2B_NonTarg_PA_CZ, Diff_Pre_1B2B_NonTarg_SB_CZ, 
                                   Diff_Pre_1B2B_NonTarg_PA_CPZ, Diff_Pre_1B2B_NonTarg_SB_CPZ, 
                                   Diff_Pre_1B2B_NonTarg_PA_PZ, Diff_Pre_1B2B_NonTarg_SB_PZ)
longformat.Diff_Pre_1B2B_Targ.df <- melt(Diff_Pre_1B2B_Targ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_1B2B_Targ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre 2B-1B NonTarget PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post 2B NonTarget - 1B NonTarget PA and SB- CZ 
CZ_PA_CON_POST_2B_NonTargets <- as.numeric(CZ_PA_CON_POST_2B_NonTargets)
CZ_SB_CON_POST_2B_NonTargets <-as.numeric (CZ_SB_CON_POST_2B_NonTargets)
CZ_PA_CON_POST_1B_NonTargets <- as.numeric(CZ_PA_CON_POST_1B_NonTargets)
CZ_SB_CON_POST_1B_NonTargets <-as.numeric (CZ_SB_CON_POST_1B_NonTargets)

Diff_Post_1B2B_NonTarg_PA_CZ <-CZ_PA_CON_POST_2B_NonTargets-CZ_PA_CON_POST_1B_NonTargets
Diff_Post_1B2B_NonTarg_SB_CZ <- CZ_SB_CON_POST_2B_NonTargets-CZ_SB_CON_POST_1B_NonTargets

Diff_Post_1B2B_NonTarg_CZ.df <-data.frame(time, Diff_Post_1B2B_NonTarg_PA_CZ, Diff_Post_1B2B_NonTarg_SB_CZ)
longformat.Diff_Post_1B2B_NonTarg_CZ.df <- melt(Diff_Post_1B2B_NonTarg_CZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_1B2B_NonTarg_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 2B-1B NonTarget PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post 2B NonTarget - 1B NonTarget PA and SB- CPZ
CPZ_PA_CON_POST_2B_NonTargets <- as.numeric(CPZ_PA_CON_POST_2B_NonTargets)
CPZ_SB_CON_POST_2B_NonTargets <-as.numeric (CPZ_SB_CON_POST_2B_NonTargets)
CPZ_PA_CON_POST_1B_NonTargets <- as.numeric(CPZ_PA_CON_POST_1B_NonTargets)
CPZ_SB_CON_POST_1B_NonTargets <-as.numeric (CPZ_SB_CON_POST_1B_NonTargets)

Diff_Post_1B2B_NonTarg_PA_CPZ <-CPZ_PA_CON_POST_2B_NonTargets-CPZ_PA_CON_POST_1B_NonTargets
Diff_Post_1B2B_NonTarg_SB_CPZ <- CPZ_SB_CON_POST_2B_NonTargets-CPZ_SB_CON_POST_1B_NonTargets

Diff_Post_1B2B_NonTarg_CPZ.df <-data.frame(time, Diff_Post_1B2B_NonTarg_PA_CPZ, Diff_Post_1B2B_NonTarg_SB_CPZ)
longformat.Diff_Post_1B2B_NonTarg_CPZ.df <- melt(Diff_Post_1B2B_NonTarg_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_1B2B_NonTarg_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 2B-1B NonTarget PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Difference wave Post 2B NonTarget - 1B NonTarget PA and SB- PZ
PZ_PA_CON_POST_2B_NonTargets <- as.numeric(PZ_PA_CON_POST_2B_NonTargets)
PZ_SB_CON_POST_2B_NonTargets <-as.numeric (PZ_SB_CON_POST_2B_NonTargets)
PZ_PA_CON_POST_1B_NonTargets <- as.numeric(PZ_PA_CON_POST_1B_NonTargets)
PZ_SB_CON_POST_1B_NonTargets <-as.numeric (PZ_SB_CON_POST_1B_NonTargets)

Diff_Post_1B2B_NonTarg_PA_PZ <-PZ_PA_CON_POST_2B_NonTargets-PZ_PA_CON_POST_1B_NonTargets
Diff_Post_1B2B_NonTarg_SB_PZ <- PZ_SB_CON_POST_2B_NonTargets-PZ_SB_CON_POST_1B_NonTargets

Diff_Post_1B2B_NonTarg_PZ.df <-data.frame(time, Diff_Post_1B2B_NonTarg_PA_PZ, Diff_Post_1B2B_NonTarg_SB_PZ)
longformat.Diff_Post_1B2B_NonTarg_PZ.df <- melt(Diff_Post_1B2B_NonTarg_PZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_1B2B_NonTarg_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 2B-1B NonTarget PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post 2B NonTarget - 1B NonTarget PA and SB - CZ, CPZ, PZ
Diff_Post_1B2B_NonTarg.df <-data.frame(time, Diff_Post_1B2B_NonTarg_PA_CZ, Diff_Post_1B2B_NonTarg_SB_CZ, 
                                    Diff_Post_1B2B_NonTarg_PA_CPZ, Diff_Post_1B2B_NonTarg_SB_CPZ, 
                                    Diff_Post_1B2B_NonTarg_PA_PZ, Diff_Post_1B2B_NonTarg_SB_PZ)
longformat.Diff_Post_1B2B_NonTarg.df <- melt(Diff_Post_1B2B_Targ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_1B2B_NonTarg.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post 2B-1B NonTarget PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre to Post 2B NonTarget - 1B NonTarget PA and SB - CZ, CPZ, PZ
Diff_PrePost_1B2B_NonTargPASB.df <-data.frame(time, Diff_Pre_1B2B_NonTarg_PA_CZ, Diff_Pre_1B2B_NonTarg_SB_CZ, 
                                           Diff_Pre_1B2B_NonTarg_PA_CPZ, Diff_Pre_1B2B_NonTarg_SB_CPZ, 
                                           Diff_Pre_1B2B_NonTarg_PA_PZ, Diff_Pre_1B2B_NonTarg_SB_PZ, 
                                           Diff_Post_1B2B_NonTarg_PA_CZ, Diff_Post_1B2B_NonTarg_SB_CZ, 
                                           Diff_Post_1B2B_NonTarg_PA_CPZ, Diff_Post_1B2B_NonTarg_SB_CPZ, 
                                           Diff_Post_1B2B_NonTarg_PA_PZ, Diff_Post_1B2B_NonTarg_SB_PZ)
longformat.Diff_PrePost_1B2B_NonTargPASB.df <- melt(Diff_PrePost_1B2B_NonTargPASB.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_PrePost_1B2B_NonTargPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre and Post 1B2B NonTarget PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))



#Import Data for FC
PA_CON_PRE_FC_Congruent <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/PRE/FC/Combined/Export_Excel/GrandAVG/SITlessPACONPREFCcongruent.xlsm")
PA_CON_PRE_FC_Incongruent <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/PRE/FC/Combined/Export_Excel/GrandAVG/SITlessPACONPREFCincongruent.xlsm")

PA_CON_POST_FC_Congruent <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/POST/FC/Combined/Export_Excel/GrandAVG/SITlessPACONPOSTFCcongruent.xlsm")
PA_CON_POST_FC_Incongruent <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/PA_CON/POST/FC/Combined/Export_Excel/GrandAVG/SITlessPACONPOSTFCincongruent.xlsm")

SB_CON_PRE_FC_Congruent <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/PRE/FC/Combined/Export_Excel/GrandAVG/SITlessSBCONPREFCcongruent.xlsm")
SB_CON_PRE_FC_Incongruent <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/PRE/FC/Combined/Export_Excel/GrandAVG/SITlessSBCONPREFCincongruent.xlsm")

SB_CON_POST_FC_Congruent <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/POST/FC/Combined/Export_Excel/GrandAVG/SITlessSBCONPOSTFCcongruent.xlsm")
SB_CON_POST_FC_Incongruent <- read_excel("/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/SB_CON/POST/FC/Combined/Export_Excel/GrandAVG/SITlessSBCONPOSTFCincongruent.xlsm")

#Call Variables for FC
#time 
#Call Variables
time <- (PA_CON_PRE_FC_Congruent$time)
time <-as.numeric(time) 

#PA_CON_PRE_FC_Congruent 
CZ_PA_CON_PRE_FC_Congruent <- (PA_CON_PRE_FC_Congruent$CZ)
CPZ_PA_CON_PRE_FC_Congruent <- (PA_CON_PRE_FC_Congruent$CPZ)
PZ_PA_CON_PRE_FC_Congruent <- (PA_CON_PRE_FC_Congruent$PZ)

#PA_CON_PRE_FC_Incongruent
CZ_PA_CON_PRE_FC_Incongruent <- (PA_CON_PRE_FC_Incongruent$CZ)
CPZ_PA_CON_PRE_FC_Incongruent <- (PA_CON_PRE_FC_Incongruent$CPZ)
PZ_PA_CON_PRE_FC_Incongruent <- (PA_CON_PRE_FC_Incongruent$PZ)

#PA_CON_POST_FC_Congruent 
CZ_PA_CON_POST_FC_Congruent <- (PA_CON_POST_FC_Congruent$CZ)
CPZ_PA_CON_POST_FC_Congruent <- (PA_CON_POST_FC_Congruent$CPZ)
PZ_PA_CON_POST_FC_Congruent <- (PA_CON_POST_FC_Congruent$PZ)

#PA_CON_POST_FC_Incongruent
CZ_PA_CON_POST_FC_Incongruent <- (PA_CON_POST_FC_Incongruent$CZ)
CPZ_PA_CON_POST_FC_Incongruent <- (PA_CON_POST_FC_Incongruent$CPZ)
PZ_PA_CON_POST_FC_Incongruent <- (PA_CON_POST_FC_Incongruent$PZ)

#SB_CON_PRE_FC_Congruent 
CZ_SB_CON_PRE_FC_Congruent <- (SB_CON_PRE_FC_Congruent$CZ)
CPZ_SB_CON_PRE_FC_Congruent <- (SB_CON_PRE_FC_Congruent$CPZ)
PZ_SB_CON_PRE_FC_Congruent <- (SB_CON_PRE_FC_Congruent$PZ)

#SB_CON_PRE_FC_Incongruent
CZ_SB_CON_PRE_FC_Incongruent <- (SB_CON_PRE_FC_Incongruent$CZ)
CPZ_SB_CON_PRE_FC_Incongruent <- (SB_CON_PRE_FC_Incongruent$CPZ)
PZ_SB_CON_PRE_FC_Incongruent <- (SB_CON_PRE_FC_Incongruent$PZ)

#SB_CON_POST_FC_Congruent 
CZ_SB_CON_POST_FC_Congruent <- (SB_CON_POST_FC_Congruent$CZ)
CPZ_SB_CON_POST_FC_Congruent <- (SB_CON_POST_FC_Congruent$CPZ)
PZ_SB_CON_POST_FC_Congruent <- (SB_CON_POST_FC_Congruent$PZ)

#SB_CON_POST_FC_Incongruent
CZ_SB_CON_POST_FC_Incongruent <- (SB_CON_POST_FC_Incongruent$CZ)
CPZ_SB_CON_POST_FC_Incongruent <- (SB_CON_POST_FC_Incongruent$CPZ)
PZ_SB_CON_POST_FC_Incongruent <- (SB_CON_POST_FC_Incongruent$PZ)


#pre Congruent and Incongruent waveforms for SB and PA on the same graph - CZ
PreCongIncongPASB_CZ.df <-data.frame(time, CZ_PA_CON_PRE_FC_Congruent, CZ_PA_CON_PRE_FC_Incongruent, CZ_SB_CON_PRE_FC_Congruent, CZ_SB_CON_PRE_FC_Incongruent)
longformat.PreCongIncongPASB_CZ.df <- melt(PreCongIncongPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreCongIncongPASB_CZ.df$value<-as.numeric(as.character(longformat.PreCongIncongPASB_CZ.df$value))

ggplot(longformat.PreCongIncongPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Pre Flanker Congruent and Incongruent Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#pre Congruent and Incongruent waveforms for SB and PA on the same graph - CPZ
PreCongIncongPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_PRE_FC_Congruent, CPZ_PA_CON_PRE_FC_Incongruent, CPZ_SB_CON_PRE_FC_Congruent, CPZ_SB_CON_PRE_FC_Incongruent)
longformat.PreCongIncongPASB_CPZ.df <- melt(PreCongIncongPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreCongIncongPASB_CPZ.df$value<-as.numeric(as.character(longformat.PreCongIncongPASB_CPZ.df$value))

ggplot(longformat.PreCongIncongPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Pre Flanker Congruent and Incongruent Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#pre Congruent and Incongruent waveforms for SB and PA on the same graph - PZ
PreCongIncongPASB_PZ.df <-data.frame(time, PZ_PA_CON_PRE_FC_Congruent, PZ_PA_CON_PRE_FC_Incongruent, PZ_SB_CON_PRE_FC_Congruent, PZ_SB_CON_PRE_FC_Incongruent)
longformat.PreCongIncongPASB_PZ.df <- melt(PreCongIncongPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreCongIncongPASB_PZ.df$value<-as.numeric(as.character(longformat.PreCongIncongPASB_PZ.df$value))

ggplot(longformat.PreCongIncongPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Pre Flanker Congruent and Incongruent Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#pre Congruent and Incongruent waveforms for SB and PA on the same graph - CZ, CPZ, PZ
PreCongIncongPASB.df <-data.frame(time, CZ_PA_CON_PRE_FC_Congruent, CZ_PA_CON_PRE_FC_Incongruent, CZ_SB_CON_PRE_FC_Congruent, CZ_SB_CON_PRE_FC_Incongruent,       
                                        CPZ_PA_CON_PRE_FC_Congruent, CPZ_PA_CON_PRE_FC_Incongruent, CPZ_SB_CON_PRE_FC_Congruent, CPZ_SB_CON_PRE_FC_Incongruent, 
                                        PZ_PA_CON_PRE_FC_Congruent, PZ_PA_CON_PRE_FC_Incongruent, PZ_SB_CON_PRE_FC_Congruent, PZ_SB_CON_PRE_FC_Incongruent)
longformat.PreCongIncongPASB.df <- melt(PreCongIncongPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreCongIncongPASB.df$value<-as.numeric(as.character(longformat.PreCongIncongPASB.df$value))

ggplot(longformat.PreCongIncongPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Pre Flanker Congruent and Incongruent Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#pre Congruent waveforms for SB and PA (a separate graph) - CZ
PreCongPASB_CZ.df <-data.frame(time, CZ_PA_CON_PRE_FC_Congruent, CZ_SB_CON_PRE_FC_Congruent)
longformat.PreCongPASB_CZ.df <- melt(PreCongPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreCongPASB_CZ.df$value<-as.numeric(as.character(longformat.PreCongPASB_CZ.df$value))

ggplot(longformat.PreCongPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Pre Flanker Congruent Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#pre Congruent waveforms for SB and PA (a separate graph) - CPZ
PreCongPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_PRE_FC_Congruent, CPZ_SB_CON_PRE_FC_Congruent)
longformat.PreCongPASB_CPZ.df <- melt(PreCongPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreCongPASB_CPZ.df$value<-as.numeric(as.character(longformat.PreCongPASB_CPZ.df$value))

ggplot(longformat.PreCongPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Pre Flanker Congruent Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#pre Congruent waveforms for SB and PA (a separate graph) - PZ
PreCongPASB_PZ.df <-data.frame(time, PZ_PA_CON_PRE_FC_Congruent, PZ_SB_CON_PRE_FC_Congruent)
longformat.PreCongPASB_PZ.df <- melt(PreCongPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreCongPASB_PZ.df$value<-as.numeric(as.character(longformat.PreCongPASB_PZ.df$value))

ggplot(longformat.PreCongPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Pre Flanker Congruent Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#pre Congruent waveforms for SB and PA (a separate graph) - CZ, CPZ, PZ
PreCongPASB.df <-data.frame(time, CZ_PA_CON_PRE_FC_Congruent, CZ_SB_CON_PRE_FC_Congruent,        
                                  CPZ_PA_CON_PRE_FC_Congruent, CPZ_SB_CON_PRE_FC_Congruent,
                                  PZ_PA_CON_PRE_FC_Congruent, PZ_SB_CON_PRE_FC_Congruent)
longformat.PreCongPASB.df <- melt(PreCongPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreCongPASB.df$value<-as.numeric(as.character(longformat.PreCongPASB.df$value))

ggplot(longformat.PreCongPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Pre Flanker Congruent Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#pre Incongruent waveforms for SB and PA (a separate graph) - CZ
PreCongPASB_CZ.df <-data.frame(time, CZ_PA_CON_PRE_FC_Incongruent, CZ_SB_CON_PRE_FC_Incongruent)
longformat.PreIncongPASB_CZ.df <- melt(PreCongPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreIncongPASB_CZ.df$value<-as.numeric(as.character(longformat.PreIncongPASB_CZ.df$value))

ggplot(longformat.PreIncongPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Pre Flanker Incongruent Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#pre Incongruent waveforms for SB and PA (a separate graph) - CPZ
PreCongPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_PRE_FC_Incongruent, CPZ_SB_CON_PRE_FC_Incongruent)
longformat.PreIncongPASB_CPZ.df <- melt(PreCongPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreIncongPASB_CPZ.df$value<-as.numeric(as.character(longformat.PreIncongPASB_CPZ.df$value))

ggplot(longformat.PreIncongPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Pre Flanker Incongruent Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#pre Incongruent waveforms for SB and PA (a separate graph) - PZ
PreCongPASB_PZ.df <-data.frame(time, PZ_PA_CON_PRE_FC_Incongruent, PZ_SB_CON_PRE_FC_Incongruent)
longformat.PreIncongPASB_PZ.df <- melt(PreCongPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreIncongPASB_PZ.df$value<-as.numeric(as.character(longformat.PreIncongPASB_PZ.df$value))

ggplot(longformat.PreIncongPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Pre Flanker Incongruent Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#pre Incongruent waveforms for SB and PA (a separate graph) - CZ, CPZ, PZ
PreCongPASB.df <-data.frame(time, CZ_PA_CON_PRE_FC_Incongruent, CZ_SB_CON_PRE_FC_Incongruent,        
                            CPZ_PA_CON_PRE_FC_Incongruent, CPZ_SB_CON_PRE_FC_Incongruent,
                            PZ_PA_CON_PRE_FC_Incongruent, PZ_SB_CON_PRE_FC_Incongruent)
longformat.PreIncongPASB.df <- melt(PreCongPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PreIncongPASB.df$value<-as.numeric(as.character(longformat.PreIncongPASB.df$value))

ggplot(longformat.PreIncongPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Pre Flanker Incongruent Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Congruent and Incongruent waveforms for SB and PA on the same graph - CZ
PostCongIncongPASB_CZ.df <-data.frame(time, CZ_PA_CON_POST_FC_Congruent, CZ_PA_CON_POST_FC_Incongruent, CZ_SB_CON_POST_FC_Congruent, CZ_SB_CON_POST_FC_Incongruent)
longformat.PostCongIncongPASB_CZ.df <- melt(PostCongIncongPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostCongIncongPASB_CZ.df$value<-as.numeric(as.character(longformat.PostCongIncongPASB_CZ.df$value))

ggplot(longformat.PostCongIncongPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Post Flanker Congruent and Incongruent Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Congruent and Incongruent waveforms for SB and PA on the same graph - CPZ
PostCongIncongPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_POST_FC_Congruent, CPZ_PA_CON_POST_FC_Incongruent, CPZ_SB_CON_POST_FC_Congruent, CPZ_SB_CON_POST_FC_Incongruent)
longformat.PostCongIncongPASB_CPZ.df <- melt(PostCongIncongPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostCongIncongPASB_CPZ.df$value<-as.numeric(as.character(longformat.PostCongIncongPASB_CPZ.df$value))

ggplot(longformat.PostCongIncongPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Post Flanker Congruent and Incongruent Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Congruent and Incongruent waveforms for SB and PA on the same graph - PZ
PostCongIncongPASB_PZ.df <-data.frame(time, PZ_PA_CON_POST_FC_Congruent, PZ_PA_CON_POST_FC_Incongruent, PZ_SB_CON_POST_FC_Congruent, PZ_SB_CON_POST_FC_Incongruent)
longformat.PostCongIncongPASB_PZ.df <- melt(PostCongIncongPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostCongIncongPASB_PZ.df$value<-as.numeric(as.character(longformat.PostCongIncongPASB_PZ.df$value))

ggplot(longformat.PostCongIncongPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Post Flanker Congruent and Incongruent Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Congruent and Incongruent waveforms for SB and PA on the same graph - CZ, CPZ, PZ
PostCongIncongPASB.df <-data.frame(time, CZ_PA_CON_POST_FC_Congruent, CZ_PA_CON_POST_FC_Incongruent, CZ_SB_CON_POST_FC_Congruent, CZ_SB_CON_POST_FC_Incongruent,       
                                   CPZ_PA_CON_POST_FC_Congruent, CPZ_PA_CON_POST_FC_Incongruent, CPZ_SB_CON_POST_FC_Congruent, CPZ_SB_CON_POST_FC_Incongruent, 
                                   PZ_PA_CON_POST_FC_Congruent, PZ_PA_CON_POST_FC_Incongruent, PZ_SB_CON_POST_FC_Congruent, PZ_SB_CON_POST_FC_Incongruent)
longformat.PostCongIncongPASB.df <- melt(PostCongIncongPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostCongIncongPASB.df$value<-as.numeric(as.character(longformat.PostCongIncongPASB.df$value))

ggplot(longformat.PostCongIncongPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Post Flanker Congruent and Incongruent Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#ROI
# ROI_PA.mean <- mean(variable1, variable2, variable 3)
# ROI_SB.mean <- mean(variable1, variable2, variable 3)
# ROI.df <- data.frame(time, ROI_PA.mean, ROI_SB.mean)
# longformat.ROI.df <- melt(ROI.df , id = "time", variable.name = "time", variable.type = numeric)
# longformat. ROI.df$value<-as.numeric(as.character(longformat. ROI.df $value))

#Post Congruent waveforms for SB and PA (a separate graph) - CZ
PostCongPASB_CZ.df <-data.frame(time, CZ_PA_CON_POST_FC_Congruent, CZ_SB_CON_POST_FC_Congruent)
longformat.PostCongPASB_CZ.df <- melt(PostCongPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostCongPASB_CZ.df$value<-as.numeric(as.character(longformat.PostCongPASB_CZ.df$value))

ggplot(longformat.PostCongPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Post Flanker Congruent Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Congruent waveforms for SB and PA (a separate graph) - CPZ
PostCongPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_POST_FC_Congruent, CPZ_SB_CON_POST_FC_Congruent)
longformat.PostCongPASB_CPZ.df <- melt(PostCongPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostCongPASB_CPZ.df$value<-as.numeric(as.character(longformat.PostCongPASB_CPZ.df$value))

ggplot(longformat.PostCongPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Post Flanker Congruent Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Congruent waveforms for SB and PA (a separate graph) - PZ
PostCongPASB_PZ.df <-data.frame(time, PZ_PA_CON_POST_FC_Congruent, PZ_SB_CON_POST_FC_Congruent)
longformat.PostCongPASB_PZ.df <- melt(PostCongPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostCongPASB_PZ.df$value<-as.numeric(as.character(longformat.PostCongPASB_PZ.df$value))

ggplot(longformat.PostCongPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Post Flanker Congruent Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Congruent waveforms for SB and PA (a separate graph) - CZ, CPZ, PZ
PostCongPASB.df <-data.frame(time, CZ_PA_CON_POST_FC_Congruent, CZ_SB_CON_POST_FC_Congruent,        
                             CPZ_PA_CON_POST_FC_Congruent, CPZ_SB_CON_POST_FC_Congruent,
                             PZ_PA_CON_POST_FC_Congruent, PZ_SB_CON_POST_FC_Congruent)
longformat.PostCongPASB.df <- melt(PostCongPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostCongPASB.df$value<-as.numeric(as.character(longformat.PostCongPASB.df$value))

ggplot(longformat.PostCongPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Post Flanker Congruent Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Post Incongruent waveforms for SB and PA (a separate graph) - CZ
PostCongPASB_CZ.df <-data.frame(time, CZ_PA_CON_POST_FC_Incongruent, CZ_SB_CON_POST_FC_Incongruent)
longformat.PostIncongPASB_CZ.df <- melt(PostCongPASB_CZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostIncongPASB_CZ.df$value<-as.numeric(as.character(longformat.PostIncongPASB_CZ.df$value))

ggplot(longformat.PostIncongPASB_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Post Flanker Incongruent Waveforms for PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Incongruent waveforms for SB and PA (a separate graph) - CPZ
PostCongPASB_CPZ.df <-data.frame(time, CPZ_PA_CON_POST_FC_Incongruent, CPZ_SB_CON_POST_FC_Incongruent)
longformat.PostIncongPASB_CPZ.df <- melt(PostCongPASB_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostIncongPASB_CPZ.df$value<-as.numeric(as.character(longformat.PostIncongPASB_CPZ.df$value))

ggplot(longformat.PostIncongPASB_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Post Flanker Incongruent Waveforms for PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Incongruent waveforms for SB and PA (a separate graph) - PZ
PostCongPASB_PZ.df <-data.frame(time, PZ_PA_CON_POST_FC_Incongruent, PZ_SB_CON_POST_FC_Incongruent)
longformat.PostIncongPASB_PZ.df <- melt(PostCongPASB_PZ.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostIncongPASB_PZ.df$value<-as.numeric(as.character(longformat.PostIncongPASB_PZ.df$value))

ggplot(longformat.PostIncongPASB_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Post Flanker Incongruent Waveforms for PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Post Incongruent waveforms for SB and PA (a separate graph) - CZ, CPZ, PZ
PostCongPASB.df <-data.frame(time, CZ_PA_CON_POST_FC_Incongruent, CZ_SB_CON_POST_FC_Incongruent,        
                             CPZ_PA_CON_POST_FC_Incongruent, CPZ_SB_CON_POST_FC_Incongruent,
                             PZ_PA_CON_POST_FC_Incongruent, PZ_SB_CON_POST_FC_Incongruent)
longformat.PostIncongPASB.df <- melt(PostCongPASB.df, id = "time", variable.name = "time", variable.type = numeric)
longformat.PostIncongPASB.df$value<-as.numeric(as.character(longformat.PostIncongPASB.df$value))

ggplot(longformat.PostIncongPASB.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Post Flanker Incongruent Waveforms for PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))


#Difference wave Pre Incongruent-Congruent for SB and PA conditions - CZ
CZ_PA_CON_PRE_FC_Congruent<- as.numeric(CZ_PA_CON_PRE_FC_Congruent)
CZ_PA_CON_PRE_FC_Incongruent<-as.numeric (CZ_PA_CON_PRE_FC_Incongruent)
CZ_SB_CON_PRE_FC_Congruent <- as.numeric(CZ_SB_CON_PRE_FC_Congruent)
CZ_SB_CON_PRE_FC_Incongruent <-as.numeric (CZ_SB_CON_PRE_FC_Incongruent)

Diff_Pre_FC_IncongCong_PA_CZ <-CZ_PA_CON_PRE_FC_Incongruent-CZ_PA_CON_PRE_FC_Congruent
Diff_Pre_FC_IncongCong_SB_CZ <- CZ_SB_CON_PRE_FC_Incongruent-CZ_SB_CON_PRE_FC_Congruent

Diff_Pre_FC_IncongCong_CZ.df <-data.frame(time, Diff_Pre_FC_IncongCong_PA_CZ, Diff_Pre_FC_IncongCong_SB_CZ)
longformat.Diff_Pre_FC_IncongCong_CZ.df <- melt(Diff_Pre_FC_IncongCong_CZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_FC_IncongCong_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre Flanker Incongruent - Congruent PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre Incongruent-Congruent for SB and PA conditions - CPZ
CPZ_PA_CON_PRE_FC_Congruent<- as.numeric(CPZ_PA_CON_PRE_FC_Congruent)
CPZ_PA_CON_PRE_FC_Incongruent<-as.numeric (CPZ_PA_CON_PRE_FC_Incongruent)
CPZ_SB_CON_PRE_FC_Congruent <- as.numeric(CPZ_SB_CON_PRE_FC_Congruent)
CPZ_SB_CON_PRE_FC_Incongruent <-as.numeric (CPZ_SB_CON_PRE_FC_Incongruent)

Diff_Pre_FC_IncongCong_PA_CPZ <-CPZ_PA_CON_PRE_FC_Incongruent-CPZ_PA_CON_PRE_FC_Congruent
Diff_Pre_FC_IncongCong_SB_CPZ <- CPZ_SB_CON_PRE_FC_Incongruent-CPZ_SB_CON_PRE_FC_Congruent

Diff_Pre_FC_IncongCong_CPZ.df <-data.frame(time, Diff_Pre_FC_IncongCong_PA_CPZ, Diff_Pre_FC_IncongCong_SB_CPZ)
longformat.Diff_Pre_FC_IncongCong_CPZ.df <- melt(Diff_Pre_FC_IncongCong_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_FC_IncongCong_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre Flanker Incongruent - Congruent PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre Incongruent-Congruent for SB and PA conditions - PZ
PZ_PA_CON_PRE_FC_Congruent<- as.numeric(PZ_PA_CON_PRE_FC_Congruent)
PZ_PA_CON_PRE_FC_Incongruent<-as.numeric (PZ_PA_CON_PRE_FC_Incongruent)
PZ_SB_CON_PRE_FC_Congruent <- as.numeric(PZ_SB_CON_PRE_FC_Congruent)
PZ_SB_CON_PRE_FC_Incongruent <-as.numeric (PZ_SB_CON_PRE_FC_Incongruent)

Diff_Pre_FC_IncongCong_PA_PZ <-PZ_PA_CON_PRE_FC_Incongruent-PZ_PA_CON_PRE_FC_Congruent
Diff_Pre_FC_IncongCong_SB_PZ <- PZ_SB_CON_PRE_FC_Incongruent-PZ_SB_CON_PRE_FC_Congruent

Diff_Pre_FC_IncongCong_PZ.df <-data.frame(time, Diff_Pre_FC_IncongCong_PA_PZ, Diff_Pre_FC_IncongCong_SB_PZ)
longformat.Diff_Pre_FC_IncongCong_PZ.df <- melt(Diff_Pre_FC_IncongCong_PZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_FC_IncongCong_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre Flanker Incongruent - Congruent PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre Incongruent-Congruent for SB and PA conditions - CZ, CPZ, PZ
Diff_Pre_FC_IncongCong.df <-data.frame(time, Diff_Pre_FC_IncongCong_PA_CZ, Diff_Pre_FC_IncongCong_SB_CZ,
                                       Diff_Pre_FC_IncongCong_PA_CPZ, Diff_Pre_FC_IncongCong_SB_CPZ,
                                       Diff_Pre_FC_IncongCong_PA_PZ, Diff_Pre_FC_IncongCong_SB_PZ)
longformat.Diff_Pre_FC_IncongCong.df <- melt(Diff_Pre_FC_IncongCong.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Pre_FC_IncongCong.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre Flanker Incongruent - Congruent PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post Incongruent-Congruent for SB and PA conditions - CZ
CZ_PA_CON_POST_FC_Congruent<- as.numeric(CZ_PA_CON_POST_FC_Congruent)
CZ_PA_CON_POST_FC_Incongruent<-as.numeric (CZ_PA_CON_POST_FC_Incongruent)
CZ_SB_CON_POST_FC_Congruent <- as.numeric(CZ_SB_CON_POST_FC_Congruent)
CZ_SB_CON_POST_FC_Incongruent <-as.numeric (CZ_SB_CON_POST_FC_Incongruent)

Diff_Post_FC_IncongCong_PA_CZ <-CZ_PA_CON_POST_FC_Incongruent-CZ_PA_CON_POST_FC_Congruent
Diff_Post_FC_IncongCong_SB_CZ <- CZ_SB_CON_POST_FC_Incongruent-CZ_SB_CON_POST_FC_Congruent

Diff_Post_FC_IncongCong_CZ.df <-data.frame(time, Diff_Post_FC_IncongCong_PA_CZ, Diff_Post_FC_IncongCong_SB_CZ)
longformat.Diff_Post_FC_IncongCong_CZ.df <- melt(Diff_Post_FC_IncongCong_CZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_FC_IncongCong_CZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post Flanker Incongruent - Congruent PA and SB at site CZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post Incongruent-Congruent for SB and PA conditions - CPZ
CPZ_PA_CON_POST_FC_Congruent<- as.numeric(CPZ_PA_CON_POST_FC_Congruent)
CPZ_PA_CON_POST_FC_Incongruent<-as.numeric (CPZ_PA_CON_POST_FC_Incongruent)
CPZ_SB_CON_POST_FC_Congruent <- as.numeric(CPZ_SB_CON_POST_FC_Congruent)
CPZ_SB_CON_POST_FC_Incongruent <-as.numeric (CPZ_SB_CON_POST_FC_Incongruent)

Diff_Post_FC_IncongCong_PA_CPZ <-CPZ_PA_CON_POST_FC_Incongruent-CPZ_PA_CON_POST_FC_Congruent
Diff_Post_FC_IncongCong_SB_CPZ <- CPZ_SB_CON_POST_FC_Incongruent-CPZ_SB_CON_POST_FC_Congruent

Diff_Post_FC_IncongCong_CPZ.df <-data.frame(time, Diff_Post_FC_IncongCong_PA_CPZ, Diff_Post_FC_IncongCong_SB_CPZ)
longformat.Diff_Post_FC_IncongCong_CPZ.df <- melt(Diff_Post_FC_IncongCong_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_FC_IncongCong_CPZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post Flanker Incongruent - Congruent PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post Incongruent-Congruent for SB and PA conditions - PZ
PZ_PA_CON_POST_FC_Congruent<- as.numeric(PZ_PA_CON_POST_FC_Congruent)
PZ_PA_CON_POST_FC_Incongruent<-as.numeric (PZ_PA_CON_POST_FC_Incongruent)
PZ_SB_CON_POST_FC_Congruent <- as.numeric(PZ_SB_CON_POST_FC_Congruent)
PZ_SB_CON_POST_FC_Incongruent <-as.numeric (PZ_SB_CON_POST_FC_Incongruent)

Diff_Post_FC_IncongCong_PA_PZ <-PZ_PA_CON_POST_FC_Incongruent-PZ_PA_CON_POST_FC_Congruent
Diff_Post_FC_IncongCong_SB_PZ <- PZ_SB_CON_POST_FC_Incongruent-PZ_SB_CON_POST_FC_Congruent

Diff_Post_FC_IncongCong_PZ.df <-data.frame(time, Diff_Post_FC_IncongCong_PA_PZ, Diff_Post_FC_IncongCong_SB_PZ)
longformat.Diff_Post_FC_IncongCong_PZ.df <- melt(Diff_Post_FC_IncongCong_PZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_FC_IncongCong_PZ.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post Flanker Incongruent - Congruent PA and SB at site PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre – Post Incongruent-Congruent for SB and PA conditions – CPZ
CPZ_PA_CON_PRE_FC_Congruent<- as.numeric(CPZ_PA_CON_PRE_FC_Congruent)
CPZ_PA_CON_PRE_FC_Incongruent<-as.numeric (CPZ_PA_CON_PRE_FC_Incongruent)
CPZ_SB_CON_PRE_FC_Congruent <- as.numeric(CPZ_SB_CON_PRE_FC_Congruent)
CPZ_SB_CON_PRE_FC_Incongruent <-as.numeric (CPZ_SB_CON_PRE_FC_Incongruent)
CPZ_PA_CON_POST_FC_Congruent<- as.numeric(CPZ_PA_CON_POST_FC_Congruent)
CPZ_PA_CON_POST_FC_Incongruent<-as.numeric (CPZ_PA_CON_POST_FC_Incongruent)
CPZ_SB_CON_POST_FC_Congruent <- as.numeric(CPZ_SB_CON_POST_FC_Congruent)
CPZ_SB_CON_POST_FC_Incongruent <-as.numeric (CPZ_SB_CON_POST_FC_Incongruent)

Diff_Pre_FC_IncongCong_PA_CPZ <-CPZ_PA_CON_PRE_FC_Incongruent-CPZ_PA_CON_PRE_FC_Congruent
Diff_Pre_FC_IncongCong_SB_CPZ <- CPZ_SB_CON_PRE_FC_Incongruent-CPZ_SB_CON_PRE_FC_Congruent
Diff_Post_FC_IncongCong_PA_CPZ <-CPZ_PA_CON_POST_FC_Incongruent-CPZ_PA_CON_POST_FC_Congruent
Diff_Post_FC_IncongCong_SB_CPZ <- CPZ_SB_CON_POST_FC_Incongruent-CPZ_SB_CON_POST_FC_Congruent

Diff_PrePost_FC_IncongCong_CPZ.df <-data.frame(time, Diff_Pre_FC_IncongCong_PA_CPZ , Diff_Pre_FC_IncongCong_SB_CPZ, Diff_Post_FC_IncongCong_PA_CPZ, Diff_Post_FC_IncongCong_SB_CPZ)
longformat.Diff_PrePost_FC_IncongCong_CPZ.df <- melt(Diff_PrePost_FC_IncongCong_CPZ.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_PrePost_FC_IncongCong_CPZ.df , aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Pre Post Flanker Incongruent - Congruent PA and SB at site CPZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Post Incongruent-Congruent for SB and PA conditions - CZ, CPZ, PZ
Diff_Post_FC_IncongCong.df <-data.frame(time, Diff_Post_FC_IncongCong_PA_CZ, Diff_Post_FC_IncongCong_SB_CZ,
                                        Diff_Post_FC_IncongCong_PA_CPZ, Diff_Post_FC_IncongCong_SB_CPZ,
                                        Diff_Post_FC_IncongCong_PA_PZ, Diff_Post_FC_IncongCong_SB_PZ)
longformat.Diff_Post_FC_IncongCong.df <- melt(Diff_Post_FC_IncongCong.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_Post_FC_IncongCong.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference wave Post Flanker Incongruent - Congruent PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))

#Difference wave Pre to Post Incongruent-Congruent for SB and PA conditions - CZ, CPZ, PZ
Diff_PrePost_FC_IncongCong.df <-data.frame(time, Diff_Pre_FC_IncongCong_PA_CZ, Diff_Pre_FC_IncongCong_SB_CZ,
                                           Diff_Pre_FC_IncongCong_PA_CPZ, Diff_Pre_FC_IncongCong_SB_CPZ,
                                           Diff_Pre_FC_IncongCong_PA_PZ, Diff_Pre_FC_IncongCong_SB_PZ,
                                           Diff_Post_FC_IncongCong_PA_CZ, Diff_Post_FC_IncongCong_SB_CZ,
                                          Diff_Post_FC_IncongCong_PA_CPZ, Diff_Post_FC_IncongCong_SB_CPZ,
                                          Diff_Post_FC_IncongCong_PA_PZ, Diff_Post_FC_IncongCong_SB_PZ)
longformat.Diff_PrePost_FC_IncongCong.df <- melt(Diff_PrePost_FC_IncongCong.df, id = "time", variable.name = "time", variable.type = numeric)

ggplot(longformat.Diff_PrePost_FC_IncongCong.df, aes(x=time, y=value, color = variable))+
  geom_line()+
  ylim(7, -10)+
  xlim(-200, 998)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Difference waves Pre and Post Flanker Incongruent - Congruent PA and SB at sites CZ, CPZ, PZ")+
  theme(plot.title = element_text(hjust = 0.5))




