#Script for CAPS 303 Term Project Part 2 Data Analysis

#First we must upload packages

library(tidyverse)
library(ez)        
library(psych)
library(ggplot2)
library(tidyr)
library(dplyr)

#Now lets begin making a data frame! For caffeiene vs. control and participants 1-7

# First we will list all numbers in each condition/ muscle and pre and post 

# Control Condition, for participants 1-7
Control_VM_Pre  <- c()  
Control_VL_Pre  <- c()
Control_VM_Post <- c()
Control_VL_Post <- c()

# Caffeine Condition
Caffeine_VM_Pre  <- c()
Caffeine_VL_Pre  <- c()
Caffeine_VM_Post <- c()
Caffeine_VL_Post <- c()

# Combine into one tidy dataframe
Participant <- 1:7

EMG_df <- data.frame(
  Participant,
  Control_VM_Pre,
  Control_VL_Pre,
  Control_VM_Post,
  Control_VL_Post,
  Caffeine_VM_Pre,
  Caffeine_VL_Pre,
  Caffeine_VM_Post,
  Caffeine_VL_Post)
EMG_df

#Out of this data frame we must isolate columns to form percentages of MVC or pre exercise with post
EMG_df$Control_VM_Post_Percent <- (EMG_df$Control_VM_Post / EMG_df$Control_VM_Pre) * 100
EMG_df$Control_VL_Post_Percent <- (EMG_df$Control_VL_Post / EMG_df$Control_VL_Pre) * 100

EMG_df$Caffeine_VM_Post_Percent <- (EMG_df$Caffeine_VM_Post / EMG_df$Caffeine_VM_Pre) * 100
EMG_df$Caffeine_VL_Post_Percent <- (EMG_df$Caffeine_VL_Post / EMG_df$Caffeine_VL_Pre) * 100

#For variability we will calculate SD in each condition and then print them out
SD_Control_VM  <- sd(EMG_df$Control_VM_Post_Percent)
SD_Control_VL  <- sd(EMG_df$Control_VL_Post_Percent)
SD_Caffeine_VM <- sd(EMG_df$Caffeine_VM_Post_Percent)
SD_Caffeine_VL <- sd(EMG_df$Caffeine_VL_Post_Percent)

SD_Control_VM
SD_Control_VL
SD_Caffeine_VM
SD_Caffeine_VL


#Anova for caffeine vs. control in VM
vm_data <- data.frame(
  Participant = factor(EMG_df$Participant),
  Condition = factor(rep(c("Control", "Caffeine"), each = nrow(EMG_df))),
  Fatigue = c(EMG_df$Control_VM_Post_Percent, EMG_df$Caffeine_VM_Post_Percent))

vm_anova <- aov(Fatigue ~ Condition + Error(Participant/Condition), data = vm_data)
summary(vm_anova)

#Anova for caffeine vs. control in VL
vl_data <- data.frame(
  Participant = factor(EMG_df$Participant),
  Condition = factor(rep(c("Control", "Caffeine"), each = nrow(EMG_df))),
  Fatigue = c(EMG_df$Control_VL_Post_Percent, EMG_df$Caffeine_VL_Post_Percent)
)

vl_anova <- aov(Fatigue ~ Condition + Error(Participant/Condition), data = vl_data)
summary(vl_anova)

#Anova for VM vs. VL
muscle_data <- data.frame(
  Participant = factor(rep(EMG_df$Participant, 4)),
  Muscle = factor(rep(c("VM", "VL"), each = 2 * nrow(EMG_df))),
  Condition = factor(rep(c("Control","Caffeine"), times = 2, each = nrow(EMG_df))),
  Fatigue = c(
    EMG_df$Control_VM_Post_Percent,
    EMG_df$Control_VL_Post_Percent,
    EMG_df$Caffeine_VM_Post_Percent,
    EMG_df$Caffeine_VL_Post_Percent))

muscle_anova <- aov(Fatigue ~ Muscle * Condition + Error(Participant/(Muscle*Condition)), 
                    data = muscle_data)
summary(muscle_anova)

# Reshape fatigue percentage columns into long format to compare VM and VL fatigue
fatigue_df <- EMG_df %>%
  select(Participant,
         Control_VM_Post_Percent,
         Control_VL_Post_Percent,
         Caffeine_VM_Post_Percent,
         Caffeine_VL_Post_Percent) %>%
  pivot_longer(
    cols = c(Control_VM_Post_Percent, Control_VL_Post_Percent,
             Caffeine_VM_Post_Percent, Caffeine_VL_Post_Percent),
    names_to = c("Condition", "Muscle", "Measure"),
    names_sep = "_",
    values_to = "Fatigue"
  )

# Change labels to be characters
fatigue_df$Condition <- factor(fatigue_df$Condition,
                               levels=c("Control","Caffeine"))

fatigue_df$Muscle <- factor(fatigue_df$Muscle,
                            levels=c("VM","VL"))

#Now lets make our figures for VM comparison of fatiguability

VM_plot<-ggplot(EMG_df, aes(x = factor(Participant))) +
  geom_bar(aes(y = Control_VM_Post_Percent, fill="Control"), stat="identity", position="dodge") +
  geom_bar(aes(y = Caffeine_VM_Post_Percent, fill="Caffeine"), stat="identity", position="dodge") +
  labs(title="VM %MVC Post-Exercise in Control vs Caffeine",
       y="%MVC", x="Participant") +
  scale_fill_manual(name="Condition", values=c("Control"="steelblue","Caffeine"="firebrick")) +
  theme_minimal()
VM_plot

#Figure for VL comparison of fatiguability
VL_plot<-ggplot(EMG_df, aes(x = factor(Participant))) +
  geom_bar(aes(y = Control_VL_Post_Percent, fill="Control"), stat="identity", position="dodge") +
  geom_bar(aes(y = Caffeine_VL_Post_Percent, fill="Caffeine"), stat="identity", position="dodge") +
  labs(title="VL %MVC Post-Exercise in Control vs Caffeine",
       y="%MVC", x="Participant") +
  scale_fill_manual(name="Condition", values=c("Control"="steelblue","Caffeine"="firebrick")) +
  theme_minimal()
VL_plot

# Make the figure to compare VL vs VM under both conditions (after pivot_longer)
Fatigue_Comparison_Plot <- ggplot(fatigue_df,
                                  aes(x = Muscle,
                                      y = Fatigue,
                                      fill = Condition)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Comparison of VM and VL Fatigability\nAcross Control and Caffeine Conditions",
       x = "Muscle",
       y = "%MVC Post (Relative to Pre)") +
  scale_fill_manual(values=c("Control"="steelblue","Caffeine"="firebrick")) +
  theme_minimal() +
  theme(text = element_text(size = 12))
Fatigue_Comparison_Plot