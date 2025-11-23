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

VM_means <- data.frame(
  Condition = c("Control", "Caffeine"),
  Mean = c(
    mean(EMG_df$Control_VM_Post_Percent, na.rm = TRUE),
    mean(EMG_df$Caffeine_VM_Post_Percent, na.rm = TRUE)),
  SD = c(
    SD_Control_VM,
    SD_Caffeine_VM))

VM_plot_mean <- ggplot(VM_means,
                       aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean - SD,
                    ymax = Mean + SD),
                width = 0.15) +
  labs(title="Average VM Fatigability (Control vs Caffeine)",
       x="Condition", y="Mean %MVC Post") +
  scale_fill_manual(values=c("Control"="steelblue","Caffeine"="firebrick")) +
  theme_minimal()
VM_plot_mean


#Figure for VL comparison of fatiguability
VL_means <- data.frame(
  Condition = c("Control", "Caffeine"),
  Mean = c(
    mean(EMG_df$Control_VL_Post_Percent, na.rm = TRUE),
    mean(EMG_df$Caffeine_VL_Post_Percent, na.rm = TRUE)),
  SD = c(
    SD_Control_VL,
    SD_Caffeine_VL))

VL_plot_mean <- ggplot(VL_means,
                       aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean - SD,
                    ymax = Mean + SD),
                width = 0.15) +
  labs(title="Average VL Fatigability (Control vs Caffeine)",
       x="Condition", y="Mean %MVC Post") +
  scale_fill_manual(values=c("Control"="steelblue","Caffeine"="firebrick")) +
  theme_minimal()
VL_plot_mean


# Make the figure to compare VL vs VM under both conditions (after pivot_longer)

fatigue_summary <- data.frame(
  Muscle = c("VM", "VM", "VL", "VL"),
  Condition = c("Control", "Caffeine", "Control", "Caffeine"),
  Mean = c(
    mean(EMG_df$Control_VM_Post_Percent, na.rm = TRUE),
    mean(EMG_df$Caffeine_VM_Post_Percent, na.rm = TRUE),
    mean(EMG_df$Control_VL_Post_Percent, na.rm = TRUE),
    mean(EMG_df$Caffeine_VL_Post_Percent, na.rm = TRUE)),
  SD = c(
    SD_Control_VM,
    SD_Caffeine_VM,
    SD_Control_VL,
    SD_Caffeine_VL))

Fatigue_Comparison_MeanPlot <- ggplot(fatigue_summary,
                                      aes(x = Muscle, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.7),
           width = 0.6) +
  geom_errorbar(aes(ymin = Mean - SD,
                    ymax = Mean + SD),
                width = 0.15,
                position = position_dodge(width = 0.7)) +
  labs(title="Mean VM vs VL Fatigability (Control vs Caffeine)",
       x="Muscle", y="Mean %MVC Post (Relative to Pre)") +
  scale_fill_manual(values=c("Control"="steelblue", "Caffeine"="firebrick")) +
  theme_minimal()
Fatigue_Comparison_MeanPlot