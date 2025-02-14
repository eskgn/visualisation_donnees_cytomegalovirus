#Enis Kaygun

library(readxl)
data <- read_excel("C:/Users/enisk/Desktop/python2/Cytomegalovirus.xlsx")
head(data)
library(ggplot2)
library(dplyr)
library(tidyr)

#Utilisation plus lisible des données qualitatives
data <- data %>%
  mutate(
    sex = recode(sex, `0` = "Female", `1` = "Male"),
    race = recode(race, `0` = "African American", `1` = "White"),
    `diagnosis type` = recode(`diagnosis type`, `0` = "Lymphoid", `1` = "Myeloid"),
    `prior radiation` = recode(`prior radiation`, `0` = "No", `1` = "Yes"),
    `prior transplant` = recode(`prior transplant`, `0` = "No", `1` = "Yes"),
    `recipient cmv` = recode(`recipient cmv`, `0` = "Negative", `1` = "Positive"),
    `donor cmv` = recode(`donor cmv`, `0` = "Negative", `1` = "Positive"),
    `donor sex` = recode(`donor sex`, `0` = "Female", `1` = "Male"),
    `C1/C2` = recode(`C1/C2`, `0` = "Heterozygous", `1` = "Homozygous"),
    cmv = recode(cmv, `0` = "No", `1` = "Yes"),
    agvhd = recode(agvhd, `0` = "No", `1` = "Yes"),
    cgvhd = recode(cgvhd, `0` = "No", `1` = "Yes")
  )


# Mettre les données en format long
df_long <- data %>%
  pivot_longer(cols = c("time to cgvhd", "time to agvhd"),
               names_to = "GVHD_Type", values_to = "Time")

# Boxplot 1

ggplot(df_long, aes(x = Time, y = factor(aKIRs), fill = GVHD_Type)) +
  geom_boxplot() +
  labs(
    title = "Comparison of GVHD type durations based on aKIRs",
    x = "Time (months)",
    y = "Number of activating KIR receptors (aKIRs)",
    fill = "Type of GVHD"
  ) +
  scale_fill_manual(values = c("#aec7e8", "#ffbb78")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + #Centrer le titre
  geom_boxplot(outlier.shape = 8) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1))  #Rotation des étiquettes sur l'axe Y


#Calcul des effectifs
effectifs <- data %>%
group_by(sex) %>%
summarise(n = n())

# Boxplot 2

ggplot(data, aes(x = `time to cmv`, y = sex, fill = sex)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA, alpha = 0.7) + 
  labs(
    title = "Time until cytomegalovirus (CMV) reactivation based on gender",
    x = "Time until CMV reactivation (months)",
    y = "Sex",
    fill = "Sex"
  ) +
  scale_fill_manual(values = c("lightcoral", "lightblue")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + #Centrer le titre
  theme(axis.text.y = element_text(angle = 0, hjust = 1),  #Rotation des étiquettes sur l'axe des Y
    legend.position = "none") +  #Supprimer la légende
  geom_text(data = effectifs, aes(x = 90, y = sex, label = paste("n =", n)), 
          color = "black", size = 4, hjust = 0,
          position = position_nudge(x = 8))