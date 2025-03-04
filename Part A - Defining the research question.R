#### Load packages ----
library (tidyverse)
library (ggplot2)
library (ggdist)
library (dplyr)

#### Upload raw data ----

df <- read.csv (file = "Meternal_state_baby_sleep_duration_raw_data.csv")
data.frame(df)

#### Exploratory data presentation ----
age_M <- ggplot(data = df, aes (x = Age)) +
  stat_halfeye (.width = c (0.7), fill = "lightblue") +
  xlab ("Age") +
  theme_classic()
print (age_M) #a plot that show the density of each age of the mother in the dataset

age_B <- ggplot(data = df, aes (x = Age)) +
  stat_density (fill = "lightblue") +
  xlab ("Age") +
  theme_classic()

df$sex_baby1 <- factor(df$sex_baby1, levels = c (1, 2), labels = c ("girl", "boy"))
sex_of_the_baby <- ggplot (
  data = df, aes (x = sex_baby1, fill = sex_baby1)) +
  geom_bar () +
  xlab ("sex of the baby") +
  ylab ("count")
  theme_minimal()
print(sex_of_the_baby) # A plot that show the number of the babys in each sex (girls = 212, boys = 198)

df$Age_bb <- factor (df$Age_bb, 
                        levels = c (1, 2, 3), 
                        labels = c ("3  months ≥ to > 6 months", "6 months ≥ to > 9 months", "9 months ≥ to > 12 months"))
Age_baby <- ggplot (
  data = df, aes (x = Age_bb, fill = Age_bb)) +
  geom_bar () +
  xlab ("Age of the baby") +
  ylab ("num") +
  theme_classic()
print (Age_baby) # A plot that show the number of the babys in each age group (1 = 147,2 = 133, 3 = 130)

df$Education <- factor (df$Education, 
                        levels = c (1, 2, 3, 4, 5), 
                        labels = c ("no education", "compulsory school", "post-compulsory education", "university of Applied Science or University Technology Degree", "university"))
education <- ggplot (
  data = df, aes (x = Education, fill = Education)) +
  geom_bar () +
  xlab ("type of education") +
  ylab ("num") +
  theme_classic()
print (education) # A plot that show the number of the every education status (1 = 2,2 = 25, 3 = 103, 4 = 88, 5 = 192)

### Because the questionnaires data are in separate items, i will combine them right now to show them.
## The maternal City Birth Trauma Scale scoring as total score from Q3-Q22 inclusive. Total range 0-60 (Total PTSD symptoms).
# The higher the score the more symptoms the mother have.
## The Edinburgh Postnatal Depression Scale scoring as total score of all 10 statements.
# The scale is between 0-3, the higher the score the more depression symptoms. items 3 & 5 - 10 are revers items.
# The total score is found by adding together the scores of each of the 10 items.
## The Hospital Anxiety and Depression Scale (anxiety sub-scale). The sub-scale have 7 Q.
# The scale is between 0-3, the higher the score the more anxiety symptoms.
# The total score is found by adding together the scores of each of the 7 items (0-7 = normal, 8-10 = Borderline abnormal, 11-21 = abnormal).
## The Very Short Form of the Infant Behavior Questionnaire-Revised (Negative Emotionality dimension) scoring as total score of all 10 statements.
# The scale is between 1-7, the higher the score the baby have more temperament behavior.
# The total score is found by adding together the scores of each of the 10 items.

#### Build the score of each questionnaire
## CBTS score
col_start <- "CBTS_M_3"
col_end <- "CBTS_22"

start_index <- which (names(df) == col_start)
end_index <- which( names(df) == col_end)

# New col for the sum
df$CBTS_score <- NA

for (i in 1:nrow(df)) {
  df$CBTS_score[i] <- sum(as.numeric(df[i, start_index:end_index]), na.rm = TRUE)
}

df <- df |>
  select(1:end_index, CBTS_score, (end_index + 1):ncol(df))

## EPDS score
col_start <- "EPDS_1"
col_end <- "EPDS_10"

start_index <- which (names(df) == col_start)
end_index <- which( names(df) == col_end)

# New col for the sum
df$EPDS_score <- NA

for (i in 1:nrow(df)) {
  df$EPDS_score[i] <- sum(as.numeric(df[i, start_index:end_index]), na.rm = TRUE)
}

df <- df |>
  select(1:end_index, EPDS_score, (end_index + 1):ncol(df))

## HADS score
col_start <- "HADS_1"
col_end <- "HADS_13"

start_index <- which (names(df) == col_start)
end_index <- which( names(df) == col_end)

# New col for the sum
df$HADS_score <- NA

for (i in 1:nrow(df)) {
  df$HADS_score[i] <- sum(as.numeric(df[i, start_index:end_index]), na.rm = TRUE)
}

df <- df |>
  select(1:end_index, HADS_score, (end_index + 1):ncol(df))

## IBQ_R score
col_start <- "IBQ_R_VSF_3_bb1"
col_end <- "IBQ_R_VSF_33_bb1"

start_index <- which (names(df) == col_start)
end_index <- which( names(df) == col_end)

# New col for the sum
df$IBQ_R_score <- NA

for (i in 1:nrow(df)) {
  df$IBQ_R_score[i] <- sum(as.numeric(df[i, start_index:end_index]), na.rm = TRUE)
}

df <- df |>
  select(1:end_index, IBQ_R_score, (end_index + 1):ncol(df))

#### Exploratory data presentation - continue ----

CBTS_density <- ggplot(data = df, aes (x = CBTS_score)) +
  stat_slab (fill = "black") +
  xlab ("CBTS_score") +
  ylab ("density") +
  theme_classic()
print (CBTS_density) #a plot that show the density of the CBTS score in the dataset

EPDS_density <- ggplot(data = df, aes (x = EPDS_score)) +
  geom_density (fill = "turquoise") +
  xlab ("EPDS_score") +
  ylab ("density") +
  theme_classic()
print (EPDS_density) #a plot that show the density of the EPDS score in the dataset

HADS_density <- ggplot(data = df, aes (x = HADS_score)) +
  geom_density (fill = "red") +
  xlab ("HADS_score") +
  ylab ("density") +
  theme_classic()
print (HADS_density) #a plot that show the density of the HADS score in the dataset

IBQ_R_plot <- ggplot (data = df,
                      aes (x = sex_baby1, y = IBQ_R_score, fill = sex_baby1)
                      ) +
  geom_boxplot (width = 0.2, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(color = sex_baby1), width = 0.15, size = 2, alpha = 0.6) +
  stat_halfeye(adjust = 0.5, width = 0.4, justification = -0.3, alpha = 0.5) +
  xlab ("sex of the baby") +
  ylab ("IQR_R_score") +
  theme_minimal()
print (IBQ_R_plot) #a plot that show the IQR_R_score saperate between sex of the baby

df$Sleep_night_duration_bb1 <- as.numeric(sub(":", ".", df$Sleep_night_duration_bb1))
df$Sleep_night_duration_bb1 <- as.numeric(sub(".3", ".5", df$Sleep_night_duration_bb1))
sleep_dur_density <- ggplot(data = df, aes (x = Sleep_night_duration_bb1)) +
  geom_density (fill = "purple") +
  xlim (0,13) +
  xlab ("sleep duration (7pm - 7am)") +
  ylab ("density") +
  theme_classic()
print (sleep_dur_density) #a plot that show the density of the number of sleep hours in the dataset

df$night_awakening_number_bb1 <- as.numeric(df$night_awakening_number_bb1)
num_awakening <- ggplot (
  data = df, aes (x = night_awakening_number_bb1)) +
  geom_bar () +
  xlab ("number of awakening") +
  ylab ("count")
theme_minimal()
print(num_awakening) # A plot that show the number of the babys in each sex (girls = 212, boys = 198)

##table of statistic values
summary_stats <- df |>
  summarise(
    mean_Age = mean (Age, na.rm = TRUE),
    sd_Age = sd (Age, na.rm = TRUE),
    mean_Gestationnal_age = mean (Gestationnal_age, na.rm = TRUE),
    sd_Gestationnal_age = sd (Gestationnal_age, na.rm = TRUE),
    mean_CBTS_score = mean (CBTS_score, na.rm = TRUE),
    sd_CBTS_score = sd (CBTS_score, na.rm = TRUE),
    mean_EPDS_score = mean (EPDS_score, na.rm = TRUE),
    sd_EPDS_score = sd (EPDS_score, na.rm = TRUE),
    mean_HADS_score= mean (HADS_score, na.rm = TRUE),
    sd_HADS_score = sd (HADS_score, na.rm = TRUE),
    mean_IBQ_R_score = mean (IBQ_R_score, na.rm = TRUE),
    sd_IBQ_R_score = sd (IBQ_R_score, na.rm = TRUE),
    mean_Sleep_night_duration_bb1 = mean (Sleep_night_duration_bb1, na.rm = TRUE),
    sd_Sleep_night_duration_bb1 = sd (Sleep_night_duration_bb1, na.rm = TRUE),
    mean_night_awakening_number_bb1 = mean (night_awakening_number_bb1, na.rm = TRUE),
    sd_night_awakening_number_bb1 = sd (night_awakening_number_bb1, na.rm = TRUE)
    )
print (summary_stats)

# Save df after measure the scores of the questionnaires

write.csv(df, "./Modified_Data.csv", row.names = FALSE)

#### end of the script, continue in the script: Part B - Date pre-processing  ----