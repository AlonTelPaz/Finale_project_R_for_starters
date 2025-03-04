#### Load data & packages ----

df <- read.csv (file = "Modified_Data.csv")
library (dplyr)
library (tidyverse)

#### Pre-processing data ----

df <- select (.data = df,
              c (Participant_number,
                 Age,
                 CBTS_score,
                 EPDS_score,
                 HADS_score,
                 Sleep_night_duration_bb1
                 )) # Select only the impotent variables

df <- df |>
  mutate(Sleep_night_duration_bb1 = na_if(Sleep_night_duration_bb1, 99.99)) # There is a missing data

## Build a function that build a new variable with factor of high or low then the median of CBTS score.

classify_CBTS <- function(df) {
  median_CBTS <- median(df$CBTS_score, na.rm = TRUE)
  
  df <- df |>
    mutate(
      `high_low_CBTS` = factor(
        ifelse(CBTS_score > median_CBTS, 1, 
               ifelse(CBTS_score < median_CBTS, 0, NA)),
        levels = c(0, 1),
        labels = c("low", "high")
      )
    )
  
  return(df)
}

df <- classify_CBTS(df)

## Build a function that build a new variable with factor of high or low then the median of EPDS score.

classify_EPDS <- function(df) {
  median_EPDS <- median(df$EPDS_score, na.rm = TRUE)
  
  df <- df |>
    mutate(
      `high_low_EPDS` = factor(
        ifelse(EPDS_score > median_EPDS, 1, 
               ifelse(EPDS_score < median_EPDS, 0, NA)),
        levels = c(0, 1),
        labels = c("low", "high")
      )
    )
  
  return(df)
}

df <- classify_EPDS(df)

## Build a function that build a new variable with factor of high or low then the median of HADS score.

classify_HADS <- function(df) {
  median_HADS <- median(df$HADS_score, na.rm = TRUE)
  
  df <- df |>
    mutate(
      `high_low_HADS` = factor(
        ifelse(HADS_score > median_HADS, 1, 
               ifelse(HADS_score < median_HADS, 0, NA)),
        levels = c(0, 1),
        labels = c("low", "high")
      )
    )
  
  return(df)
}

df <- classify_HADS(df)

## Delete every participant who have NA in one of the variables
df <- df |> 
  drop_na() 

#### Save processed data ----

write.csv(df, "./processed data.csv", row.names = FALSE)

#### End of the script, continue in the script: Part C - Data analysis  ----