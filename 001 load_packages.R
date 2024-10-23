
# Load packages -----------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(forcats)


# Create useful functions --------------------------------------------------------


myfunction <- function() {
  df <- df |> mutate(Sex = if_else(Female == 1, "Female", "Male"), .after = Female) |> mutate(AHI = factor(AHI))
  return(df)
}
df <- myfunction()
