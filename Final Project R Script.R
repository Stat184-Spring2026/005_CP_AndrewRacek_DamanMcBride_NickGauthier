library(tidyverse)
library(rvest)
library(dplyr)
library(esquisse)
library(plotly)
library(knitr)
library(kableExtra)
library(haven)

anes_data_2024 <- read_dta("C:\\Users\\atrac\\Downloads\\anes_timeseries_2024_stata_20250808 (1).dta")

religion_data <- anes_data_2024 |>
  select(
    Party_vote = V242067,
    Religion = V241422,
    Importance = V241420,
    Attendance = V241440
  ) |>
  mutate(across(everything(), as.numeric))

#Recode party vote
recode_party <- function(x) {
  recode(x,
         `1` = "Democrat",
         `2` = "Republican",
         `3` = "Other",
         `4` = "Other",
         `5` = "Other",
         `6` = "Other",
         .default = NA_character_
  )
}


#Recode religion
recode_religion <- function(x) {
  recode(x,
         `1` = "Protestant",
         `2` = "Roman Catholic",
         `3` = "Orthodox Christian",
         `4` = "Latter-Day Saints (LDS)",
         `5` = "Jewish",
         `6` = "Muslim",
         `7` = "Buddhist",
         `8` = "Hindu",
         `9` = "Atheist/Agnostic",
         `10` = "Atheist/Agnostic",
         `11` = "Something Else",
         `12` = "Nothing in Particular",
         .default = NA_character_
  )
}


#Recode importance of religion
recode_importance <- function(x) {
  recode(x,
         `1` = "Extremely Important",
         `2` = "Very Important",
         `3` = "Moderately Important",
         `4` = "A Little Important",
         `5` = "Not Important",
         .default = NA_character_
  )
}


#Recode attendance
recode_attendance <- function(x) {
  recode(x,
         `1` = "More than once a week",
         `2` = "Once a week",
         `3` = "Once or twice a month",
         `4` = "A few times a year",
         `5` = "Once a year",
         `6` = "Never",
         .default = NA_character_
  )
}

religion_clean <- religion_data |>
  
  #Remove refused, don't know, and inapplicable responses
  filter(
    !Party_vote %in% c(-9, -8, -1),
    !Religion %in% c(-9, -8, -1),
    !Importance %in% c(-9, -8, -1),
    !Attendance %in% c(-9, -8, -1)
  ) |>
  
  #Apply recodes
  mutate(
    Party = recode_party(Party_vote),
    Religion = recode_religion(Religion),
    Importance = recode_importance(Importance),
    Attendance = recode_attendance(Attendance)
  ) |>
  
  #Set ordered factors for plotting
  mutate(
    Importance = factor(
      Importance,
      levels = c(
        "Not Important",
        "A Little Important",
        "Moderately Important",
        "Very Important",
        "Extremely Important"
      )
    ),
    
    Attendance = factor(
      Attendance,
      levels = c(
        "Never",
        "Once a year",
        "A few times a year",
        "Once or twice a month",
        "Once a week",
        "More than once a week"
      )
    )
  )