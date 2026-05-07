library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(haven)

anes_data_2024 <- read_dta("C:\\Users\\atrac\\Downloads\\anes_timeseries_2024_stata_20250808 (1).dta")

religion_data <- anes_data_2024 |>
  select(
    party_vote = V242067,
    religion = V241422,
    importance = V241420,
    attendance = V241440
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
    !party_vote %in% c(-9, -8, -1),
    !religion %in% c(-9, -8, -1),
    !importance %in% c(-9, -8, -1),
    !attendance %in% c(-9, -8, -1)
  ) |>
  
  #Apply recodes
  mutate(
    party = recode_party(party_vote),
    religion = recode_religion(religion),
    importance = recode_importance(importance),
    attendance = recode_attendance(attendance)
  ) |>
  
  drop_na() |>
  
  #Set ordered factors for plotting
  mutate(
    importance = factor(
      importance,
      levels = c(
        "Not Important",
        "A Little Important",
        "Moderately Important",
        "Very Important",
        "Extremely Important"
      )
    ),
    
    attendance = factor(
      attendance,
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





religion_table |>
  kable(
    caption = "Party Vote by Religion (Democrat vs Republican)",
    align = "c"
  ) |>
  kable_classic(
    font_size = 12,
    lightable_options = "striped"
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position"))


importance_table |>
  kable(
    caption = "Party Vote by Importance of Religion (Democrat vs Republican)",
    align = "c"
  ) |>
  kable_classic(
    font_size = 12,
    lightable_options = "striped"
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position"))


attendance_table |>
  kable(
    caption = "Party Vote by Religious Attendance (Democrat vs Republican)",
    align = "c"
  ) |>
  kable_classic(
    font_size = 12,
    lightable_options = "striped"
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position"))





religion_plot <- religion_table |>
  
  ggplot(aes(
    x = religion,
    y = ifelse(party == "Democrat", -proportion, proportion),
    fill = party
  )) +
  
  geom_col() +
  
  scale_fill_manual(values = c(
    "Democrat" = "blue",
    "Republican" = "red"
  )) +
  
  scale_y_continuous(
    breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
    labels = c("75%", "50%", "25%", "0", "25%", "50%", "75%"),
    name = "Proportion of Votes"
  ) +
  
  labs(
    title = "Party Vote by Religion",
    x = "Religion",
    fill = "Party"
  ) +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

religion_plot


importance_plot <- importance_table |>
  
  ggplot(aes(
    x = importance,
    y = ifelse(party == "Democrat", -proportion, proportion),
    fill = party
  )) +
  
  geom_col() +
  
  scale_fill_manual(values = c(
    "Democrat" = "blue",
    "Republican" = "red"
  )) +
  
  scale_y_continuous(
    breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
    labels = c("75%", "50%", "25%", "0", "25%", "50%", "75%"),
    name = "Proportion of Votes"
  ) +
  
  labs(
    title = "Party Vote by Importance of Religion",
    x = "Importance of Religion",
    fill = "Party"
  ) +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

importance_plot


attendance_plot <- attendance_table |>
  
  ggplot(aes(
    x = attendance,
    y = ifelse(party == "Democrat", -proportion, proportion),
    fill = party
  )) +
  
  geom_col() +
  
  scale_fill_manual(values = c(
    "Democrat" = "blue",
    "Republican" = "red"
  )) +
  
  scale_y_continuous(
    breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
    labels = c("75%", "50%", "25%", "0", "25%", "50%", "75%"),
    name = "Proportion of Votes"
  ) +
  
  labs(
    title = "Party Vote by Religious Attendance",
    x = "Attendance",
    fill = "Party"
  ) +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

attendance_plot


religion_long <- religion_table |>
  mutate(variable = "Religion") |>
  rename(category = religion) |>
  
  bind_rows(
    importance_table |>
      mutate(variable = "Importance") |>
      rename(category = importance)
  ) |>
  
  bind_rows(
    attendance_table |>
      mutate(variable = "Attendance") |>
      rename(category = attendance)
  )


combined_plot <- religion_long |>
  
  ggplot(aes(
    x = category,
    y = ifelse(party == "Democrat", -proportion, proportion),
    fill = party
  )) +
  
  geom_col() +
  
  facet_wrap(~ variable, scales = "free_x") +
  
  scale_fill_manual(values = c(
    "Democrat" = "blue",
    "Republican" = "red"
  )) +
  
  scale_y_continuous(
    breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
    labels = c("75%", "50%", "25%", "0", "25%", "50%", "75%"),
    name = "Proportion of Votes"
  ) +
  
  labs(
    title = "Party Vote by Religious Characteristics",
    x = NULL,
    fill = "Party"
  ) +
  
  theme_bw() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

combined_plot

