#install.packages("haven")
library("haven")
data2024 <- read_dta("anes_timeseries_2024_stata_20250808 (1).dta")
#data2020 <- read_dta("anes_timeseries_2020_stata_20220210.dta")

library(tidyverse)
library(rvest)
library(dplyr)
library(esquisse)
library(plotly)
library(knitr)
library(kableExtra)

nickData <- data2024 |>
  #Here are the 8 questions I need to get the voter data from this survey
  #V241035 asks "PRE: ALREADY VOTED IN GENERAL ELECTION"
  #V241036 asks "PRE: CONFIRMATION VOTED (EARLY) IN NOVEMBER 5 ELECTION"
  #V241038 asks "PRE: DID R VOTE FOR PRESIDENT"
  #V241039 asks "PRE: FOR WHOM DID R VOTE FOR PRESIDENT"
  #V241042 asks "PRE: DOES R INTEND TO VOTE FOR PRESIDENT"
  #V241043 asks "PRE: FOR WHOM DOES R INTEND TO VOTE FOR PRESIDENT"
  #V242065 asks "POST: DID R VOTE IN THE 2024 ELECTION"
  #V242066 asks "POST: DID R VOTE FOR PRESIDENT"
  #V242067 asks "POST: FOR WHOM DID R VOTE FOR PRESIDENT"
  select("V241035", "V241036", "V241038", "V241039", "V241042", "V241043", 
         "V242065", "V242066", "V242067") |>
  mutate(across(c(V241035, V241036, V241038, V241039, V241042, V241043, 
                  V242065, V242066, V242067), as.numeric))


earlyVoters <- nickData |>
  #Filter to those who answered and confirmed they voted early
  filter(V241035 == 1, V241036 == 1) |>
  #Select the question did they vote and if so who did they vote for
  select("V241038", "V241039") |>
  #Remove those who refused to answer who they voted for
  filter(V241039 != -9) |>
  #Select just the who they voted for column
  #Value will be -1 if inapplicable and because of filters only inapplicable if didn't vote
  #Values 1 through 6 are different candidates
  select(V241039) |>
  #Use count to get the number of answers each value has
  count(V241039) |>
  #Mutate to change V241039 to Candidate Names
  mutate(V241039 = recode(V241039,
                          `-1` = "Did Not Vote",
                          `1` = "Kamala Harris",
                          `2` = "Donald Trump",
                          `3` = "Robert F. Kennedy, Jr.",
                          `4` = "Cornel West",
                          `5` = "Jill Stein",
                          `6` = "Another Candidate")) |>
  #Use rename to change column name to Candidate
  rename("Candidate" = V241039)


intendedVoters <- nickData |>
  #Filter out those who voted early
  filter(V241035 == 2 | V241036 == 2) |>
  #Select the question will they vote and if so who will they vote for
  select("V241042", "V241043") |>
  #Remove those who refused to answer or didn't know if/who to vote
  filter(V241042 != -9, V241042 != -8, V241043 != -9, V241043 != -8) |>
  #Select just the who they will vote for column
  #Value will be -1 if inapplicable and because of filters only inapplicable if didn't vote
  #Values 1 through 6 are different candidates
  select(V241043) |>
  #Use count to get the number of answers each value has
  count(V241043) |>
  #Mutate to change V241043 to Candidate Names
  mutate(V241043 = recode(V241043,
                          `-1` = "Did Not Vote",
                          `1` = "Kamala Harris",
                          `2` = "Donald Trump",
                          `3` = "Robert F. Kennedy, Jr.",
                          `4` = "Cornel West",
                          `5` = "Jill Stein",
                          `6` = "Another Candidate")) |>
  #Use rename to change column name to Candidate
  rename("Candidate" = V241043)

preElectionData <- intendedVoters |>
  #Use full_join to combine early voter data to show all pre election data
  full_join(earlyVoters) |>
  #group by candidate and summarize n as sum of n to add the totals together
  group_by(Candidate) |>
  summarize(n = sum(n)) |>
  #Mutate to create new column seeing the proportion each candidate got
  mutate("Pre Election Proportion" = n / sum(n)) |>
  rename("Pre Election Voters" = n)


postElectionVoters <- nickData |>
  #Filter out those who voted early
  #Do this because they weren't asked these questions in post
  filter(V241035 == 2 | V241036 == 2) |>
  #Select the question did they vote, did they vote for president and if so who did they vote for
  select(V242065, V242066, V242067) |>
  #Filter out those who did not give an answer to whether they voted
  filter(V242065 != -9, V242065 != -8, V242065 != -7, V242065 != -6) |>
  #Select the did they vote for president and who they voted for president columns
  select(V242066, V242067) |>
  #Filter out those who did not give an answer for if/who they voted 
  filter(V242066 != -1, V242067 != -9, V242067 != -8) |>
  #Select just the who they voted for column
  #Value will be -1 if inapplicable and because of filters only inapplicable if didn't vote
  #Values 1 through 6 are different candidates
  select(V242067) |>
  #Use count to get the number of answers each value has
  count(V242067) |>
  #Mutate to change V242067 to Candidate Names
  mutate(V242067 = recode(V242067,
                          `-1` = "Did Not Vote",
                          `1` = "Kamala Harris",
                          `2` = "Donald Trump",
                          `4` = "Cornel West",
                          `5` = "Jill Stein",
                          `6` = "Another Candidate")) |>
  #Use rename to change column names to Candidate and Post Election Voters
  rename("Candidate" = V242067)

postElectionData <- postElectionVoters |>
  #Use full_join to combine early voter data to show all votes
  full_join(earlyVoters) |>
  #group by candidate and summarize n as sum of n to add the totals together
  group_by(Candidate) |>
  summarize(n = sum(n)) |>
  #Mutate to create new column seeing the proportion each candidate got
  mutate("Post Election Proportion" = n / sum(n)) |>
  rename("Post Election Voters" = n)

electionData <- postElectionData |>
  #Use full join by Candidate to combine all data into one data frame
  full_join(preElectionData, by = "Candidate") |>
  #Round the proportions to 4 digits
  mutate(across("Post Election Proportion", function(x) round(x, 4))) |>
  mutate(across("Pre Election Proportion", function(x) round(x, 4)))

#Use kable and kable_classic to title, align, change font, and alternate row color
electionData |>
  kable(
    caption = "Votes and Proportion of Votes by Candidate",
    align = "c"
  ) |>
  kable_classic(
    font_size = 18,
    lightable_options = "striped"
  )

electionDataPlot <- electionData |>
  #rename the columns to make them better names for plot
  rename("Before Election" = "Pre Election Proportion", 
         "Post Election" = "Post Election Proportion") |>
  #pivot longer to have all proportions in one column 
  #where the timing of the proportion is in another column
  pivot_longer(cols = c("Before Election", "Post Election"),
               names_to = "beforeOrPost", 
               values_to = "Proportion") 


#Create plot where x is the timing, y is the proportion, and it is grouped by candidate
ggplot(electionDataPlot, aes(x = beforeOrPost, y = Proportion, group = Candidate)) +
  #Use point to plot the proportions
  geom_point(aes(color = Candidate)) +
  #Use line to connect the points
  geom_line(aes(color = Candidate)) +
  #Use labs to give the plot a title, and rename the axes
  labs(
    title = "Pre and Post Election Proportion of Support by Candidate",
    x = "Before or Post Election",
    y = "Proportion of Votes",
    color = "Candidate") +
  theme_bw()

