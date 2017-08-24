# Set up packages
library(tidyverse)
library(readstata13)

# Read data
mira <- read.dta13('mira.dta')

# Define cases and controls, per Pedro's comment:
# Malaria case was defined as a patient with 2 or more malaria episodes
# Controls are the ones with only 1 episode of malaria....

# Create and object called cc
cc <- mira %>%
  # group by each person
  group_by(perm_id) %>%
  # calculate how many episodes of malaria they had
  summarise(episodes_of_malaria = sum(malaria, na.rm = TRUE)) %>%
  ungroup %>%
  # Define anybody with more than 1 episode as a case
  # and anybody with only 1 episode as a control
  mutate(case_control = ifelse(episodes_of_malaria > 1,
                               'case',
                               ifelse(episodes_of_malaria == 1,
                                      'control',
                                      'other')))

# Now we have an object named cc which tells us exactly the case/control
# status of each invidiual. We'll simply merge that object with mira,
# which will give us our ORIGINAL dataset, plus two new columns:
# - episodes_of_malaria (the total number of malaria episodes of that person) and
# - case_control (whether that person is a "case", "control", or "other")
# ("other" means someone with 0 malaria episodes)
mira <-
  left_join(mira,
            cc,
            by = 'perm_id')

# Having joined all the data, we'll write out a new dataset
# for further analysis in whatever software you want to use
write_csv(mira,
          'mira_with_case_control_status.csv')

# (the above csv can be read into R or stata)
