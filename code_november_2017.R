library(dplyr)
library(readr)
library(cism)

# Read in the data
df <- read_csv('merged_data_sec.csv')

# Make date a R date object
df$date <- as.Date(df$vis_date, format = '%d-%b-%y')

# Arrange by date
df <- df %>% arrange(date)

# Remove those with no date (why are there any with no date?)
df <- df %>% filter(!is.na(date))

# Create a "dummy" id (just showing the order in the dataset)
df$dummy <- 1:nrow(df)

# Keep only the last (most recent) observation for each person
only_one <- df %>%
  group_by(permid) %>%
  filter(dummy == dplyr::last(dummy)) %>%
  ungroup %>%
  dplyr::select(-dummy)

# Write a new csv
write_csv(only_one, 'merged_data_sec_only_one.csv')

# Read in coordinates from Aura
coords <- read_csv('Coordenadas.csv')

# Convert lat and lon
ll <- cism::ll_from_utm(x = coords$LongUTM, y = coords$LatUTM)
coords <- cbind(coords, ll)
coords <- coords %>%
  rename(family_id = Family_id)
# Join lat/lon to our data
only_one <- only_one %>%
  left_join(coords,
            by = 'family_id')

# Now we have "x" and "y" columns for longitude and latitude in our dataset

# Plot
cism_map(lng = only_one$x,
         lat = only_one$y,
         fspdf = man3_fortified,
         x = only_one$case_control)

cism_map_interactive(lng = only_one$x,
         lat = only_one$y,
         spdf = man3,
         x = only_one$case_control,
         popup = only_one$permid)
