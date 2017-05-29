library(dplyr)
library(reshape)

# import raw field data from working directory
Census <- read.csv("Data/Census_data.csv")
Individual <- read.csv("Data/Morphometric_data.csv")
Mating <- read.csv("Data/Morphometric_data.csv")

# filter dataset to include only individuals with number tags
Census_tagged <- filter(Census, !is.na(ID))

# join census dataframe with morphometric dataframe to get sexes
Census_tagged_sex <- left_join(Census_tagged, Individual, by = c("ID", "Population"))

# Make ID numbers unique to each population
Census_tagged_sex$ID_pop <- as.factor(paste(Census_tagged_sex$ID, Census_tagged_sex$Population, sep = ""))

Census_tagged_sex$Date_of_1st_session <- as.factor(Census_tagged_sex$Date_of_1st_session)

ID_date_pivot <- reshape::cast(Census_tagged_sex, ID_pop + Sex.y + Injury + Femur + Weight ~ Date_of_1st_session)

head(Census_tagged)
