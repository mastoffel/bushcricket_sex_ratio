library(dplyr)

# load data
census <- read.csv("data/raw/census_data.csv")
morphometrics <- read.csv("data/raw/morphometric_data.csv")

# load mating and add treatment
mating <- read.csv("data/raw/mating_data.csv", stringsAsFactors = FALSE, na.strings = "") %>% 
    mutate(sex_ratio = as.factor(ifelse(Population == "A" | Population == "E" | Population == "G", 0.5,
        ifelse(Population == "B" | Population == "F" | Population == "H", 0.75, 0.25))))


str(mating)

mating2 <- mating %>% 
                group_by(sex_ratio, Population, Day_of_1st_session) %>%
                summarise(matings = sum(Mating, na.rm = TRUE)) 

library(ggplot2)
d <- ggplot(mating2, aes(as.factor(Day_of_1st_session), matings, colour = sex_ratio)) + geom_point()
d + stat_summary(fun.data = "mean_cl_boot", size = 1)

mating3 <-  mating %>% 
    group_by(sex_ratio) %>%
    summarise(matings = sum(Mating, na.rm = TRUE)) 
