library(dplyr)

library(wesanderson)
## morphometrics data
morphometrics <- read.csv("data/raw/morphometric_data.csv") %>% 
                        mutate(popid = paste0(Population, ID)) %>% # creates unique id
                        mutate(Sex = as.character(Sex))
# load census 
census <- read.csv("data/raw/census_data.csv") %>% 
                        rename(session_day = Date_of_1st_session) %>% 
                        mutate(popid = paste0(Population, ID)) %>% 
                        mutate(sex_ratio = as.factor(ifelse(Population == "A" | Population == "E" | Population == "G", 0.5,
                        ifelse(Population == "B" | Population == "F" | Population == "H", 0.75, 0.25))))# creates unique id
                        census[is.na(census$ID), "popid"] <- NA
                        
# fill in sex variable
fill_in_sex <- function(x){
    if (x["Sex"] == "") {
        if (x["popid"] %in% (morphometrics$popid)){
        rownum <- which(morphometrics$popid == x["popid"])
        x["Sex"] <- morphometrics[rownum, "Sex"]
        }
    }
    x
}

census <- as.data.frame(t(apply(census, 1, fill_in_sex)))

# load mating and add treatment factor
mating <- read.csv("data/raw/mating_data.csv", stringsAsFactors = FALSE, na.strings = "") %>% 
                                 mutate(sex_ratio = as.factor(ifelse(Population == "A" | Population == "E" | Population == "G", 0.5,
                                 ifelse(Population == "B" | Population == "F" | Population == "H", 0.75, 0.25)))) %>%
                                 rename(session_day = Day_of_1st_session) 

# transform all variable names to lowercase
out <- lapply(list(census, mating, morphometrics), function(x){
    names(x) <- tolower(names(x))
    x
}) 

# reassign to dataframes
census <- out[[1]]
mating <- out[[2]]
morphometrics <- out[[3]]

# transform variables to factor
mating <- mating %>% mutate(population = as.factor(population))

mating_over_time <- mating %>% 
                group_by(sex_ratio, session_day, population) %>%
                summarise(matings = sum(mating, na.rm = TRUE)) 

means <- aggregate(matings ~ sex_ratio, mating_over_time, mean)

library(ggthemes)
library(ggplot2)
library(ggrepel)

d <- ggplot(mating_over_time, aes(x = sex_ratio, y = matings)) + 
    geom_boxplot(outlier.shape = NA, lwd = 0.5) + 
    geom_jitter(aes(color = population), alpha = 0.7, size = 2) +
    scale_y_continuous(breaks=0:9) +
    #stat_summary(fun.y=mean, colour="red", geom="point", 
    #     size=3) +
    xlab("sex ratio") +
    ylab("number of matings per night") +
    # scale_color_manual(values = wes_palette("GrandBudapest")) + 
    theme_classic() +
    theme(axis.line.x = element_line(colour = 'grey', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'grey', size=0.5, linetype='solid'),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        axis.title.x=element_text(margin=margin(20,0,0,0)))

ggplot2::ggsave(d, 
    filename = "matings_per_sexratio.jpg", 
    #  path = "D:/recent R",
    width = 14,
    scale = 0.6,
    height = 9, units = "in",
    dpi = 300)

   # geom_text(data = means, aes(label = matings, y = matings - 0.5))
just_mating <- mating %>% filter(mating == 1) %>%
                mutate(time_of_day = ifelse(time > 1000, "21-24", (ifelse((time > 0 & time < 300), "0-3", "3-6"))))

just_mating$time_of_day <- as.factor(just_mating$time_of_day )
just_mating$time_of_day <- factor(just_mating$time_of_day ,levels(just_mating$time_of_day )[c(2,1,3)])

d <- ggplot(just_mating, aes(sex_ratio)) + 
    geom_bar(aes(fill=time_of_day)) +
    xlab("sex ratio") +
    ylab("overall number of matings") +
    theme_classic() +
    theme(axis.line.x = element_line(colour = 'grey', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'grey', size=0.5, linetype='solid'),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        axis.title.x=element_text(margin=margin(20,0,0,0))) +
    scale_fill_manual(values = wes_palette("Chevalier")) 

ggplot2::ggsave(d, 
    filename = "overall_matings.jpg", 
    #  path = "D:/recent R",
    width = 14,
    scale = 0.6,
    height = 9, units = "in",
    dpi = 300)

library(ggplot2)
d <- ggplot(mating_over_time, aes(session_day, matings, colour = sex_ratio)) + 
     stat_summary(fun.y = "sum", size = 0.5, geom = "line") +
    stat_summary(fun.y = "sum", size = 2, geom = "point") +
    scale_x_continuous(breaks=6:18) +
    scale_y_continuous(breaks=0:12) +
    xlab("session date") +
    ylab("number of matings") +
    theme_classic() +
    theme(axis.line.x = element_line(colour = 'grey', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'grey', size=0.5, linetype='solid'),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        axis.title.x=element_text(margin=margin(20,0,0,0))) +
    scale_color_manual(values = wes_palette("Darjeeling")) 
d

ggplot2::ggsave(d, 
    filename = "overall_matings.jpg", 
    #  path = "D:/recent R",
    width = 14,
    scale = 0.6,
    height = 9, units = "in",
    dpi = 300)

# normalize number of mating by number of individuals
num_ind_per_treatment <- census %>% 
                            group_by(session_day, sex_ratio) %>%
                            summarise(n= n()) %>%
                            arrange(session_day, sex_ratio)
num_ind_per_treatment <- num_ind_per_treatment[-nrow(num_ind_per_treatment), ]

sum_mating <- mating_over_time %>% 
                    group_by(sex_ratio, session_day) %>%
                    summarise(matings = sum(matings)) %>%
                    arrange(session_day, sex_ratio)

sum_mating$n <- num_ind_per_treatment$n

sum_mating <- sum_mating %>% 
                    mutate(mating_per_ind = round(matings/n, 3))

# relative mating

d <- ggplot(sum_mating, aes(session_day, mating_per_ind, colour = sex_ratio)) + 
    stat_summary(fun.y = "sum", size = 0.5, geom = "line") +
    stat_summary(fun.y = "sum", size = 2, geom = "point") +
    scale_x_continuous(breaks=6:18) +
   # scale_y_continuous(breaks=0:12) +
    xlab("session date") +
    ylab("matings per individual") +
    theme_classic() +
    theme(axis.line.x = element_line(colour = 'grey', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'grey', size=0.5, linetype='solid'),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        axis.title.x=element_text(margin=margin(20,0,0,0))) +
    scale_color_manual(values = wes_palette("Darjeeling")) 
d

ggplot2::ggsave(d, 
    filename = "relative_matings.jpg", 
    #  path = "D:/recent R",
    width = 14,
    scale = 0.6,
    height = 9, units = "in",
    dpi = 300)

# sort and add




# ggplot2::ggsave(d, 
#     filename = "matings_over_time.jpg", 
#     #  path = "D:/recent R",
#     width = 14,
#     scale = 0.6,
#     height = 9, units = "in",
#     dpi = 300)

# census data
# census <- census %>% mutate(session_day = as.factor(session_day))
census_summary <- census %>% 
                    mutate(session_day = as.numeric(as.character(session_day))) %>%
                    filter(is.na(immigrant) | is.na(pre_census)) %>%
                    group_by(session_day, sex_ratio, sex) %>%
                    summarise(num_ind = length(id)) %>%
                    filter(!(is.na(session_day))) %>%
                    filter(sex == "F" | sex == "M") %>%
                    mutate(num_ind == as.numeric(as.character(sex_ratio)))
              
# absolute sex-specific dispersal
labels <- c(F = "Females", M = "Males")
sp + facet_grid(. ~ sex, labeller=labeller(sex = labels))

d <- ggplot(census_summary, aes(session_day, num_ind, colour = sex_ratio)) + 
     geom_point(size = 2) + 
     stat_summary(fun.y = "sum", size = 0.5, geom = "line") +
     facet_wrap(~ sex, labeller=labeller(sex = labels)) +
    scale_x_continuous(breaks=seq(from = 6, to = 18, by = 2)) +
    scale_y_continuous(breaks=seq(from = 0, to = 90, by = 10)) +
    xlab("census date") +
    ylab("number of remaining individuals") +
    theme_classic() +
    theme(axis.line.x = element_line(colour = 'grey', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'grey', size=0.5, linetype='solid'),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        strip.text.x = element_text(size=13, face="bold" ),
        strip.background = element_rect(colour="white"),
        panel.margin = unit(1.5, "lines")) +
    scale_color_manual(values = wes_palette("Darjeeling2")) 
d

ggplot2::ggsave(d, 
    filename = "census_absolute.jpg", 
    #  path = "D:/recent R",
    width = 14,
    scale = 0.6,
    height = 9, units = "in",
    dpi = 300)

# relative sex-specific_dispersal
census_summary$rel_num_ind <- NA
for (sex in c("M", "F")) {
    for (i in c(0.25, 0.5, 0.75)) {
        full_num_ind <- sum(morphometrics$sex == sex & morphometrics$treatment == i)
        census_summary[census_summary$sex_ratio == i & census_summary$sex == sex, "rel_num_ind"] <- 
            census_summary[census_summary$sex_ratio == i & census_summary$sex == sex, "num_ind"] / full_num_ind
    }
}

d <- ggplot(census_summary, aes(session_day, rel_num_ind, colour = sex_ratio)) +  
    geom_point(size = 2) + 
    stat_summary(fun.y = "sum", size = 0.5, geom = "line") +
    facet_wrap(~ sex, labeller=labeller(sex = labels)) +
    scale_x_continuous(breaks=seq(from = 6, to = 18, by = 2)) +
    #scale_y_continuous(breaks=seq(from = 0, to = 90, by = 10)) +
    xlab("census date") +
    ylab("proportion of remaining individuals") +
    theme_classic() +
    theme(axis.line.x = element_line(colour = 'grey', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'grey', size=0.5, linetype='solid'),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        strip.text.x = element_text(size=13, face="bold" ),
        strip.background = element_rect(colour="white"),
        panel.margin = unit(1.5, "lines")) +
    scale_color_manual(values = wes_palette("Darjeeling2")) 
d

ggplot2::ggsave(d, 
    filename = "census_relative.jpg", 
    #  path = "D:/recent R",
    width = 14,
    scale = 0.6,
    height = 9, units = "in",
    dpi = 300)





## number of matings / population size against sex ratio plot 



