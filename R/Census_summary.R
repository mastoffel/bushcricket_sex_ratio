library(dplyr)
library(reshape)
library(ggplot2)
library(RColorBrewer)
library(arm)

# import raw field data from working directory
Census <- read.csv("data/raw/census_data.csv", na.strings = "",
                   stringsAsFactors = FALSE)
                   # colClasses = c("factor", "integer", "factor", "factor", 
                   #                "factor", "factor", "factor", "factor",
                   #                "factor", "character"))
Individual <- read.csv("data/raw/morphometric_data.csv", na.strings = "",
                       stringsAsFactors = FALSE)
                       # colClasses = c("factor", "factor", "numeric", "factor",
                       #                "numeric", "numeric", "factor"))
Mating <- read.csv("data/raw/mating_data.csv", na.strings = "",
                   stringsAsFactors = FALSE)
                   # colClasses = c("factor", "integer", "character", "factor", 
                   #                "factor", "factor", "factor", "factor", 
                   #                "factor"))

# rename the day column of the Mating data to match that of the Census data
colnames(Mating)[2] <- "Date_of_1st_session"

# join sex of morphometric data to Census data by ID and population
Census_sexes <- left_join(Census, 
                          Individual[, c("Population", "Sex","ID")], 
                          by = c("Population", "ID"))

# Assign the sex of a census observation based on the sex from the morpometric
# data or from the field (in the case that the ID was lost or the individual
# was an immigrant the sex is thus absent from the previous join). "U" means 
# the the sex was unrecorded.
Census_sexes$Sex <- as.factor(ifelse(is.na(Census_sexes$Sex.x) & 
                                       !is.na(Census_sexes$Sex.y),
                       Census_sexes$Sex.y,
                       ifelse(!is.na(Census_sexes$Sex.x) & 
                                is.na(Census_sexes$Sex.y),
                               Census_sexes$Sex.x, 
                              ifelse(is.na(Census_sexes$Sex.x) & 
                                       is.na(Census_sexes$Sex.y),
                                     "U", 
                                    ifelse(Census_sexes$Sex.x == 
                                             Census_sexes$Sex.y,
                                           Census_sexes$Sex.y,
                                           ifelse(Census_sexes$Sex.x 
                                                  != Census_sexes$Sex.y,
                                                  Census_sexes$Sex.y, 
                                                  "No sex"))))))

# summarize the number of males and females recorded each night in each
# population. Exclude all observations made in the "Pre_census" sessions
Census_sexes_summary <- 
  Census_sexes[is.na(Census_sexes$Pre_census),] %>%
  group_by(Date_of_1st_session, Population) %>%
  summarise(Female = sum(Sex == "F"),
            Male = sum(Sex == "M"))

# calculate the ASR (proportion male) of each population on each day
Census_sexes_summary$ASR <- 
    Census_sexes_summary$Male/
    (Census_sexes_summary$Male+Census_sexes_summary$Female)

# melt the summarized dataframe by Sex
Census_sexes_summary_melt <- 
    reshape::melt(as.data.frame(Census_sexes_summary),
                  id.vars = c("Date_of_1st_session", 
                              "Population", "ASR"),
                  variable_name = c("Sex"))

# cast the Individual data to get the treatments assignd to each population
Population_treatment <- cast(Individual, Treatment + Population ~ ., 
                             fun.aggregate = length, value = "ID")

# join the treatment to the melted dataframe by population
Census_sexes_summary_melt_treatment <- 
    left_join(Census_sexes_summary_melt, 
              Population_treatment[,c("Population", "Treatment")], 
              by = "Population")

# plot the sex-specific temporal dynamics of each population 
Time_Sex_Census_plot <- 
  ggplot(aes(y = value, x = Date_of_1st_session, group = Sex, color = Sex), 
         data = Census_sexes_summary_melt_treatment) + 
  theme_bw() +
  geom_line() +
  theme(text = element_text(family="Arial"),
        axis.title.x = element_text(size = 10),
        axis.text.x  = element_text(size = 8), 
        axis.title.y = element_text(size = 10),
        axis.text.y  = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_line(size = 0.3, colour = "grey40"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks.x = element_line(size = 0.3, colour = "grey40")) +
  xlab("Day") + 
  ylab("Number of individuals") +
  scale_x_continuous(breaks = c(6, 8, 10, 12, 14, 16, 18)) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(Treatment ~ Population, scales = "free_x")
Time_Sex_Census_plot

# plot the ASR temporal dynamics of each population 
Time_ASR_Census_plot <- 
  ggplot(aes(y = ASR, x = Date_of_1st_session), 
         data = Census_sexes_summary_melt_treatment) + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.5, alpha = 0.5,
         fill = brewer.pal(8, "Set1")[c(1)]) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf, alpha = 0.5,
         fill = brewer.pal(8, "Set1")[c(2)]) +
  theme_bw() +
  geom_line() +
  theme(text = element_text(family="Arial"),
        axis.title.x = element_text(size = 10),
        axis.text.x  = element_text(size = 8), 
        axis.title.y = element_text(size = 10),
        axis.text.y  = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_line(size = 0.3, colour = "grey40"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks.x = element_line(size = 0.3, colour = "grey40")) +
  xlab("Day") + 
  ylab("Adult sex ratio (proportion \u2642)") +
  scale_x_continuous(breaks = c(6, 8, 10, 12, 14, 16, 18)) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(Treatment ~ Population, scales = "free_x")
Time_ASR_Census_plot

# save plots into the "figs" directory
# ggsave(Time_Sex_Census_plot, 
#        filename = "Time_Sex_Census_plot.jpg", 
#        path = "figs/",
#        width = 6,
#        height = 4, units = "in",
#        dpi = 300)
# 
# ggsave(Time_ASR_Census_plot, 
#        filename = "Time_ASR_Census_plot.jpg", 
#        path = "figs/",
#        width = 6,
#        height = 4, units = "in",
#        dpi = 300)

# summarize mating data by population and date (exclude observations that
# do not record matings such as dispersal events, etc.)
Mating_summary <- 
    Mating[!is.na(Mating$Mating),] %>%
    group_by(Date_of_1st_session, Population) %>%
    summarise(Matings = n())

# join the matings summary to the census summary data created above
Census_and_matings <- 
    left_join(Census_sexes_summary_melt_treatment, 
              Mating_summary, 
              by = c("Date_of_1st_session", "Population"))

# Assign 0's to all rows in which the Matings == NA.  These are true 0's since
# no matings were recorded
Census_and_matings$Matings <- 
    ifelse(is.na(Census_and_matings$Matings), 0, Census_and_matings$Matings)

# calculate the per capita mating rate for males and females
Census_and_matings$Mating_rate <- 
    Census_and_matings$Matings/Census_and_matings$value
# There are a few nights in which there were 0 females or males seen in the 
# census, but there were >= 0 matings recorded. This resulted in an "Inf" 
# or a "NaN" value for female mating rate. Seen here:
filter(Census_and_matings, 
       is.na(Mating_rate) | 
       is.infinite(Mating_rate))
# since these are not real mating rates, they are changed to "NA"
Census_and_matings$Mating_rate <- 
    ifelse(is.na(Census_and_matings$Mating_rate) | 
               is.infinite(Census_and_matings$Mating_rate), NA, 
           Census_and_matings$Mating_rate)

# There are three cases in which the male_mating_rate was > 1. Seen here:
filter(Census_and_matings, 
       Mating_rate > 1)

# These are not possible, but one could assume that all recorded males
# had mated. Thus, we change these rates to 1 to restrict the bounds of
# the mating rate to be between 0 and 1.
Census_and_matings$Mating_rate <- 
    ifelse(Census_and_matings$Mating_rate > 1, 
           1, Census_and_matings$Mating_rate)

# convert the table to a data frame
Census_and_matings_df <- as.data.frame(Census_and_matings)

# create a column that is the number of individuals not mating
Census_and_matings_df$Non_matings <- 
    Census_and_matings$value - Census_and_matings$Matings

# make sure that there are not less than 0 non-matings (i.e. account for the
# rare occasion when there were more matings that there were individuals)
Census_and_matings_df$Non_matings <- 
    ifelse(Census_and_matings_df$Non_matings < 0, 0, 
           Census_and_matings_df$Non_matings)

# Calculate the 95% confidence interval of a mixed model for each sex by 
# creating a function which applies varying steps of the predictor (i.e., ASR)
# to the model and returns the coefficents of the intercept and beta.
CI_Mating_m <- function(offs) {
    model <- lme4::glmer(cbind(Matings, Non_matings) ~ 
                             I(ASR-offs) + (1| Population) + (1| Date_of_1st_session), 
                         data = filter(Census_and_matings_df, Sex == "Male"), family = binomial)
    ests <- summary(model)$coefficients[1,1:2]
    # backlink the coefficients to the probability scale
    return(c(offs,ests,invlogit(ests[1]+c(-1,0,1)*1.96*ests[2])))
}

CI_Mating_f <- function(offs) {
    model <- lme4::glmer(cbind(Matings, Non_matings) ~ 
                             I(ASR-offs) + (1| Population) + (1| Date_of_1st_session), 
                         data = filter(Census_and_matings_df, Sex == "Female"), family = binomial)
    ests <- summary(model)$coefficients[1,1:2]
    # backlink the coefficients to the probability scale
    return(c(offs,ests,invlogit(ests[1]+c(-1,0,1)*1.96*ests[2])))
}

# specify the offs (i.e., vector of numbers from 0 to 1 stepped by 0.05)
offs_mating <- seq(0,1,0.05)

# apply the offs vector to the function (retuning a matrix)
result_mating_m <- sapply(offs_mating, CI_Mating_m)
result_mating_f <- sapply(offs_mating, CI_Mating_f)

# transpose the matrix
result_mating_m <- t(result_mating_m)
result_mating_f <- t(result_mating_f)

# convert the matrix to a data.frame
result_mating_m <- data.frame(result_mating_m)
result_mating_f <- data.frame(result_mating_f)

# define the column names and create a Sex column
colnames(result_mating_m) <- 
    c("ASR", "Coefficient", "Std. Error", "Upper", "Mean", "Lower")
result_mating_m$Sex <- "Male"
colnames(result_mating_f) <- 
    c("ASR", "Coefficient", "Std. Error", "Upper", "Mean", "Lower")
result_mating_f$Sex <- "Female"

# stack the two sexes together
result_mating <- rbind(result_mating_m, result_mating_f)

# dinfe a color palette for the plotting of the two sexes
cbPalette <- c(brewer.pal(8, "Dark2")[c(1)], brewer.pal(8, "Dark2")[c(2)])

ASR_mating_rate_regression <- 
ggplot2::ggplot() + 
    geom_point(data = Census_and_matings_df, 
               aes(x = ASR, y = Mating_rate), size = 3, alpha = 0.5) + 
    geom_ribbon(data = result_mating, 
                aes(x = ASR, y = Mean, ymin = Lower, ymax = Upper, fill = Sex), 
                alpha = 0.25) +
    geom_line(data = result_mating, 
              aes(x = ASR, y = Mean, colour = Sex), size = 1.5) +
    theme_bw() +
    theme(text=element_text(family="Arial"),
          legend.position="none",
          legend.position = c(0, 1), 
          legend.justification = c(0, 1),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height=unit(0.8,"line"),
          legend.key.width=unit(0.8,"line"),
          legend.background = element_rect(fill=NA),
          axis.title.x = element_text(size = 14),
          axis.text.x  = element_text(size = 13), 
          axis.title.y = element_text(size = 14),
          axis.text.y  = element_text(size = 13), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size=14, face = "bold"),
          strip.text.y = element_text(size=14, face = "bold")) +
    scale_y_continuous(limits=c(0,1)) +
    ylab("Per capita matings observed (± 95% CI)") +
    xlab("Adult sex ratio (proportion \u2642)") +
    facet_grid(Sex ~ .) +
    scale_fill_manual(values = cbPalette) +
    scale_color_manual(values = cbPalette)
ASR_mating_rate_regression

# save plots into the "figs" directory
ggsave(ASR_mating_rate_regression,
       filename = "ASR_mating_rate_regression.jpg",
       path = "figs/",
       width = 4,
       height = 6, units = "in",
       dpi = 300)

# Mixed model to assess
# Random effects: Population, Date
# Fixed effect: interaction between Sex and ASR
model3 <- lme4::glmer(cbind(Matings, Non_matings) ~ 
                         ASR*Sex + (1| Population) + (1| Date_of_1st_session), 
                     data = Census_and_matings_df, family = binomial)