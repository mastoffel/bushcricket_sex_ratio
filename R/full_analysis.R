# full analysis of bushcricket data
# code mostly from luke

# main questions
# (1) Are there differences in Sex-specific survival in the three ASR treatments?
# (2) Is the realized ASR constant over time, despite dispersion and death of individuals?
# (3) Is the sex-specific per-capita mating rate different in the three (0.25, 0.5, 0.75) ASR treatments?


library(dplyr)
library(reshape)
library(ggplot2)
library(RColorBrewer)
library(arm)
library(readr)
library(reshape2)
library(wesanderson)
library(ggthemr)
ggthemr('fresh')

# import raw field data from working directory as tibbles
census <- read_csv("data/raw/census_data.csv")

individual <- read_csv("data/raw/morphometric_data.csv")

mating <- read_csv("data/raw/mating_data.csv")

# rename the day column of the Mating data to match that of the Census data
colnames(mating)[2] <- "Date_of_1st_session"



# Part 1: Survival ---------------------------------------------------------------------------------

# join sex of morphometric data to Census data by ID and population
census_sexes <- left_join(census, 
    individual[, c("Population", "Sex","ID")], 
    by = c("Population", "ID"))

# Assign the sex of a census observation based on the sex from the morpometric
# data or from the field (in the case that the ID was lost or the individual
# was an immigrant the sex is thus absent from the previous join). "U" means 
# the the sex was unrecorded. ----------------------------------------------------------------------
census_sexes$Sex <- as.factor(ifelse(is.na(census_sexes$Sex.x) & 
        !is.na(census_sexes$Sex.y),
    census_sexes$Sex.y,
    ifelse(!is.na(census_sexes$Sex.x) & 
            is.na(census_sexes$Sex.y),
        census_sexes$Sex.x, 
        ifelse(is.na(census_sexes$Sex.x) & 
                is.na(census_sexes$Sex.y),
            "U", 
            ifelse(census_sexes$Sex.x == 
                    census_sexes$Sex.y,
                census_sexes$Sex.y,
                ifelse(census_sexes$Sex.x 
                    != census_sexes$Sex.y,
                    census_sexes$Sex.y, 
                    "No sex"))))))

# summarize the number of males and females recorded each night in each
# population. Exclude all observations made in the "Pre_census" sessions
census_sexes_summary <- 
    census_sexes[is.na(census_sexes$Pre_census),] %>%
    group_by(Date_of_1st_session, Population) %>%
    summarise(Female = sum(Sex == "F"),
        Male = sum(Sex == "M"))

# calculate the ASR (proportion male) of each population on each day
census_sexes_summary$ASR <- 
    census_sexes_summary$Male/
    (census_sexes_summary$Male+census_sexes_summary$Female)

# melt the summarized dataframe by Sex
census_sexes_summary_melt <- 
    reshape::melt(as.data.frame(census_sexes_summary),
        id.vars = c("Date_of_1st_session", "Population", "ASR"),
        variable_name = c("Sex"))

# cast the Individual data to get the treatments assignd to each population
population_treatment <- cast(individual, Treatment + Population ~ ., 
    fun.aggregate = length, value = "ID")

# join the treatment to the melted dataframe by population
census_sexes_summary_melt_treatment <- 
    left_join(census_sexes_summary_melt, 
        population_treatment[,c("Population", "Treatment")], 
        by = "Population")

# add variable for day running since start for a given population
census_sexes_summary_melt_treatment <- 
    census_sexes_summary_melt_treatment %>% 
    mutate(day_running = ifelse(Population %in% c("A", "B", "C", "D", "E", "F"), 
                                    ifelse(Population %in% c("A", "B", "C"), Date_of_1st_session - 5, Date_of_1st_session - 7),
                                    Date_of_1st_session - 8)) 

# plot the sex-specific temporal dynamics of each population ---------------------------------------
Time_Sex_Census_plot <- 
    ggplot(aes(y = value, x = day_running, group = Sex, color = Sex), 
        data = census_sexes_summary_melt_treatment) + 
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
    scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
    scale_color_brewer(palette = "Set1") +
    facet_wrap(Treatment ~ Population, scales = "free_x")
Time_Sex_Census_plot

# dataframe summarizing number of males/females per treatment 
census_sum <- census_sexes_summary_melt_treatment %>% 
    group_by(day_running, Treatment, Sex) %>% 
    summarise(nind = mean(value))

# plot number of individuals against time by treatment
group_colors <- c(Female = wes_palette("GrandBudapest1")[c(1)], Male =  brewer.pal(8, "Set1")[c(2)])
census_sum_plot <- ggplot(census_sexes_summary_melt_treatment, aes(y = value, x = day_running, color = Sex))+ 
    theme_bw() +
    geom_point(size = 2.5, alpha = 0.5) +
    facet_grid(. ~ Treatment) +
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
    scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11, 13)) +
    geom_smooth(method = "lm", alpha = 0.1, aes(fill = Sex), size = 0.5)  +
    scale_color_manual(values = group_colors) +
    scale_fill_manual(values = group_colors)
census_sum_plot

# save to figs/
ggsave(census_sum_plot,
    filename = "census_over_time.jpg",
    path = "figs/",
    width = 8,
    height = 3, units = "in",
    dpi = 300)


# Part 2: realized AST over time -------------------------------------------------------------------

# exchange Date_of_1st_session with day_running to equalize the start of all populations
# to the same day. It looks a bit weird as the datapoints at the last days are based
# on small sample sizes which are not averaged any more, as the experiment already
# stopped for the other populations. For this reason, I left Date_of_1st_session in 
# the model and plotted accoring to the date, not the day running per pop.

# calculate mean ASR weighted  by number of individuals in the population 
census_sexes_weighted_mean <- 
    census_sexes_summary_melt_treatment %>% 
    dcast(Date_of_1st_session + Population + Treatment + ASR ~ Sex) %>% 
    mutate(total_n = Female + Male) %>% 
    group_by(Date_of_1st_session, Treatment) %>% 
    mutate(weights = total_n/sum(total_n)) %>% 
    mutate(weighted_ASR = weights * ASR) %>% 
    mutate(sum_weighted_ASR = sum(weighted_ASR))

# summarise weighted means
census_sexes_weighted_mean_summary <- census_sexes_weighted_mean %>% 
    group_by(Date_of_1st_session, Treatment, Population) %>% 
    summarise(ASR = mean(sum_weighted_ASR))

# plot population per-day ASRs as points with weighted mean line -----------------------------------
weighted_mean_plot <- 
    ggplot(census_sexes_weighted_mean, 
        aes(y = ASR, x = Date_of_1st_session)) +
    # annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.5, alpha = 0.5,
    #     fill = brewer.pal(8, "Set1")[c(1)]) +
    # annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf, alpha = 0.5,
    #     fill = brewer.pal(8, "Set1")[c(2)]) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.5, alpha = 0.5,
        fill = wes_palette("GrandBudapest1")[c(1)]) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf, alpha = 0.5,
        fill = brewer.pal(8, "Set1")[c(2)]) +
    geom_line(data = census_sexes_weighted_mean_summary,  aes(y = ASR, x = Date_of_1st_session),
        size = 1, colour = "black") +
    geom_point(aes(size = total_n), alpha = 0.5, colour = "black") +
    scale_size_continuous(range = c(1, 7)) +
    theme_bw() +
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
    # scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11, 13)) +
    scale_color_brewer(palette = "Set1") +
    facet_grid(. ~ Treatment) +
    guides(size = guide_legend("Population \nsize"))
weighted_mean_plot

ggsave(weighted_mean_plot,
    filename = "weighted_mean_with_date.jpg",
    path = "figs/",
    width = 8,
    height = 3, units = "in",
    dpi = 300)

# plot the ASR temporal dynamics of each population ------------------------------------------------
Time_ASR_Census_plot <- 
    ggplot(aes(y = ASR, x = Date_of_1st_session), 
        data = census_sexes_summary_melt_treatment) + 
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

ggsave(Time_ASR_Census_plot,
       filename = "Time_ASR_Census_plot.jpg",
       path = "figs/",
       width = 6,
       height = 4, units = "in",
       dpi = 300)

# Part 3: Mating -----------------------------------------------------------------------------------

# summarize mating data by population and date (exclude observations that
# do not record matings such as dispersal events, etc.)
mating_summary <- 
    mating[!is.na(mating$Mating),] %>%
    group_by(Date_of_1st_session, Population) %>%
    summarise(Matings = n())

# join the matings summary to the census summary data created above
census_and_matings <- 
    left_join(census_sexes_summary_melt_treatment, 
        mating_summary, 
        by = c("Date_of_1st_session", "Population"))

# Assign 0's to all rows in which the Matings == NA.  These are true 0's since
# no matings were recorded
census_and_matings$Matings <- 
    ifelse(is.na(census_and_matings$Matings), 0, census_and_matings$Matings)

# calculate the per capita mating rate for males and females
census_and_matings$Mating_rate <- 
    census_and_matings$Matings/census_and_matings$value
# There are a few nights in which there were 0 females or males seen in the 
# census, but there were >= 0 matings recorded. This resulted in an "Inf" 
# or a "NaN" value for female mating rate. Seen here:
filter(census_and_matings, 
    is.na(Mating_rate) | 
        is.infinite(Mating_rate))
# since these are not real mating rates, they are changed to "NA"
census_and_matings$Mating_rate <- 
    ifelse(is.na(census_and_matings$Mating_rate) | 
            is.infinite(census_and_matings$Mating_rate), NA, 
        census_and_matings$Mating_rate)

# There are three cases in which the male_mating_rate was > 1. Seen here:
filter(census_and_matings, 
    Mating_rate > 1)

# These are not possible, but one could assume that all recorded males
# had mated. Thus, we change these rates to 1 to restrict the bounds of
# the mating rate to be between 0 and 1.
census_and_matings$Mating_rate <- 
    ifelse(census_and_matings$Mating_rate > 1, 
        1, census_and_matings$Mating_rate)

# convert the table to a data frame
census_and_matings_df <- as.data.frame(census_and_matings)

# create a column that is the number of individuals not mating
census_and_matings_df$Non_matings <- 
    census_and_matings$value - census_and_matings$Matings


# subset to check out effects without pop ABC
#census_and_matings_df <- census_and_matings_df %>% 
#                                filter((Population != "A") & (Population != "B") & (Population != "C"))


# make sure that there are not less than 0 non-matings (i.e. account for the
# rare occasion when there were more matings that there were individuals)
census_and_matings_df$Non_matings <- 
    ifelse(census_and_matings_df$Non_matings < 0, 0, 
        census_and_matings_df$Non_matings)

census_and_matings_df_sum <- census_and_matings_df %>% 
    group_by(Treatment) %>% 
    summarise(ASR)

# Calculate the 95% confidence interval of a mixed model for each sex by 
# creating a function which applies varying steps of the predictor (i.e., ASR)
# to the model and returns the coefficents of the intercept and beta.
CI_Mating_m <- function(offs) {
    model <- lme4::glmer(cbind(Matings, Non_matings) ~ 
            I(ASR-offs) + (1| Population) + (1| Date_of_1st_session), 
        data = filter(census_and_matings_df, Sex == "Male"), family = binomial)
    ests <- summary(model)$coefficients[1,1:2]
    # backlink the coefficients to the probability scale
    return(c(offs,ests,invlogit(ests[1]+c(-1,0,1)*1.96*ests[2])))
}

CI_Mating_f <- function(offs) {
    model <- lme4::glmer(cbind(Matings, Non_matings) ~ 
            I(ASR-offs) + (1| Population) + (1| Date_of_1st_session), 
        data = filter(census_and_matings_df, Sex == "Female"), family = binomial)
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
    geom_point(data = census_and_matings_df, 
        aes(x = ASR, y = Mating_rate), size = 3, alpha = 0.5) + 
    geom_ribbon(data = result_mating, 
        aes(x = ASR, y = Mean, ymin = Lower, ymax = Upper, fill = Sex), 
        alpha = 0.25) +
    geom_line(data = result_mating, 
        aes(x = ASR, y = Mean, colour = Sex), size = 1.5) +
    theme_bw() +
    theme(# text=element_text(family="Arial"),
        #legend.position="none",
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




# modeling -----------------------------------------------------------------------------------------
# Random effects: Population, Date
# Fixed effect: interaction between Sex and ASR
model3 <- lme4::glmer(cbind(Matings, Non_matings) ~ 
        ASR:Sex + (1| Population) + (1| Date_of_1st_session), 
    data = census_and_matings_df, family = binomial)

library(effects)
library(coefplot2)

confint(model3, method = "boot")


# check for overdispersion
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#testing-for-overdispersioncomputing-overdispersion-factor
overdisp_fun <- function(model) {
    ## number of variance parameters in 
    ##   an n-by-n variance-covariance matrix
    vpars <- function(m) {
        nrow(m)*(nrow(m)+1)/2
    }
    model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
    rdf <- nrow(model.frame(model))-model.df
    rp <- residuals(model,type="pearson")
    Pearson.chisq <- sum(rp^2)
    prat <- Pearson.chisq/rdf
    pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
    c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(model3)

# check for overdispersion with rptR
library(rptR)
out <- rptProportion(cbind(Matings, Non_matings) ~ 
        ASR:Sex + (1| Population) + (1| Date_of_1st_session), data= census_and_matings_df,
        grname=c("Overdispersion"), nperm = 100, nboot = 100)
summary(out)


