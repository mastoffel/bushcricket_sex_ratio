library(dplyr)
library(reshape)
library(ggplot2)
library(RColorBrewer)

# import raw field data from working directory
Census <- read.csv("Data/Census_data.csv", na.strings = "",
                   stringsAsFactors = FALSE)
                   # colClasses = c("factor", "integer", "factor", "factor", 
                   #                "factor", "factor", "factor", "factor",
                   #                "factor", "character"))
Individual <- read.csv("Data/Morphometric_data.csv", na.strings = "",
                       stringsAsFactors = FALSE)
                       # colClasses = c("factor", "factor", "numeric", "factor",
                       #                "numeric", "numeric", "factor"))
Mating <- read.csv("Data/Mating_data.csv", na.strings = "",
                   stringsAsFactors = FALSE)
                   # colClasses = c("factor", "integer", "character", "factor", 
                   #                "factor", "factor", "factor", "factor", 
                   #                "factor"))

# join sexes to Census data
Census_sexes <- left_join(Census, 
                          Individual[, c("Population", "Sex","ID")], 
                          by = c("Population", "ID"))
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
Census_sexes_summary <- 
  Census_sexes[is.na(Census_sexes$Pre_census),] %>%
  group_by(Date_of_1st_session, Population) %>%
  summarise(Female = sum(Sex == "F"),
            Male = sum(Sex == "M"))
Census_sexes_summary$ASR <- Census_sexes_summary$Male/(Census_sexes_summary$Male+Census_sexes_summary$Female)

Census_sexes_summary_melt <- reshape::melt(as.data.frame(Census_sexes_summary[,-5]),
                                      id.vars = c("Date_of_1st_session", 
                                                  "Population"),
                                      variable_name = c("Sex"))

Census_sexes_summary_melt <- left_join(Census_sexes_summary_melt, 
                                  Individual[,c("Population", "Treatment")], 
                                  by = "Population")

Census_sexes_summary <- left_join(Census_sexes_summary, 
                                   Individual[,c("Population", "Treatment")], 
                                   by = "Population")

Time_Sex_Census_plot <- 
  ggplot(aes(y = value, x = Date_of_1st_session, group = Sex, color = Sex), 
         data = Census_sexes_summary_melt) + 
  theme_bw() +
  geom_line() +
  theme(text = element_text(family="Arial"),
        axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=10), 
        axis.title.y = element_text(size=12),
        axis.text.y  = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_line(size = 0.5, colour = "grey40"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.ticks.x = element_line(size = 0.5, colour = "grey40"))+
        #strip.background = element_blank(),
        #strip.text.x = element_blank())
  xlab("Day") + 
  ylab("Number of individuals") +
  scale_x_continuous(breaks = c(6, 8, 10, 12, 14, 16, 18)) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(Treatment ~ Population, scales = "free_x")
Time_Sex_Census_plot

Time_ASR_Census_plot <- 
  ggplot(aes(y = ASR, x = Date_of_1st_session), 
         data = Census_sexes_summary) + 
  theme_bw() +
  geom_line() +
  theme(text = element_text(family="Arial"),
        axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=10), 
        axis.title.y = element_text(size=12),
        axis.text.y  = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_line(size = 0.5, colour = "grey40"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.ticks.x = element_line(size = 0.5, colour = "grey40"))+
  #strip.background = element_blank(),
  #strip.text.x = element_blank())
  xlab("Day") + 
  ylab("Adult sex ratio (proportion \u2642)") +
  scale_x_continuous(breaks = c(6, 8, 10, 12, 14, 16, 18)) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(Treatment ~ Population, scales = "free_x") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0.5, alpha=0.5,
           fill=brewer.pal(8, "Set1")[c(1)]) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=0.5, ymax=Inf, alpha=0.5,
           fill=brewer.pal(8, "Set1")[c(2)])
Time_ASR_Census_plot
