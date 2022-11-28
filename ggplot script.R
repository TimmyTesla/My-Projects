############### loading required packages
library(tidyverse)

###### loading data set
read_csv2("./weekly_wage.csv",
          col_names = TRUE, 
          col_types = cols(
           Year = col_character(),
           Public_school_teachers = col_number(),
           Other_college_grads  = col_number()))

#### Data cleaning and converting to tidy data for R
trial2 <- read_csv2("./weekly_wage.csv") %>% 
  mutate(Public_school_teachers = parse_number(Public_school_teachers),
         Other_college_grads = parse_number(Other_college_grads))
trial2 <- pivot_longer(data = trial2, cols = c(Public_school_teachers, Other_college_grads), 
                       names_to = "teacher_graduate")


##################################### FIRST PLOT #####################################
################################### FIGURE A #########################################
## Annotation trial
annotation <- data.frame(
  x = 500, y = 1200,
  label = "1250")

## Plotting trial
ggplot(data = trial2, mapping = aes(x = Year, y = value, group = teacher_graduate)) +
  geom_line(aes(colour = teacher_graduate), size = 1.5, lineend = "round") +
  expand_limits(y = c(750, 2250)) + # forces the scale to start from 750 and end at 2250
  scale_y_continuous(breaks = seq(from = 750, to = 2250, by = 250),
                     labels = c(750,"1,000","1,250","1,500","1,750","2,000","$2,250")) +
  scale_colour_discrete(type = c("darkblue", "skyblue"), # specifies the colour we want the lines to be
        labels = c("Other college grads", "Public school teachers"), # changes name of legends to proper name
## guide switches the position of the legends such that Public school teacher shows first and not Other college grads as was in the graph
        guide = guide_legend(reverse = TRUE)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", colour = "grey", size = 0.7),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.2,0.87),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(margin = margin(b = 17, t = 7)),
## add straight visible line to x axis
        axis.line.x = element_line(colour = "grey"),
        axis.ticks.x = element_line(colour = "grey"),
## increasing the length of x axis 
        axis.ticks.length.x = unit(0.19, "cm")
        )



# labs(title = "Teachers’ weekly wages have remained relatively flat for\n25 years",
# subtitle = "Average weekly wages of public school teachers and other college \ngraduates, 1979–2021",
# caption = "Notes: Figure shows average weekly wages (2021$) of public school 
# teachers (elementary, middle, and secondary) and other college graduate (nonteacher) 
# peers. Data points for 1994 and 1995 are unavailable; dotted lines represent 
# interpolated data. See Allegretto and Mishel 2019, Appendix A, for more details 
# on data and methodology.
# Source: Author’s analysis of Current Population Survey Outgoing Rotation 
# Group data accessed via the EPI Current Population Survey Extracts, Version 1.0.29
# (EPI 2022a), https://microdata.epi.org.")






