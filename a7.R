library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(httr)
library(jsonlite)
library(maps)
library(plotly)
library(stringr)
library(ggthemes)
library(highcharter)

survey_data <- read.csv("data/intro_survey.csv", stringsAsFactors = FALSE)

#############################
### 1. Summary Statistics ###
#############################
exp_by_info_interest <- survey_data %>% 
  group_by(info_interest) %>%  
  summarize(mean = mean(programming_exp, na.rm = TRUE), 
            median = median(programming_exp,na.rm = TRUE),
            minimum = min(programming_exp, na.rm = TRUE),
            maximum = max(programming_exp, na.rm = TRUE)
  )

median_no_info <- exp_by_info_interest %>% 
  filter(info_interest == "No") %>% 
  select(median)

median_yes_info <- exp_by_info_interest %>% 
  filter(info_interest == "Yes") %>% 
  select(median)

median_unsure_info <- exp_by_info_interest %>% 
  filter(info_interest == "Not sure") %>% 
  select(median)

mean_no_info <- exp_by_info_interest %>% 
  filter(info_interest == "No") %>% 
  select(mean) %>% 
  round(2)

mean_yes_info <- exp_by_info_interest %>% 
  filter(info_interest == "Yes") %>% 
  select(mean) %>% 
  round(2)


mean_unsure_info <- exp_by_info_interest %>% 
  filter(info_interest == "Not sure") %>% 
  select(mean) %>% 
  round(2)

exp_by_os <- survey_data %>% 
  filter(os %in% c("Windows", "Linux (any)", "Mac OS X")) %>% 
  group_by(os) %>%  
  summarize(mean = mean(programming_exp, na.rm = TRUE), 
            median = median(programming_exp,na.rm = TRUE),
            minimum = min(programming_exp, na.rm = TRUE),
            maximum = max(programming_exp, na.rm = TRUE)
  )

median_mac <- exp_by_os %>% 
  filter(os == "Mac OS X") %>% 
  select(median)

median_windows <- exp_by_os %>% 
  filter(os == "Windows") %>% 
  select(median)

median_linux <- exp_by_os %>% 
  filter(os == "Linux (any)") %>% 
  select(median)

mean_mac <- exp_by_os %>% 
  filter(os == "Mac OS X") %>% 
  select(mean) %>% 
  round(2)

mean_windows <- exp_by_os %>% 
  filter(os == "Windows") %>% 
  select(mean) %>% 
  round(2)

mean_linux <- exp_by_os %>% 
  filter(os == "Linux (any)") %>% 
  select(mean) %>% 
  round(2)

####################################
### 2. Response Statistics Table ###
####################################

#This section will include a table reporting the break-downs 
#of technical experience among the survey respondents. This table will report the 
#total number of people who gave each of the different responses to the survey, 
#as well as any relevant aggregate data.


unique(survey_data$programming_exp)

colnames(survey_data)
rows <- c("Command Line", "Version Control", "Markdown", "R", "Web")
cols <- c("never", "few", "intermediate", "expert")

tech_exp <- survey_data %>% 
  select(cli_exp, vcs_exp, md_exp, r_exp, web_exp)

all_exp <- colnames(tech_exp)
row <- c("0")

never <- row
few <- row
intermediate <- row
expert <- row

new_frame <- data.frame(all_exp)

count <- nrow(tech_exp)
#this function counts the total number of people with the certain level of
#experience for each category 
tech_exp_count <- function(exp_type, n) {
  x <- select(tech_exp, exp_type)
  data_type <- rlang::sym(exp_type)
  y <- filter(x, !!data_type == n) %>% 
    nrow() / count * 100
  return(round(y, 0))
}

cl <- tech_exp_count("cli_exp", 0)
vc <- tech_exp_count("vcs_exp", 0)
md <- tech_exp_count("md_exp", 0)
r <- tech_exp_count("r_exp", 0)
web <- tech_exp_count("web_exp", 0)

never <- c(cl, vc, md, r, web)
new_frame <- mutate(new_frame, never = never)

cl <- tech_exp_count("cli_exp", 1)
vc <- tech_exp_count("vcs_exp", 1)
md <- tech_exp_count("md_exp", 1)
r <- tech_exp_count("r_exp", 1)
web <- tech_exp_count("web_exp", 1)

few <- c(cl, vc, md, r, web)
new_frame <- mutate(new_frame, few = few)

cl <- tech_exp_count("cli_exp", 2)
vc <- tech_exp_count("vcs_exp", 2)
md <- tech_exp_count("md_exp", 2)
r <- tech_exp_count("r_exp", 2)
web <- tech_exp_count("web_exp", 2)

intermediate <- c(cl, vc, md, r, web)
new_frame <- mutate(new_frame, intermediate = intermediate)

cl <- tech_exp_count("cli_exp", 3)
vc <- tech_exp_count("vcs_exp", 3)
md <- tech_exp_count("md_exp", 3)
r <- tech_exp_count("r_exp", 3)
web <- tech_exp_count("web_exp", 3)

expert <- c(cl, vc, md, r, web)
new_frame <- mutate(new_frame, expert = expert)

colnames(new_frame) <- c("Levels of Experience", "Never", "A Few Times", "Intermediate", "Expert")
rownames(new_frame) <- c("Command Line", "Version Control", "Markdown", "R", "Web")

new_frame
###############################
### 3. First Visualizations ###
###############################

cli_experience_plot_2 <- ggplot(data = survey_data) +
  geom_point(mapping = aes(x = cli_exp, y = programming_exp)) +
  geom_smooth(mapping = aes(x = cli_exp, y = programming_exp), color = "red", method = "") +
  labs(x = "Command-Line/Terminal Experience", y = "Overall Programming Experience") +
  facet_wrap(~info_interest)

vcs_experience_plot_2 <- ggplot(data = survey_data) +
  geom_point(mapping = aes(x = vcs_exp, y = programming_exp)) +
  geom_smooth(mapping = aes(x = vcs_exp, y = programming_exp), color = "blue") +
  labs(x = "Version Control Experience", y = "Overall Programming Experience") +
  facet_wrap(~info_interest)

md_experience_plot_2 <- ggplot(data = survey_data) +
  geom_point(mapping = aes(x = md_exp, y = programming_exp)) +
  geom_smooth(mapping = aes(x = md_exp, y = programming_exp), color = "green") +
  labs(x = "Markdown Experience", y = "Overall Programming Experience") +
  facet_wrap(~info_interest)

r_experience_plot_2 <- ggplot(data = survey_data) +
  geom_point(mapping = aes(x = r_exp, y = programming_exp)) +
  geom_smooth(mapping = aes(x = r_exp, y = programming_exp), color = "orange") +
  labs(x = "R Programming Experience", y = "Overall Programming Experience") +
  facet_wrap(~info_interest)

web_experience_plot_2 <- ggplot(data = survey_data) +
  geom_point(mapping = aes(x = web_exp, y = programming_exp)) +
  geom_smooth(mapping = aes(x = web_exp, y = programming_exp), color = "pink") +
  labs(x = "Web Programming Experience", y = "Overall Programming Experience") +
  facet_wrap(~info_interest)


###############################
### 4. Second Visualization ###
###############################

#color palettes
colors <- c("#922626",  "#895b5b", "#6e617e"	, "#c7a0a0", "#a94267")
seahawks_colors <- c("#36578c", "#4ea701", "#90918c")

#cat lovers and programming experience vs dog lovers and programming experience piecharts

#get filtered data
exp_and_cats <- select(survey_data, programming_exp, pet_preference) %>%
  filter(str_detect(pet_preference, "cat")) %>%
  group_by(programming_exp) %>%
  summarize( 
    cat_lovers = n()
  )
cat_lovers_calc <- exp_and_cats$cat_lovers*100/(sum(exp_and_cats$cat_lovers))
exp_and_cats <- mutate(exp_and_cats, cat_lovers_avg = cat_lovers_calc)

exp_and_dogs <- select(survey_data, programming_exp, pet_preference) %>%
  filter(str_detect(pet_preference, "dog")) %>%
  group_by(programming_exp) %>%
  summarize( 
    dog_lovers = n()
  )
dog_lovers_calc <- exp_and_dogs$dog_lovers*100/(sum(exp_and_dogs$dog_lovers))
exp_and_dogs <- mutate(exp_and_dogs, dog_lovers_avg = dog_lovers_calc)

cat_vs_dog_lovers <- left_join(exp_and_cats, exp_and_dogs)


#cat lovers piechart
cat_lovers_piechart <- plot_ly(cat_vs_dog_lovers, labels = ~programming_exp, values = ~cat_lovers_avg, type = 'pie',
                               textposition = 'inside',
                               textinfo = 'label+percent',
                               insidetextfont = list(color = '#FFFFFF'),
                               hoverinfo = 'text',
                               text = ~paste( cat_lovers_avg, ' percent'),
                               marker = list(colors = colors,
                                             line = list(color = '#FFFFFF', width = 1)),
                               showlegend = FALSE) %>%
  layout(title = 'Programming Experience Levels Among Cat Lovers (1-Low ~ 5-High)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

cat_lovers_piechart

#dog lovers piechart
dog_lovers_piechart <- plot_ly(cat_vs_dog_lovers, labels = ~programming_exp, values = ~dog_lovers_avg, type = 'pie',
                               textposition = 'inside',
                               textinfo = 'label+percent',
                               insidetextfont = list(color = '#FFFFFF'),
                               hoverinfo = 'text',
                               text = ~paste( cat_lovers_avg, ' percent'),
                               marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), 
                               showlegend = FALSE) %>% 
  layout(title = 'Programming Experience Levels Among Dog Lovers (1-Low ~ 5-High)', 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

dog_lovers_piechart


#coffee cups a day and programming experience scatterplot
coffee_ggplot <- ggplot(survey_data, aes(x = coffee_cups, y = programming_exp)) + 
  theme(legend.position = "top", axis.text = element_text(size = 18))

coffee_ggplot_color <- coffee_ggplot + geom_point(aes(color = programming_exp), alpha = 0.2, size = 10.5, 
                                                  position = position_jitter(width = .5, height = 0)) +
  labs(x = "Coffee Cups a Day", y = "Programming Exp. (1-little exp. ~ 5- most exp.)")

coffee_ggplot_color + theme_hc(bgcolor = "#b8bcc8")


#seahawks and programming exp stacked bar chart
seahawks <- select(survey_data, programming_exp, seahawks_fan)  

seahawks_ggplot <- ggplot(data = seahawks, mapping = aes(x = programming_exp)) + geom_bar(aes(fill=seahawks_fan))+
  theme(axis.title.y = element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_manual(values = seahawks_colors) +
  labs(x = "Programming Exp. (1-little exp. ~ 5- most exp.)") +
  theme_hc()

#Find average amount of programming exp.=5 dog lovers
dogs <- filter(exp_and_dogs, programming_exp == "5") %>%
  select(dog_lovers_avg)
dogs_five <- as.character(round(dogs, digits = 1))
# programming exp.=1
dogs <- filter(exp_and_dogs, programming_exp == "1") %>%
  select(dog_lovers_avg)
dogs_one <- as.character(round(dogs, digits = 1))
# programming exp.=2
dogs <- filter(exp_and_dogs, programming_exp == "2") %>%
  select(dog_lovers_avg)
dogs_two <- as.character(round(dogs, digits = 1))

#Find average amount of programming exp.=5 cat lovers
cats <- filter(exp_and_cats, programming_exp == "5") %>%
  select(cat_lovers_avg)
cats_five <- as.character(round(cats, digits = 1))
# programming exp.=1
cats <- filter(exp_and_cats, programming_exp == "1") %>%
  select(cat_lovers_avg)
cats_one <- as.character(round(cats, digits = 1))
# programming exp.=2
cats <- filter(exp_and_cats, programming_exp == "2") %>%
  select(cat_lovers_avg)
cats_two <- as.character(round(cats, digits = 1))