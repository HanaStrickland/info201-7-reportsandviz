library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(httr)
library(jsonlite)
library(maps)
library(plotly)

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

#2. Response Statistics Table

#This section will include a table reporting the break-downs 
#of technical experience among the survey respondents. This table will report the 
#total number of people who gave each of the different responses to the survey, 
#as well as any relevant aggregate data.

library("dplyr")
library("tidyr")

survey_data <- read.csv("data/intro_survey.csv", stringsAsFactors = FALSE)

View(survey_data)
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
    nrow()./ count * 100
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
View(new_frame)
