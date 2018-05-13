library("highcharter")
library("dplyr")
library("stringr")
library("ggplot2")
library("plotly")
library("ggthemes")

survey_data <- read.csv("data/intro_survey.csv")

colors <- c("#922626",  "#895b5b", "#6e617e"	, "#c7a0a0", "#a94267")
seahawks_colors <- c("#36578c", "#4ea701", "#90918c")

exp_and_cats <- select(survey_data, programming_exp, pet_preference) %>%
  filter(str_detect(pet_preference, "cat")) %>%
  group_by(programming_exp) %>%
    summarize( 
    cat_lovers = n()
  ) %>%
  mutate(cat_lovers_avg = cat_lovers*100/(sum(exp_and_cats$cat_lovers)))

exp_and_dogs <- select(survey_data, programming_exp, pet_preference) %>%
  filter(str_detect(pet_preference, "dog")) %>%
  group_by(programming_exp) %>%
  summarize( 
    dog_lovers = n()
  ) %>%
  mutate(dog_lovers_avg = dog_lovers*100/(sum(exp_and_dogs$dog_lovers)))

cat_vs_dog_lovers <- left_join(exp_and_cats, exp_and_dogs)


#cat lovers and programming experience vs dog lovers and programming experience piecharts
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

dog_lovers_piechart <- plot_ly(cat_vs_dog_lovers, labels = ~programming_exp, values = ~dog_lovers_avg, type = 'pie',
                               textposition = 'inside',
                               textinfo = 'label+percent',
                               insidetextfont = list(color = '#FFFFFF'),
                               hoverinfo = 'text',
                               text = ~paste( cat_lovers_avg, ' percent'),
                               marker = list(colors = colors,
                                             line = list(color = '#FFFFFF', width = 1)),
                                             showlegend = FALSE) %>%
  layout(title = 'Programming Experience Levels Among Dog Lovers (1-Low ~ 5-High)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

dog_lovers_piechart


#coffee cups and programming experience scatterplot
coffee_ggplot <- ggplot(survey_data,
                                 aes(x = coffee_cups,
                                     y = programming_exp)) + 
  theme(legend.position="top",
        axis.text=element_text(size = 18))
(coffee_ggplot_color <- coffee_ggplot + geom_point(aes(color = programming_exp),
                                                                     alpha = 0.2,
                                                                     size = 10.5,
                                                                     position = position_jitter(width = .5, height = 0))) 

coffee_ggplot_color + theme_hc(bgcolor = "#b8bcc8")

#seahawks and programming exp stacked bar chart
#can not get rid of y axis labels
seahawks <- select(survey_data, programming_exp, seahawks_fan)  

ggplot(data = seahawks, mapping = aes(x = programming_exp)) + geom_bar(aes(fill=seahawks_fan))+
  theme(axis.title.y = element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_manual(values = seahawks_colors) +
  theme_hc()


View(cat_vs_dog_lovers)
View(exp_and_dogs)
View(exp_and_cats)
View(survey_data)