library(tidyverse)

getwd()

African_names <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv")

freed.rodeur <- African_names %>% 
  filter(port_disembark=="Freetown", ship_name== "Rôdeur") %>% 
  mutate(Gender = ifelse(gender == "Man", "Male",
                         if_else(gender=="Boy", "Male",
                                 if_else(gender=="Woman", "Female", "Female"))))

View(freed.rodeur)

graph_data <- ggplot2::ggplot(data = freed.rodeur, mapping = aes(x = forcats::fct_infreq(gender), fill = gender))+
  
  geom_bar( position = "dodge", width = 0.5)+
  
  scale_y_continuous(breaks = seq(0,10,1),
                     expand = c(0,1))+
  
  labs(title = "Individuals Freed Using Rôdeur Ship Who had Disembarked from Freetown",
       subtitle = "Distributed by Gender",
       x = "Gender",
       y = "Number of Rescued Individuals",
       caption = "ngina_wangui")+
  
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top",
        legend.spacing.x = unit(0.3, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))+
  
  scale_fill_brewer(palette = "Dark2")
  
 graph_data

ggsave("rescued.png")
