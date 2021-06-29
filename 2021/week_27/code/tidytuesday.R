library(tidyverse)
library(lubridate)
library(patchwork)
library(ggimage)

# Parameters -------------------------------------------------
colors<-c("#b25037","#c1ad38","#619070","gray50","#8b6e8b")
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Load and tidy data -------------------------------------------------
animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv') %>%
  mutate(across(everything(),~ifelse(.x=="NULL",NA,.x))) %>%
  mutate(date_time_of_call=dmy_hm(date_time_of_call),
         week=week(date_time_of_call),
         month=month(date_time_of_call))

# Plot the time series
toplot<-animal_rescues %>%
  group_by(week,cal_year) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(point_order=week+(52*(as.numeric(as.factor(cal_year))-1)))
toplot_annot<-toplot %>%
  group_by(cal_year) %>%
  summarise(xmin=min(point_order),
            xmax=max(point_order)) %>%
  mutate(ymin=min(toplot$n),
         ymax=max(toplot$n)) %>%
  mutate(ispair=cal_year%%2==1) %>%
  filter(cal_year!=2021)

g1<-ggplot(data=toplot,aes(x=point_order,y=n)) +
  geom_rect(data=toplot_annot,aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=ispair),inherit.aes = F,alpha=0.7) +
  geom_line() +
  geom_text(data=toplot_annot,aes(x=xmin+(xmax-xmin)/2,y=-1,label=cal_year),inherit.aes = F,size=3,family="Perpetua") +
  scale_fill_manual(values=c("#6e9fa1",NA)) +
  theme_minimal() +
  labs(title=str_wrap("Animal rescue incidents attended by the London Fire Brigade",70),
       subtitle=str_wrap("The London Fire Brigade attends a range of non-fire incidents. These 'special services' include assistance to animals that may be trapped or in distress.",120)) +
  ylab("Number of rescues\nper week") +
  theme(text = element_text(family="Perpetua",colour = "black"),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size=9,hjust=1),
        axis.title.x = element_blank(),
        axis.text = element_text(color="black"),
        plot.title = element_text(face = "bold",hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

# Plot the number of incidents by month of the year
g2<-animal_rescues %>%
  mutate(animal_group_parent=ifelse(animal_group_parent %in% c("Cat","Bird","Dog","Fox"),animal_group_parent,"Other")) %>%
  group_by(month,cal_year,animal_group_parent) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  group_by(month,animal_group_parent) %>%
  summarise(median=mean(n),
            quant_25=quantile(n,0.25),
            quant_75=quantile(n,0.75),
            min=min(n),
            max=max(n)) %>%
  ungroup() %>%
  mutate(animal_group_parent=fct_reorder(animal_group_parent,median,sum,.desc = T)) %>%
  ggplot() +
  geom_ribbon(aes(x=month,y=median,ymin=min,ymax=max,fill=animal_group_parent),alpha=0.7) +
  geom_line(aes(x=month,y=median,color=animal_group_parent),size=0.4) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = 1:12,labels = month(1:12,label = T)) +
  ylab("Number of rescues\nper month") +
  facet_wrap(~animal_group_parent,scales = "free_y",nrow = 1) +
  theme_minimal() +
  labs(caption="#Tidytuesday | Data Source: London.gov (by way of Data is Plural and Georgios Karamanis)") +
  theme(legend.position = "none",
        text = element_text(family="Perpetua",colour = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(color="black"),
        axis.title.y = element_text(size=9,hjust=1),
        axis.text.x = element_text(angle=90,size=6,hjust=0,vjust=0),
        plot.caption = element_text(size=6,hjust=1))

# Save the plot -------------------------------------------------
g<-(g1 / g2) + plot_layout(heights=c(3,2))
ggsave("../img/plot.png",g,height=twitter_dim$height,width=twitter_dim$width)

