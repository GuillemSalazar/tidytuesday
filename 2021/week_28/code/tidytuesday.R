library(tidyverse)
library(ggbump)
library(pals)
library(ggtext)

# Parameters ¬ self-defined functions -------------------------------------------------
colors<-glasbey(n=27)
names(colors)<-NULL
range01 <- function(x,fct=1){(x-mean(x))/fct*(max(x)-min(x))}


# Load and tidy data -------------------------------------------------
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv') %>%
  filter(name_of_holiday!="None") %>%
  filter(!is.na(independence_from)) %>%
  filter(country!="Congo, Democratic Republic of the") %>% # Remove several consecutive independece dates for Congo
  select(independence_from,independence_as=country,year) %>%
  mutate(independence_from=fct_recode(independence_from,
                                      "China"="Qing China[65][66]",
                                      "Soviet Union"="Russian Soviet Federative Socialist Republic",
                                      "Soviet Union"="Soviet Union[55]",
                                      "Soviet Union"="Soviet Union[80]",
                                      "Spain / Spanish Empire"="Spain",
                                      "Spain / Spanish Empire"="Spanish Empire",
                                      "Spain / Spanish Empire"="Spanish Empire[72]",
                                      "France,United Kingdom,United States,Soviet Union"="Allied occupying powers (France, United Kingdom, United States and Soviet Union)",
                                      "Austro-Hungarian Empire"="Austria-Hungary",
                                      "Egypt,United Kingdom"="Egypt and the United Kingdom",
                                      "Japan / Empire of Japan"="Empire of Japan",
                                      "Japan / Empire of Japan,France"="Empire of Japan and France",
                                      "France,Spain / Spanish Empire"="France and Spain",
                                      "Italy,United Kingdom"="Italy and United Kingdom",
                                      "United Kingdom"="Kingdom of Great Britain",
                                      "Germany"="Nazi Germany",
                                      "Netherlands,Japan / Empire of Japan"="Netherlands and Japan",
                                      "Soviet Union,Germany"="Russian Soviet Federative Socialist Republic and German Empire",
                                      #"SFR Yugoslavia"="Socialist Federal Republic of Yugoslavia",
                                      "United Kingdom,France"="United Kingdom and France",
                                      "United Kingdom,British Mandate for Palestine"="United Kingdom and the British Mandate for Palestine",
                                      "Portugal"="United Kingdom of Portugal, Brazil and the Algarves",
                                      "Netherlands"="United Netherlands")) %>%
  mutate(independence_as=fct_recode(independence_as,
                                    "Czech Rep."="Czech Republic",
                                    "Republic of the Congo"="Congo, Republic of the")) %>%
  mutate(independence_from=as.character(independence_from)) %>%
  separate_rows(independence_from,convert = T,sep=",") %>%
  mutate(independence_from=as.factor(independence_from)) %>%
  arrange(year) %>%
  group_by(independence_from,independence_as) %>% # Remove consecutive independence events
  filter(year==max(year)) %>%
  ungroup()

# Define the coordinates of independence events -------------------------------------------------
toplot<-holidays %>%
  mutate(independence_from=fct_reorder(independence_from,year,mean)) %>%
  arrange(independence_from,year) %>%
  mutate(independence_as=fct_inorder(independence_as)) %>%
  mutate(x=1500,
         xend=year,
         y=range01(as.numeric(independence_from)),
         yend=range01(as.numeric(independence_as),fct = 15)) %>%
  mutate(event=1:n())

# Re-set the coordinates for secondary independence events -------------------------------------------------
pos<-which(toplot$independence_from %in% toplot$independence_as)
toplot$x[pos]<-toplot$xend[match(toplot$independence_from[pos],toplot$independence_as)]
toplot$y[pos]<-toplot$yend[match(toplot$independence_from[pos],toplot$independence_as)]

# Plot -------------------------------------------------
g<-ggplot(data=toplot,aes(x=x,y=y,xend=xend,yend=yend,group=event,color=independence_from)) +
  geom_sigmoid(smooth = 10,alpha=1) +
  geom_point(aes(x=xend,y=yend),color="black",size=0.8) +
  geom_text(data=toplot %>% distinct(xend,yend,independence_as),aes(x=xend,y=yend,label=independence_as),size=1.8,hjust=0,nudge_x = 7,color="black",family = "Avenir Next",fontface="bold",inherit.aes = F) +
  geom_text(data=toplot %>% filter(x==1500) %>% distinct(x,y,independence_from),aes(x=x,y=y,label=independence_from),size=1.8,hjust=1,nudge_x = -5,color="black",family = "Avenir Next",fontface="bold",inherit.aes = F) +
  scale_x_continuous(limits = c(1300,2020),breaks = c(1600,1700,1800,1900,2000)) +
  coord_cartesian(clip = "off") +
  labs(title="Timeline of national independence events",
       subtitle="The timeline is based on a partial list of national independence days<br>from Wikipedia compiled and cleaned by Isabella Velásquez<br>for the **#TidyTuesday** challange") +
  theme_minimal() +
  theme(text = element_text(family = "Avenir Next"),
        legend.position = "none",
        axis.text.x = element_text(angle=0,hjust=0.5,vjust=1),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_line(linetype = 3,color="gray60"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold",hjust=0,size=18),
        plot.subtitle = element_markdown(hjust=0,size=12,margin = unit(c(0,0,1,0),"cm")),
        plot.margin = unit(c(0.5,2.3,0.5,0.5),"cm"))

ggsave("../img/plot.png",g,width=unit(6,"cm"),height=unit(10,"cm"))


