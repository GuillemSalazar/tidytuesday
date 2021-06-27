library(tidyverse)
library(ggtree)
library(patchwork)
library(ape)

# Parameters -------------------------------------------------
colors<-c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Load and tidy data -------------------------------------------------
parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv') %>%
  mutate(across(contains("pct"),~as.numeric(gsub("%","",.x)))) %>%
  mutate(across(contains("spend"),~as.numeric(gsub("\\$","",.x)))) %>%
  select(!contains("_points")) %>%
  select(-rank,-total_pct,-city_dup,-park_benches) %>%
  filter(year==2020) %>%
  select(-year) %>%
  mutate(city=ifelse(city=="Charlotte/Mecklenburg County","Charlotte",city)) %>%
  na.exclude()

# Hierarchical Clustering -------------------------------------------------
parks_hclust<-parks %>%
  column_to_rownames(var="city") %>%
  scale(center = T,scale = T) %>%
  dist() %>%
  hclust(method = "ward.D2")

parks_hclust_groups<-parks_hclust %>%
  cutree(k = 5) %>%
  as.data.frame() %>%
  rename(hclust_group=".") %>%
  rownames_to_column(var="city") %>%
  mutate(hclust_group=paste("City cluster",hclust_group))

parks<-parks %>%
  left_join(parks_hclust_groups,by="city")

# Plot the hclust results -------------------------------------------------

parks_hclust_phylo<-as.phylo(parks_hclust)

node_df<-NULL
for (i in distinct(parks,hclust_group) %>% pull()){
  node_df<-node_df %>%
    bind_rows(data.frame(hclust_group=i,mrca_node=getMRCA(parks_hclust_phylo,parks$city[parks$hclust_group==i])))
}

g1<-ggtree(parks_hclust_phylo,size=0.2) +
  geom_tiplab(size=2,angle=90,vjust=0.5,hjust=0,family = "Perpetua") +
  geom_hilight(node = node_df$mrca_node[1],fill=colors[1],alpha = 0.4,extend = 15) +
  geom_hilight(node = node_df$mrca_node[2],fill=colors[2],alpha = 0.4,extend = 15) +
  geom_hilight(node = node_df$mrca_node[3],fill=colors[3],alpha = 0.4,extend = 15) +
  geom_hilight(node = node_df$mrca_node[4],fill=colors[4],alpha = 0.4,extend = 15) +
  geom_hilight(node = node_df$mrca_node[5],fill=colors[5],alpha = 0.4,extend = 15) +
  coord_flip() +
  theme(text = element_text(family = "Perpetua"),
        plot.margin = unit(c(0,0,0,0),"cm")) +
  xlim(0,25)

# Plot the distribution of data for each cluster -------------------------------------------------
g2<-parks %>%
  select(-city) %>%
  pivot_longer(-hclust_group,names_to="variable",values_to="value") %>%
  mutate(variable=fct_recode(variable,
                             "Median park size (acres)"="med_park_size_data",
                             "Parkland as percentage of city area"="park_pct_city_data",
                             "Percent of residents within a 10 minute walk to park"="pct_near_park_data",
                             "Spending per resident (USD)"="spend_per_resident_data",
                             "Basketball hoops per 10,000 residents"="basketball_data",
                             "Dog parks per 100,000 residents"="dogpark_data",
                             "Playgrounds per 10,000 residents"="playground_data",
                             "Recreation and senior centers per 20,000 residents"="rec_sr_data",
                             "Restrooms per 10,000 residents"="restroom_data",
                             "Splashgrounds and splashpads per 100,000 residents"="splashground_data")) %>%
  mutate(variable=str_wrap(variable,30)) %>%
  ggplot(aes(x=hclust_group,y=value,color=hclust_group,fill=hclust_group)) +
  facet_wrap("variable",scales = "free_y",nrow = 2) +
  geom_boxplot(color="black",alpha=0.4,size=0.2,outlier.shape = NA) +
  #geom_violin(draw_quantiles = 0.5,scale = "width",alpha=0.5) +
  geom_jitter(alpha=1,size=0.3) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  labs(color=NULL,fill=NULL,
       title="Clustering of US cities based on parks' features in 2020",
       subtitle="#TidyTuesday | Data source: The Trust for Public Land") +
  theme(text = element_text(family = "Perpetua"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 6,color="black"),
        axis.title = element_blank(),
        axis.ticks.y.left = element_line(size = 0.1),
        legend.position="bottom",
        legend.text = element_text(size=6),
        panel.grid = element_blank(),
        strip.text = element_text(size=6,face = "bold"),
        plot.title = element_text(hjust=0.5,size=18),
        plot.subtitle = element_text(hjust=0.5,size=10))

# Save the plot -------------------------------------------------
g<-(g2 / g1) + plot_layout(heights = c(3,2))
ggsave("../img/plot.png",g,height=twitter_dim$height,width=twitter_dim$width)

