library(tidyverse)
library(ggthemes)

#read in MM data
df <- read.csv("https://query.data.world/s/veewfq2ez5bf6ctmidtlvis5zr7hoi", header=TRUE, stringsAsFactors=FALSE);

#read in continent data from World in Data
cont <- read.csv("R/continents.csv", stringsAsFactors = FALSE)

#find non matching countries
df %>% filter(is.na(X)) %>% select(Country.Name) %>% distinct()

df_plot <-df %>% rename(percent_women = "Proportion.of.seats.held.by.women.in.national.parliaments....") %>%
  left_join(cont %>% select(-Year), by=c("Country.Name"="Entity"))

#identify growth
growth <- df_plot %>% pivot_wider(names_from="Year", values_from="percent_women") %>%
  mutate(growth=`2019`-`1997`) %>% arrange(desc(growth)) %>% 
  group_by(X) %>%
  top_n(1, wt=growth)

#rename main variable
df_plot <- df_plot %>%
  mutate(top_growth = ifelse(Country.Name %in% growth$Country.Name,1,0)) %>% 
  filter(!is.na(X))

ggplot(df_plot, aes(Year, percent_women, group=Country.Name,
       col=as.factor(top_growth))) + 
  geom_line(alpha=0.5) + 
  geom_point(data=df_plot %>% filter(top_growth ==1 & Year == 2019), 
             aes(Year, percent_women), col="purple3", size=3) +
  geom_label(data=df_plot %>% filter(top_growth ==1 & Year == 2019), 
            aes(Year, percent_women, label=Country.Name), col="black",
            hjust=01, vjust=-.4, family="Avenir") +
  scale_color_manual(values=c("grey81", "purple3")) +
  facet_wrap(~X) + theme_few() +
  theme(text=element_text(family="Avenir", size=14),
        plot.title=element_text(family="DIN Condensed Bold", size=30),
        legend.position="none",
        plot.margin=grid::unit(c(15,15,15,15), "mm")) +
  labs(title = "Which countries have seen the largest increase in women's participation in parliament?",
       subtitle = "Percentage of parliamentary seats held by women, by country, 1997 - 2019",
       caption="Source: Our World in Data, World Bank",
      y="") +
  scale_y_continuous(labels=scales::percent, limits=c(0,0.7))
ggsave("R/women.png", dpi="retina", width=14, height=9)  
