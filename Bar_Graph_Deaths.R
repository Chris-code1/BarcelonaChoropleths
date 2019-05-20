# Load libraries
library(tidyverse)
library(knitr)
library(magrittr)

# Read the data 
population <- read.csv("Data/2018_padro_edat_any_a_any_per_sexe.csv", sep=",", fileEncoding="UTF-8")
deaths <- read.csv("Data/2017_defuncions_edats-quinquennals.csv", sep=",", fileEncoding="UTF-8")

# Structure
str(population)
str(deaths)

# Deaths by district (2017)
deaths 
group_by(Nom_Districte) %>%
  summarise(Count=sum(Number)) %>%
  arrange(desc(Count)) %>%
  ggplot(aes(x=reorder(Nom_Districte, Count), y=Count)) +
  geom_bar(stat="identity", aes(fill=Count), show.legend=FALSE) +
  geom_label(aes(label=Count)) +
  scale_fill_gradient(low="paleturquoise", high="paleturquoise4") +
  labs(x="Age", y="Deaths", title="Deaths by district (2017)") +
  scale_y_continuous(labels=comma) +
  coord_flip() +
  theme_bw() 

