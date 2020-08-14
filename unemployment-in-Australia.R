# Install necessary packages
#install.packages("gtrendsR")

# Load libraries
library(gtrendsR)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(gtrendsR)
library(tidytext)
library(igraph)
library(ggraph)


# Grab data using gTrends
searches <- gtrends("unemployment", 
                    geo=c("AU"),
                    time="today 12-m")

# Convert data into table
searches_timeseries <- as_tibble(searches$interest_over_time) %>% 
  mutate(date = ymd(date)) # convert date format


# Generate Timeseries
ggplot(searches_timeseries) +
  
  # Mark outbreak period
  geom_rect(aes(NULL, NULL, xmin=as.Date("2020-03-24"), xmax=date[52], ymin=-Inf, ymax=Inf),fill="azure2") +
  geom_vline(xintercept=as.Date("2020-01-25"),size=0.7,linetype=4, colour="red")+
  
  # Plot timeseries
  geom_line(aes(date, hits)) +
  
  # Add labels
  labs(x="Time",
       y="Relative Search Interest",
       title="Google trend data for the word 'unemployment'",
       subtitle="Australians turn to the internet for help as COVID-19 layoffs devastate the economy")+
  
  # Add annotations
  annotate(geom="text", x=as.Date(as.Date("2020-02-13")), y=95, label="First case", 
           fontface="italic",size=3,color="red")+
  
  # Add annotations
  annotate(geom="text", x=as.Date(as.Date("2020-04-19")), y=10, label="Non-essential\nservices closed",
           fontface="italic",size=3,color="blue4")+
  
  # Modify axis labels
  scale_x_date(date_breaks="1 month", date_labels="%b\n%Y")+
  
  # Customise aesthetics
  theme_classic()

# Save as png
ggsave("lineplot.png")


# Search for related queries
related_searches <- as_tibble(searches$related_queries) %>% 
  filter(keyword=="unemployment")

# Count the words in the related searches
wordcount <- related_searches %>% 
  select(value) %>% 
  unnest_tokens(word, value) %>% 
  count(word)

# Format data for network plot
network <- related_searches %>% 
  
  # Use tokenizing to examine consecutive words
  unnest_tokens(bigram, value, token='ngrams', n=2) %>% # examining a set of 2 consecutive words
  
  # Separate into columns
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  count(word1, word2) %>%
  
  # filter out NAs
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>%
  
  # Turn into igraph object
  graph_from_data_frame(vertices=wordcount)


# Create plot
ggraph(network, layout ="lgl")+
  
  # Modify edges
  geom_edge_link(edge_alpha=0.2,
                 arrow=grid::arrow(type="closed", length=unit(.3, "centimeters")), 
                 end_cap=circle(.03, 'centimetres')) +
  
  # Customise nodes
  geom_node_point(aes(size=log(n), alpha=log(n), colour="black"),color='purple') + 
  geom_node_text(aes(label=name), vjust=1.5) + # labels
  
  # Add labels
  labs(title="Related search queries for 'unemployment'")+ 
  
  # Customise aesthetics
  theme_void()+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(hjust = 0.5))

# Save as png
ggsave("network.png")
