library(tidyverse)
library(rvest)
library(ggpage)
library(tidytext)
library(patchwork)
library(ggalt)

lyrics <- "I will leave my heart at the door\nI won't say a word\nThey've all been said before\nSo why don't we just play pretend\nLike we're not scared of what's coming next\nOr scared of having nothing left"

song <- tibble(text = lyrics) %>%
  separate_rows(text, 
                sep = "\n")

prebuild <- song %>%
  ggpage_build() %>%
  left_join(get_sentiments("bing"), 
            by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment),
                            "none", 
                            sentiment),
         x_pos = (xmax + xmin)/2,
         y_pos = (ymin + ymax)/2)

# don't know how to remove data$xmin warnings
prebuild %>%
  ggpage_plot(aes(fill = sentiment)) +
  scale_fill_manual(values = c( "black", "grey", "#f5af86"),
                    name = "Sentiment") +
  scale_colour_manual(values = c( "white", "white", "black"),
                      name = "Sentiment") +
  geom_text(aes(label = word, 
                x = x_pos, 
                y = y_pos,
                colour = sentiment), 
            size = 3)

ggsave("images/adele_sentiment.png", units="in", width=8, height=5)