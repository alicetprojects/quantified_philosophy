# libraries used

library(tidyverse)
library(tidytext)
library(pdftools)
library(tokenizers)
library(textdata)
library(gutenbergr)
library(syuzhet)
library(wordcloud)
library(reshape2)
library(ggplot2)


# read in pre-processed pdf file

sartre <- pdf_text("sartre.pdf") %>%
  tibble(txt = .) %>%
  unnest_tokens(word, txt)


# create a word-number column from rownames

sartre <- rownames_to_column(sartre)
sartre$rowname <- as.numeric(sartre$rowname)


# count occurrences of each word, minus stop-words

s_count <- sartre %>% anti_join(stop_words) %>%
  count(word, sort = TRUE)


# generate a wordcloud

s_wordcloud <- wordcloud(s_count$word, s_count$n, scale = c(3, .2), min.freq=3,
                         max.words = 50, random.order = FALSE, rot.per = 0.25, colors = c("turquoise", "magenta", "dark blue", "blue", "purple", "green", "pink"))


# collect bing sentiments from the data

s_sentiments <- sartre %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


# generate a comparison cloud for the bing sentiments data

s_comparison_cloud <- sartre %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = FALSE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue", "magenta"),
                   max.words = 50)


# generate a plot showing counts of important words (>500)

sartre_plot <- sartre %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

sartre_plot
