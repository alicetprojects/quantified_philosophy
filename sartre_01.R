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

# read in the pre-processed pdf file

sartre <- pdf_text("sartre.pdf")


# collect the sentences

sartre_sentences <- get_sentences(sartre)


# view the first few lines

head(sartre_sentences)


# collect the sentiments to a vector

sartre_syuzhet <- get_sentiment(sartre_sentences)


# calculate summary statistics for the data

sum(sartre_syuzhet)
mean(sartre_syuzhet)
summary(sartre_syuzhet)


# plot the sentiments over narrative time

sartre_plot <- plot(
  sartre_syuzhet,
  type = "l",
  main = "Being and Nothingness",
  xlab = "Narrative Time",
  ylab = "Emotional Valence"
)


# generate a simple plot showing the plot arc

sartre_simple_plot <- simple_plot(sartre_syuzhet, title = "Being and Nothingness Syuzhet Plot")
sartre_simple_plot


# create a dataframe from the syuzhet sentiment data

ss_df <- tibble(sentiment_value = sartre_syuzhet)


# plot this distribution to determine normality

sartre_distr <- ggplot(ss_df, aes(x = sentiment_value)) +
  geom_histogram(position = "identity", binwidth = 0.3)
sartre_distr

