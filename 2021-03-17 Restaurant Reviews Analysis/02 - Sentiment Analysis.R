# code to get the data and replicate results for the restaurant review sentiment
# analysis blog post: https://natural-blogarithm.com/post/restaurant-reviews-sentiment-analysis/
library(tidyverse)
library(tidytext)
library(textdata)
library(sentimentr)
library(magrittr)
library(reticulate)

# to run this code you will need to have the results from the 
# '01 - Get Restaurant Reviews.R' file available in your environment which 
# can be done by running source("01 - Get Restaurant Reviews.R"). Please make sure
# to read the instructions in that file and be careful about the costs it will 
# incur when querying the Google APIs!
theme_set(theme_minimal())

# ggplot theme settings inspired by Cédric Scherer (https://github.com/z3tt)
theme_update(
   axis.text.x = element_text(size = 9, color = "grey25", vjust = 1,
                              margin = margin(t = -10)),
   legend.position = "top",
   legend.title = element_blank(),
   panel.grid.major.y = element_line("grey92", size = .9),
   panel.grid.minor.x = element_blank(),
   panel.grid.major.x = element_blank(),
   plot.background = element_rect(fill = "white", color = NA),
   plot.subtitle = ggtext::element_textbox_simple(
      color = "grey25", size = 14, lineheight = 1.2, margin = margin(t = 15, b = 0)
   ),
   plot.caption = element_text(color = "grey25", size = 9, hjust = .5,
                               face = "italic", margin = margin(t = 12, b = 5))
)

reviews_processed <-
   reviews_translated %>%
   select(city_name,
          rating,
          review_text = text_translated) %>% 
   filter(!is.na(review_text), 
          review_text != "") %>%
   mutate(row_id = row_number()) %>%
   select(row_id, everything()) %>% 
   rowwise() %>% 
   mutate(n_words = length(strsplit(review_text, split = " ")[[1]]), 
          n_chars = nchar(review_text)) %>% 
   ungroup()

sentiment_scores <-
   reviews_processed

# simple lexicon lookup approach ----
# see: https://www.tidytextmining.com/sentiment.html
# we are using the afinn lexicon (http://www2.imm.dtu.dk/pubdb/edoc/imm6006.pdf) 
# which is based on twitter data and provides a granular score (-5 to +5)
afinn_lexicon <-
   get_sentiments("afinn")

set.seed(31324)
afinn_lexicon %>% sample_n(10)

sentiment_scores %<>%
   left_join(reviews_processed %>% 
                unnest_tokens(word, review_text) %>% 
                left_join(afinn_lexicon, 
                          by = "word") %>%
                mutate(value = ifelse(is.na(value),0,value)) %>%
                group_by(row_id) %>%
                summarise(score_afinn = mean(value), 
                          .groups = "drop"),
             by = "row_id") %>% 
   mutate(score_afinn = ifelse(is.na(score_afinn),0,score_afinn))

# example cases where lexicon approach fails
example_sentiments <-
   tibble(review_text = c(
      "This restaurant is good",
      "This restaurant is really good",
      "This restaurant is not good"
   )) %>%
   mutate(row_id = row_number()) %>%
   select(row_id, review_text)

example_sentiments %>%
   left_join(example_sentiments %>% 
                unnest_tokens(word,review_text) %>%
                left_join(afinn_lexicon,
                          by = "word") %>%
                mutate(value = ifelse(is.na(value),0,value)) %>%
                group_by(row_id) %>%
                summarise(score_afinn = mean(value),
                          .groups = "drop"),
             by = "row_id")

# average review length by city
reviews_processed %>% 
   group_by(city_name) %>% 
   summarise(mean(n_words), 
             .groups = "drop")

# sentimentr ----
# see: https://github.com/trinker/sentimentr
# uses concept of valence shifters to avoid issues of negations seen above
sentiment_scores %<>%
   left_join(reviews_processed %>%
                mutate(text_split = get_sentences(review_text)) %$%
                sentiment_by(text_split, list(row_id)) %>% 
                select(row_id, 
                       score_sentimentr = ave_sentiment),
             by = "row_id")

# check how example reviews are processed
example_sentiments %>%
   left_join(example_sentiments %>%
                mutate(text_split = get_sentences(review_text)) %$%
                sentiment_by(text_split, list(row_id)) %>%
                select(row_id,score_sentimentr = ave_sentiment),
             by = "row_id")

# flair package ----
# pre-trained neural network model from flair framework
# https://github.com/flairNLP/flair
# use reticulate to interface with Python library. conda environment can be 
# reproduced by calling conda env create -f env_nlp_py3.8.yml
use_condaenv("nlp_py3.8", required = T)
source_python("get_flair_sentiments.py")

sentiments_flair <-
   get_flair_sentiments(reviews_processed, 
                        text_col = "review_text")

sentiment_scores %<>%
   left_join(sentiments_flair %>% select(row_id, 
                                         score_flair = score),
             by = "row_id")

# rescale scores so the support is -1 to +1 (without gap between -0.5 and 0.5)
sentiment_scores %<>% 
   mutate(score_flair = ifelse(score_flair < 0, 
                               2*score_flair+1,
                               2*score_flair-1))

# plots ----
scores_long <-
   sentiment_scores %>% 
   select(-(review_text:n_chars)) %>% 
   gather(key,value,-row_id,-city_name,-rating) %>%
   mutate(key = str_replace_all(key,
                                pattern = c("score_afinn" = "AFINN Score",
                                            "score_flair" = "Flair Score",
                                            "score_sentimentr" = "sentimentr Score")))

plots <-
   lapply(split(scores_long,scores_long$key), function(x) {
      key <- unique(x$key)
      
      scores_summarised <-
         x %>% 
         group_by(city_name,rating) %>%
         summarise(average_score = mean(value),
                   .groups = "drop")
      
      y_axis_limits <- c(-max(abs(scores_summarised$average_score)),
                         max(abs(scores_summarised$average_score)))
      scores_summarised %>%
         ggplot(aes(x = rating,
                    y = average_score,
                    fill = city_name)) +
         geom_col(position = position_dodge()) +
         xlab("Point Rating") +
         ylab(key) +
         scale_fill_viridis_d() +
         scale_y_continuous(limits = y_axis_limits)
   })

# distribution of predictions from flair model 
sentiment_scores %>% 
   ggplot(aes(x = score_flair)) +
   geom_histogram(binwidth = 0.01) +
   facet_wrap(~city_name,scales = "free_y") +
   xlab("Predicted Probability")
