# code to replicate the results for the restaurant review modelling post:
# https://natural-blogarithm.com/post/restaurant-reviews-modelling/
library(magrittr)
library(tidyverse)
library(text2vec)
library(glmnet)
library(stopwords)
library(parallel)
library(doMC)

# to run this code you will need to have the results from the 
# '01 - Get Restaurant Reviews.R' file available in your environment which 
# can be done by running source("01 - Get Restaurant Reviews.R"). Please make sure
# to read the instructions in that file and be careful about the costs it will 
# incur when querying the Google APIs!

# validation split
test_split <- 0.2

# number of CV folds for glmnet 
NFOLDS = 4

# set up parallel support for glmnet
num_cores <- 
   detectCores()
registerDoMC(cores = num_cores)

# ggplot theme settings inspired by Cédric Scherer (https://github.com/z3tt)
theme_set(theme_minimal())
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
                               face = "italic", margin = margin(t = 12, b = 5)))

# data preparation ----
reviews_processed <-
   reviews_translated %>%
   select(city_name,
          rating,
          review_text = text_translated) %>% 
   filter(!is.na(review_text), 
          review_text != "") %>%
   mutate(row_id = row_number()) %>%
   select(row_id, everything()) %>% 
   mutate(five_star = rating == 5) %>% 
   mutate(weight = 1)

# pre-processing 
# as described here: https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html
text_col = "review_text"
id_col = "row_id"
target_var = "five_star"

# build vocabulary vectorizer on full dataset that we can then apply on the 
# two separate city datasets
tok_fun = word_tokenizer
prep_fun = tolower

# negations are important for our classification so exclude them from stopwords
stopwords_without_negations <-
   setdiff(stopwords::stopwords(),
           c("isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", 
             "haven't", "hadn't", "doesn't", "don't", "didn't", "won't", 
             "wouldn't", "shan't", "shouldn't", "can't", "couldn't", "mustn't",
             "cannot", "couldn't", "mustn't", "no", "nor", "not"))

it_reviews = itoken(reviews_processed[[text_col]],
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    ids = reviews_processed[[id_col]])
vocab = create_vocabulary(it_reviews, 
                          stopwords = stopwords_without_negations,
                          ngram = c(1L, 3L))

vocab = prune_vocabulary(vocab,
                         term_count_min = 10,
                         doc_proportion_max = 0.5,
                         doc_proportion_min = 0.001)
vectorizer = vocab_vectorizer(vocab)

# helper function to build dtm objects
build_dtm <-
   function(data,
            vectorizer_,
            text_col_,
            id_col_,
            tok_fun_,
            prep_fun_) {
      it = itoken(data[[text_col_]],
                  preprocessor = prep_fun_,
                  tokenizer = tok_fun_,
                  ids = data[[id_col_]])
      dtm = create_dtm(it, vectorizer_)
      
      # TF-IDF is favorable in general but we will use L1 here as it is can 
      # be applied independently for each row (and therefore consistently across 
      # different datasets)
      dtm = normalize(dtm,"l1")
      return(dtm)
   }

models <- 
   list()

# separate models, no resampling/weighting -----
models <-
   c(models,
     lapply(split(reviews_processed, f = reviews_processed$city_name),
            function(x) {
               list(data = x,
                    dtm = build_dtm(x,
                                    vectorizer_ = vectorizer,
                                    text_col_ = text_col,
                                    id_col_ = id_col,
                                    tok_fun_ = tok_fun,
                                    prep_fun_ = prep_fun),
                    test_ids = sample(1:nrow(x),
                                      size = round(nrow(x)*test_split),
                                      replace = F),
                    model_type = "regularised")
            }))

# sanity checks to make sure pre-processing worked correctly
all(colnames(models$Berlin$dtm) == colnames(models$Stockholm$dtm))
lapply(models,function(x) dim(x$dtm))

# separate models with resampling/weighting ----
# calculate case weights to account for class imbalance between the two datasets
counts_stockholm <-
   models$Stockholm$data %>% 
   count(five_star) %>% 
   mutate(five_star = ifelse(five_star,"pos","neg")) %>% 
   deframe()

target_prop <-
   models$Berlin$data %>% 
   count(five_star) %>% 
   mutate(perc = n/sum(n)) %>%
   filter(five_star) %>% 
   pull(perc)

wgt <-
   counts_stockholm['neg']/(counts_stockholm['pos']/target_prop - counts_stockholm['pos'])

models <-
   c(models,
     setNames(models, nm = paste0(names(models),"_resampling")))

models$Stockholm_resampling$data %<>%
   mutate(weight = ifelse(five_star,wgt,1))

# double check weighting worked as expected
lapply(models[c("Berlin_resampling","Stockholm_resampling")],function(x) x$data %>% 
          count(five_star,wt = weight) %>%
          mutate(perc = n/sum(n)))

# unified model ----
# build design matrix step by step
# step 1: city dummy
city_reference_level <-
   "Stockholm"
city_dummy <-
   Matrix(as.matrix(as.numeric(reviews_processed$city_name == city_reference_level)),sparse = T)
colnames(city_dummy) <- "city_dummy"

# step 2: dtm predictors
dtm_full <-
   build_dtm(reviews_processed,
             vectorizer_ = vectorizer,
             text_col_ = text_col,
             id_col_ = id_col,
             tok_fun_ = tok_fun,
             prep_fun_ = prep_fun)

# step 3: interaction terms
interaction_multiplier <-
   Matrix(matrix(rep(city_dummy,ncol(dtm_full)),ncol=ncol(dtm_full),byrow=F),sparse = T)
interaction_terms <-
   dtm_full*interaction_multiplier
colnames(interaction_terms) <-
   paste0(city_reference_level,"_",colnames(interaction_terms))

# put everything together in one matrix
dtm_unified_model <-
   cbind(city_dummy,dtm_full,interaction_terms)

# some sanity checks
dtm_unified_model %>% rowSums() %>% summary
sum((dtm_unified_model %>% rowSums()) > 2) # should be close to #of Stockholm reviews 

set.seed(1252154)
models[["unified"]] <-
   list(data = reviews_processed,
        dtm = dtm_unified_model,
        test_ids = sample(1:nrow(reviews_processed),
                          size = round(nrow(reviews_processed)*test_split),
                          replace = F),
        model_type = "regularised"
   )

# unified model with embeddings ----
# running through reticulate gives extremely long run times. therefore call 
# python script directly as a shell command. also need to split up data in 
# batches as otherwise memory clogs up
write_csv(reviews_processed %>% select(row_id, review_text),
          path = "review_texts.csv")

#system("zsh -c 'source ~/.zshrc && conda activate nlp_py3.8 && python get_flair_embeddings.py review_texts.csv 1000'")

flair_embeddings <-
   read.csv("embeddings.csv") %>% select(-1)

# make sure order matches our main dataframe
flair_embeddings <-
   reviews_processed %>% 
   select(row_id) %>% 
   left_join(flair_embeddings,
             by = "row_id")
all(flair_embeddings$row_id == reviews_processed$row_id)

# build matrix in the same way as with the dtm matrix
embeddings_matrix <-
   Matrix(as.matrix(flair_embeddings %>% select(-row_id),
                    ncol = ncol(flair_embeddings %>% select(-row_id)),
                    byrow = F),sparse = T)

interaction_multiplier_embeddings <-
   Matrix(matrix(rep(city_dummy,
                     ncol(embeddings_matrix)),
                 ncol=ncol(embeddings_matrix),
                 byrow=F),
          sparse = T)
interaction_terms_embeddings <-
   embeddings_matrix*interaction_multiplier_embeddings

colnames(interaction_terms_embeddings) <-
   paste0(city_reference_level,"_",colnames(interaction_terms_embeddings))

full_matrix_embeddings <-
   cbind(city_dummy,embeddings_matrix,interaction_terms_embeddings)

models[["unified_embeddings"]] <-
   models[["unified"]]
models[["unified_embeddings"]]$dtm <-
   full_matrix_embeddings

models[["unified_embeddings"]]$model_type <-
   "unregularised"

# fit all models ----
models <-
   lapply(models,function(x) {
      if(x$model_type == "regularised") {
         x$model_train <-
            cv.glmnet(x = x$dtm[-x$test_ids,], 
                      y = x$data[-x$test_ids,][[target_var]], 
                      weights = x$data[-x$test_ids,][['weight']],
                      family = 'binomial', 
                      alpha = 1,
                      type.measure = "auc",
                      nfolds = NFOLDS,
                      thresh = 1e-5,
                      maxit = 1e5,
                      parallel = T)
         
         x$model_full <-
            cv.glmnet(x = x$dtm, 
                      y = x$data[[target_var]], 
                      weights = x$data[['weight']],
                      family = 'binomial', 
                      alpha = 1,
                      type.measure = "auc",
                      nfolds = NFOLDS,
                      thresh = 1e-5,
                      maxit = 1e5,
                      parallel = T)
      } else {
         x$model_train <-
            glmnet(x = x$dtm[-x$test_ids,], 
                   y = x$data[-x$test_ids,][[target_var]], 
                   weights = x$data[-x$test_ids,][['weight']],
                   family = 'binomial',
                   alpha = 0,
                   lambda = 0)
         
         x$model_full <-
            glmnet(x = x$dtm, 
                   y = x$data[[target_var]], 
                   weights = x$data[['weight']],
                   family = 'binomial',
                   alpha = 0,
                   lambda = 0)
      }
      x
   })

# performance evaluation ----
models_performance <-
   lapply(models,function(x) {
      assess.glmnet(object = x$model_train, 
                    newx = x$dtm[x$test_ids,], 
                    newy = x$data[x$test_ids,][[target_var]],
                    weights = x$data[x$test_ids,][["weight"]]) %>% 
         as_tibble() %>% 
         mutate(data = "test") %>% 
         bind_rows(assess.glmnet(object = x$model_train, 
                                 newx = x$dtm[-x$test_ids,], 
                                 newy = x$data[-x$test_ids,][[target_var]],
                                 weights = x$data[-x$test_ids,][["weight"]]) %>% 
                      as_tibble() %>% 
                      mutate(data = "train")) %>% 
         bind_rows(assess.glmnet(object = x$model_train, 
                                 newx = x$dtm, 
                                 newy = x$data[[target_var]],
                                 weights = x$data[["weight"]]) %>% 
                      as_tibble() %>% 
                      mutate(data = "full"))
   }) %>% bind_rows(.id ="model") %>% 
   select(model,data,everything()) %>% 
   arrange(data,model)
models_performance

# get predictions on data
models <-
   lapply(models,function(x) {
      x$pred_model <-
         tibble(row_id = x$data$row_id,
                pred = predict(x$model_full,
                               newx = x$dtm,
                               type = "response")[,1])
      x
   })

# compare predictions on five point reviews from Berlin ----
# separate models (with resampling)
berlin_five_point_dtm <-
   models$Berlin_resampling$dtm[which(models$Berlin_resampling$data$rating == 5),]

preds_berlin_five_points <-
   tibble(model = "separate_models",
          city_used = "Berlin",
          row_id = rownames(berlin_five_point_dtm),
          pred = predict(models$Berlin_resampling$model_full,
                         newx = berlin_five_point_dtm,
                         type = "response")[,1]) %>% 
   bind_rows(tibble(model = "separate_models",
                    city_used = "Stockholm",
                    row_id = rownames(berlin_five_point_dtm),
                    pred = predict(models$Stockholm_resampling$model_full,
                                   newx = berlin_five_point_dtm,
                                   type = "response")[,1]))

# unified model 
# create dtm such that we predict with Stockholm coefficients (but Berlin intercept)
n_coefficients <- 
   dim(models$Berlin_resampling$dtm)[2]
dtm_unified_berlin_fivepoint <-
   models$unified$dtm[which((models$unified$data$city_name == "Berlin") & 
                               (models$unified$data$five_star)),]

dtm_unified_berlin_fivepoint_stockholm_coefficients <-
   cbind(dtm_unified_berlin_fivepoint[,1:(n_coefficients+1)],
         dtm_unified_berlin_fivepoint[,2:(n_coefficients+1)])
colnames(dtm_unified_berlin_fivepoint_stockholm_coefficients) <- 
   colnames(dtm_unified_model)

preds_berlin_five_points %<>%
   bind_rows(tibble(model = "unified_model",
                    city_used = "Berlin",
                    row_id = rownames(dtm_unified_berlin_fivepoint),
                    pred = predict(models$unified$model_full,
                                   newx = dtm_unified_berlin_fivepoint, 
                                   type = "response")[,1])) %>% 
   bind_rows(tibble(model = "unified_model",
                    city_used = "Stockholm",
                    row_id = rownames(dtm_unified_berlin_fivepoint_stockholm_coefficients),
                    pred = predict(models$unified$model_full,
                                   newx = dtm_unified_berlin_fivepoint_stockholm_coefficients, 
                                   type = "response")[,1]))

# unified model with embeddings
matrix_embeddings_berlin_fivepoint <-
   models$unified_embeddings$dtm[which((models$unified_embeddings$data$city_name == "Berlin") & 
                                          (models$unified_embeddings$data$five_star)),]
rownames(matrix_embeddings_berlin_fivepoint) <-
   models$unified_embeddings$data %>% filter(city_name == "Berlin" & five_star) %>% pull(row_id)

n_coefficients_embeddings <- 
   ncol(flair_embeddings %>% select(-row_id))

matrix_embeddings_berlin_fivepoint_stockholm_coefficients <-
   cbind(matrix_embeddings_berlin_fivepoint[,1:(n_coefficients_embeddings+1)],
         matrix_embeddings_berlin_fivepoint[,2:(n_coefficients_embeddings+1)])
colnames(matrix_embeddings_berlin_fivepoint_stockholm_coefficients) <- 
   colnames(matrix_embeddings_berlin_fivepoint)

preds_berlin_five_points %<>%
   bind_rows(tibble(model = "unified_model_embeddings",
                    city_used = "Berlin",
                    row_id = rownames(matrix_embeddings_berlin_fivepoint),
                    pred = predict(models$unified_embeddings$model_full,
                                   newx = matrix_embeddings_berlin_fivepoint, 
                                   type = "response")[,1])) %>% 
   bind_rows(tibble(model = "unified_model_embeddings",
                    city_used = "Stockholm",
                    row_id = rownames(matrix_embeddings_berlin_fivepoint_stockholm_coefficients),
                    pred = predict(models$unified_embeddings$model_full,
                                   newx = matrix_embeddings_berlin_fivepoint_stockholm_coefficients, 
                                   type = "response")[,1]))

# summarise prediction difference
preds_berlin_five_points %>% 
   spread(city_used, pred) %>% 
   group_by(model) %>% 
   summarise(mean_diff_berlin_stockholm = mean(Berlin - Stockholm),.groups = "drop")

preds_berlin_five_points %>% 
   spread(city_used, pred) %>%
   ggplot(aes(x = Berlin,
              y = Stockholm)) +
   geom_point(alpha = 0.15) +
   geom_line(data = tibble(x = c(0,1),
                           y = c(0,1)),
             aes(x = x, 
                 y = y), 
             color = "blue",
             alpha = 0.8) +
   facet_wrap(~ model,ncol = 2) +
   scale_y_continuous(labels = scales::percent) +
   scale_x_continuous(labels = scales::percent)

# example reviews ---- 
example_reviews <-
   tibble(review_text = 
             c("This is a great restaurant, I really enjoyed the food and nice atmosphere.",
               "One of the best restaurants I have ever been. Amazing food, awesome staff, good prices. Highly Recommend!",
               "The staff is super friendly and the food is magnificient. Always extremely satisfied here!",
               "If your looking for the best food in town, this is the place. Never disappoints!",
               "Had a brilliant time here with my family. The food was superb and the atmosphere was delightful. One of the best places around!",
               "")) %>%
   mutate(row_id = row_number())

example_reviews_dtm <-
   build_dtm(example_reviews,
             vectorizer_ = vectorizer,
             text_col_ = text_col,
             id_col_ = id_col,
             tok_fun_ = tok_fun,
             prep_fun_ = prep_fun)

example_reviews %<>%
   bind_cols(lapply(models[c("Berlin",
                             "Stockholm",
                             "Berlin_resampling",
                             "Stockholm_resampling")],
                    function(x) {
                       predict(x$model_full,
                               example_reviews_dtm,
                               type = "response")[,1]
                    }) %>% bind_cols())

# show effect of unbalanced data on predictions when fitting on unbalanced data
example_reviews %>% 
   select(review_text,
          model_berlin = Berlin,
          model_stockholm = Stockholm)

# difference in prediction for empty review texts disappears after balancing
example_reviews %>% 
   select(review_text,
          model_berlin_resampled = Berlin_resampling,
          model_stockholm_resampled = Stockholm_resampling)

# coefficients analysis ---- 
coefficients_all_models <-
   lapply(models,function(x) {
      tibble(coefficient = rownames(as.matrix(coefficients(x$model_full))),
             value = as.matrix(coefficients(x$model_full))[,1])
   }) %>% 
   bind_rows(.id = "model")

# coefficients that are non-zero in both separate models
coefficients_comparison_separate_models <-
   coefficients_all_models %>% 
   filter(model %in% c("Berlin_resampling",
                       "Stockholm_resampling"
   )) %>% 
   spread(model,value)

coefficients_summary_separate_models <-
   coefficients_comparison_separate_models %>% 
   summarise(non_zero_berlin_model = mean(Berlin_resampling != 0),
             non_zero_stockholm_model = mean(Stockholm_resampling != 0),
             non_zero_both = mean((Berlin_resampling != 0) & 
                                     (Stockholm_resampling != 0))) %>% 
   gather(key,value) %>% deframe
coefficients_summary_separate_models

# look at some example coefficients that have high impact in both models
coefficients_comparison_separate_models %<>% 
   arrange(desc(Berlin_resampling)) %>% 
   mutate(rank_berlin = row_number()) %>% 
   arrange(desc(Stockholm_resampling)) %>% 
   mutate(rank_stockholm = row_number()) %>% 
   mutate(average_rank = (rank_berlin + rank_stockholm)/2) %>% 
   arrange(average_rank)

top_bottom_shared <-
   coefficients_comparison_separate_models %>% 
   arrange(average_rank) %>% 
   select(coefficient, 
          coefficient_berlin = Berlin_resampling, 
          coefficient_stockholm = Stockholm_resampling) %>% 
   head(12) %>% 
   mutate(group = "top") %>% 
   bind_rows(coefficients_comparison_separate_models %>% 
                arrange(desc(average_rank)) %>% select(coefficient, 
                                                       coefficient_berlin = Berlin_resampling, 
                                                       coefficient_stockholm = Stockholm_resampling) %>% 
                head(12) %>% 
                mutate(group = "bottom"))

top_bottom_shared %>% 
   mutate(coefficient = factor(coefficient,
                               levels = rev(coefficient)),
          group = factor(group,
                         levels = c("top","bottom"), 
                         labels = c("Positive","Negative"))) %>%
   gather(key,value,-coefficient,-group) %>% 
   mutate(key = factor(key, label = c("Berlin","Stockholm"))) %>% 
   ggplot(aes(x = coefficient, 
              y = value, 
              color = key)) + 
   geom_point(size = 4) + 
   coord_flip() + 
   facet_wrap(~group,scales = "free_y") + 
   scale_color_viridis_d() + 
   xlab("Predictor") + 
   ylab("Coefficient Estimate")

# coefficients only important in Berlin model
coefficients_comparison_separate_models %>% 
   filter(Stockholm_resampling == 0) %>% 
   arrange(desc(Berlin_resampling)) %>% 
   filter((row_number() < 12) | 
             (row_number() >= (n())-12)) %>% 
   mutate(coefficient = factor(coefficient, levels = rev(coefficient)),
          group = factor(ifelse(row_number() <= 12,
                                "Positive",
                                "Negative"),
                         levels = c("Positive",
                                    "Negative"))) %>% 
   select(coefficient, 
          Berlin = Berlin_resampling, 
          Stockholm = Stockholm_resampling,
          group)  %>% 
   gather(key,value,-coefficient,-group) %>% 
   ggplot(aes(x = coefficient, 
              y = value, 
              color = key)) + 
   geom_point(size = 4) + 
   coord_flip() + 
   facet_wrap(~group,scales = "free_y") + 
   scale_color_viridis_d() + 
   xlab("Predictor") + 
   ylab("Coefficient Estimate")

# coefficients only important in Stockholm model
coefficients_comparison_separate_models %>% 
   filter(Berlin_resampling == 0) %>% 
   arrange(desc(Stockholm_resampling)) %>% 
   filter((row_number() <= 12) | 
             (row_number() >= (n())-11)) %>% 
   mutate(group = factor(ifelse(row_number() <= 12,
                                "Positive",
                                "Negative"),
                         levels = c("Positive",
                                    "Negative"))) %>% 
   select(coefficient, 
          Berlin = Berlin_resampling, 
          Stockholm = Stockholm_resampling,
          group)  %>% 
   gather(key,value,-coefficient,-group) %>% 
   ggplot(aes(x = coefficient, 
              y = value, 
              color = key)) + 
   geom_point(size = 4) + 
   coord_flip() + 
   facet_wrap(~group,scales = "free_y") + 
   scale_color_viridis_d() + 
   xlab("Predictor") + 
   ylab("Coefficient Estimate")

# unified model
coefficients_comparison_unified_model <-
   coefficients_all_models %>% 
   filter(model %in% c("unified"), 
          !coefficient %in% c("(Intercept)","city_dummy"))
average_difference_unified <-
   preds_berlin_five_points %>% filter(model == "unified_model") %>% 
   select(-model) %>% 
   spread(city_used,pred) %>% summarise(mean(Berlin-Stockholm)) %>% pull(1)
average_difference_unified

coefficients_summary_unified_model <-
   coefficients_comparison_unified_model %>% 
   mutate(coefficient_type = ifelse(grepl(coefficient, pattern = "^Stockholm_"),
                                    "city_interaction",
                                    "main_effect"
   ),
   coefficient = str_replace_all(coefficient, 
                                 pattern = c("^Stockholm_"=""))) %>% 
   spread(coefficient_type,value) %>% 
   summarise(non_zero_main_effect = mean(main_effect != 0),
             non_zero_city_interaction = mean(city_interaction != 0)) %>% 
   gather(key,value) %>% deframe
coefficients_summary_unified_model
