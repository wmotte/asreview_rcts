# machine-learning
#
# https://cfss.uchicago.edu/notes/supervised-text-classification/#sample-set-of-documents-uscongress
################################################################################

library( 'tidyverse' )
library( 'tidymodels' )
library( 'tidytext' )

set.seed( 1234 )
theme_set( theme_minimal() )

################################################################################
# END FUNCTIONS
################################################################################

# outdir
outdir <- 'out.02.machinelearning'
dir.create( outdir, showWarnings = FALSE )

# load data
df <- read.csv( 'data.wim/labeled_data/dataset_systematic-reviews-only.csv' )
df <- df[ !is.na( df$included ), ]
df$record_id <- df$asreview_ranking <- NULL
df$text <- paste0( df$title, '|', df$abstract )
df$title <- df$abstract <- NULL

df$included <- as.factor( df$included )

# save training data
readr::write_tsv( df, file = paste0( outdir, '/training_data_s.tsv' ) )

# 413
dim( df )
colnames( df )


###########
#

set.seed( 123 )

# train 80% / test 20%
data_split <- initial_split( data = df, strata = included, prop = 0.99 )

# split
dim( data_train <- training( data_split ) )
dim( data_test <- testing( data_split ) )

# preprocessing step
data_rec <- recipe( included ~ text, data = data_train )



library( 'textrecipes' ) # required for step_tfidf

data_rec <- data_rec %>%
    step_tokenize( text ) %>%
    step_stopwords( text ) %>%
    step_tokenfilter( text, max_tokens = 500 ) %>%
    step_tfidf( text )

library( 'discrim' )

# niave Bayes model
nb_spec <- naive_Bayes() %>%
    set_mode( "classification" ) %>%
    set_engine( "naivebayes" )

nb_spec

# workflow
nb_wf <- workflow() %>%
    add_recipe( data_rec ) %>%
    add_model( nb_spec )
nb_wf

library( 'naivebayes' )

# fit
nb_wf %>%
    fit( data = data_train )


################ EVAL ##########

set.seed( 1234 )

data_folds <- vfold_cv( data = data_train, strata = included )
data_folds

nb_cv <- nb_wf %>%
    fit_resamples(
        data_folds,
        control = control_resamples( save_pred = TRUE )
    )

# metrics
nb_cv_metrics <- collect_metrics( nb_cv )
nb_cv_predictions <- collect_predictions( nb_cv )

nb_cv_metrics

# null model
null_classification <- null_model() %>%
    set_engine("parsnip") %>%
    set_mode("classification")

null_cv <- workflow() %>%
    add_recipe( data_rec ) %>%
    add_model(null_classification) %>%
    fit_resamples(
        data_folds
    )

null_cv_metrics <- null_cv %>% collect_metrics()


ggplot( data = df, mapping = aes(x = fct_infreq( included) %>% fct_rev())) +
    geom_bar() +
    coord_flip() +
    labs(
        title = "Distribution of legislation",
        subtitle = "By major policy topic",
        x = NULL,
        y = "Number of bills"
    )

nb_cv_metrics
null_cv_metrics
