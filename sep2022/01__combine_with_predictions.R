#!/usr/bin/env Rscript
#
# Wim Otte
#
# Combine with predictions.
#
# 1. systematic_review
# 2. meta_analysis
# 3. human_intervention
# 4. randomized_trial
# 5. protocol
# 6. rodent
########################################


################################################################################
# FUNCTIONS
################################################################################

###
# Get manual data
##
get_manual_data_christiaan <- function()
{
    sys_s <- read.csv( 'manually_labeled/christiaan/asreview_dataset_2500-systematic-review.csv' )
    sys_s$label <- 'misc'
    sys_s[ sys_s$included == 1 & !is.na( sys_s$included ), 'label' ] <- 'systematic_review'
    
    sys_m <- read.csv( 'manually_labeled/christiaan/asreview_dataset_2500-meta-analysis.csv' )
    sys_p <- read.csv( 'manually_labeled/christiaan/asreview_dataset_2500-protocols.csv' )
    sys_r <- read.csv( 'manually_labeled/christiaan/asreview_dataset_2500-rodent.csv' )
    sys_t <- read.csv( 'manually_labeled/christiaan/asreview_dataset_2500-rcts.csv' )
    sys_h <- read.csv( 'manually_labeled/christiaan/asreview_dataset_2500-humanintervention.csv' )
    
    dim( sys_m <- sys_m[ sys_m$included == 1 & !is.na( sys_m$included ), ] )
    dim( sys_p <- sys_p[ sys_p$included == 1 & !is.na( sys_p$included ), ] )
    dim( sys_r <- sys_r[ sys_r$included == 1 & !is.na( sys_r$included ), ] )
    dim( sys_t <- sys_t[ sys_t$included == 1 & !is.na( sys_t$included ), ] )
    dim( sys_h <- sys_h[ sys_h$included == 1 & !is.na( sys_h$included ), ] )
    
    # human intervention
    sys_s[ sys_s$pmid %in% sys_h$pmid, 'label' ] <- 'human_intervention'
    
    # meta
    sys_s[ sys_s$pmid %in% sys_m$pmid, 'label' ] <- 'meta_analysis'
    
    # protocol
    sys_s[ sys_s$pmid %in% sys_p$pmid, 'label' ] <- 'protocol'
    
    # rodent
    sys_s[ sys_s$pmid %in% sys_r$pmid, 'label' ] <- 'rodent'    
    
    # rct
    sys_s[ sys_s$pmid %in% sys_t$pmid, 'label' ] <- 'randomized_trial'    
    
    sys_s$included <- NULL
    sys_s$asreview_ranking <- NULL
    sys_s$record_id <- NULL
    sys_s$who <- 'christiaan'
    
    # remove too much misc (biases validation, so trim to random 200)
    idx_misc <- sys_s$label == 'misc'
    
    # keep
    dim( df_keep <- sys_s[ !idx_misc, ] )
    dim( df_misc <- sys_s[ idx_misc, ] )
    
    set.seed( 123 )
    
    # set number, so that total sum == 1000
    n_misc <- 1000 - nrow( df_keep )
    
    dim( df_misc_subset <- df_misc[ sample( 1:nrow( df_misc ), n_misc ), ] )
    
    dim( out <- rbind( df_keep, df_misc_subset ) )
    
    return( out )
}

###
# Get manual data Wim
##
get_manual_data_wim <- function()
{
    sys_s <- read.csv( 'manually_labeled/wim/asreview_dataset_2500-systematic-review.csv' )
    sys_s$label <- 'misc'
    sys_s[ sys_s$included == 1 & !is.na( sys_s$included ), 'label' ] <- 'systematic_review'
    summary(as.factor( sys_s$label ))
    
    sys_m <- read.csv( 'manually_labeled/wim/asreview_dataset_2500-meta-analysis.csv' )
    sys_p <- read.csv( 'manually_labeled/wim/asreview_dataset_2500-protocols.csv' )
    sys_r <- read.csv( 'manually_labeled/wim/asreview_dataset_2500-rodent.csv' )
    sys_t <- read.csv( 'manually_labeled/wim/asreview_dataset_2500-rcts.csv' )
    sys_h <- read.csv( 'manually_labeled/wim/asreview_dataset_2500-humanintervention.csv' )
    
    dim( sys_m <- sys_m[ sys_m$included == 1 & !is.na( sys_m$included ), ] )
    dim( sys_p <- sys_p[ sys_p$included == 1 & !is.na( sys_p$included ), ] )
    dim( sys_r <- sys_r[ sys_r$included == 1 & !is.na( sys_r$included ), ] )
    dim( sys_t <- sys_t[ sys_t$included == 1 & !is.na( sys_t$included ), ] )
    dim( sys_h <- sys_h[ sys_h$included == 1 & !is.na( sys_h$included ), ] )
    
    # human intervention
    sys_s[ sys_s$pmid %in% sys_h$pmid, 'label' ] <- 'human_intervention'
    
    # meta
    sys_s[ sys_s$pmid %in% sys_m$pmid, 'label' ] <- 'meta_analysis'
    
    # protocol
    sys_s[ sys_s$pmid %in% sys_p$pmid, 'label' ] <- 'protocol'
    
    # rodent
    sys_s[ sys_s$pmid %in% sys_r$pmid, 'label' ] <- 'rodent'    
    
    # rct
    sys_s[ sys_s$pmid %in% sys_t$pmid, 'label' ] <- 'randomized_trial'    
    
    sys_s$included <- NULL
    sys_s$asreview_ranking <- NULL
    sys_s$record_id <- NULL
    sys_s$who <- 'wim'
    
    # remove too much misc (biases validation, so trim to random 200)
    idx_misc <- sys_s$label == 'misc'
    
    # keep
    dim( df_keep <- sys_s[ !idx_misc, ] )
    dim( df_misc <- sys_s[ idx_misc, ] )
    
    set.seed( 123 )
    
    # set number, so that total sum == 1000
    n_misc <- 1000 - nrow( df_keep )
    
    dim( df_misc_subset <- df_misc[ sample( 1:nrow( df_misc ), n_misc ), ] )
    
    dim( out <- rbind( df_keep, df_misc_subset ) )
    
    return( out )
}

###
# Get predicted labels
##
get_predicted_labels <- function()
{
    # read input
    final <- as.data.frame( readr::read_tsv( 'davids.predictions/tiabs_2500__set_christiaan_and_wim_outcomes_16sep22__predictions.tsv' ) )
    final$...1 <- NULL
    final$abstract <- NULL
    final$text <- NULL
    final$title <- NULL
    final$ml_probs <- NULL
    final$who <- NULL
    
    return( final )
}

################################################################################
# END FUNCTIONS
################################################################################

# output dir
outdir <- 'out.01.combined.with.predictions'
dir.create( outdir, showWarnings = FALSE )

# manual data
df_c <- get_manual_data_christiaan()
df_w <- get_manual_data_wim()

# check
summary( as.factor( df_c$label ) )
summary( as.factor( df_w$label ) )

# write to disk
write.csv( df_c, file = paste0( outdir, '/single_label_dataset__christiaan.csv' ) )
write.csv( df_w, file = paste0( outdir, '/single_label_dataset__wim.csv' ) )

# combine
dim( df_comb <- rbind( df_c, df_w ) )

# get predicted labels
dim( df_pred <- get_predicted_labels() )

# combine
dim( out <- merge( df_comb, df_pred ) )

# merge with predictions
write.csv( out, file = paste0( outdir, '/single_label_dataset__combined.csv' ) )








