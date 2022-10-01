#!/usr/bin/env Rscript
#
# Wim Otte
#
# Get sample from PubMed to use for AI, using PubMed's own filters.
###################################################################

library( "rentrez" )

################################################################################
# FUNCTIONS
################################################################################

###
# Get pmids.
##
get_pmids <- function( type = 'meta_analysis' )
{
    if( type == 'meta_analysis' )
    {
        # prior covid
        term <- 'all[sb] AND 2018/01/01:2018/01/01[dp] AND hasabstract AND "meta-analysis"[pt]'
    }
    
    # sr
    if( type == 'systematic_review' )
    {
        term <- 'all[sb] AND 2018/01/01:2018/01/01[dp] AND hasabstract AND "systematic review"[pt]'
    }
    
    # rct
    if( type == 'randomized_controlled_trial' )
    {
        term <- 'all[sb] AND 2018/01/01:2018/01/01[dp] AND hasabstract AND "randomized controlled trial"[pt]'
    }
        
    # get max 30,000 records
    results <- rentrez::entrez_search( "pubmed", term = term, retmax = 30000 )
    
    # get PMIDS
    pmids <- results$ids
    
    return( pmids )
}

###
# Suppose a 2x2 table with notation
#               Reference	
#   Predicted	Event	No Event
#       Event	A	    B
#    No Event	C	    D
##
get_two_by_two_table <- function( response, pred_bin )
{
    TP <- sum( pred_bin == 1 & response == 1 )
    TN <- sum( pred_bin == 0 & response == 0 )
    
    FP <- sum( pred_bin == 1 & response == 0 )
    FN <- sum( pred_bin == 0 & response == 1 )
    
    # 2x2 Table
    mtx <- as.table( matrix( c( TP, FP, FN, TN ), nrow = 2, byrow = TRUE,
                             dimnames=list( predicted = c( "Event", "No Event" ),
                                            reference = c( "Event", "No Event" ) ) ) ) 
    
    return( mtx )
}

###
# Get performance based on 2x2 table.
##
get_performance_mtx <- function( mtx )
{
    # get cells
    TP <- mtx[ 'Event', 'Event' ]
    TN <- mtx[ 'No Event', 'No Event' ]
    
    FP <- mtx[ 'Event', 'No Event' ]
    FN <- mtx[ 'No Event', 'Event' ]
    
    # Total
    N <- TP + TN + FP + FN
    
    ## Sensitivity with 95% confidence interval 
    sensMean <- caret::sensitivity( mtx )
    sens_errors <- sqrt( caret::sensitivity( mtx ) * ( 1 - caret::sensitivity( mtx ) ) / sum( mtx[ , 1 ] ) )
    sensLower <- caret::sensitivity( mtx ) - 1.96 * sens_errors
    sensUpper <- caret::sensitivity( mtx ) + 1.96 * sens_errors
    
    ## Specificity with 95% confidence interval 
    specMean <- caret::specificity( mtx )
    spec_errors <- sqrt( caret::specificity( mtx ) * ( 1 - caret::specificity( mtx ) ) / sum( mtx[ , 2 ] ) )
    specLower <- caret::specificity( mtx ) - 1.96 * spec_errors
    specUpper <- caret::specificity( mtx ) + 1.96 * spec_errors
    
    ## Positive Predictive Value with 95% confidence interval 
    ppvMean <- caret::posPredValue( mtx )
    ppv_errors <- sqrt( caret::posPredValue( mtx ) * ( 1 - caret::posPredValue( mtx ) ) / sum( mtx[ 1 , ] ) )
    ppvLower <- caret::posPredValue( mtx ) - 1.96 * ppv_errors
    ppvUpper <- caret::posPredValue( mtx ) + 1.96 * ppv_errors
    
    ## Negative Predictive Value with 95% confidence interval 
    npvMean <- caret::negPredValue( mtx )
    npv_errors <- sqrt( caret::negPredValue( mtx ) * ( 1 - caret::negPredValue( mtx ) ) / sum( mtx[ 2 , ] ) )
    npvLower <- caret::negPredValue( mtx ) - 1.96 * npv_errors
    npvUpper <- caret::negPredValue( mtx ) + 1.96 * npv_errors
    
    ## Accuracy with 95% confidence interval
    additional <- caret::confusionMatrix( mtx )
    acc <- additional$overall[ c( 'Accuracy', 'AccuracyLower', 'AccuracyUpper' ) ]
    accMean <- acc[ 1 ]
    accLower <- acc[ 2 ]
    accUpper <- acc[ 3 ]
    
    ## Balanced accuracy (average of sens/spec)
    baccMean <- ( sensMean + specMean ) / 2
    baccLower <- ( sensLower + specLower ) / 2
    baccUpper <- ( sensUpper + specUpper ) / 2
    
    if( sensLower < 0 )
        sensLower <- 0
    
    if( specLower < 0 )
        specLower <- 0
    
    if( ppvLower < 0 )
        ppvLower <- 0
    
    if( npvLower < 0 )
        npvLower <- 0
    
    # collect output
    out <- cbind( N, TP, TN, FP, FN,
                  baccMean = round( baccMean, 3 ), baccLower = round( baccLower, 3 ), baccUpper = round( baccUpper, 3 ),
                  accMean = round( accMean, 3 ), accLower = round( accLower, 3 ), accUpper = round( accUpper, 3 ),  
                  sensMean = round( sensMean, 3 ), sensLower = round( sensLower, 3 ), sensUpper = round( sensUpper, 3 ),  
                  specMean = round( specMean, 3 ), specLower = round( specLower, 3 ), specUpper = round( specUpper, 3 ), 
                  ppvMean = round( ppvMean, 3 ), ppvLower = round( ppvLower, 3 ), ppvUpper = round( ppvUpper, 3 ), 
                  npvMean = round( npvMean, 3 ), npvLower = round( npvLower, 3 ), npvUpper = round( npvUpper, 3 ) ) 
    
    rownames( out ) <- NULL
    out <- data.frame( out )
    
    return( out )
}

###
# Get kappa
##
get_kappa <- function( response, pred_bin )
{
    # get Cohen's kappa
    ck <- psych::cohen.kappa( data.frame( response, pred_bin ) )
    
    kappaMean <- ck$confid[ 'unweighted kappa', 'estimate' ]
    kappaLower <- ck$confid[ 'unweighted kappa', 'lower' ]
    kappaUpper <- ck$confid[ 'unweighted kappa', 'upper' ]
    
    out <- cbind( kappaMean = round( kappaMean, 3 ), kappaLower = round( kappaLower, 3 ), kappaUpper = round( kappaUpper, 3 ) )
    
    rownames( out ) <- NULL
    out <- data.frame( out )
    
    return( out )
}

################################################################################
# END FUNCTIONS
################################################################################

# output dir
outdir <- 'out.06.filtered.pubmed'
dir.create( outdir, showWarnings = FALSE )

# get pmids
length( pmids_meta <- get_pmids( 'meta_analysis' ) )    # 1713
length( pmids_sr <- get_pmids( 'systematic_review' ) )  # 2152
length( pmids_rct <- get_pmids( 'randomized_controlled_trial' ) ) # 3053

# dfs
df_meta <- data.frame( pubmed_label = 'meta_analysis', pmid = pmids_meta )
df_sr <- data.frame( pubmed_label = 'systematic_review', pmid = pmids_sr )
df_rct <- data.frame( pubmed_label = 'randomized controlled trial', pmid = pmids_rct )

# write to file
write.csv( df_meta, file = paste0( outdir, '/pmids_meta_analysis.csv' ) )
write.csv( df_sr, file = paste0( outdir, '/pmids_systematic_review.csv' ) )
write.csv( df_rct, file = paste0( outdir, '/pmids_randomized_controlled_trial.csv' ) )


# read 5000 manually labeled data (determined in two iterations)
# with empty abstracts removed: N=4867
df <- data.frame( readr::read_tsv( 'out.05.prepare.full.set/full_labeled.tsv.gz' ) )
df$abstract <- NULL
df$text <- NULL

# systematic review
#df$pubmed_systematic_review <- 0
#df[ df$pmid %in% df_sr$pmid, 'pubmed_systematic_review' ] <- 1

# meta
df$pubmed_meta <- 0
df[ df$pmid %in% df_meta$pmid, 'pubmed_meta' ] <- 1

# rct
df$pubmed_rct <- 0
df[ df$pmid %in% df_rct$pmid, 'pubmed_rct' ] <- 1


####### META ######

# get 2x2 table
mtx_meta <- get_two_by_two_table( df$meta_analysis, df$pubmed_meta )

# get performance
perf_meta <- get_performance_mtx( mtx_meta )

# get kappa
kappa_meta <- get_kappa( df$meta_analysis, df$pubmed_meta )

# combine outputs
out_meta <- cbind( label = 'meta_analysis', perf_meta, kappa_meta )
    


####### RCT ######

# get 2x2 table
mtx_rct <- get_two_by_two_table( df$randomized_trial, df$pubmed_rct )

# get performance
perf_rct <- get_performance_mtx( mtx_rct )

# get kappa
kappa_rct <- get_kappa( df$randomized_trial, df$pubmed_rct )

# combine outputs
out_rct <- cbind( label = 'rct', perf_rct, kappa_rct )



### Combine and save ###
comb <- rbind( out_meta, out_rct )

# collect
readr::write_tsv( comb, file = paste0( outdir, '/pubmed_filter_performance.tsv' ), quote = 'all' )


