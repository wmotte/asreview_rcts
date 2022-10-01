#!/usr/bin/env Rscript
#
# Wim Otte
#
# Validate.
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
outdir <- 'out.02.validate'
dir.create( outdir, showWarnings = FALSE )

# get data
df <- read.csv( 'out.01.combined.with.predictions/single_label_dataset__combined.csv', row.names = 1 )

#df$label <- gsub( "human-intervention", "human_intervention", df$label )
#df$label <- gsub( "meta-analysis", "meta_analysis", df$label )
#df$label <- gsub( "systematic-review", "systematic_review", df$label )
#df$label <- gsub( "rct", "randomized_trial", df$label )

# loops
users <- c( 'wim', 'christiaan' )

# container
collect <- NULL

for( user in users )
{
    dim( df_user <- df[ df$who == user, ] )

    # get labels    
    labels <- unique( df_user$label )
    
    for( label in labels )
    {
        print( paste( user, label ) )
        
        # reference labels manually entered by user
        reference <- as.integer( df_user$label == label )
        
        # predictions
        pred_bin <- as.integer( df_user$ml_type == label )
        
         # get 2x2 table
        mtx <- get_two_by_two_table( reference, pred_bin )
        
        # get performance
        out_bin <- get_performance_mtx( mtx )
        
        # combine outputs
        out <- cbind( who = rep( user, nrow( out_bin ) ), label = rep( label, nrow( out_bin ) ), out_bin )
        
        # add to container
        collect <- rbind( collect, out )
        
    }
}

# collect
readr::write_tsv( collect, file = paste0( outdir, '/combined_performance.tsv' ), quote = 'all' )




