#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
################################################################################

# required: plyr, ggplot2
library( 'plyr' )
library( 'ggplot2' )

################################################################################
# BEGIN FUNCTIONS
################################################################################

# decimal transformation function
scaleFUN <- function(x) sprintf("%.2f", x )

# decimal transformation function
scaleFUN1 <- function(x) sprintf("%.1f", x )


# ticks
number_ticks <- function( n ) { function( limits ) pretty( limits, n ) }

###
# Get test data.
##
prepare_test_data <- function()
{
    # read input
    final <- as.data.frame( readxl::read_xlsx( 'data/test_output_22aug22.xlsx' ) )

    final$abstract <- NULL
    final$text <- NULL
    final$title <- NULL
    
    final$probabilities <- gsub( '\\[|\\]|\\n', '', final$probabilities )
    
    prob <- data.frame( stringr::str_split_fixed( final$probabilities, ' ', 7 ) )
    colnames( prob ) <- c( 'prob_human_intervention', 'prob_meta_analysis', 'prob_misc', 
                            'prob_protocol', 'prob_randomized_trial', 
                            'prob_rodent', 'prob_systematic_review' )
    
    for( i in 1:ncol( prob ) )
    {
        prob[ , i ] <- as.numeric( prob[ , i ] )  
    }
    
    out <- cbind( final, prob )
    out$probabilities <- NULL
    out$...1 <- NULL
    out$level_0 <- NULL
    out$index <- NULL
    
    return( out )
}

###
# Suppose a 2x2 table with notation
#               Reference	
#   Predicted	Event	No Event
#       Event	A	    B
#    No Event	C	    D
##
get_two_by_two_table <- function( response, predictor, thres_value )
{
    # continuous predictor -> binary predictor, based on theshold
    pred_bin <- rep.int( 0, times = length( predictor ) )
    pred_bin[ predictor > thres_value ] <- 1
    
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
    out <- cbind( thres_value, N, TP, TN, FP, FN,
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
# Get performance of ROC-curve.
##
get_performance_roc <- function( roc )
{
    
    # get mean + 95% CIs for AUC
    aucRaw <- pROC::ci.auc( roc )
    
    # add AUC to data.frame
    aucMean <- aucRaw[ 2 ]
    aucLower <- aucRaw[ 1 ]
    aucUpper <- aucRaw[ 3 ]
    
    out <- cbind( aucMean = round( aucMean, 3 ), aucLower = round( aucLower, 3 ), aucUpper = round( aucUpper, 3 ) )
    
    rownames( out ) <- NULL
    out <- data.frame( out )
    
    return( out )
}

###
# Get binarized predictor
##
get_pred_bin <- function( predictor, thres_value )
{
    # continuous predictor -> binary predictor, based on threshold
    pred_bin <- rep.int( 0, times = length( predictor ) )
    pred_bin[ predictor > thres_value ] <- 1   
    
    return( pred_bin )
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

# outdir 
outdir <- 'out.00.val'
dir.create( outdir, showWarnings = FALSE )

# get test data
df <- prepare_test_data()

# write raw data to disk
write.csv( df, file = paste0( outdir, '/data_test.csv' ) )

#########
## ROC ##
#########

labels <- c( 'human_intervention', 'meta_analysis', 'misc', 
                'protocol', 'randomized_trial', 
                'rodent', 'systematic_review' )

# container
collect <- NULL

for( label in labels )
{
    response <- as.integer( df$standard == label )
    text <- paste0( 'prob_', label )
    
    predictor <- df[, text ]
    
    roc_levels <- base::levels( as.factor( response ) ) # '0',  1'

    # get ROC-curve
    roc <- pROC::roc( response = response, predictor = predictor, levels = roc_levels, direction = '<' )
    
    # prepare plotting data.frame (with sens/spec as percentage)
    data <- data.frame( threshold = roc$thresholds, spec = 100 * roc$specificities, sens = 100 * roc$sensitivities )
    
    # roc curve with inverted specificity axis
    p <- ggplot( data = data, aes( x = spec, y = sens ) ) +
        geom_line( size = 1.2, colour = '#00AFBB' ) +
        geom_abline( intercept = 100, slope = 1, size = 1, linetype = 3, colour = '#E7B800' ) +
        xlab( "Specificity (%)" ) +				
        ylab( "Sensitivity (%)" ) +
        scale_y_continuous( breaks = number_ticks( 8 ) ) +
        scale_x_reverse( breaks = number_ticks( 8 ) ) +
        theme_classic() 
    
    # save
    ggsave( plot = p, file = paste0( outdir, '/p_roc__', label, '.png' ), dpi = 300, height = 5, width = 5, bg = 'white' )

    # remove infinite
    data <- data[ !is.infinite( data$threshold ), ]

    # get values
    #sens_value <- 95
    #spec_value <- max( data[ data$sens > sens_value, 'spec' ] )
    #sens_value <- max( data[ data$spec == spec_value, 'sens' ] )
    #thres_value <- data[ ( data$sens == sens_value & data$spec == spec_value ), 'threshold' ]
    thres_value <- 0.5
    
    print( paste0( "[sens, spec, thres]: ", sens_value, ' ', spec_value, ' ', thres_value ) )
    
    # add cross-hair
    p_roc <- p + geom_vline( xintercept = spec_value, size = 0.5, linetype = 3, colour = 'gray30' ) +
                geom_hline( yintercept = sens_value, size = 0.5, linetype = 3, colour = 'gray30' )
    
    # save p only
    ggsave( plot = p_roc, file = paste0( outdir, '/p_roc__spec_', round( spec_value, 1 ), 
                                         '__sens_', round( sens_value, 1 ), '__', label, '.png' ), 
            dpi = 300, height = 5, width = 5, bg = 'white' )

    # get 2x2 table
    mtx <- get_two_by_two_table( response, predictor, thres_value )

    # binarize predictor
    pred_bin <- get_pred_bin( predictor, thres_value )
    
    # get performance
    out_bin <- get_performance_mtx( mtx )
    out_roc <- get_performance_roc( roc )
    
    # combine outputs
    out <- cbind( label = rep( label, nrow( out_roc ) ), out_roc, out_bin )

    # add to container
    collect <- rbind( collect, out )
    
    # write to disk
    write.csv( out, file = paste0( outdir, '/performance__threshold_', thres_value, '__', label, '.csv' ) )
}

# collect
readr::write_tsv( collect, file = paste0( outdir, '/combined_performance.tsv' ), quote = 'all' )
