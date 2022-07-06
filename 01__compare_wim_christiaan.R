# kappa


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

###
# Get kappa data for type
##
get_kappa_data <- function( type )
{
    # infiles
    infile_c <- paste0( 'data.christiaan/labeled_data/dataset_', type, '.csv' )
    infile_w <- paste0( 'data.wim/labeled_data/dataset_', type, '.csv' )
    
    # get data
    df_c <- read.csv( infile_c )
    df_w <- read.csv( infile_w )
    
    df_c$record_id <- NULL
    df_c$asreview_ranking <- NULL
    
    df_w$record_id <- NULL
    df_w$asreview_ranking <- NULL
    
    dim( df_c )
    dim( df_w )
    
    df_c$included_c <- df_c$included
    df_w$included_w <- df_w$included
    
    df_c$included <- NULL
    df_w$included <- NULL
    
    m <- merge( df_c, df_w )
    
    out <- m[ !is.na( m$included_c ) & !is.na( m$included_w ), ]
    
    res <- get_kappa( out$included_c, out$included_w )
    res$type <- type
    
    final <- list( res = res, m = m ) 
    
    return( final )
}

################################################################################
#
################################################################################


m_final <- get_kappa_data( type = 'meta-analysis-only' )
s_final <- get_kappa_data( type = 'systematic-reviews-only' )
r_final <- get_kappa_data( type = 'rodent-studies-only' )

res <- rbind( m_final$res, s_final$res, r_final$res )


