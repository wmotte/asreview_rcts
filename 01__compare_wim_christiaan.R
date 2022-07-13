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
    
    out <- cbind( n = length( response ), kappaMean = round( kappaMean, 3 ), kappaLower = round( kappaLower, 3 ), kappaUpper = round( kappaUpper, 3 ) )
    
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
    
    final <- list( res = res, out = out ) 
    
    return( final )
}

################################################################################
#
################################################################################

# outdir
outdir <- 'out.01.compare'
dir.create( outdir, showWarnings = FALSE )


m_final <- get_kappa_data( type = 'meta-analysis-only' )
s_final <- get_kappa_data( type = 'systematic-reviews-only' )
r_final <- get_kappa_data( type = 'rodent-studies-only' )
h_final <- get_kappa_data( type = 'human-studies-only' )
p_final <- get_kappa_data( type = 'protocols-only' )

# TODO
#rt_final <- get_kappa_data( type = 'randomised-trials' )
#c_final <- get_kappa_data( type = 'children-only' )



# combine
res <- rbind( m_final$res, s_final$res, r_final$res, h_final$res, p_final$res )

# write to file
readr::write_tsv( res, file = paste0( outdir, '/kappas_w_c.tsv' ) )

# write disagreements to file

# n = 7 disagreements
dim( disagreements_m <- m_final$out[ m_final$out$included_c != m_final$out$included_w, ] )

# n = 32 disagreements
dim( disagreements_s <- s_final$out[ s_final$out$included_c != s_final$out$included_w, ] )

# n = 46 disagreements
dim( disagreements_r <- r_final$out[ r_final$out$included_c != r_final$out$included_w, ] )

# n = 27 disagreements
dim( disagreements_h <- h_final$out[ h_final$out$included_c != h_final$out$included_w, ] )
disagreements_h$Unnamed..0 <- NULL

# n = 4 disagreements
dim( disagreements_p <- p_final$out[ p_final$out$included_c != p_final$out$included_w, ] )

# write to file
readr::write_tsv( disagreements_m, file = paste0( outdir, '/disagreements_w_c__meta-analysis.tsv' ) )
readr::write_tsv( disagreements_s, file = paste0( outdir, '/disagreements_w_c__systematic-reviews.tsv' ) )
readr::write_tsv( disagreements_r, file = paste0( outdir, '/disagreements_w_c__rodents.tsv' ) )
readr::write_tsv( disagreements_h, file = paste0( outdir, '/disagreements_w_c__human.tsv' ) )
readr::write_tsv( disagreements_p, file = paste0( outdir, '/disagreements_w_c__protocols.tsv' ) )

