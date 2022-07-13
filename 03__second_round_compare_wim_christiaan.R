# kappa
# second round
################################################################################

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
    infile_c <- paste0( 'data.christiaan/second_round/dataset_', type, '.csv' )
    infile_w <- paste0( 'data.wim/second_round/dataset_', type, '.csv' )
    
    # get data
    df_c <- read.csv( infile_c )
    df_w <- read.csv( infile_w )
    
    df_c <- df_c[ , colnames( df_c ) %in% c( 'pmid', 'included', 'title', 'abstract' ) ]
    df_w <- df_w[ , colnames( df_w ) %in% c( 'pmid', 'included', 'title', 'abstract' ) ]
    
    df_c$included_c <- df_c$included
    df_w$included_w <- df_w$included
    
    df_c$included <- NULL
    df_w$included <- NULL
    
    m <- merge( df_c, df_w )
    
    out <- m[ !is.na( m$included_c ) & !is.na( m$included_w ), ]
    
    res <- get_kappa( out$included_c, out$included_w )
    res$type <- type
    
    # get total disagreement
    res$n_disagree <- sum( out$included_c != out$included_w )
    
    # into list
    final <- list( res = res, out = out ) 
    
    return( final )
}

################################################################################
#
################################################################################

# outdir
outdir <- 'out.03.second.round.compare'
dir.create( outdir, showWarnings = FALSE )

#s_final <- get_kappa_data( type = '1-round-ii-systematic-reviews' )
m_final <- get_kappa_data( type = '2-round-ii-meta-analysis' )
r_final <- get_kappa_data( type = '3-round-ii-rodents' )
h_final <- get_kappa_data( type = '4-round-ii-humans' )
#t_final <- get_kappa_data( type = '5-round-ii-trials' )
rt_final <- get_kappa_data( type = '6-round-ii-randomised-trials' )
c_final <- get_kappa_data( type = '7-round-ii-children' )
p_final <- get_kappa_data( type = '8-round-ii-protocols' )

# combine
res <- rbind( #s_final$res, 
                m_final$res, r_final$res, h_final$res, 
                #t_final$res, 
                rt_final$res, c_final$res, p_final$res )

# write to file
readr::write_tsv( res, file = paste0( outdir, '/kappas_w_c.tsv' ) )

##############################
# write disagreements to file
##############################

# 1) 
#dim( disagreements_s <- s_final$out[ s_final$out$included_c != s_final$out$included_w, ] )

# 2) 
dim( disagreements_m <- m_final$out[ m_final$out$included_c != m_final$out$included_w, ] )

# 3) 
dim( disagreements_r <- r_final$out[ r_final$out$included_c != r_final$out$included_w, ] )

# 4) 
dim( disagreements_h <- h_final$out[ h_final$out$included_c != h_final$out$included_w, ] )

# 5) 
#dim( disagreements_t <- t_final$out[ t_final$out$included_c != t_final$out$included_w, ] )

# 6) 
dim( disagreements_rt <- rt_final$out[ rt_final$out$included_c != rt_final$out$included_w, ] )

# 7) 
dim( disagreements_c <- c_final$out[ c_final$out$included_c != c_final$out$included_w, ] )

# 8) 
dim( disagreements_p <- p_final$out[ p_final$out$included_c != p_final$out$included_w, ] )


# write to file
#readr::write_tsv( disagreements_s, file = paste0( outdir, '/disagreements_w_c__systematic-reviews.tsv' ) )
readr::write_tsv( disagreements_m, file = paste0( outdir, '/disagreements_w_c__meta-analysis.tsv' ) )
readr::write_tsv( disagreements_r, file = paste0( outdir, '/disagreements_w_c__rodents.tsv' ) )
readr::write_tsv( disagreements_h, file = paste0( outdir, '/disagreements_w_c__human.tsv' ) )
#readr::write_tsv( disagreements_t, file = paste0( outdir, '/disagreements_w_c__trials.tsv' ) )
readr::write_tsv( disagreements_rt, file = paste0( outdir, '/disagreements_w_c__randomised-trials.tsv' ) )
readr::write_tsv( disagreements_c, file = paste0( outdir, '/disagreements_w_c__child.tsv' ) )
readr::write_tsv( disagreements_p, file = paste0( outdir, '/disagreements_w_c__protocols.tsv' ) )


