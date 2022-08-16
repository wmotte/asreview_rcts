# read input and combine into single df.

###
# Get part 1.
##
get_part_one <- function()
{
    dim( df_m <- readr::read_tsv( '../aug2022/labeled/out.cleaned/meta_analysis.tsv.gz' ) )
    df_m$class <- 'meta_analysis'
    
    dim( df_sr <- readr::read_tsv( '../aug2022/labeled/out.cleaned/systematic_review.tsv.gz' ) )
    df_sr$class <- 'systematic_review'
    
    dim( df_p <- readr::read_tsv( '../aug2022/labeled/out.cleaned/protocol.tsv.gz' ) )
    df_p$class <- 'protocol'
    
    dim( df_m <- df_m[ df_m$included == 1, ] )      # 208
    dim( df_sr <- df_sr[ df_sr$included == 1, ] )   # 151
    dim( df_p <- df_p[ df_p$included == 1, ] )      # 226
    
    # combine
    dim( df <- as.data.frame( rbind( df_m, df_sr, df_p ) ) )
    summary( as.factor( df$class ) )
    
    # process duplicates [n=62]
    summary( dup <- duplicated( df$pmid ) )
    
    pmids <- df[ dup, 'pmid' ]
    pmid <- pmids[ 1 ]
    
    for( pmid in pmids )
    {
        labels <- df[ df$pmid == pmid, 'class' ]   
        
        # if both meta and sr -> meta
        if( ( sum( stringr::str_detect( labels, 'meta_analysis' ) ) > 0 ) &
            ( sum( stringr::str_detect( labels, 'systematic_review' ) ) > 0 ) )
        {
            print( labels )
            df[ df$pmid == pmid, 'class' ] <- 'meta_analysis'
        } else{
            if( sum( stringr::str_detect( labels, 'protocol' ) ) > 0 )
            {
                df[ df$pmid == pmid, 'class' ] <- 'protocol'
            } else {
                print( paste0( "Different: ", labels ) ) 
            }
        }
    }
    
    # remove duplicates [total: 523]
    dim( df <- df[ !duplicated( df$pmid ), ] )
    summary( as.factor( df$class ) )
    
    df$included <- NULL
    
    return( df )
}

###
# Get part 2.
##
get_part_two <- function()
{
    dim( df_i <- readr::read_tsv( '../aug2022/labeled/out.cleaned/trial.tsv.gz' ) )
    df_i$class <- 'human_intervention'
    
    dim( df_rct <- readr::read_tsv( '../aug2022/labeled/out.cleaned/randomized_trial.tsv.gz' ) )
    df_rct$class <- 'randomized_trial'
    
    dim( df_i <- df_i[ df_i$included == 1, ] )          # 158
    dim( df_rct <- df_rct[ df_rct$included == 1, ] )    # 195
    
    # combine
    dim( df2 <- as.data.frame( rbind( df_i, df_rct ) ) )
    summary( as.factor( df2$class ) )
    
    # process duplicates [n=62]
    summary( dup <- duplicated( df2$pmid ) )
    
    pmids <- df2[ dup, 'pmid' ]
    pmid <- pmids[ 1 ]
    
    for( pmid in pmids )
    {
        labels <- df2[ df2$pmid == pmid, 'class' ]   
        
        # if both meta and sr -> meta
        if( ( sum( stringr::str_detect( labels, 'human_intervention' ) ) > 0 ) &
            ( sum( stringr::str_detect( labels, 'randomized_trial' ) ) > 0 ) )
        {
            print( labels )
            df2[ df2$pmid == pmid, 'class' ] <- 'randomized_trial'
        }
    }
    
    # remove duplicates [total: 284]
    dim( df2 <- df2[ !duplicated( df2$pmid ), ] )
    summary( as.factor( df2$class ) )
    
    df2$included <- NULL
    
    return( df2 )
}

###
# Rodents + control group (misc)
##
get_part_three <- function()
{
    #'rodent' (192)
    basis <- as.data.frame( readr::read_tsv( '../out.05.prepare.full.set/full_labeled.tsv.gz' ) )
    
    basis[1,]
    # make 'misc' group
    score <- basis$systematic_review + basis$meta_analysis + basis$rodent + basis$human + basis$trial + basis$randomized_trial + basis$child + basis$protocol
    basis$misc <- 1
    basis[ score > 0, 'misc' ] <- 0
    
    df_misc <- basis[ basis$misc == 1, ]
    df_misc$text <- NULL
    df_misc$class <- 'misc'
    df_misc <- df_misc[ , c( 'pmid', 'title', 'abstract', 'class' ) ]
    
    dim( df_misc )
    colnames( df_misc )
    
    # rodents
    dim( df_rodent <- basis[ basis$rodent == 1, ] )
    df_rodent$text <- NULL
    df_rodent$class <- 'rodent'
    df_rodent <- df_rodent[ , c( 'pmid', 'title', 'abstract', 'class' ) ]
    
    out <- rbind( df_rodent, df_misc )
    return( out )
}

################################################################################
# outdir
outdir <- 'out.single'
dir.create( outdir, showWarnings = FALSE )

####################
# PART 2: trials
####################

dfa <- get_part_one()
dfb <- get_part_two()
dfc <- get_part_three()

summary( as.factor( dfa$class ) )
summary( as.factor( dfb$class ) )
summary( as.factor( dfc$class ) )

all <- rbind( dfa, dfb, dfc )
summary( as.factor( all$class ) )

colnames( all )

# write to disk
readr::write_tsv( all, file = gzfile( paste0( outdir, '/single_class.tsv.gz' ) ), quote = 'all' )

