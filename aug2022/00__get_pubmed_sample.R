#!/usr/bin/env Rscript
#
# Wim Otte
#
# Get sample from PubMed to use for AI.
########################################

library( "rentrez" )

################################################################################
# FUNCTIONS
################################################################################

###
# Get pmids.
##
get_pmids <- function()
{
    # prior covid
    term <- 'all[sb] AND 2018/01/01:2018/01/01[dp] AND hasabstract'
    
    # get 50,000 records
    results <- rentrez::entrez_search( "pubmed", term = term, retmax = 50000 )
    
    # get PMIDS
    pmids <- results$ids
    
    return( pmids )
}


###
# Get tiab.
##
get_tiab <- function( pmids )
{
    # create a splitting index
    idx <- split( seq( 1, length( pmids ) ), 
                  ceiling( seq_along( seq( 1, length( pmids ) ) ) / 100 ) )
    
    # create an empty list to hold summary contents
    all <- NULL
    
    for( i in 1:length( idx ) )
    {
        pids <- idx[ i ]
        pmin <- min( pids[[ 1 ]] )
        pmax <- max( pids[[ 1 ]] )
        
        print( paste0( "idx: ", i, " min: ", pmin, " max: ", pmax ) )
        
        tiabs <- rentrez::parse_pubmed_xml( rentrez::entrez_fetch( 
            db = "pubmed", id = pmids[ pmin:pmax ], rettype = "xml" ) )
        
        for( j in 1:length( tiabs ) )
        {
            pmid <- NA
            title <- NA
            abstract <- NA
            
            pmid <- paste( tiabs[[ j ]]$pmid, collapse = " " )
            title <- paste( tiabs[[ j ]]$title, collapse = " " )
            abstract <- paste( tiabs[[ j ]]$abstract, collapse = " " )
            
            if( is.list( pmid ) ){ pmid <- "" }
            if( is.list( title ) ){ title <- "" }            
            if( is.list( abstract ) ){ title <- "" }            
            
            single <- data.frame( pmid, title, abstract )
            all <- rbind( all, single )
        }
        Sys.sleep( 2 )
    }
    
    return( all )
} 

###
# Get gold data
## 
get_gold_data <- function( keepname )
{
    gold <- na.omit( read.csv( '../out.05.prepare.full.set/full_labeled.tsv.gz', sep = '\t' ) )
    gold$text <- NULL
    
    to_keep <- c( 'pmid', 'title', 'abstract', keepname )
    out <- gold[ , to_keep ]
    colnames( out ) <- c( 'pmid', 'title', 'abstract', 'included' )
    return( out )
}

################################################################################
# END FUNCTIONS
################################################################################

# output dir
outdir <- 'out.00.pubmed.data'
dir.create( outdir, showWarnings = FALSE )

# get 30,000 PMIDS
pmids <- get_pmids()

# get 50,000 tiabs
table_df <- get_tiab( pmids )

# fix explicit double quote in titles and abstracts
table_df$title <- gsub( '\"', '', table_df$title )
table_df$abstract <- gsub( '\"', '', table_df$abstract )

# remove empty titles
df <- table_df[ table_df$title != '', ]
df <- table_df[ table_df$abstract != '', ]

# write to disk
readr::write_tsv( table_df, file = gzfile( paste0( outdir, '/tiabs_50000__2022.tsv.gz' ) ) )

##################################

# calculate proportion
class <- c( 'systematic_review', 'meta_analysis', 'trial', 'randomized_trial', 'protocol' )
n <- c( 93, 40, 81, 39, 28 )

calc <- data.frame( class, n )
calc$ratio <- 250 / calc$n
calc$req_sample_size <- floor( calc$ratio * 5000 ) - 5000
sum( calc$req_sample_size[ 1:3 ] ) # 45,122
sum( calc$req_sample_size[ 4:5 ] ) # 66,693

head( calc )

write.csv( calc, file = 'out.00.pubmed.data/calc.csv' )

#####################

# get 50k
table_df <- data.frame( readr::read_tsv( 'out.00.pubmed.data/tiabs_50000__2022.tsv.gz' ) )

# fix explicit double quote in titles and abstracts
table_df$title <- gsub( '\"', '', table_df$title )
table_df$abstract <- gsub( '\"', '', table_df$abstract )

# remove empty titles
df <- table_df[ table_df$title != '', ]
df <- table_df[ table_df$abstract != '', ]
df <- na.omit( df )

# remove 'empty' abstracts (e.g., DOI re-refs).
df$n_words <- stringr::str_count( df$abstract, ' ' ) + 1
df <- df[ df$n_words > 100, ]

summary( df$n_words )
dim( df )
df$n_words <- NULL
# write
#               class  n    ratio req_sample_size
# 1 systematic_review 93 2.688172            8440
# 2     meta_analysis 40 6.250000           26250
# 3             trial 81 3.086420           10432
# 4  randomized_trial 39 6.410256           27051
# 5          protocol 28 8.928571           39642
df_sr <- df[ 1:8500, ]
df_sr$included <- NA

df_meta <- df[ 1:26300, ]
df_meta$included <- NA

df_trial <- df[ 1:10500, ]
df_trial$included <- NA

df_rct <- df[ 1:27100, ]
df_rct$included <- NA

df_prot <- df[ 1:39700, ]
df_prot$included <- NA

# combine with previously labeled 5000 'gold'
df_sr_all <- rbind( get_gold_data( 'systematic_review' ), df_sr )
df_meta_all <- rbind( get_gold_data( 'meta_analysis' ), df_meta )
df_trial_all <- rbind( get_gold_data( 'trial' ), df_trial )
df_rct_all <- rbind( get_gold_data( 'randomized_trial' ), df_rct )
df_prot_all <- rbind( get_gold_data( 'protocol' ), df_prot )

# check
summary( as.factor( df_sr_all$included ) )
summary( as.factor( df_meta_all$included ) )
summary( as.factor( df_trial_all$included ) )
summary( as.factor( df_rct_all$included ) )
summary( as.factor( df_prot_all$included ) )


write.csv( df_sr_all, file = gzfile( paste0( outdir, '/data__systematic_review.csv.gz' ) ) )
write.csv( df_meta_all, file = gzfile( paste0( outdir, '/data__meta_analysis.csv.gz' ) ) )
write.csv( df_trial_all, file = gzfile( paste0( outdir, '/data__trial.csv.gz' ) ) )
write.csv( df_rct_all, file = gzfile( paste0( outdir, '/data__randomized_trial.csv.gz' ) ) )
write.csv( df_prot_all, file = gzfile( paste0( outdir, '/data__protocol.csv.gz' ) ) )

