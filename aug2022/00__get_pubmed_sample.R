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

df$n_words <- stringr::str_count( df$abstract, ' ' ) + 1

df <- df[ df$n_words > 100, ]

summary( df$n_words )
dim( df )

# write
#               class  n    ratio req_sample_size
# 1 systematic_review 93 2.688172            8440
# 2     meta_analysis 40 6.250000           26250
# 3             trial 81 3.086420           10432
# 4  randomized_trial 39 6.410256           27051
# 5          protocol 28 8.928571           39642
write.csv( df[ 1:8500, ], file = gzfile( paste0( outdir, '/data__systematic_review.csv.gz' ) ) )
write.csv( df[ 1:26300, ], file = gzfile( paste0( outdir, '/data__meta_analysis.csv.gz' ) ) )
write.csv( df[ 1:10500, ], file = gzfile( paste0( outdir, '/data__trial.csv.gz' ) ) )
write.csv( df[ 1:27100, ], file = gzfile( paste0( outdir, '/data__randomized_trial.csv.gz' ) ) )
write.csv( df[ 1:39700, ], file = gzfile( paste0( outdir, '/data__protocol.csv.gz' ) ) )
