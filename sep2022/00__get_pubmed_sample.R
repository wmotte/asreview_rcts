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
    # post covid
    term <- 'all[sb] AND 2022/07/01:2022/07/31[dp] AND hasabstract'
    
    # get 6,000 records
    results <- rentrez::entrez_search( "pubmed", term = term, retmax = 6000 )
    
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

# get 6,000 PMIDS
pmids <- get_pmids()

# get 6,000 tiabs
table_df <- get_tiab( pmids )

# fix explicit double quote in titles and abstracts
table_df$title <- gsub( '\"', '', table_df$title )
table_df$abstract <- gsub( '\"', '', table_df$abstract )

# remove empty titles
df <- table_df[ table_df$title != '', ]
df <- table_df[ table_df$abstract != '', ]

# select 2,500 for set A.
set_christiaan <- df[ 1:2500,]

# select 2,500 for set B.
set_wim <- df[ 2501:5000, ]

write.table( set_wim, file = paste0( outdir, '/tiabs_wim.tsv' ), sep = '\t', quote = TRUE )


# write to disk
readr::write_tsv( set_christiaan, file = paste0( outdir, '/tiabs_2500__set_christiaan.tsv' ) )
readr::write_tsv( set_wim, file = paste0( outdir, '/tiabs_2500__set_wim.tsv' ) )

# 1. systematic_review
# 2. meta_analysis
# 3. human_intervention
# 4. randomized_trial
# 5. protocol
# 6. rodent


