#!/usr/bin/env Rscript
#
# Prepare full dataset for training!
################################################################################

 

################################################################################
# END FUNCTIONS
################################################################################

# outdir
outdir <- 'out.05.prepare.full.set'
dir.create( outdir, showWarnings = FALSE )

# indir
indir <- 'out.04.final.compare'

labels <- c( 'systematic-reviews', 'meta-analysis', 'rodent-studies', 'human-studies', 
            'trials', 'randomised-trials', 'children', 'protocols' )

# containre
all <- NULL

label <- labels[ 1 ]

for( label in labels )
{
    # TODO
    infile <- paste0( indir, '/df__', label, '-only.tsv' )

    df <- data.frame( readr::read_tsv( infile ), show_col_types = FALSE )

    # choose review 'wim' as default and set all NA's to 'irrelevant'
    df$included_w[ is.na( df$included_w ) ] <- 0
    
    single <- data.frame( label = df$included_w )
    colnames( single ) <- c( label )
    
    if( is.null( all ) ) {
        
        all <- single
        all$pmid <- df$pmid
        
    } else {
        all <- cbind( all, single )
    }
}

# rename cols
colnames( all ) <- c( "systematic_review", "pmid", "meta_analysis", "rodent", "human", "trial", "randomized_trial", "child", "protocol" ) 

# post-process, all 'child' should be also 'human'

# n = 31
all[ all$child == 1 & all$human == 0, 'human' ] <- 1

# get raw tiab
raw <- data.frame( readr::read_tsv( 'out.00.pubmed.data/tiabs_5000__2019.tsv' ) )
raw$text <- paste0( raw$title, "|", raw$abstract )

# merge with abstract and title version
final <- merge( raw, all )

# write to disk
readr::write_tsv( final, file = gzfile( paste0( outdir, '/full_labeled.tsv.gz' ) ) )



