# machine-learning
#
# https://cfss.uchicago.edu/notes/supervised-text-classification/#sample-set-of-documents-uscongress
################################################################################

library( 'tidyverse' )
library( 'tidymodels' )
library( 'tidytext' )

set.seed( 1234 )
theme_set(theme_minimal())