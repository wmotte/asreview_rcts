#!/usr/bin/env Rscript
#
# Correlation of GBDx data.
#
# Wim Otte (w.m.otte@umcutrecht.nl)
#
################################################################################
library( 'ggplot2' )

################################################################################
# BEGIN FUNCTIONS
################################################################################

################################################################################
# END FUNCTIONS
################################################################################

# outdir
df1 <- read.csv( 'data/1990_1999.csv' )
df2 <- read.csv( 'data/2000_2009.csv' )
df3 <- read.csv( 'data/2010_2019.csv' )

data <- rbind( rbind( df1, df2 ), df3 )

data$measure_id <- NULL
data$location_id <- NULL
data$sex_id <- NULL
data$age_id <- NULL
data$cause_id <- NULL
data$metric_id <- NULL

data$location_name <- NULL
data$metric_name <- NULL
data$age_name <- NULL
data$sex_name <- NULL

rownames( data ) <- NULL

# Numbers, Global, Both sexes, All ages
write.csv( data, file = 'data/combined.csv', quote = TRUE )

df_daly <- data[ data$measure_name == 'DALYs (Disability-Adjusted Life Years)' & data$cause_name == 'Diphtheria', ]
df_deaths <- data[ data$measure_name == 'Deaths' & data$cause_name == 'Diphtheria', ]

#ggplot( data = df_daly, aes( x= year, y = val, ymin = lower, ymax = upper ) ) + geom_line() + geom_errorbar()
#ggplot( data = df_deaths, aes( x= year, y = val, ymin = lower, ymax = upper ) ) + geom_line() + geom_errorbar()
