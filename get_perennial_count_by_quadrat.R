# code for creating species counts by time tables for each quadrat separately
# for use in LDA methods
# by Erica Christensen 10/2018

# TO DO:
#   - deal with unknown species in data (NA = '.' sometimes?)
#   - deal with empty quadrats/bare ground
#   - some missing data (i.e. month) that can be filled in
#   - need data post 2001

library(dplyr)
library(tidyr)

source('get_single_quad.R')

# read in data from EDI data portal ----
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-jrn/210351001/47/10c1bf759b5700581368e64387c2a347" 
infile1 <- sub("^https","http",infile1) 
dat1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "quadrat",     
                 "year",     
                 "month",     
                 "USDA_code",     
                 "scientific_name",     
                 "common_name",     
                 "duration",     
                 "form",     
                 "area",     
                 "perimeter"    ), check.names=TRUE, stringsAsFactors = F)

infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-jrn/210351002/75/10c1bf759b5700581368e64387c2a347" 
infile2 <- sub("^https","http",infile2) 
dat2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "quadrat",     
                 "year",     
                 "month",     
                 "USDA_code",     
                 "scientific_name",     
                 "common_name",     
                 "duration",     
                 "form",     
                 "density"   ), check.names=TRUE, stringsAsFactors = F)

# month is blank for years 1995 and 2001, but I know from other sources they were done in october
dat1$month[dat1$year == 1995] <- '10'
dat1$month[dat1$year == 2001] <- '10'
dat2$month[dat2$year == 1995] <- '10'
dat2$month[dat2$year == 2001] <- '10'

# convert month to integer for easier indexing [there are still a couple missing months to deal with]
dat1$month = as.integer(dat1$month)
dat2$month = as.integer(dat2$month)


# extract data for a single quadrat ----
a1 = get_single_quad(dat1,dat2,quad_name='a1',dur='P',min_percent = 0.02, write_to_file = F)

get_single_quad(dat1,dat2,quad_name = 'a2',dur='P',min_percent = 0.02, write_to_file = T)

# all quadrats at once
quadrat_list = unique(dat1$quadrat)

# look at time series of a particular species
sp = 3
plot(a1$date,a1[,sp],main=colnames(a1)[sp])


# ===================================================================
# try LDATS
library(LDATS)
library(multipanelfigure)
library(ggplot2)
source('C:/Users/echriste/Desktop/git/LDA-kratplots/functions/lda_plot_function.R')

a1 = read.csv('perennial_count_tables/a1_perennial_counts.csv',stringsAsFactors = F)

#### Run LDAs ####
LDA_models = LDATS::parLDA(data = dplyr::select(a1, -date), ntopics =  c(2:7),
                           nseeds = 200, ncores = 2)

#### Select the best LDA (AICc) ####
selected = LDATS:::LDA_select(lda_models = LDA_models, LDA_eval = quote(AIC), correction = TRUE,
                              LDA_selector = quote(min))

## make some plots
ntopics = selected@k
dates = as.Date(a1$date)

composition = community_composition(selected)
comp_plots = plot_community_composition_gg(composition,
                                           topic_order = seq(ntopics),
                                           ylim=c(0,1))
timeseries_plot = plot_component_communities(selected,ntopics,xticks = dates)
timeseries_plot
