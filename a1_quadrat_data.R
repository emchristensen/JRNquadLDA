# extracting all data for one single quadrat
library(dplyr)
library(tidyr)

setwd('C:/Users/echriste/Documents/Jornada quadrats')
source('quadrat_functions.R')

dat1 = read.csv('data/data portal/JornadaStudy_351_permanent_chart_quadrat_perennial_forb_density_data.csv',stringsAsFactors = F)
dat2 = read.csv('data/data portal/JornadaStudy_351_permanent_chart_quadrat_shrub_and_perennial_grass_cover_data.csv',stringsAsFactors = F)

# month is blank for years 1995 and 2001, but I know from other sources they were done in october
dat1$month[dat1$year == 1995] <- '10'
dat1$month[dat1$year == 2001] <- '10'
dat2$month[dat2$year == 1995] <- '10'
dat2$month[dat2$year == 2001] <- '10'

# extract quad a1 perennials
a1_count = dat1 %>% filter(quadrat=='a1',duration=='P')
a1_dens = dat2 %>% filter(quadrat=='a1',duration=='P')

# convert density data to count
a1_polygon_counts = a1_dens %>% group_by(year,month,USDA_code) %>% summarise(density = n()) %>% ungroup()

# combine
a1 = rbind(a1_polygon_counts,a1_count[,c('year','month','USDA_code','density')])
a1$date = as.Date(paste(a1$year,a1$month,'01',sep='-'))

# remove species represented in less than 2% of samples (Bagchi et al 2017 used 2%)
spcount = a1 %>% group_by(USDA_code) %>% summarize(n = n())
raresp = spcount$USDA_code[spcount$n<min(1,(.02*length(unique(a1$date))))]

a1 = a1[!(a1$USDA_code %in% raresp),]

# make into table
a1_table = tidyr::spread(a1, USDA_code, density)
# replace NAs with zeros
sp_table = a1_table[,-c(1:3)]
sp_table[is.na(sp_table)] <- 0

a1_table_dates = cbind(date = a1_table$date,sp_table)

#plot(a1_table_dates$date,a1_table_dates[,22])

# write
write.csv(a1_table_dates,'a1_perennial_counts.csv',row.names = F)

# ===================================================================
# try LDATS
library(LDATS)
library(multipanelfigure)
library(ggplot2)
source('C:/Users/echriste/Desktop/git/LDA-kratplots/functions/lda_plot_function.R')

a1 = read.csv('C:/Users/echriste/Documents/Jornada quadrats/a1_perennial_counts.csv',stringsAsFactors = F)

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
