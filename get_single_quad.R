#' @title get data for a single quadrat
#' @description extracts data for a single quadrat from cover and density datasets
#' 
#' @param dat1 data frame of cover data downloaded from data portal (contains columns 'quadrat','year','month','USDA_code','duration')
#' @param dat2 data frame of density data downloaded from data portal
#' @param quad_name name of quadrat to get data from (string)
#' @param dur category of plant species desired: 'P' = perennial; 'A' = annual; 'all' = both A and P
#' @param min_percent minimum percentage of samples a species must be found in, to be included in analysis 
#'                    (i.e. min_percent = 0.02 will exclude species found in <2% of samples)
#'                    default is 0.02
#' @param write_to_file T = table will be written as a csv in the perennial_count_tables folder; F = data frame will be returned
#' 
#' @return data frame of counts by species and time, in crosstab form
#'
get_single_quad = function(dat1,dat2,quad_name,dur,min_percent=.02,write_to_file=F) {
  cover = dat1 %>% filter(quadrat==quad_name,duration==dur)
  count = dat2 %>% filter(quadrat==quad_name,duration==dur)
  
  # convert density data to count
  polygon_counts = cover %>% group_by(year,month,USDA_code) %>% summarise(density = n()) %>% ungroup()
  
  # combine
  quad = rbind(polygon_counts,count[,c('year','month','USDA_code','density')])
  quad$date = as.Date(paste(quad$year,quad$month,'01',sep='-'))
  
  # remove species represented in less than 2% of samples (Bagchi et al 2017 used 2%)
  spcount = quad %>% group_by(USDA_code) %>% summarize(n = n())
  raresp = spcount$USDA_code[spcount$n<max(0,(min_percent*length(unique(quad$date))))]
  quad = quad[!(quad$USDA_code %in% raresp),]
  
  # make into table
  quad_table = tidyr::spread(quad, USDA_code, density)
  
  # replace NAs with zeros
  sp_table = quad_table[,-c(1:3)]
  sp_table[is.na(sp_table)] <- 0
  table_dates = cbind(date = quad_table$date,sp_table)
  
  if (write_to_file == T) {
    filename = paste0('perennial_count_tables/',quad_name,'_perennial_counts.csv')
    write.csv(table_dates, filename, row.names = F)
  } 
  else {return(table_dates)}
}
