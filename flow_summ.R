library(rnrfa)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)

######
# # get unique station ids from the metadata
# ids <- catalogue() %>% 
#   .$id %>% 
#   as.character
# 
# # loop through the ids to get all flow data
# dat <- vector('list', length = length(ids))
# names(dat) <- ids
# for(id in ids){
#   
#   cat(id, '\n')
#   gdf_tmp <- try({GDF(ID = id)})
#   
#   # only append to output if data retrieval worked
#   if(!inherits(gdf_tmp, 'try-error'))
#     dat[[id]] <- gdf_tmp
#   
# }
#  
# # remove empties
# torm <- unlist(lapply(dat, is.null))
# dat <- dat[!torm]

# # combine the list
# dat <- lapply(dat, function(x){
#   flo <- as.numeric(x)
#   dts <- attr(x, 'index')
#   data.frame(dts, flo)
#   }) %>% 
#   melt(., id.var = c('flo', 'dts')) %>% 
#   rename(id = L1)
# 
# # save output
# save(dat, file = 'dat.RData', compress = 'xz')
 
######
# load the flow data from before
load(file = 'dat.RData')

# get site meta
meta <- catalogue(metadataColumn = 'id', entryValue = unique(dat$id)) %>% 
  select(id, lat, lon)

# get annual summaries
datsumm <- mutate(dat, 
    year = year(dts)
  ) %>% 
  group_by(id, year) %>% 
  summarize(flo = mean(flo, na.rm = TRUE)) %>% 
  group_by(id) %>% 
  summarize(flow = mean(flo, na.rm = TRUE)) %>% 
  left_join(meta, by = 'id')

# plot annual summary by location
# could easily combine with a map for more interesting viz
ggplot(datsumm, aes(x = lon, y = lat, size = log(flow), colour = log(flow))) +
  geom_point(alpha = 0.8) + 
  guides(colour = guide_legend()) +
  theme_minimal()
    
# get monthly boxplots
datsumm2 <- mutate(dat, month = month(dts))

# not very interesting but could facet by operator or some other category
ggplot(datsumm2, aes(x = month, y = log(flo), group = month)) + 
  geom_boxplot() + 
  theme_minimal()