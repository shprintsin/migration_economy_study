
library(stringr)
res=fread('orthodox_churches_var_ethn_codebook.csv')
counties=readRDS('data/counties_full.rds')
ls=paste0('data/relig_',c(2020,2010,2000),'.dta')


# Apply the mapping to the dataset

codes=res %>% select(ethn,var)
data_mapped=lapply(data,group_and_map)
data_ethnic=rbindlist(data_mapped) %>% left_join(codes,by='var') %>% filter(cng+adh>0) %>% mutate(group=ethn,year=as.numeric(year)) %>% select(-ethn)
