source('libraries.r')
result=readRDS('data/full_relig_long.rds')
acs = readRDS('acs32_modified.rds')
source('create_county_stat.r',echo=TRUE)
county_pop=readRDS('data/emp_pop.rds')
acs=acs %>% mutate(
    group=case_when(
  BPLD == 46500 ~ 'rus',
    #BPLD == 46510 ~ 'bel',
    BPLD == 46530 ~ 'ukr',
    BPLD == 45730 ~ 'serb',
    BPLD == 45700 ~ 'serb',
    BPLD == 43000 ~ 'alb',
    BPLD == 43330 ~ 'mac',
    #BPLD == 46540 ~ 'arm',
    #BPLD == 46542 ~ 'gor',
    BPLD == 45600 ~ 'rom',
    BPLD == 45100 ~ 'bulg',
    TRUE ~ 'other'
    )
)

acs=acs[!is.na(group) & group!='other']
result$year=as.numeric(result$year)
acs$year=as.numeric(acs$YEAR)
df[,.N,.(group,year)]

df=result %>% left_join(  county_stat[,-c('pop')],by=c('fips','year')) 
df=df %>% left_join(county_pop,by=c('fips','year'))

df=df %>%right_join(acs,by=c('fips','year','group'))
setnafill(df,cols=c('adh','cng'),fill=0)
df[,sum(cng),.(group,year)]

#saveRDS(df,'data/df_with_relig_ethn_only.rds')

