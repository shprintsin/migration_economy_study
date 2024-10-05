
acs=acs %>% mutate(
    ethn=case_when(
    BPLD == 46500 ~ 'rus',
    BPLD == 43000 ~ 'alb',
    BPLD == 45700 ~ 'serb',
    BPLD == 45730 ~ 'serb',
    BPLD == 43330 ~ 'mac',
    BPLD == 45100 ~ 'bulg',
    BPLD == 46530 ~ 'ukr',
    BPLD == 43300 ~ 'greek',
    BPLD == 45600 ~ 'rom',
    TRUE ~ 'other'
    )
)

groups =unique(acs$group)
groups = 'rus'

result <- counties[rep(seq_len(nrow(counties)), each = length(groups))][
  , group := rep(groups, nrow(counties))
]

result=result %>% select(fips,group,year) %>% left_join(data_ethnic,by=c('fips','group','year'))
setnafill(result,cols=c("adh","cng"),fill=0)
comparsion=data_ethnic  %>% filter(!group %in% groups)
data_ethnic=data_ethnic %>% filter(group %in% groups)
result=result %>% select(fips,group,year) %>% left_join(acs,by=c('fips','group','year'))   %>% left_join(data_ethnic,by=c('fips','group','year')) 
result=result %>% left_join(county_stat,by=c('fips','year')) %>% left_join(county_pop,by=c('fips','year')) %>% setnames('pop.y','pop') %>% select(-pop.x)
setnafill(result,cols=c("adh","cng"),fill=0)

result[,church:=as.numeric(adh+cng>0)]

dt=result %>% mutate(ln_wage:=log(INCWAGE+1),ln_wage_pop=log(INCWAGE_POP+1),ln_wage_mom=log(INCWAGE_MOM+1),male=as.numeric(SEX==1)) 

dt=dt %>% select(
    ethn=group,
    county=fips,
    household=SERIAL,
    household_cluster=CLUSTER,
    state=STATEFIP,
    PERWT,HHWT, # Person and household weights
    year,
### outcome variables
    isCitizen, # is citizen
    isEnglishProficient, # is english proficient
    marriedToLocal, # is married to local
    ln_wage, # log wage
    SPEAKENG,
### main regressors
    cng,adh,church, # number of congregations, number of adherents, and existence of russian church in county, for county c and year t

#### controls for county characteristics in year t and county c
    pBlack=share_black, # share of african american in county
    pImm=Proportion_Immigrants, # share of immigrants in county
    pEng=Proportion_English_Proficient, # share of english proficient in county
    pCitiz=Proportion_Citizens, # share of citizens in county
    pEastEU=pEastern, # share of eastern european ancestros origin in county
    pSlav, # share of slavic ancestros origin in county
   # immigrant_wage, # average wage of immigrants in county by year
    pop, #population_in_county by year
    unemp, # unemployment rate in county by year
    ## controls for county characteristics in county c invariant over time
    #ec_county, # social capital index in county for year 2022
# infividual characteristics
    wage=INCWAGE, # wage
    ln_wage_mom,
    ln_wage_pop,
    russiaBorn=isRussian, # is born in russia
    years_in_us=YRSUSA1,
    onlyEnglish, # speak only english
    age, # age
    education_years, # years of education
    gender=male, # is male (dummy)
# household characteristics
    hh_size=PERNUM,
    #lastmig, # year of last migrant in household
    #firstmig, # year of first migrant in household
    married, # is married
    birth_year=BIRTHYR,
    imm_year=YRIMMIG, # year of yimmigration
    imm_year_fa=YRIMMIG_POP, # year of yimmigration
    imm_year_ma=YRIMMIG_MOM, # year of yimmigration
    )

dt=result

dt[, lag_cng := shift(cng), by = county]
dt=dt %>% filter(!is.na(PERWT))
saveRDS(dt,'data/merged_ethnic_10.2.rds')

