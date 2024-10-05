dt=dt %>% mutate(ln_wage:=log(INCWAGE+1),ln_wage_pop=log(INCWAGE_POP+1),ln_wage_mom=log(INCWAGE_MOM+1),male=as.numeric(SEX==1)) 
dt=dt %>% select(
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

### main regressors
    cng,adh,rc, # number of congregations, number of adherents, and existence of russian church in county, for county c and year t

#### controls for county characteristics in year t and county c
    pBlack=share_black, # share of african american in county
    pImm=Proportion_Immigrants, # share of immigrants in county
    pEng=Proportion_English_Proficient, # share of english proficient in county
    pCitiz=Proportion_Citizens, # share of citizens in county
    pEastEU=pEastern, # share of eastern european ancestros origin in county
    pSlav, # share of slavic ancestros origin in county
    immigrant_wage, # average wage of immigrants in county by year
    pop, #population_in_county by year
    unemp, # unemployment rate in county by year
    ## controls for county characteristics in county c invariant over time
    ec_county, # social capital index in county for year 2022
    

# infividual characteristics
    wage=INCWAGE, # wage
    ln_wage_mom,
    ln_wage_pop,
    russiaBorn=isRussian, # is born in russia
    years_in_us=YRSUSA1,
    onlyEnglish, # speak only english
    age, # age
    education_years, # years of education
    male, # is male (dummy)
# household characteristics
    hh_size=PERNUM,
    lastmig, # year of last migrant in household
    firstmig, # year of first migrant in household
    married, # is married
    birth_year=BIRTHYR,
    imm_year=YRIMMIG, # year of yimmigration
    imm_year_fa=YRIMMIG_POP, # year of yimmigration
    imm_year_ma=YRIMMIG_MOM, # year of yimmigration
    )
dt[, lag_cng := shift(cng), by = county]
# i have two basic models, first, is the number of congregations in county c and year t using fixed effects for year and county for diff in diff analysis:
feols(isCitizen ~cng +  | fips + year, cluster = "fips", data = dt)
# second, is the number of congregations in county c and year t using fixed effects for year and county for diff in diff analysis and adding instrument variable of lagged number of congregations:
feols(marriedToLocal ~ t |fips + year | cng ~ lag_cng,cluster = "fips", data = dt)