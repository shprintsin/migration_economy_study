feols(marriedToLocal ~church +hh_size|ethn+ county + year, cluster = "county", data = dt )

feols(marriedToLocal ~age+I(age^2) + education_years+ln_wage+gender+years_in_us+pBlack+pImm+pop+unemp+hh_size|ethn+ county + year|cng~lag_cng, cluster = "county", data = dt )

individual_controls="age+I(age^2) + education_years+ln_wage+gender + years_in_us"
county_controls='pBlack+pImm+pop+unemp'
household_controls='hh_size'

outcome=c('marriedToLocal','isCitizen','isEnglishProficient')
models=sprintf("%s ~church + %s + %s  + %s  |ethn+ county + year",outcome,individual_controls,county_controls,household_controls)
ind=sprintf("%s ~church + %s  |ethn+ county + year",outcome,individual_controls)
hh=sprintf("%s ~church + %s + %s  |ethn+ county + year",outcome,individual_controls,household_controls)
coun=sprintf("%s ~church + %s + %s + %s  |ethn+ county + year",outcome,individual_controls,household_controls,county_controls)
county=sprintf("%s ~church + %s + %s  |ethn+ county + year",outcome,individual_controls,county_controls)
models=c(ind,hh,coun,county)


models_res=lapply(models, function(x) {feols(as.formula(x), cluster = "county", data = dt )})
etable(models_res)

# i have two basic models, first, is the number of congregations in county c and year t using fixed effects for year and county for diff in diff analysis:


grouped_data <- dt %>% group_by(ethn)

# Apply the regression model to each group
ts=acs[year==2000,.N,.(county=fips,ethn)] %>% left_join(dt[year==2020,.(cng=sum(cng)),.(county,ethn)],by=c('county','ethn')) 
ts[is.na(cng),cng:=0]
summary(lm(N ~ cng+factor(ethn),data=ts))
first_stage <- lm(EthnicChurch ~ HistoricalEthnicPop + age + I(age^2) + education_years +
                  ln_wage + gender + years_in_us + pBlack + pImm + pop + unemp +
                  hh_size + factor(ethn) + factor(county) + factor(year), data = df)

binded=rbind(fread('data/historic/nhgis0007_ds41_1916_county.csv') %>% select(c(colnames(d)[1:9],"A7G029")) %>%mutate(rus=A7G029)  %>% select(-A7G029) %>% filter(rus!=0),
fread('data/historic/nhgis0007_ds51_1926_county.csv') %>% select(c(colnames(d)[1:9],"BCV028")) %>%mutate(rus=BCV028)  %>% select(-BCV028) %>% filter(rus!=0),
fread('data/nhgis0007_ds74_1936_county.csv') %>% select(c(colnames(d)[1:9],"BTV030")) %>%mutate(rus=BTV030)  %>% select(-BTV030) %>% filter(rus!=0)) %>% setDT()
binded=binded[,max(rus),.(STATEICP,COUNTYICP)]

icp=readxl::read_excel('data/full_icp_fips_mappings.xlsx') %>% setDT()
icp=icp %>% mutate(
  STATEFIP=sprintf("%02s",STATEFIPS),
  COUNTYFIP=sprintf("%03s",COUNTYFIPS),
  fips=paste0(STATEFIP,COUNTYFIP)
) %>% select(fips,STATEICP,COUNTYICP)
icp[,.N,STATEICP]
binded$STATEICP=as.numeric(binded$STATEICP)
binded$COUNTYICP  <-  as.numeric(binded$COUNTYICP)

binded 
icp[,.N,.(STATEICP,COUNTYICP)]
binded=binded %>% left_join(icp,by=c("STATEICP","COUNTYICP")) %>% select(fips,rus=V1)
de=data_ethnic %>% filter(group=='rus',year==2020) %>% select(fips,year,rus_2020=cng)

setDT(binded)
merge(binded,de,by='fips',all=TRUE)
binded %>% filter(year==2020)
rr=merge(binded,de,by='fips',all=TRUE)
rr=data.table(fips=counties$fips) %>% left_join(rr,by="fips") 
setnafill(rr,cols=c('rus','rus_2020'),fill=0)
rr=rr %>% select(-year) %>% mutate(rus=as.numeric(rus>0))
rr=rr %>% mutate(rus=as.numeric(rus>0))
rr=rr %>% select(rus,rus_2020,county=fips)

dr=dr %>% setnames('fips','county')%>% filter(ethn=='rus') %>% left_join(rr,'county')
setnames(dr,'rus.y','rus_1936')

feols( marriedToLocal~age+age^2+pop++unemp+years_in_us|year|cng~rus_1936, cluster = "county", data = dr)

feols( marriedToLocal~age+age^2+pop++unemp+years_in_us|cng~rus_1936, cluster = "county", data = dr)
