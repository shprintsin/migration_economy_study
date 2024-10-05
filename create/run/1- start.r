lapply(c("aopdata","usmap",'stargazer',"data.table", "ipumsr", "lmtest", "defactor", "sandwich", "fixest", "stringr", "dplyr", "tidyr", "stringr", "tidyverse", "haven","clipr"),library, character.only = TRUE)
source('create/run/functions.r')
# Load each package
# Set working directory (modify this path to your local directory)

# ===========================================================
# 2. Data Loading and Preprocessing
# ===========================================================

# Load ACS data using IPUMS
ddi <- read_ipums_ddi("data/usa_00037.xml")
# filter_mn <- function(x, pos) {
#   x[x$BPLD== 46500|x$BPLD_MOM==46500|x$BPLD_POP==46500, ]
# }
# filter_mn_callback <- IpumsDataFrameCallback$new(filter_mn)
# df=read_ipums_micro_chunked(ddi,callback=filter_mn_callback,verbose = TRUE)

acs <- as.data.table(ipumsr::read_ipums_micro(ddi))
print('Load ACS data')
acs=readRDS('data/acs_37_pure.rds')
ethnic_codebook=fread('codes/ethnic_codebook.csv')

# --- 1 Data Cleaning and Variable Preparation ---

print('Create Immigrant variables')

bpl=fread('data/bpld.csv')
ethnic_codebook=fread('codes/ethnic_codebook.csv')
slavic_codes = ethnic_codebook[var=="ANCESTOR"]$value
# filters   
df = copy(acs)
acs=acs%>% mutate( 
    # Match Etnicity
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
acs_sampled <- acs %>%
  group_by(fips) %>%
  sample_n(20, replace = FALSE) %>%
  ungroup()
acs  <- acs_sampled
acs <- acs %>% 
  mutate(
    YRNATUR = if_else(YRNATUR > 3000 | YRNATUR < 0, NA_real_, YRNATUR),
    BIRTHYR = if_else(BIRTHYR > 3000 | BIRTHYR < 0, NA_real_, BIRTHYR),
    BIRTHYR_POP = if_else(BIRTHYR_POP > 3000 | BIRTHYR_POP < 0, NA_real_, BIRTHYR_POP),
    BIRTHYR_MOM = if_else(BIRTHYR_MOM > 3000 | BIRTHYR_MOM < 0, NA_real_, BIRTHYR_MOM),
    BIRTHYR_SP = if_else(BIRTHYR_SP > 3000 | BIRTHYR_SP < 0, NA_real_, BIRTHYR_SP),
    YRMARR = if_else(YRMARR > 3000 | YRMARR < 0, NA_real_, YRMARR),
    YRMARR_POP = if_else(YRMARR_POP > 3000 | YRMARR_POP < 0, NA_real_, YRMARR_POP),
    YRMARR_MOM = if_else(YRMARR_MOM > 3000 | YRMARR_MOM < 0, NA_real_, YRMARR_MOM),
    YRMARR_SP = if_else(YRMARR_SP > 3000 | YRMARR_SP < 0, NA_real_, YRMARR_SP),
    YRIMMIG = if_else(YRIMMIG > 3000 | YRIMMIG < 0, NA_real_, YRIMMIG),
    YRIMMIG_POP = if_else(YRIMMIG_POP > 3000 | YRIMMIG_POP < 0, NA_real_, YRIMMIG_POP),
    YRIMMIG_MOM = if_else(YRIMMIG_MOM > 3000 | YRIMMIG_MOM < 0, NA_real_, YRIMMIG_MOM),
    YRIMMIG_SP = if_else(YRIMMIG_SP > 3000 | YRIMMIG_SP < 0, NA_real_, YRIMMIG_SP),
    UHRSWORK = if_else(UHRSWORK == 0, 0, UHRSWORK),
    INCWAGE = if_else(INCWAGE %in% c(999998, 999999), 0, INCWAGE),
    INCWAGE_POP = if_else(INCWAGE_POP %in% c(999998, 999999), 0, INCWAGE_POP),
    INCWAGE_MOM = if_else(INCWAGE_MOM %in% c(999998, 999999), 0, INCWAGE_MOM),
    INCWAGE_SP = if_else(INCWAGE_SP %in% c(999998, 999999), 0, INCWAGE_SP),
    INCTOT = if_else(INCTOT %in% c(999998, 999999), 0, INCTOT),
    AGE = if_else(AGE > 130, NA_real_, AGE)
  # ) %>% mutate(
  #   INCWAGE = if_else(is.na(INCWAGE), 0, INCWAGE),
  #   INCWAGE_POP = if_else(is.na(INCWAGE_POP), 0, INCWAGE_POP),
  #   INCWAGE_MOM = if_else(is.na(INCWAGE_MOM), 0, INCWAGE_MOM),
  #   INCWAGE_SP = if_else(is.na(INCWAGE_SP), 0, INCWAGE_SP),
    
 )
# individual variables
acs <- acs  %>%
  mutate(
    fips = sprintf("%02d%03d", as.numeric(STATEFIP), as.numeric(COUNTYFIP)),
    year = YEAR,
    age = as.numeric(AGE),
    age_sq = AGE^2,
    gender = as.numeric(SEX == 1), # male
    hh_size=PERNUM,
    birth_year=as.numeric(BIRTHYR),
    married=as.numeric(MARST %in% c(1,2)),
    year_married=as.numeric(YRMARR),
    isImmigrant = as.numeric(!BPL_POP %in% 1:100),
    isImmigrant_FA = as.numeric(!BPL_POP %in% 1:100),
    isImmigrant_MA = as.numeric(!BPL_POP %in% 1:100),
    usBorn = as.numeric(BPL %in% 1:100),
    imm_year=YRIMMIG %>% as.numeric(), # year of yimmigration
    imm_year_fa=YRIMMIG_POP %>% as.numeric(),
    imm_year_ma=YRIMMIG_MOM %>% as.numeric(),
    age_at_mig:=age-YRSUSA1,
    
    isCitizen = as.numeric(CITIZEN %in% c(0, 1, 2)),
    isNaturalized = as.numeric(CITIZEN == 2),
    english_proficient = as.numeric(SPEAKENG %in% 3:5),
    intermarriage = as.numeric(BPL_SP %in% c(1:100) & (MARST %in% 1:2)),
    married_in_us = as.numeric(YRMARR - YRIMMIG > 0),
    langisolated = as.numeric(LINGISOL == 2),
    onlyEnglish = as.numeric(SPEAKENG == 3),
    years_in_us = as.numeric(YEAR - YRIMMIG),
    log_ocp = log(OCCSCORE + 1),
    unemployed= as.numeric(EMPSTAT == 2),
    eng = case_when(
      SPEAKENG == 1 ~ 1, # Does not speak English
      SPEAKENG == 6 ~ 2, # Yes, but not well
      SPEAKENG == 2 ~ 3, # Yes, speaks english,
      SPEAKENG == 5 ~ 4, # Yes, speaks well
      SPEAKENG == 4 ~ 5, # Yes, speaks very well
      SPEAKENG == 3 ~ 5, # Yes, speaks only English
      TRUE ~ 0
    ),
    ln_wage = log(INCWAGE + 1),
    ln_wage_fa = log(INCWAGE_POP + 1),
    ln_wage_ma = log(INCWAGE_MOM + 1),
    ln_wage_sp = log(INCWAGE_SP + 1),
    ln_par_wage = log(mean(INCWAGE_MOM + INCWAGE_POP)),  # Add comma and na.rm to handle missing data
    black = as.numeric(RACE == 2),    
    
    education_years = case_when(
        EDUCD %in% 0:17  ~ 4,   # Nursery school to grade 4
        EDUCD %in% 20:26 ~ 8,   # Grade 5 to grade 8
        EDUCD == 30      ~ 9,   # Grade 9
        EDUCD == 40      ~ 10,  # Grade 10
        EDUCD == 50      ~ 11,  # Grade 11
        EDUCD %in% 60:64 ~ 12,  # High school graduate or GED
        EDUCD == 65      ~ 13,  # Some college, but less than 1 year
        EDUCD %in% 70:71 ~ 14,  # 1 year of college
        EDUCD %in% 80:83 ~ 15,  # 2 years of college or associate's degree
        EDUCD == 90      ~ 16,  # 3 years of college
        EDUCD %in% 100:101 ~ 16,  # Bachelor's degree
        EDUCD == 110     ~ 17,  # 5+ years of college
        EDUCD %in% 111:113 ~ 18,  # 6-8+ years of college
        EDUCD == 114     ~ 18,  # Master's degree
        EDUCD == 115     ~ 19,  # Professional degree
        EDUCD == 116     ~ 20,  # Doctoral degree
        EDUCD == 999     ~ NA_real_  # Missing
      ),
    slavic = as.numeric(ANCESTR1 %in% slavic_codes),
    eastern = as.numeric(ANCESTR1 %in% c(100:179)),
    slavs = as.numeric(ANCESTR1 %in% slavic_codes),
    rus = as.numeric(ANCESTR1 %in% c(148, 150)),
    isRussian = as.numeric(BPL == 465),
    isFaRussian = as.numeric(BPL_POP == 465),
    isMaRussian = as.numeric(BPL_MOM == 465),
    filterrus = as.numeric(isRussian | isFaRussian | isMaRussian)
  ) 
  
acs  <- acs  %>%  # county Controls
  group_by(fips,year) %>% mutate(
    acs = 1,
    mean_age = mean(AGE),
    avg_occscore = mean(OCCSCORE),
    avg_education_years = mean(education_years),
    avg_unemped = mean(EMPSTAT == 2),
    avg_ln_wage = mean(ln_wage),
    avg_ln_wage_locals = mean(ln_wage*isImmigrant),
    
     hhi_ancestry = {
    ancestry_counts = table(ANCESTR1)
    sum((ancestry_counts / sum(ancestry_counts))^2)
  },
    population = n(),
    pEnglishOnly = mean(onlyEnglish),
    # Mean_Migration_Age = mean(mig_age),
    pEnglish_Proficient = mean(english_proficient),
    pLangisolated = mean(langisolated),
    pIntermarriagee = mean(intermarriage),
    pNaturalized = mean(isNaturalized),
    pCitiz = mean(isCitizen),
    pImm = mean(isImmigrant),
    pRus = mean(ethn == 'rus'),
    pSerb= mean(ethn == 'serb'),
    pAlb= mean(ethn == 'alb'),
    pMac= mean(ethn == 'mac'),
    pBulg= mean(ethn == 'bulg'),
    pUkr= mean(ethn == 'ukr'),
    pGreek= mean(ethn == 'greek'),
    pRom= mean(ethn == 'rom'),
    pSlav = mean(slavs),
    pEastEU = mean(eastern),
    pBlack = mean(black),
    # Mean_Num_Adults = mean(num_adults, na.rm = TRUE),
  ) %>% ungroup()  %>% group_by(fips,year,ethn) %>%
   mutate( # ethnic specific controls
    ethn_mean_age = mean(AGE),
    ethn_avg_occscore = mean(OCCSCORE),
    ethn_avg_education_years = mean(education_years),
    ethn_avg_unemped = mean(EMPSTAT == 2),
    ethn_avg_ln_wage = mean(ln_wage),
    pEthn = n()/population,
    ethn_intermarriage = mean(intermarriage),
    ethn_langisolated = mean(langisolated),
  )



########################################################

# (2) - County data 
county_pop=readRDS('data/emp_pop.rds')



# (3) Match Ethnicity
###################################
# function from 'functions.r'
relig_codebook=fread('orthodox_churches_var_ethn_codebook.csv')
dataset_list=paste0('data/relig_',c(2020,2010,2000),'.dta')
term_mapping <- fread('data/mappingvar.csv')
term_mapping <- setNames(term_mapping_df$mapping, term_mapping_df$term)

datasrc=lapply(dataset_list,reader)
data=lapply(datasrc,clean_and_pivot) %>%
    lapply(createType) %>% 
    lapply(remove_and_filter) %>%
    lapply(fixfips) %>%
    lapply(group_and_map) %>%
    rbindlist() %>%
    left_join(relig_codebook[,.(var,ethn)]) %>%
    mutate(year=as.numeric(year))
# add ethnic identification


# merge with basis county template by groups identified in acs
counties=readRDS('data/counties_full.rds')
groups =unique(acs$ethn)
#groups = 'rus'  # for now, only for russian
# duplicate by ethn * year

# join 
result <- counties[rep(seq_len(nrow(counties)), each = length(groups))][
  , ethn := rep(groups, nrow(counties))
] %>% select(fips,ethn,year) %>% 
    left_join(data,by=c('fips','ethn','year')) %>%
    as.data.table() %>% 
    setnafill(cols = c("adh", "cng"), fill = 0) %>%
    mutate( church=as.numeric(adh+cng>0) ) %>%
    left_join(county_stat,by=c('fips','year')) %>%
    left_join(county_pop,by=c('fips','year'))


## (4) Historic data  (for russian only for now)
# convert to fips

icp=readxl::read_excel('data/full_icp_fips_mappings.xlsx') %>% 
mutate(
    STATEFIP=sprintf("%02s",STATEFIPS),
    COUNTYFIP=sprintf("%03s",COUNTYFIPS),
    STATEICP=sprintf("%02d",as.numeric(STATEICP)),
    COUNTYICP=sprintf("%04d",as.numeric(COUNTYICP)),
    fips=paste0(STATEFIP,COUNTYFIP)
) %>%
 select(fips,STATEICP,COUNTYICP) %>% setDT()

hrelig=rbind(
    fread('data/historic/nhgis0007_ds41_1916_county.csv') %>% select(c(colnames(d)[1:9],"A7G029")) %>%mutate(rus=A7G029)  %>% select(-A7G029) %>% filter(rus!=0),
    fread('data/historic/nhgis0007_ds51_1926_county.csv') %>% select(c(colnames(d)[1:9],"BCV028")) %>%mutate(rus=BCV028)  %>% select(-BCV028) %>% filter(rus!=0),
    fread('data/nhgis0007_ds74_1936_county.csv') %>% select(c(colnames(d)[1:9],"BTV030")) %>%mutate(rus=BTV030)  %>% select(-BTV030) %>% filter(rus!=0)) %>% 
    group_by(STATEICP,COUNTYICP) %>% mutate(
    rus_1936=max(rus),
    STATEICP=sprintf("%02d",as.numeric(STATEICP)),
    COUNTYICP=sprintf("%04d",as.numeric(COUNTYICP))
    )  %>% 
    ungroup()  %>%
    left_join(icp,by=c("STATEICP","COUNTYICP")) %>% 
    select(fips,rus_1936) 

# bind the historic data with the county-relig dataset
result=result %>% left_join(hrelig,by='fips') %>%
        setnafill(cols='rus_1936',fill=0) %>%
        mutate(rus_1936=as.numeric(rus_1936>0))

# sink('res.txt',type='output')
###### Analysis and subsets
# acs=acs %>% select(year,household=SERIAL,fips,ethn,age,age_sq,isImmigrant,isImmigrant_FA,isImmigrant_MA,isCitizen,isNaturalized,isEnglishProficient,marriedToLocal,onlyEnglish,years_in_us,eng,ln_wage,ln_wage_fa,education_years,isRussian,isFaRussian,isMaRussian,filterrus,YRSUSA1,imm_year)
# acs=acs %>% select(year,household,fips,ethn,age,age_sq,isImmigrant,isImmigrant_FA,isImmigrant_MA,isCitizen,isNaturalized,isEnglishProficient,marriedToLocal,onlyEnglish,years_in_us,eng,ln_wage,ln_wage_fa,education_years,isRussian,isFaRussian,isMaRussian,filterrus,YRSUSA1,imm_year)

acs[,russian_origin:=max(as.numeric(isRussian),na.rm = TRUE),household]
colnames(imm_year)
# dt=acs %>% filter(russian_origin==1)
# dt[russian_origin==1,ethn:='rus']
# dt=dt %>%filter(age>=18,age<=64)
# that section is problematic
dt=acs %>% left_join(result,by=c('ethn','fips','year'))

library()
## second subset
dt.2=acs %>% filter(!is.na(ethn) & ethn!='other')
dt[russian_origin==1,ethn:='rus']
dt.2=dt.2 %>%filter(age>=18,age<=64)
# that section is problematic
# dt=dt %>% group_by(household) %>% 
#   mutate(min_mig=min(imm_year),max_mig=max(imm_year)) %>%
#   filter(max_mig<2010,min_mig>1980,age>=18,age<=64)

dt.2=dt.2 %>% left_join(result,by=c('ethn','fips','year'))

######## (5) Summary statistics
colnames(res)
setDT(dt)
dt=dt[!duplicated(dt)]
dt[,secondGen:=as.numeric(isImmigrant==0)]



feols( marriedToLocal~cng+age+age^2+pop+unemp+age_at_mig+secondGen+education_years+years_in_us|year, cluster = "fips", data = dt %>% filter(russian_origin==1))
feols( marriedToLocal~cng+age+age^2+pop+unemp+secondGen+secondGen:cng+education_years+years_in_us|year, cluster = "fips", data = dt %>% filter(russian_origin==1))
feols( marriedToLocal~cng+age+age^2+pop+unemp+secondGen+education_years+years_in_us|year, cluster = "fips", data = dt %>% filter(russian_origin==1))

feols( marriedToLocal~cng+age+age^2+pop++unemp++secondGen+education_years+years_in_us|year+county, cluster = "fips", data = dt %>% filter(russian_origin==1))

feols( eng~secondGen+ln_wage+age_at_mig +isRussian+cng+age+age^2+pop+unemp+secondGen:cng+education_years+years_in_us|year, cluster = "fips", data = dt %>% filter(russian_origin==1))
feols( isEnglishProficient~age+age^2+pop++unemp+secondGen+education_years+years_in_us|year|cng~rus_1936, cluster = "fips", data = dt %>% filter(russian_origin==1))

feols( ln_wage~isCitizen:cng+age+age^2+ln_wage_fa+marriedToLocal+education_years+years_in_us|year, cluster = "fips", data = dt %>% filter(russian_origin==1,years_in_us>5|years_in_us<15))
feols( isCitizen~cng+age+age^2+pop++unemp+secondGen+education_years+years_in_us|year, cluster = "fips", data = dt %>% filter(russian_origin==1,years_in_us>5|years_in_us<15))
feols( isCitizen~age+age^2+pop++unemp+secondGen+education_years+years_in_us|year|cng~rus_1936, cluster = "fips", data = dt %>% filter(russian_origin==1,years_in_us>5|years_in_us<15))

feols( ln_wage~ln_wage_fa+age+age^2+pop+unemp+years_in_us|year|cng~rus_1936, cluster = "fips", data = dt)
feols( ln_wage~cng+ln_wage_fa+age+age^2+pop+unemp+years_in_us|year, cluster = "fips", data = dt)

feols( eng~cng+age+age^2+pop+unemp+secondGen+education_years+years_in_us|year+ethn+household, cluster = "fips", data = dt )
feols( eng~cng+age+age^2+pop+unemp+secondGen+education_years+years_in_us|year+ethn+ln_wage, cluster = "fips", data = dt )
feols( marriedToLocal~cng+age+age^2+pop++unemp++secondGen+education_years+years_in_us|year, cluster = "fips", data = dt %>% filter(russian_origin==1))
sink()
dt[,roor:=max(as.numeric(var=='roor')),.(fips,ethn)]
dt[,roc:=max(as.numeric(var=='roc')),.(fips,ethn)]
setnafill(dt,cols=c('cng','church','adh','rus_1936'),fill=0)
stargazer(dt,out="tables/summery_statistics_df_russianorigin_subset.tex")
stargazer(dt,type="text",out='tables/markdown/summery_statistics_df_russianorigin.md')
dt <- dt %>%
  group_by(fips) %>%
  arrange(year) %>%
  mutate(lag_cng = lag(cng))

### Analysis in file 5 - analysis.r


# **Model 1: Impact on Intermarriage (`marriedToLocal`)**

  model1 <- feols(
    marriedToLocal ~ cng + age + I(age^2) + pop + unemp + secondGen + education_years + years_in_us |
    year + fips,
    cluster = "fips",
    data = dt
  )



# - **Model 2: Impact on English Proficiency (`isEnglishProficient`)**

  model2 <- feols(
    isEnglishProficient ~ cng + age + I(age^2) + pop + unemp + secondGen + education_years + years_in_us |
    year + fips,
    cluster = "fips",
    data = dt
  )

  model3 <- feols(
    isCitizen ~ cng + age + I(age^2) + pop + unemp + secondGen + education_years + years_in_us |
    year + fips,
    cluster = "fips",
    data = dt[years_in_us >= 5, ]  # Only include those eligible for citizenship
  )


  model4 <- feols(
    ln_wage ~ cng + ln_wage_fa + age + I(age^2) + pop + unemp + years_in_us |
    year + fips,
    cluster = "fips",
    data = dt
  )


model_improved <- feols(
  marriedToLocal ~ cng + age + I(age^2) + education_years + years_in_us |
  year + fips,
  cluster = "fips",
  data = dt
)
  iv_model <- feols(
    marriedToLocal ~ age + I(age^2) + education_years + years_in_us | 
    year + fips | 
    cng ~ rus_1936,
    cluster = "fips",
    data = dt
  )

feols(marriedToLocal ~ cng * years_in_us + ln_wage_fa + age + age_sq | year+ethn, cluster = "fips", data = dt)
feols(marriedToLocal ~ years_in_us + ln_wage_fa + age + age_sq | year+ethn|cng~lag_cng, cluster = "fips", data = dt) 



plot=ggplot(dt%>%filter(russian_origin==1), aes(x = church, y = marriedToLocal, fill = secondGen)) +
  geom_bar(stat = "identity") +
  labs(title = "Intermarriage Rates by Ethnicity", 
       x = "Ethnicity", y = "Intermarriage Rate (%)") +
  theme_minimal()
ggsave("income_trends_by_ethnic_group.png", plot = plot)
shell.exec("income_trends_by_ethnic_group.png")