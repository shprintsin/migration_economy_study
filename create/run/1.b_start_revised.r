# ===========================================================
# 1. Load Libraries and Functions
# ===========================================================
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

#acs <- as.data.table(ipumsr::read_ipums_micro(ddi))
print('Load ACS data')
acs=readRDS('data/acs_37_pure.rds')
ethnic_codebook=fread('codes/ethnic_codebook.csv')

# --- 1 Data Cleaning and Variable Preparation ---

print('Create Immigrant variables')
bpl=fread('data/bpld.csv')
ethnic_codebook=fread('codes/ethnic_codebook.csv')
slavic_codes = ethnic_codebook[var=="ANCESTOR"]$value
# filters   

# ===========================================================
# 2. Data Loading and Preprocessing
# ===========================================================
# Load ACS data using IPUMS


# Define a function to map birthplace codes to ethnicity
get_ethn <- function(code) {
  case_when(
    code == 46500 ~ 'rus',
    code == 43000 ~ 'alb',
    code %in% c(45700, 45730) ~ 'serb',
    code == 43330 ~ 'mac',
    code == 45100 ~ 'bulg',
    code == 46530 ~ 'ukr',
    code == 43300 ~ 'greek',
    code == 45600 ~ 'rom',
    TRUE ~ 'other'
  )
}

acs <- acs %>%
  # Drop unwanted columns with a more efficient `select()` 
  # Assign ethnicity based on individual's and parents' birthplace
  mutate(
    ethn = get_ethn(BPLD),
    ethn_pop = get_ethn(BPLD_POP),
    ethn_mom = get_ethn(BPLD_MOM),
    ethn = coalesce(ethn, ethn_pop, ethn_mom)  # Use coalesce for cleaner final ethnicity assignment
  )


# Clean and prepare data
acs <- acs %>% select(colnames(acs)[-grep('_MOM2|_POP2',colnames(acs))]) %>%
  # Assign ethnicity based on individual's and parents' birthplace
  mutate(
    ethn = get_ethn(BPLD),
    ethn_pop = get_ethn(BPLD_POP),
    ethn_mom = get_ethn(BPLD_MOM),
    # Assign final ethnicity
    ethn = coalesce(ethn, ethn_pop, ethn_mom)  # Use coalesce for cleaner final ethnicity assignment

  ) %>%
  # Clean variables using mutate(across())
  mutate(across(
    c(YRNATUR, BIRTHYR, BIRTHYR_POP, BIRTHYR_MOM, BIRTHYR_SP,
      YRMARR, YRMARR_POP, YRMARR_MOM, YRMARR_SP,
      YRIMMIG, YRIMMIG_POP, YRIMMIG_MOM, YRIMMIG_SP),
    ~ if_else(. > 3000 | . < 0, NA_real_, .)
  )) %>%
  mutate(across(
    c(INCWAGE, INCWAGE_POP, INCWAGE_MOM, INCWAGE_SP, INCTOT),
    ~ if_else(. %in% c(999998, 999999), NA_real_, .)
  )) %>%
  mutate(
    AGE = if_else(AGE > 130, NA_real_, AGE),
    UHRSWORK = if_else(UHRSWORK == 0, 0, UHRSWORK)
  ) %>%
  # Individual-level variables
  mutate(
    fips = sprintf("%02d%03d", as.numeric(STATEFIP), as.numeric(COUNTYFIP)),
    year = YEAR,
    age = as.numeric(AGE),
    age_sq = AGE^2,
    gender = as.numeric(SEX == 1),  # Male = 1
    hh_size = PERNUM,
    birth_year = as.numeric(BIRTHYR),
    married = as.numeric(MARST %in% c(1, 2)),
    year_married = as.numeric(YRMARR),
    isImmigrant = as.numeric(!BPL %in% 1:100),          # Corrected
    isImmigrant_FA = as.numeric(!BPL_POP %in% 1:100),   # Corrected
    isImmigrant_MA = as.numeric(!BPL_MOM %in% 1:100),   # Corrected
    usBorn = as.numeric(BPL %in% 1:100),
    imm_year = as.numeric(YRIMMIG),
    imm_year_fa = as.numeric(YRIMMIG_POP),
    imm_year_ma = as.numeric(YRIMMIG_MOM),
    age_at_mig = if_else(isImmigrant == 1, age - (YEAR - YRIMMIG), NA_real_),
    isCitizen = as.numeric(CITIZEN %in% c(0, 1, 2)),
    isNaturalized = as.numeric(CITIZEN == 2),
    english_proficient = as.numeric(SPEAKENG %in% 3:5),
    intermarriage = as.numeric(BPL_SP %in% c(1:100) & married == 1),
    married_in_us = as.numeric(YRMARR - YRIMMIG > 0),
    langisolated = as.numeric(LINGISOL == 2),
    onlyEnglish = as.numeric(SPEAKENG == 3),
    years_in_us = if_else(isImmigrant == 1, YEAR - YRIMMIG, NA_real_),
    log_ocp = log(OCCSCORE + 1),
    unemployed = as.numeric(EMPSTAT == 2),
    eng = case_when(
      SPEAKENG == 1 ~ 1,  # Does not speak English
      SPEAKENG == 6 ~ 2,  # Yes, but not well
      SPEAKENG == 2 ~ 3,  # Yes, speaks English
      SPEAKENG == 5 ~ 4,  # Yes, speaks well
      SPEAKENG == 4 ~ 5,  # Yes, speaks very well
      SPEAKENG == 3 ~ 6   # Yes, speaks only English
    ),
    ln_wage = log(INCWAGE + 1),
    ln_wage_fa = log(INCWAGE_POP + 1),
    ln_wage_ma = log(INCWAGE_MOM + 1),
    ln_wage_sp = log(INCWAGE_SP + 1),
    ln_par_wage = log(rowMeans(cbind(INCWAGE_MOM, INCWAGE_POP), na.rm = TRUE) + 1),
    black = as.numeric(RACE == 2),
    education_years = case_when(
      EDUCD %in% 0:17    ~ 4,   # Nursery school to grade 4
      EDUCD %in% 20:26   ~ 8,   # Grade 5 to grade 8
      EDUCD == 30        ~ 9,   # Grade 9
      EDUCD == 40        ~ 10,  # Grade 10
      EDUCD == 50        ~ 11,  # Grade 11
      EDUCD %in% 60:64   ~ 12,  # High school graduate or GED
      EDUCD == 65        ~ 13,  # Some college, less than 1 year
      EDUCD %in% 70:71   ~ 14,  # 1 year of college
      EDUCD %in% 80:83   ~ 15,  # 2 years of college or associate's degree
      EDUCD == 90        ~ 16,  # 3 years of college
      EDUCD %in% 100:101 ~ 16,  # Bachelor's degree
      EDUCD == 110       ~ 17,  # 5+ years of college
      EDUCD %in% 111:116 ~ 18,  # Master's or higher
      EDUCD == 999       ~ NA_real_  # Missing
    ),
    slavic = as.numeric(ANCESTR1 %in% slavic_codes),
    eastern = as.numeric(ANCESTR1 %in% c(100:179)),
    slavs = as.numeric(ANCESTR1 %in% slavic_codes),
    rus = as.numeric(ANCESTR1 %in% c(148, 150)),
    isRussian = as.numeric(BPL == 465),
    filterrus = as.numeric(isRussian | isImmigrant_FA == 1 | isImmigrant_MA == 1)
  )


# ===========================================================
# 3. Compute County-Level Means Using the Whole Sample
# ===========================================================
# Compute county-level variables

county_vars <- acs %>% select(fips,year,age,OCCSCORE,education_years,unemployed,ln_wage,isImmigrant,onlyEnglish,english_proficient,langisolated,intermarriage,isNaturalized,isCitizen,isImmigrant,ethn,slavs,eastern,black) %>%
  group_by(fips, year) %>%
  summarise(
    population = n(),
    mean_age = mean(age, na.rm = TRUE),
    avg_occscore = mean(OCCSCORE, na.rm = TRUE),
    avg_education_years = mean(education_years, na.rm = TRUE),
    avg_unemped = mean(unemployed, na.rm = TRUE),
    avg_ln_wage = mean(ln_wage, na.rm = TRUE),
    avg_ln_wage_locals = mean(ln_wage * (isImmigrant == 0), na.rm = TRUE),
    pEnglishOnly = mean(onlyEnglish, na.rm = TRUE),
    pEnglish_Proficient = mean(english_proficient, na.rm = TRUE),
    pLangisolated = mean(langisolated, na.rm = TRUE),
    pIntermarriagee = mean(intermarriage, na.rm = TRUE),
    pNaturalized = mean(isNaturalized, na.rm = TRUE),
    pCitiz = mean(isCitizen, na.rm = TRUE),
    pImm = mean(isImmigrant, na.rm = TRUE),
    pRus = mean(ethn == 'rus', na.rm = TRUE),
    pSerb = mean(ethn == 'serb', na.rm = TRUE),
    pAlb = mean(ethn == 'alb', na.rm = TRUE),
    pMac = mean(ethn == 'mac', na.rm = TRUE),
    pBulg = mean(ethn == 'bulg', na.rm = TRUE),
    pUkr = mean(ethn == 'ukr', na.rm = TRUE),
    pGreek = mean(ethn == 'greek', na.rm = TRUE),
    pRom = mean(ethn == 'rom', na.rm = TRUE),
    pSlav = mean(slavs, na.rm = TRUE),
    pEastEU = mean(eastern, na.rm = TRUE),
    pBlack = mean(black, na.rm = TRUE)
  ) %>%
  ungroup()

# ===========================================================
# 4. Filter the Data to Keep Only Relevant Individuals
# ===========================================================
# Keep only immigrants or those with immigrant parents
acs_filtered <- acs %>%
  filter(isImmigrant == 1 | isImmigrant_FA == 1 | isImmigrant_MA == 1) %>%
  filter(ethn != 'other') %>%
  # Merge county-level variables
  left_join(county_vars, by = c("fips", "year"))

# ===========================================================
# 5. Compute Ethnicity-Specific Variables
# ===========================================================
# Compute ethnicity-specific variables
ethn_vars <- acs_filtered %>%
  group_by(fips, year, ethn) %>%
  summarise(
    ethn_population = n(),
    ethn_mean_age = mean(AGE, na.rm = TRUE),
    ethn_avg_occscore = mean(OCCSCORE, na.rm = TRUE),
    ethn_avg_education_years = mean(education_years, na.rm = TRUE),
    ethn_avg_unemped = mean(unemployed, na.rm = TRUE),
    ethn_avg_ln_wage = mean(ln_wage, na.rm = TRUE),
    ethn_intermarriage = mean(intermarriage, na.rm = TRUE),
    ethn_langisolated = mean(langisolated, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Merge county population to compute proportions
  left_join(county_vars %>% select(fips, year, population), by = c("fips", "year")) %>%
  mutate(
    pEthn = ethn_population / population
  )

# Merge ethnicity-specific variables back into the main dataset
acs_filtered <- acs_filtered %>%
  left_join(ethn_vars, by = c("fips", "year", "ethn"))


# ===========================================================
# 6. Select Relevant Variables
# ===========================================================

acs_final <- acs_filtered %>%
  select(
    year, SERIAL, HHWT, CLUSTER, REGION, PERWT, fips, ethn, 
    age,age_sq,
    gender, hh_size,education_years,
    birth_year, married, year_married, isImmigrant, usBorn, imm_year, imm_year_fa,
    imm_year_ma, age_at_mig, isCitizen, isNaturalized, english_proficient,
    intermarriage, langisolated, years_in_us, log_ocp, unemployed, eng, ln_wage,
    ln_wage_fa, ln_wage_ma, ln_wage_sp, ln_par_wage, isRussian, filterrus,
     mean_age, avg_occscore, avg_education_years, avg_unemped, avg_ln_wage,
    avg_ln_wage_locals, population.x,population.y, pEnglishOnly, pEnglish_Proficient,
    pLangisolated, pIntermarriagee, pNaturalized, pCitiz, pImm, pRus, pSerb,
    pAlb, pMac, pBulg, pUkr, pGreek, pRom, pSlav, pEastEU, pBlack,
    ethn_mean_age, ethn_avg_occscore, ethn_avg_education_years,
    ethn_avg_unemped, ethn_avg_ln_wage, pEthn, ethn_intermarriage,
    ethn_langisolated
  )

saveRDS(acs_filtered, "data/acs_final_full.rds")
saveRDS(acs_final, "data/acs_final_reduceds.rds")
saveRDS(acs, "data/acs_modified.rds")
saveRDS(county_vars, "data/county_vars.rds")
saveRDS(ethn_vars, "data/ethn_vars.rds")



# (3) religion
###################################
# function from 'functions.r'

# ===========================================================
# 3. Merge with Ethnic Church Data
# ===========================================================
# Load and process religious congregation data
code_list <- fread('orthodox_churches_var_ethn_codebook.csv')
dataset_list <- paste0('data/relig_', c(2020, 2010, 2000), '.dta')
term_mapping <- fread('data/mappingvar.csv')
term_mapping <- setNames(term_mapping$mapping, term_mapping$term)

datasrc <- lapply(dataset_list, reader)
relig_data <- lapply(datasrc, clean_and_pivot) %>%
  lapply(createType) %>%
  lapply(remove_and_filter) %>%
  lapply(fixfips) %>%
  lapply(group_and_map) %>%
  rbindlist() %>%
  left_join(code_list[, .(var, ethn)], by = "var") %>%
  mutate(year = as.numeric(year)) %>%
  group_by(fips, ethn, year) %>%
  summarise(
    cng = sum(cng, na.rm = TRUE),
    adh = sum(adh, na.rm = TRUE),
    church = as.numeric(sum(cng + adh, na.rm = TRUE) > 0)
  ) %>%
  ungroup()

# Create a full grid of fips, ethn, and year
all_counties <- unique(acs$fips)
all_ethn <- unique(acs$ethn)
all_years <- unique(acs$year)
county_ethn_year_grid <- expand.grid(fips = all_counties, ethn = all_ethn, year = all_years)
distance=readRDS('data/dist_from_eastern_church.rds')
# Merge religious data with the grid
relig_data_full <- county_ethn_year_grid %>%
  left_join(relig_data, by = c("fips", "ethn", "year")) %>%
  mutate(
    cng = if_else(is.na(cng), 0, cng),
    adh = if_else(is.na(adh), 0, adh),
    church = if_else(is.na(church), 0, church)
  ) %>% filter(ethn!='other') %>% left_join(distance) %>% 
  group_by(fips,ethn) %>%
    arrange(year) %>%
    mutate(lag_cng = lag(cng))

# join 

result <- acs_final %>%
  left_join(relig_data_full, by = c("fips", "ethn", "year"))

saveRDS(result,'result.rds')
# sink('res.txt',type='output')
###### Analysis and subsets
# acs=acs %>% select(year,household=SERIAL,fips,ethn,age,age_sq,isImmigrant,isImmigrant_FA,isImmigrant_MA,isCitizen,isNaturalized,isEnglishProficient,marriedToLocal,onlyEnglish,years_in_us,eng,ln_wage,ln_wage_fa,education_years,isRussian,isFaRussian,isMaRussian,filterrus,YRSUSA1,imm_year)
# acs=acs %>% select(year,household,fips,ethn,age,age_sq,isImmigrant,isImmigrant_FA,isImmigrant_MA,isCitizen,isNaturalized,isEnglishProficient,marriedToLocal,onlyEnglish,years_in_us,eng,ln_wage,ln_wage_fa,education_years,isRussian,isFaRussian,isMaRussian,filterrus,YRSUSA1,imm_year)



# Now acs_final contains the dataset ready for analysis

# ===========================================================
# 7. Run Econometric Models (Optional)
# ===========================================================
# You can proceed to run your models using acs_final

# Example model (Intermarriage)
# Ensure factor variables for fixed effects
result$year <- as.factor(result$year)
result$fips <- as.factor(result$fips)
result$ethn <- as.factor(result$ethn)

result=result %>% mutate(EthnicChurch=cng)
naive_model_intermarriage <- lm(intermarriage ~ EthnicChurch, data = result)

summary(naive_model_intermarriage)
naive_model_ln_wage <- lm(ln_wage ~ EthnicChurch, data = result)
summary(naive_model_ln_wage)

naive_model_eng <- lm(eng ~ EthnicChurch, data = result %>% filter(age >= 15))
summary(naive_model_eng)

naive_model_isCitizen <- lm(isCitizen ~ EthnicChurch, data = result %>% filter(years_in_us >= 5))
summary(naive_model_isCitizen)

# Model 1: Basic Fixed Effects with Ethnic Church Presence
# Introducing fixed effects for county, year, and ethnicity to control for time-invariant unobserved characteristics at these levels. This helps isolate the impact of ethnic churches on assimilation outcomes.
model_1_intermarriage <- feols(intermarriage ~ EthnicChurch | fips + year + ethn, data = result)
summary(model_1_intermarriage)

model_1_ln_wage <- feols(ln_wage ~ EthnicChurch | fips + year + ethn, data = result)
summary(model_1_ln_wage)

model_1_eng <- feols(eng ~ EthnicChurch | fips + year + ethn, data = result %>% filter(age >= 15))
summary(model_1_eng)

model_1_isCitizen <- feols(isCitizen ~ EthnicChurch | fips + year + ethn, data = result %>% filter(years_in_us >= 5))
summary(model_1_isCitizen)

# Model 2: Adding Interaction Term for Second-Generation Immigrants
# Adding interaction between ethnic church presence and second-generation status to examine differential effects on first- and second-generation immigrants. This model provides insights into generational differences in assimilation outcomes.
model_2_intermarriage <- feols(intermarriage ~ EthnicChurch * usBorn | fips + year + ethn, data = result)
summary(model_2_intermarriage)

model_2_ln_wage <- feols(ln_wage ~ EthnicChurch * usBorn | fips + year + ethn, data = result)
summary(model_2_ln_wage)

model_2_eng <- feols(eng ~ EthnicChurch * usBorn | fips + year + ethn, data = result %>% filter(age >= 15))
summary(model_2_eng)

model_2_isCitizen <- feols(isCitizen ~ EthnicChurch * usBorn | fips + year + ethn, data = result %>% filter(years_in_us >= 5))
summary(model_2_isCitizen)

# Model 3: Fixed Effects with County-Level and Individual-Level Controls
# Incorporating additional controls at both the county and individual levels to mitigate concerns about omitted variable bias. Controls include demographic and economic indicators to better isolate the effect of ethnic churches.
colnames(result)
feols(isCitizen ~ EthnicChurch + age + age_sq + usBorn+usBorn:EthnicChurch + education_years| fips + year + ethn, data = result) %>% summary()
feols(intermarriage ~ EthnicChurch + age + age_sq + usBorn+usBorn:EthnicChurch + education_years+ln_wage| fips + year, data = result) %>% summary()
feols(isCitizen ~ EthnicChurch + age + age_sq + usBorn+usBorn:EthnicChurch + education_years+ln_wage| fips + year, data = result) %>% summary()
feols(ln_wage ~ EthnicChurch + age + age_sq + usBorn+usBorn:EthnicChurch + education_years+ln_par_wage+avg_ln_wage| fips + year, data = result) %>% summary()
feols(ln_wage ~ EthnicChurch + age + age_sq + usBorn+usBorn:EthnicChurch + education_years+ln_par_wage+avg_ln_wage| fips, data = result) %>% summary()

feols(intermarriage ~  lag(EthnicChurch, 1) + age + age_sq + usBorn+usBorn: lag(EthnicChurch, 1) + education_years+ln_par_wage+avg_ln_wage| fips+year+ethn, data = result) %>% summary()

