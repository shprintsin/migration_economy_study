feols( marriedToLocal~cng+age+age^2+pop++unemp++secondGen+education_years+years_in_us|year, cluster = "fips", data = dt %>% filter(russian_origin==1))
feols( marriedToLocal~cng+age+age^2+pop++unemp++secondGen+education_years+years_in_us|year, cluster = "fips", data = dt %>% filter(russian_origin==1))

feols( marriedToLocal~cng+age+age^2+pop++unemp++secondGen+education_years+years_in_us|year+county, cluster = "fips", data = dt %>% filter(russian_origin==1))

feols( isEnglishProficient~cng+roc+age+age^2+pop+unemp+secondGen+education_years+years_in_us|year, cluster = "fips", data = dt %>% filter(russian_origin==1))
feols( isEnglishProficient~age+age^2+pop++unemp+secondGen+education_years+years_in_us|year|cng~rus_1936, cluster = "fips", data = dt %>% filter(russian_origin==1))

feols( isCitizen~cng+age+age^2+pop++unemp+secondGen+education_years+years_in_us|year, cluster = "fips", data = dt %>% filter(russian_origin==1,years_in_us>5|years_in_us<15))
feols( isCitizen~age+age^2+pop++unemp+secondGen+education_years+years_in_us|year|cng~rus_1936, cluster = "fips", data = dt %>% filter(russian_origin==1,years_in_us>5|years_in_us<15))

feols( ln_wage~ln_wage_fa+age+age^2+pop+unemp+years_in_us|year|cng~rus_1936, cluster = "fips", data = dt)
feols( ln_wage~cng+ln_wage_fa+age+age^2+pop+unemp+years_in_us|year, cluster = "fips", data = dt)

feols( eng~cng+age+age^2+pop+unemp+secondGen+education_years+years_in_us|year+ethn+household, cluster = "fips", data = dt )
feols( eng~cng+age+age^2+pop+unemp+secondGen+education_years+years_in_us|year+ethn+ln_wage, cluster = "fips", data = dt )
feols( marriedToLocal~cng+age+age^2+pop++unemp++secondGen+education_years+years_in_us|year, cluster = "fips", data = dt %>% filter(russian_origin==1))
