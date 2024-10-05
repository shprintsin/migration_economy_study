# acs[,.N,SPEAKENG]
#    SPEAKENG          SPEAKENG_labels
# 1         0              N/A (Blank)
# 2         1   Does not speak English
# 3         2   Yes, speaks English...
# 4         3 Yes, speaks only English
# 5         4    Yes, speaks very well
# 6         5         Yes, speaks well
# 7         6        Yes, but not well
# 8         7                  Unknown
# 9         8                Illegible
# 10        9                    Blank
# > 

"1. does not speak english
2. speak but not well
3. speak english
4. speak well
5. speak very well
6. speak only english
0. NA"
acs=acs %>% mutate(eng= case_when(
  SPEAKENG == 1 ~ 1,
  SPEAKENG == 6 ~ 2,
  SPEAKENG == 2 ~ 4,
  SPEAKENG == 5 ~ 5,
  SPEAKENG == 4 ~ 6,

TRUE~0
),
)
print('modified speak english')