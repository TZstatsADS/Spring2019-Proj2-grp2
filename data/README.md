# Project 2: Shiny App Development Version 2.0

### Data folder

The data directory contains data used in the analysis. This is treated as read only; in paricular the R/python files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.

@@ -4,3 +4,64 @@

The data directory contains data used in the analysis. This is treated as read only; in paricular the R/python files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.

### college_data.csv

This csv file contains clean data that we may need to use in our project.
Following is the meaning of each column:
```
+ UNITID: ID for each college
+ NAME
+ CITY
+ STABBR
+ ZIP: zip code
+ INSTURL: homepage 
+ RANK: college ranking
+ PERCENTAGEOFINTERNATIONAL: international student percentage
+ TUITION
+ NUMBEROFUNDERGRAD: undergrads number
+ TYPE: PNFP = private non for profit, Pub = public
+ RESOURCESOCRE: resource score
+ ENGAGEMENTSCORE: engagement score
+ OUTCOMESCORE: outcome socore
+ ENVIRONMENTSCORE: environment score
+ OVERALLSCORE: overall score
+ SCH_DEG: Missing values, 0s, and 4s from PREDDEG recoded according to NSLDS program length
+ HCM2: Schools that are on Heightened Cash Monitoring 2 by the Department of Education
+ MAIN: main campus?
+ NUMBRANCH: Number of branch campuses
+ PREDDEG: "Predominant undergraduate degree awarded
 0 Not classified
 1 Predominantly certificate-degree granting
 2 Predominantly associate's-degree granting
 3 Predominantly bachelor's-degree granting
 4 Entirely graduate-degree granting"

+ HIGHDEG: Highest degree awarded
 0 Non-degree-granting
 1 Certificate degree
 2 Associate degree
 3 Bachelor's degree
 4 Graduate degree

+ CONTROL: Control of institution
+ ST_FIPS: FIPS code for state
+ REGION: Region (IPEDS)
+ LOCALE: Locale of institution(city size)
+ LOCALE2: same but different measure
+ CCBASIC, CCUGPROF, CCSIZSET: Carnegie Classification -- ...
+ ADM_RATE: admission rate
+ ADM_RATE_ALL: Admission rate for all campuses rolled up to the 6-digit OPE ID
+ SATVR: X percentile of SAT scores at the institution (critical reading)
+ SATMT: X percentile of SAT scores at the institution (math) 
+ SATWR: X percentile of SAT scores at the institution (writing)
+ SATVRMID: mid point of CR
+ SATMTMID: mid point of MATH
+ SATWRMID: mid point of writing
+ ACTxxxx: .... of ACT scores....
+ SAT_AVG_ALL: Average SAT equivalent score of students admitted for all campuses rolled up to the 6-digit OPE ID
+ PCIP01 - PCIP54: subject categories
+ AGE_ENTRY: age when entering college
+ MN_EARN_WNE_P6: Mean earnings of students working and not enrolled 6 years after entry
+ MD_EARN_WNE_P6: Mid earnings of students working and not enrolled 6 years after entry
```
