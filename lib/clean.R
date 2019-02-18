# data cleaning 

setwd('/Users/tianchenwang/Git/project2data')
library(dplyr)

all.data <- read_csv('alldata.csv')
ranking.data <- read_csv('rankingdata.csv')
colnames(ranking.data) <- toupper(colnames(ranking.data))

names(all.data)

pci <- strsplit('PCIP01
              PCIP03
                PCIP04
                PCIP05
                PCIP09
                PCIP10
                PCIP11
                PCIP12
                PCIP13
                PCIP14
                PCIP15
                PCIP16
                PCIP19
                PCIP22
                PCIP23
                PCIP24
                PCIP25
                PCIP26
                PCIP27
                PCIP29
                PCIP30
                PCIP31
                PCIP38
                PCIP39
                PCIP40
                PCIP41
                PCIP42
                PCIP43
                PCIP44
                PCIP45
                PCIP46
                PCIP47
                PCIP48
                PCIP49
                PCIP50
                PCIP51
                PCIP52
                PCIP54', '\n')
pci <- sapply(pci, trimws)
pci <- pci[,1]

keep <- c("UNITID", 'INSTNM', 'CITY', 'STABBR','ZIP',
          'INSTURL', 'SCH_DEG', 'HCM2', 'MAIN', 'NUMBRANCH', 'PREDDEG', 'HIGHDEG',
          'CONTROL', 'ST_FIPS', 'REGION', 'LOCALE', 'LOCALE2', 'LATITUDE', 'LONGITUDE',
          'CCBASIC', 'CCUGPROF', 'CCSIZSET', 'ADM_RATE', 'ADM_RATE_ALL', 'SATVR25',
          'SATVR75', 'SATMT25', 'SATMT75', 'SATWR25', 'SATWR75', 'SATVRMID', 'SATMTMID',
          'SATWRMID', 'ACTCM25', 'ACTCM75', 'ACTEN25', 'ACTEN75', 'ACTMT25', 'ACTMT75',
          'ACTWR25', 'ACTWR75', 'ACTCMMID', 'ACTENMID', 'ACTMTMID', 'ACTWRMID', 'SAT_AVG',
          'SAT_AVG_ALL', pci, 'AGE_ENTRY', 'MN_EARN_WNE_P6', 'MD_EARN_WNE_P6', 'MN_EARN_WNE_P8',
          'MD_EARN_WNE_P8'
          )

df <- all.data[, keep]
df <- as_tibble(df)
## delete non main campus college
df <- df %>% filter(MAIN == 1)
# df$NAME <- as.character(df$NAME)
# ranking.data$NAME <- as.character(ranking.data$NAME)
colnames(df)[2] <- 'NAME'



## find unmatched name in df and ranking
new.df <- right_join(df, ranking.data, by ='NAME')
missing.raw <-new.df %>% filter(is.na(UNITID)) %>% select(NAME) 
missing.raw <- missing.raw[[1]]
missing <- sub(', ','-', missing.raw)

needtorep <- lapply(missing, function(x) 
  grep(x, df$NAME))

l.needtorep <- sapply(needtorep, length)
missing[l.needtorep == 1] <- df$NAME[unlist(needtorep[l.needtorep == 1])]
missing[l.needtorep > 1] <- c('Columbia University in the City of New York',
                              'University of Virginia-Main Campus',
                              'Pennsylvania State University-Main Campus',
                              'The University of Alabama',
                              'Oklahoma State University-Main Campus',
                              'Louisiana State University and Agricultural & Mechanical College'
                              )
missing[l.needtorep == 0] <- c('Purdue University-Main Campus',
                               'University of Pittsburgh-Pittsburgh Campus',
                               'Texas A&M University-Texarkana',
                               'College of William and Mary',
                               'Franklin and Marshall College',
                               'University of Minnesota-Twin Cities',
                               'Indiana University-Bloomington',
                               'Furman',
                               'Art Center College of Design',
                               NA,
                               'Hobart William Smith Colleges',
                               'Wheaton College',
                               'Saint Mary\'s College',
                               'University of Massachusetts-Amherst',
                               'Binghamton University',
                               'Wheaton College',
                               'SUNY at Albany',
                               'Saint Johns University',
                               'Augustana College',
                               'Embry-Riddle Aeronautical University-Worldwide',
                               'Mount Saint Mary\'s University',
                               'Westminster College',
                               'Rutgers University-New Brunswick',
                               'Saint Mary\'s College of California',
                               'St Mary\'s College of Maryland',
                               NA,
                               'CUNY Bernard M Baruch College',
                               'Saint Joseph\'s University',
                               NA,
                               'CUNY City College',
                               NA,
                               'Saint Michael\'s College',
                               'Southern Illinois University-Carbondale',
                               NA,
                               'University of Cincinnati-Main Campus',
                               'St Joseph\'s College of Nursing at St Joseph\'s Hospital Health Center',
                               'University of Hawaii at Manoa',
                               'Marian University',
                               'Saint Edward\'s University',
                               'Saint John Fisher College',
                               'Doane University-Arts & Sciences',
                               'St John\'s University-New York',
                               'Saint Norbert College',
                               'Augsburg University',
                               'King\'s College',
                               NA,
                               NA,
                               'University of Massachusetts-Boston',
                               'Indiana University-Purdue University-Indianapolis',
                               'The College of Saint Scholastica',
                               'Saint Ambrose University',
                               'Citadel Military College of South Carolina',
                               'Birmingham Southern College'
                               )

## change original ranking.data name into name of all.data
missing.id <- which(ranking.data$NAME %in% missing.raw)
ranking.data$NAME[missing.id] <- missing 


# right join!!!
new.df <- right_join(df, ranking.data, by ='NAME') %>% drop_na()
# some duplicated college name
names(new.df)
new.df <- new.df[, c(1:6, 91, 94:102, 7:90)]
colnames(new.df)[3] <- 'CITY' 

write_csv(new.df, 'college_data.csv')

