#
# Clean EVS (micro)
# -----------------------------------------------------
# Steve Miller
# Date: 31 Jan 2017
# License

EVS %>%
  mutate(evsccode = s003) %>%
  arrange(evsccode) %>%
  mutate(country = countrycode(evsccode, "un", "country.name")) -> EVS

# Some light cleaning
EVS$country[EVS$evsccode == 197] <- "Northern Cyprus"
EVS$country[EVS$evsccode == 909] <- "Northern Ireland"
EVS$country[EVS$evsccode == 915] <- "Kosovo"

EVS$country[EVS$evsccode == 807] <- "Macedonia"
EVS$country[EVS$evsccode == 643] <- "Russia"
EVS$country[EVS$evsccode == 498] <- "Moldova"

EVS$ccode <- countrycode(EVS$country, "country.name", "cown")

# More light cleaning for ccodes
EVS$ccode[EVS$evsccode == 197] <- NA
EVS$ccode[EVS$evsccode == 909] <- NA
EVS$ccode[EVS$evsccode == 688] <- 345

EVS$wave <- EVS$s002evs # 1 = 1981-1984, 2 = 1990-1993, 3 = 1999-2001, 4 = 2008-2010
EVS$year <- EVS$s020
EVS$post911 <- with(EVS, ifelse(year > 2001, 1, 0))

# Czechoslovakia fixes
EVS$ccode[EVS$ccode == 316 & EVS$year == 1991] <- 315
EVS$ccode[EVS$ccode == 317 & EVS$year == 1991] <- 315
EVS$country[EVS$ccode == 315 & EVS$year == 1991] <- "Czechoslovakia"
EVS$country[EVS$ccode == 315 & EVS$year == 1991] <- "Czechoslovakia"

# I would fix Germany, but that survey was in 1981 in GFR. It'll be dropped anywho...





# Get DVs and IVs
# -----------------------------------------------

EVS %>%
  mutate(# DVs first...,
         concourts = car::recode(e069_17, "1=4; 2=3; 3=2; 4=1"),
         concourtsd = car::recode(concourts, "1:2=0; 3:4=1"),
         # IVs next...,
         age = x003,
         female = car::recode(x001, "-5:-1=NA; 1=0; 2=1"),
         collegeed = car::recode(x025, "-5:-1=NA; 1:7=0; 8=1"),
         incgroup = car::recode(x047r, "1=-1; 2=0; 3=1"),
         inceuro = x047c,
         unemployed = car::recode(x028, "-5:-1=NA; 1:6=0; 7=1; 8=0"),
         conparl = car::recode(e069_07,"-5:-1=NA; 1=4; 2=3; 3=2; 4=1")) %>%
  select(evsccode:conparl) %>%
  filter(!is.na(ccode)) %>%
  filter(ccode != 2 & ccode != 20) %>%
  group_by(ccode, wave) %>%
  mutate(zg_age = arm::rescale(age),
         zg_incgroup = arm::rescale(incgroup),
         zg_conparl = arm::rescale(conparl)) -> EVS

