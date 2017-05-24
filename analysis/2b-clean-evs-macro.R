#
# Clean EVS (macro)
# -----------------------------------------------------
# Steve Miller
# Date: 10 July 2015
# License

EVS %>% ungroup() %>%
  select(country, evsccode, wave, ccode, year) %>%
  distinct(country, evsccode, wave, ccode, year) -> Macro_EVS

Macro_EVS %>%
  mutate(ccodeyear = as.numeric(str_c("1",ccode,year)),
         ee = ifelse(ccode == 290 | ccode == 310 | ccode == 316 | ccode == 317 | 
                       ccode == 339| ccode == 341 |  ccode == 343 | ccode == 344 | 
                       ccode == 345 | ccode == 346 | ccode == 347 | ccode == 349 | 
                       (ccode >= 355 & ccode <=  373) | ccode == 640, 1, 0),
         we = ifelse(ee == 0, 1, 0)) %>%
  arrange(ccode, year) -> Macro_EVS



# GTD data now. ----------
# ------------------------

GTD7013$ccode <- countrycode(GTD7013$country_txt, "country.name", "cown")
GTD1993$ccode <- countrycode(GTD1993$country_txt, "country.name", "cown")

GTD7013 %>%
  mutate(intnl =  ifelse(INT_LOG == 1 | INT_IDEO == 1 | INT_MISC == 1, 1, 0),
         pubtarg = ifelse(targtype1 == 1 | targtype1 == 5 | targtype1 == 6 | targtype1 == 8 |
                            targtype1 == 9 | targtype1 == 14 | targtype1 == 15 | 
                            targtype1 == 15 | targtype1 == 16 | targtype1 == 18 |
                            targtype1 == 19, 1, 0)) %>%
  select(iyear, ccode, success, nkill, nwound, propextent, intnl, pubtarg) -> GTD7013s

GTD1993 %>%
  mutate(intnl = NA,
         pubtarg = ifelse(targtype1 == 1 | targtype1 == 5 | targtype1 == 6 | targtype1 == 8 |
                            targtype1 == 9 | targtype1 == 14 | targtype1 == 15 | 
                            targtype1 == 15 | targtype1 == 16 | targtype1 == 18 |
                            targtype1 == 19, 1, 0)) %>%
  select(iyear, ccode, success, nkill, nwound, propextent, intnl, pubtarg) -> GTD1993s

GTD <- bind_rows(GTD7013s, GTD1993s) %>%
  filter((ccode >= 200 & ccode < 400) | ccode == 640) %>%
  mutate(year = iyear + 1,
         ones = 1,
         # If we don't know it was successful, it wasn't.
         success = car::recode(success, "-9=0"),
         # If it's missing, we'll say it's just 1. Clearly people died, but we'll be conservative.
         nkill = ifelse(is.na(nkill), 1, nkill),
         # If it's missing, we'll say it's just 1. Clearly people were injured, but we'll be conservative.
         nwound = ifelse(is.na(nwound), 1, nwound),
         # If damage is missing, code it as a zero. This is consistent with GTI.
         propextent = ifelse(is.na(propextent), 0, propextent)
         ) 

# Let's start with the main GTI variable. ----------
# --------------------------------------------------

GTD %>%
  group_by(year, ccode) %>%
  summarize(ones = sum(ones),
            success = sum(success),
            nkill = sum(nkill),
            nwound = sum(nwound),
            propextent = sum(propextent)) -> GTDs


States %>%
  mutate(endyear = ifelse(endyear == 2011, 2013, endyear)) %>%
  rowwise() %>%
  mutate(year = list(seq(styear, endyear))) %>%
  ungroup() %>%
  unnest() %>%
  arrange(ccode, year) %>%
  select(ccode, year) %>%
  filter(year >= 1970 & ((ccode >= 200 & ccode < 400) | ccode == 640)) %>%
  distinct(ccode, year) -> CYs

CYs <- left_join(CYs, GTDs) %>%
  mutate(ones = ifelse(is.na(ones), 0, ones),
         success = ifelse(is.na(success), 0, success),
         nkill = ifelse(is.na(nkill), 0, nkill),
         nwound = ifelse(is.na(nwound), 0, nwound),
         propextent = ifelse(is.na(propextent), 0, propextent))

CYs %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(gtiraw = ones + 3*(nkill) + .5*(nwound) + 2*(propextent),
         l1_gtiraw = lag(gtiraw, 1),
         l2_gtiraw = lag(gtiraw, 2),
         l3_gtiraw = lag(gtiraw, 3),
         l4_gtiraw = lag(gtiraw, 4),
         gtiraw5ya = 16*(gtiraw) + 8*(l1_gtiraw) + 4*(l2_gtiraw) + 2*(l3_gtiraw) + l4_gtiraw,
         gtiraw5ya = ifelse(is.na(gtiraw5ya), 16*(gtiraw) + 8*(l1_gtiraw) + 4*(l2_gtiraw) + 2*(l3_gtiraw), gtiraw5ya),
         gtiraw5ya = ifelse(is.na(gtiraw5ya), 16*(gtiraw) + 8*(l1_gtiraw) + 4*(l2_gtiraw), gtiraw5ya),
         gtiraw5ya = ifelse(is.na(gtiraw5ya), 16*(gtiraw) + 8*(l1_gtiraw), gtiraw5ya),
         loggti5ya = log(gtiraw5ya + 1), base = max(gtiraw5ya, na.rm=T)^(1/20)) %>%
  select(ccode, year, gtiraw, gtiraw5ya, loggti5ya) -> GTI

Macro_EVS <- left_join(Macro_EVS, GTI)

# Let's think of a mirt approach next. ----------
# -----------------------------------------------

CYs %>%
  mutate(logones = round(log(ones+1)),
         logsuccess = round(log(success+1)),
         lognkill = round(log(nkill + 1)),
         lognwound = round(log(nwound + 1)),
         logpropextent = round(log(propextent + 1))) %>%
  group_by(ccode) %>%
  mutate(l1_logones = lag(logones, 1),
         l2_logones = lag(logones, 2),
         l3_logones = lag(logones, 3),
         l4_logones = lag(logones, 4),
         l1_logsuccess = lag(logsuccess, 1),
         l2_logsuccess = lag(logsuccess, 2),
         l3_logsuccess = lag(logsuccess, 3),
         l4_logsuccess = lag(logsuccess, 4),
         l1_lognkill = lag(lognkill, 1),
         l2_lognkill = lag(lognkill, 2),
         l3_lognkill = lag(lognkill, 3),
         l4_lognkill = lag(lognkill, 4),
         l1_lognwound = lag(lognwound, 1),
         l2_lognwound = lag(lognwound, 2),
         l3_lognwound = lag(lognwound, 3),
         l4_lognwound = lag(lognwound, 4),
         l1_propextent = lag(logpropextent, 1),
         l2_propextent = lag(logpropextent, 2),
         l3_propextent = lag(logpropextent, 3),
         l4_propextent = lag(logpropextent, 4)) %>% as.data.frame() -> LTT

TerrorM <- mirt(LTT[ ,  8:ncol(LTT)], model = 1,
                       itemtype = "graded", SE = TRUE, verbose = FALSE)

terrorscores <- fscores(TerrorM, full.scores = TRUE, full.scores.SE = TRUE)

LTT <- cbind(LTT, terrorscores) %>% tbl_df() %>%
  rename(lterrt = F1) %>%
  select(ccode, year, lterrt)

Macro_EVS <- left_join(Macro_EVS, LTT)

# Next up: internationally-oriented terror attacks. ----------
# ------------------------------------------------------------
# Note: this means we're losing 1993. We'll do four-year lags instead.

GTD %>%
  filter(intnl == 1) %>% # This is important right here...
  group_by(year, ccode) %>%
  summarize(ones = sum(ones),
            success = sum(success),
            nkill = sum(nkill),
            nwound = sum(nwound),
            propextent = sum(propextent)) -> GTDs


States %>%
  mutate(endyear = ifelse(endyear == 2011, 2013, endyear)) %>%
  rowwise() %>%
  mutate(year = list(seq(styear, endyear))) %>%
  ungroup() %>%
  unnest() %>%
  arrange(ccode, year) %>%
  select(ccode, year) %>%
  filter(year >= 1970 & ((ccode >= 200 & ccode < 400) | ccode == 640)) %>%
  distinct(ccode, year) -> CYs

CYs <- left_join(CYs, GTDs) %>%
  mutate(ones = ifelse(is.na(ones), 0, ones),
         success = ifelse(is.na(success), 0, success),
         nkill = ifelse(is.na(nkill), 0, nkill),
         nwound = ifelse(is.na(nwound), 0, nwound),
         propextent = ifelse(is.na(propextent), 0, propextent))

CYs %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(gtiraw = ones + 3*(nkill) + .5*(nwound) + 2*(propextent),
         l1_gtiraw = lag(gtiraw, 1),
         l2_gtiraw = lag(gtiraw, 2),
         l3_gtiraw = lag(gtiraw, 3),
         gtiraw5ya = 16*(gtiraw) + 8*(l1_gtiraw) + 4*(l2_gtiraw) + 2*(l3_gtiraw), 
         gtiraw5ya = ifelse(is.na(gtiraw5ya), 16*(gtiraw) + 8*(l1_gtiraw) + 4*(l2_gtiraw), gtiraw5ya),
         gtiraw5ya = ifelse(is.na(gtiraw5ya), 16*(gtiraw) + 8*(l1_gtiraw), gtiraw5ya),
         gtiraw5ya = ifelse(is.na(gtiraw5ya), 16*(gtiraw), gtiraw5ya),
         loggti5ya_int = log(gtiraw5ya + 1), base = max(gtiraw5ya, na.rm=T)^(1/20)) %>%
  select(ccode, year, loggti5ya_int) -> Intnl

Macro_EVS <- left_join(Macro_EVS, Intnl)

# Then: domestically-oriented terror attacks ----------
# -----------------------------------------------------
# Same caveat applies. Let's do four-year lags in lieu of five.

GTD %>%
  filter(intnl == 0) %>% # This is important right here...
  group_by(year, ccode) %>%
  summarize(ones = sum(ones),
            success = sum(success),
            nkill = sum(nkill),
            nwound = sum(nwound),
            propextent = sum(propextent)) -> GTDs


States %>%
  mutate(endyear = ifelse(endyear == 2011, 2013, endyear)) %>%
  rowwise() %>%
  mutate(year = list(seq(styear, endyear))) %>%
  ungroup() %>%
  unnest() %>%
  arrange(ccode, year) %>%
  select(ccode, year) %>%
  filter(year >= 1970 & ((ccode >= 200 & ccode < 400) | ccode == 640)) %>%
  distinct(ccode, year) -> CYs

CYs <- left_join(CYs, GTDs) %>%
  mutate(ones = ifelse(is.na(ones), 0, ones),
         success = ifelse(is.na(success), 0, success),
         nkill = ifelse(is.na(nkill), 0, nkill),
         nwound = ifelse(is.na(nwound), 0, nwound),
         propextent = ifelse(is.na(propextent), 0, propextent))

CYs %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(gtiraw = ones + 3*(nkill) + .5*(nwound) + 2*(propextent),
         l1_gtiraw = lag(gtiraw, 1),
         l2_gtiraw = lag(gtiraw, 2),
         l3_gtiraw = lag(gtiraw, 3),
         gtiraw5ya = 16*(gtiraw) + 8*(l1_gtiraw) + 4*(l2_gtiraw) + 2*(l3_gtiraw),
         gtiraw5ya = ifelse(is.na(gtiraw5ya), 16*(gtiraw) + 8*(l1_gtiraw) + 4*(l2_gtiraw), gtiraw5ya),
         gtiraw5ya = ifelse(is.na(gtiraw5ya), 16*(gtiraw) + 8*(l1_gtiraw), gtiraw5ya),
         gtiraw5ya = ifelse(is.na(gtiraw5ya), 16*(gtiraw), gtiraw5ya),
         loggti5ya_dom = log(gtiraw5ya + 1), base = max(gtiraw5ya, na.rm=T)^(1/20)) %>%
  select(ccode, year, loggti5ya_dom) -> Dom

Macro_EVS <- left_join(Macro_EVS, Dom)

# Next up: was the public targeted?
# ---------------------------------


GTD %>%
  filter(pubtarg == 1) %>% # This is important right here...
  group_by(year, ccode) %>%
  summarize(ones = sum(ones),
            success = sum(success),
            nkill = sum(nkill),
            nwound = sum(nwound),
            propextent = sum(propextent)) -> GTDs


States %>%
  mutate(endyear = ifelse(endyear == 2011, 2013, endyear)) %>%
  rowwise() %>%
  mutate(year = list(seq(styear, endyear))) %>%
  ungroup() %>%
  unnest() %>%
  arrange(ccode, year) %>%
  select(ccode, year) %>%
  filter(year >= 1970 & ((ccode >= 200 & ccode < 400) | ccode == 640)) %>%
  distinct(ccode, year) -> CYs

CYs <- left_join(CYs, GTDs) %>%
  mutate(ones = ifelse(is.na(ones), 0, ones),
         success = ifelse(is.na(success), 0, success),
         nkill = ifelse(is.na(nkill), 0, nkill),
         nwound = ifelse(is.na(nwound), 0, nwound),
         propextent = ifelse(is.na(propextent), 0, propextent))

CYs %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(gtiraw = ones + 3*(nkill) + .5*(nwound) + 2*(propextent),
         l1_gtiraw = lag(gtiraw, 1),
         l2_gtiraw = lag(gtiraw, 2),
         l3_gtiraw = lag(gtiraw, 3),
         l4_gtiraw = lag(gtiraw, 4),
         gtiraw5ya = 16*(gtiraw) + 8*(l1_gtiraw) + 4*(l2_gtiraw) + 2*(l3_gtiraw) + l4_gtiraw,
         gtiraw5ya = ifelse(is.na(gtiraw5ya), 16*(gtiraw) + 8*(l1_gtiraw) + 4*(l2_gtiraw) + 2*(l3_gtiraw), gtiraw5ya),
         gtiraw5ya = ifelse(is.na(gtiraw5ya), 16*(gtiraw) + 8*(l1_gtiraw) + 4*(l2_gtiraw), gtiraw5ya),
         gtiraw5ya = ifelse(is.na(gtiraw5ya), 16*(gtiraw) + 8*(l1_gtiraw), gtiraw5ya),
         loggti5ya_pubt = log(gtiraw5ya + 1), base = max(gtiraw5ya, na.rm=T)^(1/20)) %>%
  select(ccode, year, loggti5ya_pubt) -> Pubt

Macro_EVS <- left_join(Macro_EVS, Pubt)

# Finally, let's get the constituent items, log 'em, and add 'em. ----------
# --------------------------------------------------------------------------

GTD %>%
  group_by(year, ccode) %>%
  summarize(ones = sum(ones),
            success = sum(success),
            nkill = sum(nkill),
            nwound = sum(nwound),
            propextent = sum(propextent)) -> GTDs


States %>%
  mutate(endyear = 2013) %>%
  rowwise() %>%
  mutate(year = list(seq(styear, endyear))) %>%
  ungroup() %>%
  unnest() %>%
  arrange(ccode, year) %>%
  select(ccode, year) %>%
  filter(year >= 1970 & ((ccode >= 200 & ccode < 400) | ccode == 640)) %>%
  distinct(ccode, year) -> CYs

CYs <- left_join(CYs, GTDs) %>%
  mutate(ones = ifelse(is.na(ones), 0, ones),
         success = ifelse(is.na(success), 0, success),
         nkill = ifelse(is.na(nkill), 0, nkill),
         nwound = ifelse(is.na(nwound), 0, nwound),
         propextent = ifelse(is.na(propextent), 0, propextent))

CYs %>% 
  mutate(lognumterror = log(ones + 1),
         logsuccess = log(success + 1),
         lognkill = log(nkill + 1),
         lognwound = log(nwound + 1),
         logpropextent = log(propextent + 1)) %>%
  select(ccode:year, lognumterror:logpropextent) -> CYs

Macro_EVS <- left_join(Macro_EVS, CYs)

# Read judicial independence data. ----------
# -------------------------------------------

CIRI %>%
  filter(CTRY != "Soviet Union") -> CIRIs

CIRIs$COW[CIRIs$CTRY == "Montenegro"] <- 341
CIRIs$COW[CIRIs$CTRY == "Kosovo"] <- 347
# Recode the 260s to 255 for convenience sake.
CIRIs$COW[CIRIs$CTRY == "Germany"] <- 255


CIRIs %>%
  select(COW, YEAR, INJUD) %>%
  rename(ccode = COW, year = YEAR, injud = INJUD) %>%
  mutate(year = year + 1) %>%
  filter(!is.na(injud)) -> CIRIs

Macro_EVS <- left_join(Macro_EVS, CIRIs)

Macro_EVS$injud[Macro_EVS$ccode == 341] <- 1
Macro_EVS$injud[Macro_EVS$ccode == 345 & Macro_EVS$year == 2008] <- 1

Macro_EVS$gij <- with(Macro_EVS, car::recode(injud, "2=1; 1=0; 0=0"))

# UDS data. ----------
# --------------------

UDS %>% 
  rename(ccode = cowcode, udsmean=mean) %>%
  mutate(year = year + 1,
        ccode = ifelse(ccode == 260, 255, ccode)) %>%
  select(ccode, year, udsmean) -> UDSs

Macro_EVS <- left_join(Macro_EVS, UDSs)

# Polity data. ----------
# -----------------------

Polity %>%
  mutate(year = year + 1,
         ccode = ifelse(ccode == 260, 255, ccode),
         politydem7 = ifelse(polity2 > 6, 1, 0),
         politydem6 = ifelse(polity2 > 5, 1, 0)) %>%
  select(ccode, year, politydem7, politydem6) -> Politys

Macro_EVS <- left_join(Macro_EVS, Politys)

# Impute some values.
Macro_EVS$politydem7[Macro_EVS$ccode == 338 | Macro_EVS$ccode == 395] <- 1
Macro_EVS$politydem7[is.na(Macro_EVS$politydem7) & Macro_EVS$year > 1991] <- 0
Macro_EVS$politydem6[Macro_EVS$ccode == 338 | Macro_EVS$ccode == 395] <- 1
Macro_EVS$politydem6[is.na(Macro_EVS$politydem6) & Macro_EVS$year > 1991] <- 0

# Get some Imputed GDP data. ----------
# -------------------------------------
# https://github.com/svmiller/imputed-gdp

ImpGDP %>%
  select(ccode, year, explogrgdppc) -> ImpGDPs

Macro_EVS <- left_join(Macro_EVS, ImpGDPs)

# Enforcing Contracts data. ----------
# ------------------------------------

EC %>%
  mutate(ccode = countrycode(country, "country.name", "cown")) -> ECs

ECs$ccode[ECs$country == "Serbia"] <- 345
  
ECs %>%
  filter((ccode >= 200 & ccode < 400) | ccode == 640) %>%
  filter(!(country == "Russian Federation - Moscow" | country == "Russian Federation - Saint Petersburg")) %>%
  select(ccode, year, timedays) %>%
  mutate(year = year + 1) -> ECs

Macro_EVS <- left_join(Macro_EVS, ECs)

Data_EVS <- left_join(EVS, Macro_EVS) %>%
  group_by(wave) %>%
  mutate(zg_loggti5ya = arm::rescale(loggti5ya),
         zg_lterrt = arm::rescale(lterrt),
         zg_loggti5ya_int = arm::rescale(loggti5ya_int),
         zg_loggti5ya_dom = arm::rescale(loggti5ya_dom),
         zg_loggti5ya_pubt = arm::rescale(loggti5ya_pubt),
         zg_lognumterror = arm::rescale(lognumterror),
         zg_logsuccess = arm::rescale(logsuccess),
         zg_lognkill = arm::rescale(lognkill),
         zg_lognwound = arm::rescale(lognwound),
         zg_logpropextent = arm::rescale(logpropextent),
         zg_udsmean = arm::rescale(udsmean),
         zg_explogrgdppc = arm::rescale(explogrgdppc),
         z_timedays = arm::rescale(timedays))

write_csv(Data_EVS,"analysis/data-evs.csv")


# write.table(Data_EVS,file="analysis/data-evs.csv",sep=",",row.names=F,na="")



