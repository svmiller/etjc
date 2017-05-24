GTD7013 <- read_csv("~/Dropbox/data/gtd/20150622-download/gtd1970-2013.csv")
GTD1993 <- read_csv("~/Dropbox/data/gtd/20150622-download/gtd1993.csv")

EVS <- read.csv("~/Dropbox/data/evs/evs-longitudinal.csv", na.strings = c(".a",".b",".c",".d",".e")) %>% tbl_df()

States <- read_csv("~/Dropbox/data/MID/states/states2011.csv")


CIRI <- read_csv("~/Dropbox/data/CIRI/CIRI Data 1981_2011 2014.04.14.csv")
UDS <- read_csv("~/Dropbox/data/uds/uds_summary-2012.csv")
Polity <- read_csv("~/Dropbox/data/polity/p4v2014.csv")

ImpGDP <- read_csv("~/Dropbox/data/imputed-gdp/imputed-gdp.csv")

EC <- read_csv("~/Dropbox/data/worldbank/enforcing-contracts.csv")
