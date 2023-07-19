if(!requireNamespace("plyr"))
  install.packages("plyr", repos = "https://cloud.r-project.org")
library("plyr")

if(!requireNamespace("dplyr"))
  install.packages("dplyr", repos = "https://cloud.r-project.org")
library("dplyr")

if(!requireNamespace("tidyverse"))
  install.packages("tidyverse", repos = "https://cloud.r-project.org")
library("tidyverse")

if(!requireNamespace("ggplot2"))
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
library("ggplot2")
if(!requireNamespace("gridExtra"))
  install.packages("gridExtra", repos = "https://cloud.r-project.org")
library("gridExtra")

if(!requireNamespace("foreign"))
  install.packages("foreign", repos = "https://cloud.r-project.org")
library("foreign")

if(!requireNamespace("rstanarm"))
  install.packages("rstanarm", repos = "https://cloud.r-project.org")
library("rstanarm")

if(!requireNamespace("dataMeta"))
  install.packages("dataMeta", repos = "https://cloud.r-project.org")
library("dataMeta")


if(!requireNamespace("finalfit"))
  install.packages("finalfit", repos = "https://cloud.r-project.org")
library(finalfit) 

if(!requireNamespace("naniar"))
  install.packages("naniar", repos = "https://cloud.r-project.org")
library(naniar) 


if(!requireNamespace("Hmisc"))
  install.packages("Hmisc", repos = "https://cloud.r-project.org")
library(Hmisc)

if(!requireNamespace("dotwhisker"))
  install.packages("dotwhisker", repos = "https://cloud.r-project.org")
library(dotwhisker)

if(!requireNamespace("twang"))
  install.packages("twang", repos = "https://cloud.r-project.org")
library(twang)

if(!requireNamespace("survey"))
  install.packages("survey", repos = "https://cloud.r-project.org")
library(survey)
#dat = read.dta("../dataverse_files/mortality.dta")
#dat1 = read.csv("../dataverse_files/gavi.csv")
dat1 = read.dta("../dataverse_files/gavi_final.dta")
dat1 = dat1 %>% mutate(
                       ProgrammeCategory = as.factor(ProgrammeCategory),
                       cccode = as.factor(cccode),
                       ccode = as.factor(ccode),
                       region = as.factor(region), 
                       survey = as.factor(survey)) 

#give it a data dictionary
#var_desc = rep(0,length(dat1))
var_desc = c("","","","iss funding","NVS funding","?","?phase 1","?phase 2","?phase 3","?phase 4","?phase 5","?","How program is funded","received treatment","received dpt vacc","received hep b vacc", "received hib vacc","received iss funding for treat?", "received nvs funding for treat", "received pneumo vacc","received rota vaccine","?TxY1","?TxY2","?TxY3","?TxY4","?TxY5","?Y1","?Y2","?Y3","?Y4","?Y5","received gavi ever:0 or 100","?bcg","country code","?cmr_dhs","cost of dpt (trivalent vaccine) that year","cost of dipthepb that year", "cost of dpthib that year","cost of hepb that year", "cost of hib that year","cost of mcv that year", "cost of penta that year","cost of pneumo that year","cost of polio that year","cost of rota that year","amount dpthepb disbursed","amount dpthib disbursed","amount gavi disbursed","amount hepb disbursed","amount hib disbursed","amount iss disbursed","amount mcv disbursed","amount nvs disbursed","amount penta disbursed","amount pneumo disbursed","amount rota disbursed","dpt: 0-100","dpt_dhs","dpt doses","?dpt net","dpt net num?", "dpt num?", "dpt wasted according to coverage estimates","","","","","gov expenditure on immunization","gov expenditure on vaccination","gov expenditure TOTAL immunization ? larger number","gov expenditure TOTAL vaccination? larger number","percent of ?? vaccinated", "percent of children <=1yr vaccinated", " percent of children <=2 vaccinated", "factor: ever received gavi funding","ever received funding of gavi to date", "gross domestic product","gdp per capita","gross national income", "","","","","","","","","","percent coverage for hepb?", "hepb # doses", "hepb net?","hepb net num?","hepbnum", "hepb waste ...?", "hib","hib # doses?","hib net?", "hib net num?","hib num?", "hib waste ?", "ida?", "ida gross? ","?","infant mortality rate", "imr by dhs", "income level:L LM LM* UM", "factor: ever received iss funding","factor: ever received iss to date", "what the lag year is ( = year - 1)", "log of exp_govimm", "log of exp_govvax", "log of totimm", "log of totvaxx", "log of gdp", "log of gni PER CAPITA", "log of gni 98","log of gni 03","log of gni 09","log of gni 10","log of gni 11","log of gni 12","log per capita? exp_govimm","log per capita? exp_govaxx","log per capita? exp_totvimm","log per capita? exp_totvaxx", "log per capita gavi?", "log under 5 mortality rate", "%? malnourished according to dhs", "%? malnourished according to WHO", "mcv : measles", "measles dhs?","measles doses?","measles net?", "measles net num?", "measles num?", "measles waste?", "nvs funding: 1 or NA...", "?", "per capita exp govimm", "per cappita exp govax", "per capita totimm", "per capita totvax", "per capita gavi", "per capita iss", "per capita nvs", "pct govimm?", "pct govax?", "pentavalent vaccine", "penta doses?", "penta net?", "penta waste", "which phase of rollout is it? 0:5", "?", "pneumococcal meningitis","pneumo doses?", "pneumo net?","pneumo net num?", "pneumo num?", "pneumo waste?","polio","population size", "pop under 1: only NAs", "population under 5 size", "?", "factor for continent?", "rota?","rota doses?", "rota net?","rota net num?","rota num?","rota waste?","year hepb was rolled out?", "year hib rolled out?", "year mcv rolledout?","year pneumo rolled out?", "year polio rolled out", "year rota rolled out?","?", "year, then DHS or RHS or MIS?", "under 5 mortality rate", "u5mr according to dhs", "u5mr by WHO ceiling number", "u5m4 by WHO floor number", "% population (of kids?) wasted? WHO", "?","year dpt given", "year hepb given", "year hib given", "year mcv given", "year penta given", "year pneumo given", "year rota given")
var_type = rep(0,length(dat1))
linker = dataMeta::build_linker(dat1,variable_description = var_desc, variable_type = var_type)

#throwing error
dictionary <- dataMeta::build_dict(my.data = dat1, linker = linker, option_description = NULL,                        prompt_varopts = FALSE)


dat1 = dat1 %>% na_if("")

############### Work on some of the variables ################
#relevel the income variable
#4 ccodes missing - SWZ, CIV, MKD,PRK
#fill in missing data for Region
dat1$region[dat1$cccode == 'BEN'] = "Africa" #Benin
dat1$region[dat1$ccode == 'CPV'] = "Africa" #Cabo Verde
dat1$region[dat1$ccode == 'MKD'] = "Europe" #Macedomia
dat1$region[dat1$ccode == 'MMR'] = "Asia"#Myanmar
dat1$region[dat1$ccode == 'PRK'] = "Asia" #N. Korea
dat1$region[dat1$ccode == 'MNP'] = "Pacific Ocean" #N. Marina Islands
dat1$region[dat1$ccode == 'SUR'] = "South America" #Suriname
dat1$region[dat1$ccode == 'SWZ'] = "Europe" #Switzerland
dat1$region[dat1$ccode == 'XKX'] = "Europe" #Kosovo

dat1$inc[dat1$inc == "LM*"] = "LM"
dat1$inc[dat1$inc == ""]=NA

before = dat1 %>% filter(year<=2005)
before$treatment = ifelse(before$gavi_todate=="Received Gavi funding",1,0)
before$phase = ifelse(before$year<2000,0,1)
before = before %>% group_by(ccode) %>% mutate(treatment_a=ifelse(sum(treatment)>0,1,0)) %>% ungroup()

#missing = before %>% ff_glimpse()
#print(missing)

#income missing for some of the countries previous listed w/o region

############### Transform the Data ###########################
#transform the data so that there's one obs per country
gav_ave = before %>% group_by(ccode,phase) %>% summarise(ave_u5mr = mean(na.omit(u5mr)),
                                                 ave_imr = mean(na.omit(imr)),
                                                 ave_malnourished = mean(na.omit(malnourished_who)),
                                                 ave_wasted = mean(na.omit(wasted_who)),
                                                 ave_gdp = mean(gdp),
                                                 ave_gdppc= mean(gdppc),
                                                 ave_pop1 = mean(pop_1),
                                                 ave_pop5 = mean(pop_5),
                                                 ave_lpop1 = log(mean(pop_1)),
                                                 ave_lpop5 = log(mean(pop_5)),
                                                 ave_lgni_pc = mean(lgni),
                                                 ave_gni = mean(gni)
                                                
                                                 ) %>% ungroup()
                                                 
######### Pull out constants #########
########### Add back in categorical variables ##
gav_con = before %>% group_by(ccode) %>% filter(year==1999) %>%  select(inc,treatment) %>% distinct()
gav_ave = join(x=gav_ave, y=gav_con)

gav_inc = before %>% group_by(ccode) %>% filter(year==2005) %>% select(inc) %>% distinct()

gav_ave = join(x=gav_ave, y=gav_inc)
################# Outcome variables######
#inc, region
#E = eligibility
#ExY = eligibility * change in vacc and mortality rates
#Y = change in vacc and mortality rates
gav_out = before %>% group_by(ccode,phase) %>% select(E,ExY,Y) %>% distinct()

gav_ave = join(x=gav_ave,y=gav_out)
gav_ave = na.omit(gav_ave)#phase 0 and 1 of gavi

#only keep countries if there is a phase 1 and phase 0
gav_ave = gav_ave %>% group_by(ccode) %>% mutate(discard=n()) %>% ungroup()
gav_ave <- gav_ave[ which(gav_ave$discard==2),]

################### Break up Data into Before Phase 1 and Phase1
phase0 = gav_ave %>% filter(phase == 0)
phase1 = gav_ave %>% filter(phase==1)

phase1$Y_imr = phase1$ave_imr - phase0$ave_imr

