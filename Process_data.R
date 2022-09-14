###################################################################
# Input data

library(googledrive)
library(googlesheets4)
library(readxl)

# read in data from google drive; will be asked to authorise access to google drive
# pine block data
wtf <- drive_get("wtf_radiata_blocks_weight.csv") %>%
  read_sheet(sheet = "wtf_radiata_blocks_weight.csv", col_names=T, trim_ws = T, 
             na = c("","#N/A", "NA", "-", " ", NULL, "NULL"),
             col_types = c("ccccccnnnnnDDnnnnnnnnnnnncDncnnnnnncDnnnnnccllll"))

# read in excel sheet of native wood weights and wood density
wood_weights <- drive_get("Wood_weights_native") %>% 
  read_sheet(sheet = "wtf_native_harvest", col_names=T, trim_ws = T, 
             na = c("","#N/A", "NA", "-", " ", NULL, "NULL"),
             col_types = c("cnncccnDDLLLLLncLLnLncDLLLLLLLLLcLLLLLccLcLL")) # read in class for each column but will have to change as no option for factor and many columns are lists

wood_density <- drive_get("wood_density_native") %>%
  read_sheet(sheet = "Sheet1", col_names=T, trim_ws = T, 
             na = c("","#N/A", "NA", "-", " "))


# read in wood chemistry data
wood_chem<-drive_get("Zanne Australia CHN data .xlsx")

dl <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1AtvO-vYjOvgD35bPMFal6Bi9XmRfX4uf/edit#gid=556709393"),
  path = 'chem.xlsx', 
  overwrite = TRUE, 
  type = "xlsx")

cnames <- read_excel("chem.xlsx",
                     n_max = 0) %>% 
  names()

chem_data <- read_excel('chem.xlsx',
                        skip = 2, col_names = cnames,
                        trim_ws = T, .name_repair = "universal",
                        na = c("","#N/A", "NA", "-", " ", NULL, "NULL"))

chem_data<-chem_data %>% 
  dplyr::rename(Nitrogen_response = Nitrogen...6,
                Nitrogen_perc = Nitrogen...7,
                Carbon_response = Carbon...8,
                Carbon_perc = Carbon...9)
str(chem_data)


# read in wood lignin data
d2 <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/19MKwTMsPVJULJZh4GVmPPGo5uDroq0K4/edit#gid=1258460499"),
  path = 'lignin.xlsx', 
  overwrite = TRUE, 
  type = "xlsx")

lignin_data <- read_excel('lignin.xlsx', sheet = "WTF",
                          trim_ws = T, .name_repair = "universal", 
                          na = c("","#N/A", "NA", "-", " ", NULL, "NULL"))
str(lignin_data)


# read in wood pH data
d2 <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1b-T8zGE5B3yGudtE4-DgtXmK6QR00rvb/edit#gid=1104479735"),
  path = 'pH.xlsx', 
  overwrite = TRUE, 
  type = "xlsx")

pH_data <- read_excel('pH.xlsx',
                      trim_ws = T, .name_repair = "universal", 
                      na = c("","#N/A", "NA", "-", " ", NULL, "NULL"))
str(pH_data)


# read in wood ICP data
d2 <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1YZrp4BbpQDHZ10iqI2vPYqRUeco5OMVL/edit#gid=319770504"),
  path = 'ICP.xlsx', 
  overwrite = TRUE, 
  type = "xlsx")

ICP_data <- read_excel('ICP.xlsx',
                       trim_ws = T, .name_repair = "universal", 
                       na = c("","#N/A", "NA", "-", " ", NULL, "NULL"))
str(ICP_data)

# detach these packages as they interfere with dplyr working
detach(package:googledrive)
detach(package:googlesheets4)


###################################################################
# read in required packages
library(tidyr)
library(plyr)
library(dplyr)


###################################################################
# tidy pine blocks data set
wtf_out <- wtf %>%
  filter(!is.na(DW_Wood)) %>%
  select(-site, 
         -treatment_termite,
         -tag_number, 
         -Station) %>%
  rename(site = site_new,
         init_dry_wt = intial_weight_t0,
         SampleID = wood_block_number,
         harvest = harvest_number) %>%
  rowwise() %>%
  mutate(harvest_dry_wt = sum(c_across(c(DW_Wood,DW_Excess)), na.rm = TRUE)) %>%
  mutate(Species.Code = "PIRA",
         date_diff = as.numeric(harvest_date-deployment_date),
         pct.mass.rem = 100*(harvest_dry_wt)/init_dry_wt,
         pro.mass.loss = 1 - pct.mass.rem/100,
         site = replace(site, site=="Penyweight", "PNW"),
         Fire_Class = case_when(
           Fire_Class ==0~0,
           Fire_Class ==1~1,
           Fire_Class ==2~2,
           Fire_Class ==3~3,
           Fire_Class ==5~5,
           fire_present_2019==0.0~0,
           fire_present_2019==1.0~1,
           fire_present_2019==3.0~3,
           fire_present_2019==2.0~2,
           fire_present_2019==2.5~3,
           fire_present_2021==0~0,
           fire_present_2021==1~1,
           fire_present_2021==3~3
         ),
         termite.attack = case_when(
           termite_present == 1 ~ 1,
           termite_collected == T ~ 1,
           `termites_in/ex_situ` == 1 ~ 1, 
           grepl(pattern = "termite", x = Lab_notes) ~ 1, 
           grepl(pattern = "termite damage", x = notes) ~ 1,
           grepl(pattern = "lots of termite activity", x = notes) ~ 1,
           grepl(pattern = "termites!", x = notes) ~ 1,
           TRUE ~ 0),
         months = case_when(harvest == 1 ~ 6, harvest == 2 ~ 12,harvest == 3 ~ 18,
                            harvest == 4 ~ 24, harvest == 5 ~ 30, harvest == 6 ~ 36,
                            harvest == 7 ~ 42, harvest == 8 ~ 48),
         station = interaction(site,Species.Code,block))%>%
  select(site,SampleID,Species.Code,block,termite_treatment_abbreviation,harvest,deployment_date,harvest_date,
         Fire_Class,termite.attack,season_condition,months,date_diff,station,init_dry_wt,harvest_dry_wt,pro.mass.loss,pct.mass.rem) %>%
  filter(site %in% c("DRO","PNW")) %>%
  filter(months %in% c(12,18,24,30,36,42)) %>%
  filter(!(SampleID %in% c(48,256))) # remove burned blocks

# Write out tidy datasets to local directory
write.csv(wtf_out,"Pines_processed.csv", row.names=F, quote = F)


###################################################################
# tidy (native) wood_weights data set

# 1) first change NULL to NA
tr.list<-function(x){
  x[unlist(lapply(x, is.null))]<-NA
  return(x)
}

for (i in seq_along(wood_weights)) {
  wood_weights[[i]] <- tr.list(wood_weights[[i]])
}

# 2) standardise inputs for each column to account for erroneous inputs and coerce class 
fac.cols <- c("site", "species", "block", "termite_treatment_abbreviation", "insect_present", 
              "termite_present", "fungi_present", "soil_present", "root_present", "fire_present_2021", 
              "fire_present_2019", "Lab_As.", "damage_bag", 
              "termite_collected", "termites_in.ex_situ", "fungi_collected", "surface_sterilsed")

num.cols <- c("fresh_volume", "Field_fresh_wt", "FW_Wood","FW_Excess", "FW_Carton", "FW_Soil",
              "Sawdust_FW", "Post_drill_FW", "DW_Wood", "DW_Excess", "DW_Carton", "DW_Soil",
              "Termite_Class", "Fungi_Class", "Fire_Class")

wood_weights<-as.data.frame(lapply(wood_weights, unlist))%>%
  mutate(Date_removed=as.Date(Date_removed, format = "%d/%m/%Y"),
         termite_present = if_else(termite_present=="0", "0", "1"),# convert descriptions of termite present to a 1
         fungi_present = if_else(fungi_present=="0", "0", "1"), # convert descriptions of fungi present to a 1
         soil_present = if_else(soil_present=="0", "0", "1"), # convert descriptions of soil present to a 1
         root_present = if_else(root_present=="0", "0", "1"),# convert descriptions of root present to a 1
         damage_bag = if_else(damage_bag =="T","T", "F"), # convert so inputs are either T or F
         termite_collected = if_else(termite_collected =="T","T", "F"),# convert so inputs are either T or F
         termites_in.ex_situ = if_else(termites_in.ex_situ =="in situ","in situ", "ex situ"),# convert so inputs are either in situ or ex situ
         fungi_collected = if_else((fungi_collected =="0" |fungi_collected == "F"),"F", "T"),# convert so inputs are either T or F
         surface_sterilsed = if_else(surface_sterilsed =="0","F", "T"), # convert so inputs are either T or F
         Lab_As. = replace(Lab_As., Lab_As. == "2a", "2A"))%>% # change 2a to 2A
  mutate(across(all_of(num.cols), ~na_if(., NA_real_)))%>% # standardise all NA inputs
  mutate(across(all_of(num.cols), as.numeric)) %>%
  mutate(across(all_of(fac.cols), ~na_if(., NA_real_))) %>%
  mutate_at(fac.cols,as.factor)%>%
  mutate(across(all_of(c(1:44)), ~na_if(., NA_real_)))

str(wood_weights)
nrow(wood_weights)

# 3) get initial dry weight info from wood_density, combine, subset dataframes and calculate dry weights

# convert initial wet weights to dry weights data, and calculate mean density
int_wood <- wood_density %>%
  mutate(drywt = oven_dry_mass/green_weight) %>%  ## calculates the fraction of dry weight for each initial log
  group_by(species) %>%
  dplyr::summarise(wood_density = mean(wood_density_oven_dry, na.rm = TRUE),
                   init_drywt_fraction = mean(drywt, na.rm=TRUE)) %>%
  dplyr::rename(Species.Code = species)%>%
  print(n=Inf)

###################################################################
# tidy wood chem_data (C and N) to join to df of wood weights
# add Species.Code to chem_data frame
# calculate mean N and C percentage for each species

chem_data<-chem_data%>%
  slice(1:32)%>%
  mutate(AustralianSampleCodes...4 = replace(AustralianSampleCodes...4, AustralianSampleCodes...4 == "F1.1", "P1.1"))%>% # typo changed
  mutate(Species.Code = case_when(AustralianSampleCodes...4 == "A3.8" | AustralianSampleCodes...4 == "A2.7" ~ "DYPA",
                                  AustralianSampleCodes...4 == "B2.1" | AustralianSampleCodes...4 == "B1.10" ~ "MYGL",
                                  AustralianSampleCodes...4 == "C1.6" | AustralianSampleCodes...4 == "C3.12" ~ "CLOB",
                                  AustralianSampleCodes...4 == "D2.10" | AustralianSampleCodes...4 == "D1.12" ~ "CASU",
                                  AustralianSampleCodes...4 == "E1.43" ~ "NONO",
                                  AustralianSampleCodes...4 == "F2.15" | AustralianSampleCodes...4 == "F1.10" ~ "ALSC",
                                  AustralianSampleCodes...4 == "G2.10" | AustralianSampleCodes...4 == "G1.15" ~ "CAAU",
                                  AustralianSampleCodes...4 == "H1.22" | AustralianSampleCodes...4 == "H2.45" ~ "SYSA",
                                  AustralianSampleCodes...4 == "I1.11" | AustralianSampleCodes...4 == "I2.20" ~ "ARPE",
                                  AustralianSampleCodes...4 == "J6.11" | AustralianSampleCodes...4 == "J4.13" ~ "TEAR",
                                  AustralianSampleCodes...4 == "K1.2" | AustralianSampleCodes...4 == "K6.8" ~ "EUCU",
                                  AustralianSampleCodes...4 == "L7.9" | AustralianSampleCodes...4 == "L2.11" ~ "EULE",
                                  AustralianSampleCodes...4 == "M5.9" | AustralianSampleCodes...4 == "M1.6" ~ "MEST",
                                  AustralianSampleCodes...4 == "N4.7" | AustralianSampleCodes...4 == "N1.11" ~ "MEVI",
                                  AustralianSampleCodes...4 == "O6.5" | AustralianSampleCodes...4 == "O3.8" ~ "PEBA",
                                  AustralianSampleCodes...4 == "P1.1" | AustralianSampleCodes...4 == "P2.23" ~ "ROAN"))%>%
  group_by(Species.Code)%>%
  dplyr::summarise(mean.C.perc = mean(Carbon_perc),
                   mean.N.perc = mean(Nitrogen_perc))

chem_data 

###################################################################
# tidy wood lignin data to join to df of wood weights
# add Species.Code to lignin_data frame
# calculate mean S/G ratio each species

names(lignin_data)

lignin_data<-lignin_data%>%
  slice(1:32)%>%
  mutate(Species.Code = case_when(SAMPLE == "A3.8" | SAMPLE == "A2.7" ~ "DYPA",
                                  SAMPLE == "B2.1" | SAMPLE == "B1.10" ~ "MYGL",
                                  SAMPLE == "C1.6" | SAMPLE == "C3.12" ~ "CLOB",
                                  SAMPLE == "D2.10" | SAMPLE == "D1.12" ~ "CASU",
                                  SAMPLE == "E1.43" ~ "NONO",
                                  SAMPLE == "F2.15" | SAMPLE == "F1.10" ~ "ALSC",
                                  SAMPLE == "G2.10" | SAMPLE == "G1-15" ~ "CAAU",
                                  SAMPLE == "H1-22" | SAMPLE == "H2.45" ~ "SYSA",
                                  SAMPLE == "I1.11" | SAMPLE == "I2.20" ~ "ARPE",
                                  SAMPLE == "J6.11" | SAMPLE == "J4.13" ~ "TEAR",
                                  SAMPLE == "K1.2" | SAMPLE == "K6.8" ~ "EUCU",
                                  SAMPLE == "L7.9" | SAMPLE == "L2.11" ~ "EULE",
                                  SAMPLE == "M5.9" | SAMPLE == "M1.6" ~ "MEST",
                                  SAMPLE == "N4.7" | SAMPLE == "N1.11" ~ "MEVI",
                                  SAMPLE == "O6.5" | SAMPLE == "O3.8" ~ "PEBA",
                                  SAMPLE == "P1.1" | SAMPLE == "P2.23" ~ "ROAN"))%>%
  group_by(Species.Code)%>%
  dplyr::summarise(mean.S.G = mean(S.G, na.rm=T))

lignin_data

###################################################################
# tidy wood pH data to join to df of wood weights
# add Species.Code to pH data
# calculate mean pH ratio each species

names(pH_data)

pH_data<-pH_data%>%
  slice(1:32)%>%
  mutate(Species.Code = case_when(Sample.ID == "W1" | Sample.ID == "W26" ~ "CASU",
                                  Sample.ID == "W2" | Sample.ID == "W16" | Sample.ID == "W31" ~ "ROAN",
                                  Sample.ID == "W3" | Sample.ID == "W13" ~ "CLOB",
                                  Sample.ID == "W4" ~ "NONO",
                                  Sample.ID == "W5" | Sample.ID == "W18" ~ "EULE",
                                  Sample.ID == "W6" | Sample.ID == "W22" ~ "ARPE",
                                  Sample.ID == "W7" | Sample.ID == "W20" ~ "SYSA",
                                  Sample.ID == "W8" | Sample.ID == "W23" ~ "ALSC",
                                  Sample.ID == "W9" | Sample.ID == "W12" ~ "TEAR",
                                  Sample.ID == "W10" | Sample.ID == "W30" ~ "PEBA",
                                  Sample.ID == "W11" | Sample.ID == "W32" ~ "EUCU",
                                  Sample.ID == "W14" | Sample.ID == "W19" ~ "MEST",
                                  Sample.ID == "W15" | Sample.ID == "W29" ~ "MEVI",
                                  Sample.ID == "W17" | Sample.ID == "W28" ~ "DYPA",
                                  Sample.ID == "W21" | Sample.ID == "W24" ~ "MYGL",
                                  Sample.ID == "W25" | Sample.ID == "W27" ~ "CAAU"))%>%
  group_by(Species.Code)%>%
  dplyr::summarise(mean.pH = mean(pH, na.rm=T))

pH_data

###################################################################
# tidy wood ICP data: add Species.Code and calculate means
# assuming sample IDs are the same as for pH

names(ICP_data)

ICP_data<-ICP_data%>%
  rename(Sample.ID = Sample.ID...2)%>%
  slice(1:32)%>%
  mutate(Species.Code = case_when(Sample.ID == "W1" | Sample.ID == "W26" ~ "CASU",
                                  Sample.ID == "W2" | Sample.ID == "W16" | Sample.ID == "W31" ~ "ROAN",
                                  Sample.ID == "W3" | Sample.ID == "W13" ~ "CLOB",
                                  Sample.ID == "W4" ~ "NONO",
                                  Sample.ID == "W5" | Sample.ID == "W18" ~ "EULE",
                                  Sample.ID == "W6" | Sample.ID == "W22" ~ "ARPE",
                                  Sample.ID == "W7" | Sample.ID == "W20" ~ "SYSA",
                                  Sample.ID == "W8" | Sample.ID == "W23" ~ "ALSC",
                                  Sample.ID == "W9" | Sample.ID == "W12" ~ "TEAR",
                                  Sample.ID == "W10" | Sample.ID == "W30" ~ "PEBA",
                                  Sample.ID == "W11" | Sample.ID == "W32" ~ "EUCU",
                                  Sample.ID == "W14" | Sample.ID == "W19" ~ "MEST",
                                  Sample.ID == "W15" | Sample.ID == "W29" ~ "MEVI",
                                  Sample.ID == "W17" | Sample.ID == "W28" ~ "DYPA",
                                  Sample.ID == "W21" | Sample.ID == "W24" ~ "MYGL",
                                  Sample.ID == "W25" | Sample.ID == "W27" ~ "CAAU"))%>%
  group_by(Species.Code)%>%
  dplyr::summarise(mean.Ca.perc = mean(Ca.conc....),
                   mean.K.perc = mean(K.conc....),
                   mean.Mg.perc = mean(Mg.conc....),
                   mean.P.perc = mean(P.conc....),
                   mean.Mn.perc = mean(Mn.conc....),
                   mean.Al.perc = mean(Al.conc....))

ICP_data


###################################################################
# Create table of wood traits by species

wood_traits <- int_wood%>%
  left_join(chem_data)%>%
  left_join(lignin_data)%>%
  left_join(pH_data)%>%
  left_join(ICP_data)


###################################################################
# subset weights data set by removing unnecessary columns (e.g.notes) and,
# remove or correct sample errors and join with wood density

df<-subset(wood_weights, select = -c(21:23, 32:33, 39:44))%>%
  dplyr::rename(SampleID=Label,
                Species.Code = species) %>% # rename for consistency and merging
  filter(!is.na(SampleID)) %>% # remove empty rows
  filter(!(SampleID%in% c(447, # harvest date uncertain
                          531, # gained too much mass to be realistic; likely data error
                          654, # H2 not H3 but no recordings of mass
                          669, # Harvested at wrong time; actual date uncertain
                          699, # Uncertainty about sample ID and actual harvest time
                          832, # gained too much mass and weight discrepancies suggest data or drying errors
                          925, # gained too much mass and weight discrepancies suggest data or drying errors
                          970  # missing stem; no data
  )))%>% # remove samples with errors in data entry that can't be amended (n=8)
  filter(!(Fire_Class %in% c(3, 4, 5)))%>% # remove fire-affected stems (n=3)
  mutate(FW_Wood = if_else(SampleID==479, Sawdust_FW+Post_drill_FW, FW_Wood)) %>% # FW_Wood too low; sum Sawdust_FW and Post_drill_FW
  mutate(Post_drill_FW= if_else(SampleID==559, FW_Wood-Sawdust_FW, Post_drill_FW)) %>% # Post_drill_FW; using difference of Sawdust_FW and FW_Wood
  mutate(Post_drill_FW = if_else(SampleID==918, FW_Wood-Sawdust_FW, Post_drill_FW)) %>% # Post_drill_FW incorrect; using difference of Sawdust_FW and FW_Wood
  mutate(Post_drill_FW = if_else(SampleID==957, Sawdust_FW, Post_drill_FW)) %>% # Post_drill_FW was recorded as sawdust; replacing
  mutate(Sawdust_FW = if_else(SampleID==957, FW_Wood-Post_drill_FW, Sawdust_FW)) %>% # compute sawdust by difference with FW_Wood
  left_join(int_wood) %>% # join with mean initial dry weight fraction
  select(-wood_density) %>% # avoid confusion with species level analysis (this is not on individual stems)
  mutate(termite.attack = if_else(termite_present=="0",0,1)) %>% # add label for termite attack
  mutate(termite.attack = if_else(is.na(termite.attack), 0, termite.attack))%>% # convert NA values to 0 for termite attack
  mutate(termite.attack = case_when(Termite_Class > 0 ~1, TRUE ~termite.attack))%>% # include any blocks that recorded a termite decay class as attack 1
  mutate(FW_Excess = ifelse(is.na(FW_Excess), 0, FW_Excess),#change these NA to 0 in order for ifelse to work below
         Sawdust_FW = ifelse(is.na(Sawdust_FW), 0, Sawdust_FW)) %>%
  mutate(FW_Wood = if_else( Species.Code %in% c("CAAU", "NONO", "ROAN") & harvest == 3, 
                            Post_drill_FW + FW_Excess + Sawdust_FW, FW_Wood)) %>% # replace missing FW_wood for H3 of fast sp with a proxy
  mutate(FW_Wood = if_else( Species.Code %in% c("CAAU", "NONO", "ROAN") & harvest == 4, 
                            Post_drill_FW + FW_Excess + Sawdust_FW, FW_Wood)) %>% # replace missing FW_wood for H4 of fast sp with a proxy
  mutate(season_condition = case_when(harvest == 1 ~ "WET", harvest == 2 ~ "DRY",
                                      harvest == 3 ~ "WET", harvest == 4 ~ "DRY")) %>%
  mutate(harvest = if_else( Species.Code %in% c("ALSC", "MYGL", "CASU", "DYPA", "SYSA", # change harvest 3 to harvest 5 for slow decaying species as it is after harvest 4 in terms of date
                                                "CLOB", "MEVI", "ARPE", "MEST", "PEBA", "EULE",
                                                "TEAR", "EUCU") & harvest == 3, 5, harvest))%>%
  mutate(harvest = if_else( Species.Code %in% c("ALSC", "MYGL", "CASU", "DYPA", "SYSA", # change harvest 4 to harvest 6 for slow decaying species as it is after harvest 4 in terms of date
                                                "CLOB", "MEVI", "ARPE", "MEST", "PEBA", "EULE",
                                                "TEAR", "EUCU") & harvest == 4, 6, harvest))%>%
  mutate(months = case_when(harvest == 1 ~ 12, harvest == 2 ~ 18,harvest == 3 ~ 24, 
                            harvest == 4 ~ 30, harvest == 5 ~ 36, harvest == 6 ~ 42)) %>% # add in months since deployment
  mutate(date_diff = as.numeric(harvest_date - deployment_date))%>%
  mutate(station = interaction(site,Species.Code,block))%>%
  mutate(dry.wet = DW_Wood/Post_drill_FW) %>% # fraction of dry weight for harvested sample
  mutate(init_dry_wt = init_green_wt*init_drywt_fraction) %>%  # estimate of initial dry weight using mean dry wt fraction
  mutate(harvest_dry_wt = FW_Wood*dry.wet) %>%
  mutate(harvest_dry_wt = replace(harvest_dry_wt, SampleID == 573, 0), # sticking with zero rather than half the minimum
         harvest_dry_wt = replace(harvest_dry_wt, SampleID == 575, 0),
         harvest_dry_wt = replace(harvest_dry_wt, SampleID == 653, 0),
         harvest_dry_wt = replace(harvest_dry_wt, SampleID == 543, 0),
         harvest_dry_wt = replace(harvest_dry_wt, SampleID == 753, 0))%>% # Only bark or carton left so put harvested weight as 0
  mutate(mass.loss = init_dry_wt - harvest_dry_wt) %>% # mass loss as initial minus final, with final based on the fraction of dry weight in the total fresh weight at harvest (pre-drill)
  mutate(pro.mass.loss = mass.loss/init_dry_wt) %>% #convert to proportion
  mutate(pro.mass.loss = if_else(pro.mass.loss>0,pro.mass.loss,0))%>% # convert samples that gained mass to zero (n = 17)
  mutate(pct.mass.rem = 100-100*pro.mass.loss)%>%
  mutate(harvest = as.factor(harvest),
         season_condition = as.factor(season_condition))

# Write out tidy datasets to local directory
write.csv(df,"Natives_processed.csv", row.names=F, quote = F)
write.csv(wood_traits,"Wood_traits.csv", row.names = F, quote = F)

