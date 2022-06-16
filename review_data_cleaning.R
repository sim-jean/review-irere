######################################################################################################################
############## REVIEW : DATA COLLECTION AND CLEANING #################################################################
######################################################################################################################
setwd("/Users/simonjean/Desktop/PhD/Projects/Review_Lauriane/Code")
rm(list=ls())

### 0. Package set-up ####
required_packages  <- c("readxl", "dplyr", "magrittr", "tidyr", "ggthemes","ggplot2", 
                        "plotrix","ggpubr","writexl","stargazer","udpipe","mltools","data.table","DescTools","extrafont")
new_packages       <- required_packages[!(required_packages %in% installed.packages()[,"Package"]) ]

install.packages(new_packages,  repos = "http://cran.us.r-project.org")
invisible(lapply(required_packages, library, character.only = TRUE))
##################################################################################################################
### I. Data cleaning
##################################################################################################################
####### A. Import #####
df2  <- read_excel("papers_database_v2.xlsx", skip = 1)
df2b <- read_excel("papers_database_v2b.xlsx", skip=1)
####### B. Formating ####
# Rename variables for ease of computation #
df2 <-df2 %>% dplyr::rename(paper_country                = `Paper country`,
                            paper_zone                   = `Paper zone`,
                            field                        =`Dominant field`,
                            bio_monet                    =`biodiversity is directly monetized`, 
                            main_method                  = `Cost benefit analysis or cost efficiency`,
                            biodiv_measure               = `How is biodiversity measured`, # directly monetized in further versions
                            biodiv_functional_historical = `Is the measure genetic, historic or functional?`,
                            biodiv_indicator_level       = `Level of the indicator : population, species, ecosystem, genetic)`, 
                            biodiv_indicator_use         = `What is it used for? Talk about the functions of the system, its stability, its evolution?`,
                            biodiv_type                  = `What type of biodiversity : birds, bugs…`,
                            biodiv_objective             = `Biodiversity as an objective function`, 
                            biodiv_direct                = `Directly :`,
                            biodiv_proxy                 = `With a proxy : a habitat, a prairie as a proxy for the number of species`,
                            ess_binary                   = `Ecosystem Services : 1 if theme is yes, 0 if no`,
                            ess_measure                  = `How is ES measured : directly(1);       With a proxy population variable (size of bees population) (2); with habitat (3)`,
                            ess_type                     = `What ES : provision (1), regulation (2) or cultural (3)?`,
                            paper_empirical              = `Empirical paper (1=Yes,0)`,
                            method_static                = `Static (1=Yes, 0=No), not applicable 100`,
                            method_resolution            = `Resolution method : closed form (0) vs numerical (1) vs not applicable (100)`,
                            method_spatial               = `Spatial : explicit, with dispersal (0); Implicit - mention of potential heterogeneity (1);Absent (2)`,
                            positive_normative           = `Positive or normative analysis (1,2), not applicable 100`,
                            comments                     = `Any comment`,
                            keywords                     = `Keywords for bibliographical research`,
                            keywords2                    = `Keywords for bibliographical research_2`,
                            species_2                    = `Single or multiple species`,
                            ecological_model             = `Type of ecological model`, 
                            stoch_ecol                   = `Stochasticity? Ecological`,
                            partial_general              = `Partial or general equilibrium`,
                            affiliation_class            = `Affiliation Class`)

df2$habitat2    <- recode(df2$habitat2,
                                'Multiple : agriculture and wilderness'        = "Multiple",
                                'Multiple : managed forest, agriculture, park' = "Multiple",
                                'Multiple : urban, agriculture, wilderness'    = "Multiple",
                                "Multiple : forest and agriculture"            = "Multiple",
                                'Multiple : reserve and agriculture'           = "Multiple",
                                'Multiple : wilderness and agriculture'        = "Multiple",
                                'Multiple : agriculture and reserve'           = "Multiple",
                                'Multiple : forest and reserve'                = "Multiple",
                                'Multiple : urban, agricultural and reserve'   = "Multiple",
                                'Multiple : wilderness and reserve'            = "Multiple")

df2b <-df2b %>% dplyr::rename(paper_country                = `Paper country`,
                               paper_zone                   = `Paper zone`,
                               field                        = `Dominant field`,
                               bio_monet                    = `biodiversity is directly monetized`, 
                               main_method                  = `Cost benefit analysis or cost efficiency`,
                               biodiv_measure               = `How is biodiversity measured`, # directly monetized in further versions
                               biodiv_functional_historical = `Is the measure genetic, historic or functional?`,
                               biodiv_indicator_level       = `Level of the indicator : population, species, ecosystem, genetic)`, 
                               biodiv_indicator_use         = `What is it used for? Talk about the functions of the system, its stability, its evolution?`,
                               biodiv_type                  = `What type of biodiversity : birds, bugs…`,
                               biodiv_objective             = `Biodiversity as an objective function`, 
                               biodiv_direct                = `Directly :`,
                               biodiv_proxy                 = `With a proxy : a habitat, a prairie as a proxy for the number of species`,
                               ess_binary                   = `Ecosystem Services : 1 if theme is yes, 0 if no`,
                               ess_measure                  = `How is ES measured : directly(1);       With a proxy population variable (size of bees population) (2); with habitat (3)`,
                               ess_type                     = `What ES : provision (1), regulation (2) or cultural (3)?`,
                               paper_empirical              = `Empirical paper (1=Yes,0)`,
                               method_static                = `Static (1=Yes, 0=No), not applicable 100`,
                               method_resolution            = `Resolution method : closed form (0) vs numerical (1) vs not applicable (100)`,
                               method_spatial               = `Spatial : explicit, with dispersal (0); Implicit - mention of potential heterogeneity (1);Absent (2)`,
                               positive_normative           = `Positive or normative analysis (1,2), not applicable 100`,
                               comments                     = `Any comment`,
                               keywords                     = `Keywords for bibliographical research`,
                               keywords2                    = `Keywords for bibliographical research_2`,
                               species_2                    = `Single or multiple species`,
                               ecological_model             = `Type of ecological model`, 
                               stoch_ecol                   = `Stochasticity? Ecological`,
                               partial_general              = `Partial or general equilibrium`,
                               affiliation_class            = `Affiliation Class`)

df2b$habitat2    <- dplyr::recode(df2b$habitat2,
                                  'Multiple : agriculture and wilderness'        = "Multiple",
                                  'Multiple : managed forest, agriculture, park' = "Multiple",
                                  'Multiple : urban, agriculture, wilderness'    = "Multiple",
                                  "Multiple : forest and agriculture"            = "Multiple",
                                  'Multiple : reserve and agriculture'           = "Multiple",
                                  'Multiple : wilderness and agriculture'        = "Multiple",
                                  'Multiple : agriculture and reserve'           = "Multiple",
                                  'Multiple : forest and reserve'                = "Multiple",
                                  'Multiple : urban, agricultural and reserve'   = "Multiple",
                                  'Multiple : wilderness and reserve'            = "Multiple")
  ####### C- Tidy data ####
## Single row for each paper ####
out                 <- duplicated(df2$TItle)
df2_ready           <- df2[!out,]
df2_ready           <- df2_ready[-nrow(df2_ready),]

out                 <- duplicated(df2b$TItle)
df2b_ready          <- df2b[!out,]

df2_ready           <- rbind(df2_ready, df2b_ready)

unique(df2_ready$keywords2)

## Biodiversity data separation - may need a 3rd one ####

biodiversity_aggregate       <- strsplit(df2_ready$biodiv_type, " & ")
biodiversity_ag              <- data.frame(0,0)
colnames(biodiversity_ag)    <- c("biodiv_type_large","biodiv_type_precise")

# Put everything in lower case
for(i in 1:length(biodiversity_aggregate)){
  biodiversity_ag[i,1]       <- tolower(biodiversity_aggregate[[i]][1])
  biodiversity_ag[i,2]       <- tolower(biodiversity_aggregate[[i]][2])
}

biodiv_1                     <- data.frame(0,0,0,0)
biodiv_1_1                   <- strsplit(biodiversity_ag$biodiv_type_large, " - ")
for(i in 1:length(biodiv_1_1)){
  biodiv_1[i,1]              <- biodiv_1_1[[i]][1]
  biodiv_1[i,2]              <- biodiv_1_1[[i]][2]
  biodiv_1[i,3]              <- biodiv_1_1[[i]][3]
  biodiv_1[i,4]              <- biodiv_1_1[[i]][4]
}
colnames(biodiv_1)           <- c("kingdom", "class", "order","subgroup_species")
biodiv_1                     <- biodiv_1 %>% mutate_if(is.character, ~replace(., is.na(.), "not known"))

biodiv_2                     <- data.frame(0,0,0,0)
biodiv_2_2                   <- strsplit(biodiversity_ag$biodiv_type_precise, " - ")
for(i in 1:length(biodiv_1_1)){
  biodiv_2[i,1]              <- biodiv_2_2[[i]][1]
  biodiv_2[i,2]              <- biodiv_2_2[[i]][2]
  biodiv_2[i,3]              <- biodiv_2_2[[i]][3]
  biodiv_2[i,4]              <- biodiv_2_2[[i]][4]
}
colnames(biodiv_2)           <- c("kingdom2", "class2", "order2", "subgroup_species2")
biodiv_2                     <- biodiv_2 %>% mutate_if(is.character, ~replace(., is.na(.), "not applicable"))

biodiversity_ag              <- cbind(biodiv_1, biodiv_2)

df2_ready                    <- cbind(df2_ready,biodiversity_ag)

rm(biodiv_1_1, biodiv_2_2, biodiv_1, biodiv_2)

df2_ready$affiliation_class  <- recode(df2_ready$affiliation_class, 
                                       "Ecology and Economics" = "Ecology & Economics")


## Data storage #####

write_xlsx(df2_ready, path="/Users/simonjean/Desktop/PhD/Projects/Review_Lauriane/Code/data_cleared.xlsx")

