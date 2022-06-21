##################################################################################################################
######### CODE FOR THE REVIEW ON BIODIVERSITY IN ECOLOGICAL-ECONOMIC MODELS, L. MOUYSSET    ######################
##################################################################################################################

####### What is computed in this file ? ###########
# 1 - Table of methodological characteristics by pre-specified group and whole database
# 2 - Data for distribution over time and graph of distribution over time
# 3 - Graph of distribution of database over zones, data for groups over zones
# 4 - Graph of distribution of articles over time
# 5 - Table of journals (in extenso and by class) by pre-specified group and whole database
# 6 - Analysis of authorship by pre-specified group and whole database
# 7 - Analysis of biodiversity distribution across groups
###################################################
setwd("/Users/simonjean/Desktop/Thèse/Projects/Review_Lauriane/data")

rm(list=ls())
source("review_data_cleaning.R")
##################################################################################################################
### I. Analysis #################################################################################################
colors_review <- c("Extinction"="firebrick","Conservation"="gray66","Ecosystem"="dodgerblue1", "Full"="red")
colors_kmodes <- c('1'="darkorchid3", "2"="forestgreen", "3"="gold", "4"="black")
##################################################################################################################
####### A. Define methodological variables ##### 
##### ou alors rajouter une petite chose sur la biodiv qui est plus présente selon les groupes? A faire après, une fois que la variable biodiv est belle et propre
method_var          <-c("bio_monet", "main_method", "biodiv_objective", "biodiv_direct", "biodiv_proxy", "biodiv_indicator_level" ,"ess_binary",
              "paper_empirical", "method_static", "method_resolution", "positive_normative", "species", "spatial_ecology", "spatial_economy", 
              "ecological_model", "stoch_ecol", "stoch_econ", "biodiv_functional_historical", "paper_empirical", "partial_general", 
              "kingdom", "kingdom2", "class", "class2", "order", "order2")



####### C. Do the computations for the share of each variable value for each pre-specified subgroup ####
for(i in method_var){ 
  # Pick variable
  df_result<-data.frame(0)
  # Initiate data frame for data storing
  for(j in unique(df2_ready$keywords)){
    # Pick subgroup
    nms                 <- unique(df2_ready[,which(colnames(df2_ready)==i)]) # Il y avait un pull ici
    # get unique values for the variable
    df_loop             <- df2_ready %>% subset(keywords==j)
    # Subset the dataframe for group
    df_loop             <- df_loop %>% dplyr::select(c(i))
    df_loop             <- pull(df_loop)
    # Pull the variable and put it at relevant format for analysis
    share               <- c(rep(0, times=length(nms)))
    # Initiate vector for shares
      for(k in 1:length(nms)){
        # Pick variable value
        share[k]        <- sum(df_loop==nms[k])/length(df_loop)
        # Compute and store share of value
      }
    df_result           <- cbind(df_result, share)
    # Store results
  }
  df_result[,1]         <- nms
  # Set first column to variable values
  colnames(df_result)   <- c('value',unique(df2_ready$keywords) )
  # Set column names to value and each subgroup
  assign(paste("df_result", i, sep="_"), df_result)
  # Assign the name of the variable under study to the dataframe storing results
}
final_result      <-rbind(c("Monetized?", NA, NA, NA),
                    df_result_bio_monet, 
                    c("Directly measured?", NA, NA, NA),
                    df_result_biodiv_direct,
                    c("Biodiversity measured with a proxy", NA, NA, NA),
                    df_result_biodiv_proxy,
                    c("Biodiv functional, genetic, historical", NA, NA, NA), 
                    df_result_biodiv_functional_historical,
                    c("Biodiversity as an objective?", NA, NA, NA),
                    df_result_biodiv_objective,
                    c("Biodiv level", NA, NA, NA),
                    df_result_biodiv_indicator_level,
                    c("CBA or Cost effective", NA, NA, NA),
                    df_result_main_method,
                    c("Spatial ecology", NA, NA, NA),
                    df_result_spatial_ecology, 
                    c("Spatial economy", NA, NA, NA), 
                    df_result_spatial_economy,
                    c("Static or dynamic",  NA, NA, NA),
                    df_result_method_static,
                    c("Stochasticity : ecological model", NA, NA, NA), 
                    df_result_stoch_ecol,
                    c("Stochasticity : economic model", NA, NA, NA),
                    df_result_stoch_econ,
                    c("Resolution method", NA, NA, NA),
                    df_result_method_resolution,
                    c("Positive or normative",  NA, NA, NA),
                    df_result_positive_normative,
                    c("Ecological model", NA, NA, NA), 
                    df_result_ecological_model,
                    c("species",NA, NA, NA),
                    df_result_species, 
                    c("theoretical", NA, NA, NA),
                    df_result_paper_empirical,
                    c("general", NA, NA, NA),
                    df_result_partial_general, 
                    c("kingdom1", NA, NA, NA),
                    df_result_kingdom,
                    c("order1", NA, NA, NA), 
                    df_result_order,
                    c("class1", NA, NA, NA), 
                    df_result_class,
                    c("kingdom2", NA, NA, NA), 
                    df_result_kingdom2,
                    c("order2", NA, NA, NA),
                    df_result_order2,
                    c("class2", NA, NA, NA),
                    df_result_class2)
                    
                    # Consolidate results in a single dataframe
rm(i)

## Same for the whole database
for(i in method_var){
  df_result<-data.frame(0)
  nms                  <- unique(df2_ready[,which(colnames(df2_ready)==i)])
  df_loop              <- df2_ready %>% dplyr::select(c(i))
  df_loop              <- pull(df_loop)
  # Pull the variable and put it at relevant format for analysis
  share                <- c(rep(0, times=length(nms)))
  # Initiate vector for shares
  for(k in 1:length(nms)){
    # Pick variable value
    share[k]           <- sum(df_loop==nms[k])/length(df_loop)
    # Compute and store share of value
  }
  df_result            <- cbind(df_result, share)
  # Store results
df_result[,1]          <- nms
# Set first column to variable values
colnames(df_result) <-c('value','share')
# Set column names to value and each subgroup
assign(paste("df_result", i, sep="_"), df_result)
# Assign the name of the variable under study to the dataframe storing results
}
final_result_full <- rbind(c("Monetized?", NA, NA, NA),
                           df_result_bio_monet, 
                           c("Directly measured?", NA, NA, NA),
                           df_result_biodiv_direct,
                           c("Biodiversity measured with a proxy", NA, NA, NA),
                           df_result_biodiv_proxy,
                           c("Biodiv functional, genetic, historical", NA, NA, NA), 
                           df_result_biodiv_functional_historical,
                           c("Biodiv level", NA, NA, NA),
                           df_result_biodiv_indicator_level,
                           c("Biodiversity as an objective?", NA, NA, NA),
                           df_result_biodiv_objective,
                           c("CBA or Cost effective", NA, NA, NA),
                           df_result_main_method,
                           c("Spatial ecology", NA, NA, NA),
                           df_result_spatial_ecology, 
                           c("Spatial economy", NA, NA, NA), 
                           df_result_spatial_economy,
                           c("Static or dynamic",  NA, NA, NA),
                           df_result_method_static,
                           c("Stochasticity : ecol", NA, NA, NA), 
                           df_result_stoch_ecol,
                           c('Stochasticity : econ', NA, NA, NA),
                           df_result_stoch_econ,
                           c("Resolution method", NA, NA, NA),
                           df_result_method_resolution,
                           c("Positive or normative",  NA, NA, NA),
                           df_result_positive_normative,
                           c("Ecological model", NA, NA, NA), 
                           df_result_ecological_model,
                           c("species",NA, NA, NA),
                           df_result_species, 
                           c("theoretical", NA, NA, NA),
                           df_result_paper_empirical,
                           c("general", NA, NA, NA),
                           df_result_partial_general,                    
                           c("kingdom1", NA, NA, NA),
                           df_result_kingdom,
                           c("order1", NA, NA, NA), 
                           df_result_order,
                           c("class1", NA, NA, NA), 
                           df_result_class,
                           c("kingdom2", NA, NA, NA), 
                           df_result_kingdom2,
                           c("order2", NA, NA, NA),
                           df_result_order2,
                           c("class2", NA, NA, NA),
                           df_result_class2)

# See which trait is dominant at different cutoffs

for(i in 1:nrow(final_result_full)){
  if(is.na(final_result_full[i,2])){
    final_result_full[i,3]   <- NA
  }
  else if(final_result_full[i,2]>=0.6){
    final_result_full[i,3]   <- "Dominant"
  }
  else{
    final_result_full[i,3]   <- "No"
  }
}

for(i in 1:nrow(final_result_full)){
  if(is.na(final_result_full[i,2])){
    final_result_full[i,4]   <- NA
  }
  else if(final_result_full[i,2]>=0.65){
    final_result_full[i,4]   <- "Dominant"
  }
  else{
    final_result_full[i,4]   <- "No"
  }
}

for(i in 1:nrow(final_result_full)){
  if(is.na(final_result_full[i,2])){
    final_result_full[i,5] <- NA
  }
  else if(final_result_full[i,2]>=0.7){
    final_result_full[i,5]   <- "Dominant"
  }
  else{
    final_result_full[i,5]   <- "No"
  }
}

colnames(final_result_full)  <- c("value", "share","60%_cutoff","65%_cutoff","70%_cutoff")

# Remove intermediary results datasets
present                              <- ls()
present                              <- present[!(present %in% c("final_result", "final_result_full","final_result_ecosys2"))]
to_remove                            <- present[grep("df_result", present)]
remove(list=to_remove)

####### D. Export results to excel file #####

write_xlsx(final_result, path="/Users/simonjean/Desktop/Thèse/Projects/Review_Lauriane/data/final_result.xlsx")

####### E. Data for the distribution over time #####
distrib_time                         <- data.frame()
distrib_time[1,1]                    <- 'Year'
distrib_time[1,2]                    <- 'Full'
distrib_time[1,3]                    <- 'Extinction'
distrib_time[1,4]                    <- 'Conservation'
distrib_time[1,5]                    <- "Ecosystem"
#distrib_time[1,6]                    <- "Invasive_species_infectious_disease"

extinction                           <- df2_ready%>%subset(keywords=="Extinction")
conservation                         <- df2_ready%>% subset(keywords=="Conservation")
ecosystem                            <- df2_ready%>% subset(keywords=="Ecosystem")
#invasive                             <- df2_ready%>% subset(keywords=="Invasive_species_infectious_disease")


for(i in 0:47){
  distrib_time[i+2,2]                <- sum(df2_ready$Year==i+1973)
  distrib_time[i+2,3]                <- sum(extinction$Year==i+1973)
  distrib_time[i+2,4]                <- sum(conservation$Year==i+1973)
  distrib_time[i+2,5]                <- sum(ecosystem$Year==i+1973)
#  distrib_time[i+2,6]                <- sum(invasive$Year==i+1973)
  distrib_time[i+2,1]                <- 1973+i
}
d                                    <- distrib_time[,1:2]
d[,3]                                <- "Full"
d                                    <- d[-1,]
colnames(d)                          <- c("Year", "Value", "Group")

d2                                   <- distrib_time[,c(1,3)]
d2[,3]                               <- "Extinction"
d2                                   <- d2[-1,]
colnames(d2)                         <- c("Year", "Value", "Group")

d3                                   <- distrib_time[,c(1,4)]
d3[,3]                               <- "Conservation"
d3                                   <- d3[-1,]
colnames(d3)                         <- c("Year", "Value", "Group")


d4                                   <- distrib_time[,c(1,5)]
d4[,3]                               <- "Ecosystem"
d4                                   <- d4[-1,]
colnames(d4)                         <- c("Year", "Value", "Group")

#d5                                   <- distrib_time[,c(1,6)]
#d5[,3]                               <- "Invasive species & infectious diseases"
#d5                                   <- d5[-1,]
#colnames(d5)                         <- c("Year", "Value", "Group")

distrib_time                         <- rbind(d,d2,d3,d4) #,d5
distrib_time$Value                   <- as.numeric(distrib_time$Value)
distrib_time[,4]                     <- distrib_time[,2]/sum(distrib_time$Value)

rm(d,d2,d3,d4) #,d5


##################################################################################################################
### III. Graphs ##################################################################################################
##################################################################################################################
####### A. Distribution of articles across zones #####
zone_count                           <- as.data.frame(table(df2_ready$paper_zone))
zone_count$Freq                      <- as.numeric(zone_count$Freq)

continents                           <- c("Asia","Africa","Europe","North America", "Oceania","South America")
zone_count_b                         <- zone_count %>% subset(Var1 %in% continents)
zone_count_b$Freq                    <- as.numeric(zone_count_b$Freq)     
zone_count_b$Var1                    <- as.character(zone_count_b$Var1)
zone_count_b[nrow(zone_count_b)+1,2] <- nrow(df2_ready)-sum(zone_count_b$Freq)
zone_count_b[nrow(zone_count_b),1]   <- c("Mixed")
zone_count_b[,3]                     <- zone_count_b[,2]/sum(zone_count_b[,2])

colnames(zone_count_b)               <- c("lbls","count",'share')
# Two counting options: group altogether "Mixed : ..." papers into one category -> Methtod 1
# Or count one for each occurence, ie, if Mixed : Africa America, 1 goes to both. -> Method 2
zone_count_mixed                     <- zone_count %>% subset(!(Var1 %in% continents))
zone_count_mixed$Var1                <- as.character(zone_count_mixed$Var1)
zone_count_mixed[,3]                 <- c(1:nrow(zone_count_mixed))
count                                <- strsplit.data.frame(zone_count_mixed, term="Var1", group=c("V3","Freq")) %>% subset(Var1 %in% c(continents, "America"))
count                                <- count %>% group_by(Var1) %>% mutate(v3=sum(Freq)) %>% ungroup()%>% dplyr::select(Var1, v3)
count                                <- count %>% distinct() %>% rbind(c("South America", 0))
count$Var1                           <- dplyr::recode(count$Var1, "America" = "North America")
count                                <- count[order(count$Var1),]
count                                <- count$v3


zone_count_mixed                     <- data.frame(continents, count)
zone_count_mixed$count               <- as.numeric(zone_count_mixed$count)
zone_count_mixed$count               <- zone_count[zone_count$Var1 %in% continents,2]+zone_count_mixed$count
zone_count_mixed[,3]                 <- zone_count_mixed[,2] / sum(zone_count_mixed[,2])

colnames(zone_count_mixed)           <- c("lbls","count",'share')


pie                                  <- ggplot(zone_count_b, aes(x="", y=share, fill=lbls))+ geom_bar(stat="identity")+coord_polar("y", start=0)+scale_fill_economist()
pie                                  <- pie+geom_text(aes(label = paste0(round(share*100))), position = position_stack(vjust = 0.7), size=7, color="black")
pie                                  <- pie + labs(x = NULL, y = NULL, fill = NULL, title = " ")
pie                                  <- pie + theme_classic() + theme(axis.line = element_blank(),
                                                                      axis.text = element_blank(),
                                                                      axis.ticks = element_blank()
                                                                      )
pie                                  <- pie+ theme(plot.title = element_text(size=18))
pie
ggsave("pie_geographic_whole_base_method1.pdf", plot=pie, units="cm",width=20, height=13)


pie                                  <- ggplot(zone_count_mixed, aes(x="", y=share, fill=lbls))+ geom_bar(stat="identity")+coord_polar("y", start=0)+scale_fill_economist()
pie                                  <- pie
paste0(round(zone_count_mixed$share*100), "%")
pie                                  <- pie + labs(x = NULL, y = NULL, fill = NULL, title = " ")
pie                                  <- pie + theme_classic() + theme(axis.line = element_blank(),
                                                                      axis.text = element_blank(),
                                                                      axis.ticks = element_blank()
                                                                      )
pie                                  <- pie+ theme(plot.title = element_text(size=18))
pie
ggsave("pie_geographic_whole_base_method2.pdf", plot=pie, units="cm",width=20, height=13)


#### Distribution of groups across zones
geo_extinction                           <- as.data.frame(table(extinction$paper_zone))
geo_extinction[,3]                       <- geo_extinction[,2]/sum(df2_ready$keywords2=="Extinction")

geo_conservation                         <- as.data.frame(table(conservation$paper_zone))
geo_conservation[,3]                     <- geo_conservation[,2]/sum(df2_ready$keywords2=="Conservation")

geo_ecosystem                            <- as.data.frame(table(ecosystem$paper_zone))
geo_ecosystem[,3]                        <- geo_ecosystem[,2]/sum(df2_ready$keywords2=="Ecosystem")

#geo_invasive                             <- as.data.frame(table(invasive$paper_zone))
#geo_invasive[,3]                         <- geo_invasive[,2]/sum(df2_ready$keywords2=="Invasive_species_infectious_disease")



geo_full                                 <- as.data.frame(table(df2_ready$paper_zone))
geo_full[,3]                             <- geo_full[,2]/nrow(df2_ready)

####### B. Distribution of articles over time #####

plot                                 <- distrib_time %>% ggplot(aes(x=Year,y=V4, group=Group))
plot+stat_smooth(method="loess",se=F, aes(colour=Group))
plot+geom_line(aes(color=Group))
plot+stat_smooth(method='lm',formula= y~poly(x,10),aes(colour=Group), se=F)+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x=element_text(angle =45, vjust=0))+ labs(title=" Distribution of articles in the database ", y="Frequence", x=" Year ")



time_distrib <- plot+stat_smooth(method='loess',aes(colour=Group),size=1, se=F)+ scale_color_manual(values= colors_review)+
       theme(axis.text.x=element_text(angle =45, vjust=0),
          plot.title = element_text(size = 20),
          panel.background = element_rect(fill = "white"))+ grids(linetype="solid")+
   labs(title=" ", y="Freq.", x="")+ylim(0,0.02)
ggsave("temporal_distribution.pdf", plot=time_distrib, units="cm",width=30, height=18)

distrib_time %>% subset(Group=="Full") %>% ggplot(aes(x=Year,y=V4, group=Group))+stat_smooth(method='loess',aes(colour=Group),size=1, se=F)+ scale_color_manual(values= "red")+
  theme(axis.text.x=element_text(angle =45, vjust=0),
        plot.title = element_text(size = 20),
        panel.background = element_rect(fill = "white"),
        legend.position = "none")+ grids(linetype="solid")+
  labs(title=" ", y="Freq.", x="")+ylim(0,0.02)
ggsave("temporal_distribution_full.pdf", plot=last_plot(), units="cm",width=30, height=18)


####### C. Journals #####
# Whole corpus

# Table of journals in the corpus : journal title
table_journal_corpus              <- as.data.frame(table(df2_ready$Journal))
table_journal_corpus[,3]          <- table_journal_corpus[,2]/sum(table_journal_corpus[,2])
colnames(table_journal_corpus)    <- c("Journal", "Count", "Percentage")
table_journal_corpus              <- table_journal_corpus[order(-table_journal_corpus$Count),]
stargazer(table_journal_corpus, summary=F, rownames=F)

# Table of journals in the corpus : econ, ecology, sustainable science
table_journal_field               <- table(df2_ready$journals_2)
table_journal_field               <- as.data.frame(table_journal_field)

print(unique(table_journal_field$Var1))
table_journal_field$Var1          <- as.character(table_journal_field$Var1)
table_journal_field$Var1          <- dplyr::recode(table_journal_field$Var1,
                                                  "Sustainable science" = "Sustainability science",
                                                  "Sustainable Science" = "Sustainability science",
                                                   "Mathematics"  = "Applied Mathematics")
table_journal_field[4,2]          <- 21
table_journal_field               <- table_journal_field[-5,]

table_journal_field[,3]           <- table_journal_field[,2]/sum(table_journal_field[,2])
colnames(table_journal_field)     <- c("Field", "Count", "Percentage")
table_journal_field <- table_journal_field[order(-table_journal_field$Count),]
stargazer(table_journal_field, summary=F, rownames=F)

# Table of journals per field - A REFAIRE POUR BIEN S'ADAPTER AUX unique(df2_reayd$journals2)
unique(df2_ready$journals_2)
df2_ready$journals_2                                     <- dplyr::recode(df2_ready$journals_2,
                                                                         "Sustainable science" = "Sustainability science",
                                                                         "Sustainable Science" = "Sustainability science",
                                                                         "Mathematics"  = "Applied Mathematics")
unique(df2_ready$journals_2)

intermediate                                            <- df2_ready %>% subset(journals_2=="Economics")
intermediate                                            <- as.data.frame(table(intermediate$Journal))
intermediate[,3]                                        <- intermediate[,2]/nrow(df2_ready)
intermediate                                            <- intermediate[order(-intermediate$Freq),]
colnames(intermediate)                                  <- c("Economic journals", "Count", "Percentage")
intermediate$`Economic journals`                        <- as.character(intermediate$`Economic journals`)
intermediate                                            <- intermediate %>% dplyr::select("Economic journals", "Count")
intermediate[44,]                                       <- c("Applied Mathematics journals", "Count")
intermediate[43,]                                       <- NA
intermediate[45,]                                       <- NA

intermediate4                                            <- df2_ready %>% subset(journals_2=="Applied Mathematics")
intermediate4                                            <- as.data.frame(table(intermediate4$Journal))
intermediate4[,3]                                        <- intermediate4[,2]/nrow(df2_ready)
intermediate4                                            <- intermediate4[order(-intermediate4$Freq),]
colnames(intermediate4)                                  <- c("Economic journals", "Count", "Percentage")
intermediate4                                            <- intermediate4 %>% dplyr::select("Economic journals", "Count")


intermediate2                                           <- df2_ready %>% subset(journals_2=="Ecology")
intermediate2                                           <- as.data.frame(table(intermediate2$Journal))
intermediate2[,3]                                       <- intermediate2[,2]/nrow(df2_ready)
intermediate2                                           <- intermediate2[order(-intermediate2$Freq),]
colnames(intermediate2)                                  <- c("Ecology journals", "Count", "Percentage")
intermediate2                                            <- intermediate2 %>% dplyr::select("Ecology journals", "Count")
intermediate2$`Ecology journals`                         <- as.character(intermediate2$`Ecology journals`)
intermediate2[28,]                 <- NA
intermediate2[29,]                 <- c("Sustainability science journals", "Count")
intermediate2[30,]                 <- NA


intermediate3                                           <- df2_ready %>% subset(journals_2=="Sustainability science")
intermediate3                                           <- as.data.frame(table(intermediate3$Journal))
intermediate3[,3]                                       <- intermediate3[,2]/nrow(df2_ready)
intermediate3                                           <- intermediate3[order(-intermediate3$Freq),]
colnames(intermediate3)                                 <- c("Ecology journals", "Count", "Percentage")
intermediate3                                           <- intermediate3 %>% dplyr::select("Ecology journals", "Count")

longueur                                                <- max(nrow(intermediate),nrow(intermediate2),
                                                               nrow(intermediate3),nrow(intermediate4))
#intermediate2[28:longueur,]             <- NA
#intermediate3[19:longueur,]             <- NA
#intermediate4[11:longueur,]             <- NA

table_journals_corpus_field_a                           <- rbind(intermediate, intermediate4)
table_journals_corpus_field_b                           <- rbind(intermediate2,intermediate3)

table_journals_corpus_field_b[49:55,]                   <- NA

table_journals_corpus_field                              <- cbind(table_journals_corpus_field_a, table_journals_corpus_field_b)

#table_journals_corpus_field                             <- cbind(intermediate, intermediate2,intermediate3,intermediate4)
#colnames(table_journals_corpus_field)                   <- c("Economics journals","Count", "Percentage", "Ecology journals", "Count.1", "Percentage.1", "Sustainable science journals", "Count.2", "Percentage.2","Applied mathematics journals", "Count.3", "Percentage.3")

table_journals_corpus_field2                            <- table_journals_corpus_field %>% dplyr::select( "Economics journals","Ecology journals", "Sustainable science journals",'Applied mathematics journals')
stargazer(table_journals_corpus_field, summary=F, rownames=F)
stargazer(table_journals_corpus_field2, summary=F, rownames=F)

rm(intermediate, intermediate2, intermediate3)


#### By subgroup

### Extinction
journal_ext_discipline                   <- as.data.frame(table(extinction$journals_2))
journal_ext_discipline[,3]               <- journal_ext_discipline[,2]/sum(journal_ext_discipline[,2])
colnames(journal_ext_discipline)         <- c("Field", "Count", "Percentage")
stargazer(journal_ext_discipline, summary=F, rownames=F)

journal_ext                              <- as.data.frame(table(extinction$Journal))
journal_ext[,3]                          <- journal_ext[,2]/sum(journal_ext[,2])
colnames(journal_ext)                    <- c("Journal", 'Count', "Percentage")
stargazer(journal_ext, summary=F, rownames=F)

### Conservation
journal_cons_discipline                  <- as.data.frame(table(conservation$journals_2))
journal_cons_discipline[,3]              <- journal_cons_discipline[,2]/sum(journal_cons_discipline[,2])
colnames(journal_cons_discipline)        <- c("Field", "Count", "Percentage")
stargazer(journal_cons_discipline, summary=F, rownames=F)

journal_cons                             <- as.data.frame(table(conservation$Journal))
journal_cons[,3]                         <- journal_cons[,2]/sum(journal_cons[,2])
colnames(journal_cons)                   <- c("Journal", 'Count', "Percentage")
stargazer(journal_cons, summary=F, rownames=F)


### Ecosystem

journal_ecosys_discipline                <- as.data.frame(table(ecosystem$journals_2))
journal_ecosys_discipline[,3]            <- journal_ecosys_discipline[,2]/sum(journal_ecosys_discipline[,2])
colnames(journal_ecosys_discipline)      <- c("Field", "Count", "Percentage")
stargazer(journal_ecosys_discipline, summary=F, rownames=F)

journal_ecosys                           <- as.data.frame(table(ecosystem$Journal))
journal_ecosys[,3]                       <- journal_ecosys[,2]/sum(journal_ecosys[,2])
colnames(journal_ecosys)                 <- c("Journal", "Count", "Percentage")
stargazer(journal_ecosys, summary=F, rownames=F)

## Invasive species
#journal_invasive_discipline                <- as.data.frame(table(invasive$journals_2))
#journal_invasive_discipline[,3]            <- journal_invasive_discipline[,2]/sum(journal_invasive_discipline[,2])
#colnames(journal_invasive_discipline)      <- c("Field", "Count", "Percentage")
#stargazer(journal_invasive_discipline, summary=F, rownames=F)

#journal_invasive                           <- as.data.frame(table(invasive$Journal))
#journal_invasive[,3]                       <- journal_invasive[,2]/sum(journal_invasive[,2])
#colnames(journal_invasive)                 <- c("Journal", "Count", "Percentage")
#stargazer(journal_invasive, summary=F, rownames=F)
 




  


####################################################################################################################
### IV. Other ###############################################################################################
####################################################################################################################
####### A. Analysis of authors ####
# Overall
authors_tot    <- c(df2$Author, df2b$Author)
authors        <- strsplit(authors_tot, split=" ")
authors        <- sapply(authors, tail, 1)
authors        <- as.data.frame(table(authors))
authors        <- authors[order(-authors$Freq),]
authors_top20  <- authors[1:20,]

# By group
df2_cons           <- df2 %>% subset(keywords2=="Conservation")
df2b_cons          <- df2b %>% subset(keywords2=="Conservation")
authors_cons       <- c(df2_cons$Author,df2b_cons$Author)
authors_cons       <- strsplit(authors_cons, split=" ")
authors_cons       <- sapply(authors_cons, tail, 1)
authors_cons       <- as.data.frame(table(authors_cons))
authors_cons       <- authors_cons[order(-authors_cons$Freq),]
authors_cons_top10 <- authors_cons[1:10,]


df2_ext           <- df2 %>% subset(keywords2=="Extinction")
df2b_ext          <- df2b %>% subset(keywords2=="Extinction")
authors_ext       <- c(df2_ext$Author,df2b_ext$Author)
authors_ext       <- strsplit(authors_ext, split=" ")
authors_ext       <- sapply(authors_ext, tail, 1)
authors_ext       <- as.data.frame(table(authors_ext ))
authors_ext       <- authors_ext[order(-authors_ext$Freq),]
authors_ext_top10 <- authors_ext[1:10,]

df2_eco            <- df2 %>% subset(keywords2=="Ecosystem")
df2b_eco           <- df2b %>% subset(keywords2=="Ecosystem")
authors_eco        <- c(df2_eco$Author,df2b_eco$Author)
authors_eco        <- strsplit(authors_eco, split=" ")
authors_eco        <- sapply(authors_eco, tail, 1)
authors_eco        <- as.data.frame(table(authors_eco ))
authors_eco        <- authors_eco[order(-authors_eco$Freq),]
authors_eco_top10  <- authors_eco[1:10,]

#df2_invasive              <- df2 %>% subset(keywords2=="Invasive_species_infectious_disease")
#df2b_invasive             <- df2b %>% subset(keywords2=="Invasive_species_infectious_disease")
#authors_invasive          <- c(df2_invasive$Author,df2_invasive$Author)
#authors_invasive          <- strsplit(authors_invasive, split=" ")
#authors_invasive          <- sapply(authors_invasive, tail, 1)
#authors_invasive          <- as.data.frame(table(authors_invasive ))
#authors_invasive          <- authors_invasive[order(-authors_invasive$Freq),]
#authors_invasive_top10    <- authors_invasive[1:10,]

####### B. Field ##########
field_keywords <- as.data.frame(table(df2_ready$affiliation_class, df2_ready$keywords2))


field_keywords_cons               <- as.data.frame(table(conservation$affiliation_class))
field_keywords_cons[,3]           <- "Conservation"
field_keywords_cons               <- field_keywords_cons[order(-field_keywords_cons$Freq),]
colnames(field_keywords_cons)     <- c("Field","Freq","Group")
nrow(field_keywords_cons)

field_keywords_ext                <- as.data.frame(table(extinction$affiliation_class))
field_keywords_ext[,3]            <- "Extinction"
field_keywords_ext                <- field_keywords_ext[order(-field_keywords_ext$Freq),]
colnames(field_keywords_ext)      <- c("Field.1","Freq.1","Group.1")
nrow(field_keywords_ext)

field_keywords_eco                <- as.data.frame(table(ecosystem$affiliation_class))
field_keywords_eco[,3]            <- "Ecosystem"
field_keywords_eco                <- field_keywords_eco[order(-field_keywords_eco$Freq),]
colnames(field_keywords_eco)      <- c("Field.2","Freq.2","Group.2")
nrow(field_keywords_eco)

#field_keywords_invasive           <- as.data.frame(table(invasive$affiliation_class))
#field_keywords_invasive[,3]       <- "Conservation"
#field_keywords_invasive           <- field_keywords_invasive[order(-field_keywords_invasive$Freq),]
#colnames(field_keywords_invasive) <- c("Field.2","Freq.2","Group.2")
#nrow(field_keywords_invasive)

field_keywords_cons[10:12,]       <- NA
field_keywords_ext[10:12,]        <- NA
field_keywords_eco[2:12,]         <- NA

field_keywords_table              <- cbind(field_keywords_cons,field_keywords_ext,field_keywords_eco)

####### C. What type of biodiversity is the most prevalent in groups ####
table(biodiversity_ag$kingdom)
table(biodiversity_ag$class)
table(biodiversity_ag$order)
table(biodiversity_ag$subgroup_species)

table(biodiversity_ag$kingdom,df2_ready$keywords2)
table(biodiversity_ag$class,df2_ready$keywords2)
table(biodiversity_ag$order, df2_ready$keywords2)
table(biodiversity_ag$subgroup_species, df2_ready$keywords2)



