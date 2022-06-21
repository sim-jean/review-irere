### Script for descriptive statistics ###

##################################################################################################################
### I. Graphs ##################################################################################################
##################################################################################################################
df2_ready <- read_excel("data/data_cleared.xlsx")
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

zone_count_b                         <- zone_count_b %>% mutate(
                                                                share_round = as.numeric(format(round(share,2)))*100,
                                                                share_round2 = ifelse(share_round>1,share_round, NA))
pie                                  <- ggplot(zone_count_b, aes(x="", y=share, fill=lbls))+ geom_bar(stat="identity")+coord_polar("y", start=0)+scale_fill_pander()
pie                                  <- pie + labs(x = NULL, y = NULL, fill = NULL, title = " ")
pie                                  <- pie + theme_classic() + theme(axis.line = element_blank(),
                                                                      axis.text = element_blank(),
                                                                      axis.ticks = element_blank()
)
pie                                  <- pie+ theme(plot.title = element_text(size=18))
pie+ geom_label(aes(label = share_round2), color = "white",
           position = position_stack(vjust = 0.5),
           show.legend = FALSE) +labs(title="Geographical distribution of articles")+
  theme(plot.title = element_text(hjust = 0.5))

#ggsave("outputs/pie_geographic_whole_base_method1.pdf", plot=pie, units="cm",width=20, height=13)


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
#ggsave("outputs/pie_geographic_whole_base_method2.pdf", plot=pie, units="cm",width=20, height=13)



####### B. Distribution of articles over time #####

distrib_time                         <- as.data.frame(table(df2_ready$Year))
colnames(distrib_time)               <- c('Year',"Full")
distrib_time$Year                    <- levels(distrib_time$Year)
distrib_time$Year                    <- as.numeric(distrib_time$Year)

distrib_time %>% ggplot(aes(x=Year,y=Full))+stat_smooth(method='loess',size=1, se=F)+ scale_color_manual(values= "red")+
  theme(axis.text.x=element_text(angle =45, vjust=0),
        plot.title = element_text(size = 20),
        panel.background = element_rect(fill = "white"),
        legend.position = "none")+ grids(linetype="solid")+
  labs(title="Temporal distribution of articles in the database ",y="", x="")+
  theme(plot.title = element_text(hjust = 0.5))
#ggsave("outputs/temporal_distribution_full.pdf", plot=last_plot(), units="cm",width=30, height=18)


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

# Table of journals per field 
unique(df2_ready$journals_2)
df2_ready$journals_2                                     <- dplyr::recode(df2_ready$journals_2,
                                                                          "Sustainable science" = "Sustainability science",
                                                                          "Sustainable Science" = "Sustainability science",
                                                                          "Mathematics"  = "Applied Mathematics")

distribution_journals <- as.data.frame(table(df2_ready$journals_2))%>%mutate(share=Freq/sum(Freq),
                                                                             share_rounded=paste0(as.numeric(format(round(share,2)))*100,'%'))
distribution_journals %>% ggplot(aes(x="",y=Freq,fill=Var1))+
  geom_bar(stat="identity")+
  coord_polar("y", start=0)+
  scale_fill_colorblind()+
  geom_label(aes(label = share_rounded), color = "white",
             position = position_stack(vjust = 0.55),
             show.legend = FALSE)+ theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+ 
  labs(x = NULL, y = NULL, fill = NULL, title = " Field of publication of the articles ")+
  theme(plot.title = element_text(hjust = 0.5))
  
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
