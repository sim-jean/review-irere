###############################################################################################################
############################ NARRATIVE ANALYSIS   #############################################################
###############################################################################################################

rm(list=ls())
library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(plotrix)
library(ggpubr)
library(writexl)
library(scatterplot3d)
library(stargazer)
library(FactoMineR)
library(factoextra)
library(mltools)
library(data.table)
library(DescTools)
library(ggExtra)
library(textreadr)
library(scales)
library(tm)
library(qdapRegex)
library(RColorBrewer)

##### I. DATA LOADING AND CLEANING ########
colors_kmodes         <- c('1'="darkorchid3", "2"="forestgreen", "3"="gold", "4"="black")
full_bib_mod          <- read_rtf("data/base_modified.rtf")
#full_bib_mod          <- read_rtf("base_test_modified.rtf")
full_bib_mod          <- strsplit(full_bib_mod, "@")

##### ii. Modified data #####
data_mod              <- data.frame(0,0,0,0,0)
colnames(data_mod)    <- c('title',"abstract","keywords","index","collapse")
for(i in c(2:318)){
  data_mod[i-1,1]     <- rm_between(full_bib_mod[[1]][i], "title = {",'},', extract=T)[[1]]
  data_mod[i-1,2]     <- rm_between(full_bib_mod[[1]][i], 'abstract = {', '},', extract=TRUE)[1]
  data_mod[i-1,3]     <- rm_between(full_bib_mod[[1]][i], "keywords = {", "},", extract=T)[[1]]
  data_mod[i-1,4]     <- i-1
} 
data_mod[,3]          <- apply(data_mod[,3, drop=F], 2, function(x) gsub("([-])|[[:punct:]]", "\\1",x, perl=T))
data_mod[,5]          <- apply(data_mod, 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = " "));
data_mod[,5]          <- apply(data_mod[,5, drop=F],2, function(x) gsub("([-])|[[:punct:]]", "\\1", x,perl=T))

data_mod[114,]        <- NA #outlier, very long abstract
data_mod              <- data_mod[-324,]
text_paper_mod        <- tolower(as.vector(data_mod[,5]))
rm(full_bib_mod)

####### C. Clean data : remove number, stopwords ################################################
my_stopwords          <- c("we","can","be","are","do","will","when","what","how","i","ii","iii", "for",
                           "with","within","then","which","than","for","a","consider","considered","assumed",
                           "applied","given","by","high","predict","the","of", "to","a",'an',"at", "and","with","for","from","when",
                           "what","how","in","as","on","is","are","can", "does","or", 'The',"Do","Does","A","An","When",
                           "What","How","In", "This","It","On","With","I","Be","may","must","May","Must","should","Should",
                           "Can","km",'none',"many","used","show","possible","reach","deal","yields","yield", "hence","indeed",
                           "great","less","more", 'show','shown',"aimed","determine","current","paper","results","problem","allow","able",
                           "altering","alter","confirm","also","case","developed","example","form","using","two","our","one","for","however",
                           "whatever","whenever","whether", "where","whereas","yet","via","often","new","find","due","without","low","size",
                           "study","analysis",'different', "however","our","for","finally","however"
) #Create stopwords
stwds                 <- stopwords::stopwords("en", source = "stopwords-iso") 
my_stopwords          <- c(my_stopwords, stwds)

# Alternative data ####
numbers               <- c(1:10000000)

text_paper_data_mod <- as.data.frame(text_paper_mod)%>% mutate(index = 1:length(text_paper_mod),
                                                               text_paper_mod = as.character(text_paper_mod)) 
text_paper_data_mod <- text_paper_data_mod %>% tidytext::unnest_tokens(input='text_paper_mod', output='words', 
                                                                       token='words') %>%
  filter(!words %in% my_stopwords)%>%
  filter(!words %in% numbers)

print(length(unique(text_paper_data_mod$words)))

## FREQUENCE OF WORDS ####
freq_words_mod      <- data.frame(word=text_paper_data_mod$words)%>%
  group_by(word) %>% #Group to make sure each word is considered, not the whole sample
  dplyr::mutate(n = n()) %>%#Give the number of occurences of each word
  unique()%>% # Make sure there is no double occurences of words
  ungroup() %>% # We no longer consider each word
  arrange(-n)%>% # Sort the words in descending number of occurences
  mutate(word=as.character(word))

summary(freq_words_mod$n)

## DISTRIBUTION OF WORDS ####
mod_distrib               <- as.data.frame(table(freq_words_mod$n))
mod_distrib %>% ggplot(aes(x=Var1,y=Freq))+geom_point()
freq_words_mod            <- freq_words_mod[1:last(which(freq_words_mod$n==5)),]

print(paste0("We take into account ", (nrow(freq_words_mod)/length(unique(text_paper_data_mod$words))*100), " % of the text data"))

## DATA : ONLY WORDS WITH AT LEAST FIVE OCCURENCES
text_paper_data_mod       <- text_paper_data_mod[text_paper_data_mod$words %in% freq_words_mod$word,]


## COLLAPSE DATA INTO VECTOR  #####
text_ee_mod               <- c()
for(i in 1:317){
  text <- text_paper_data_mod %>% subset(index==i)
  text <- paste(text$words, collapse=' ')
  text_ee_mod <- c(text_ee_mod,text) 
}
data_text_mod         <- VCorpus(VectorSource(text_paper_mod))
data_text_mod         <- tm_map(data_text_mod, removeNumbers)
data_text_mod         <- tm_map(data_text_mod, removeWords, stopwords("english"))
data_text_mod         <- tm_map(data_text_mod, removeWords, my_stopwords)

## Alternative data #####
data_text_mod         <- VCorpus(VectorSource(text_ee_mod))

## LOAD EXISTING DATA ####
article_cluster_mod <- read_excel("data/article_cluster_mod.xlsx")
colnames(article_cluster_mod) <- c("title","index","og_cluster","mod_cluster")

df2_ready           <- read_excel("data/data_cleared_post_ACM.xlsx") %>% subset(Journal !="Environmental Modelling & Assessment")

### Worddcloud for presentation
toofreq            <- pull(freq_words_mod[1:10,1])

total_base         <- VCorpus(VectorSource(as.vector(data_mod$collapse)))
total_base         <- tm_map(total_base, removeNumbers)
total_base         <- tm_map(total_base, removeWords, stopwords("en"))
total_base         <- tm_map(total_base, removeWords, my_stopwords)
total_base         <- tm_map(total_base, removeWords, toofreq)
wordcloud::wordcloud(total_base, 
                     max.words = 200, colors=c(brewer.pal(8,"Dark2"),brewer.pal(8, "Paired")), random.color = F, random.order = F)


## GET ARTICLES FROM KMODES CLUSTERING ####

for(i in 1:4){
  choice <- which(df2_ready$kmodes_result==i)
  set    <- article_cluster_mod[choice, ]
  setb   <- data_mod[data_mod$title %in% set$title,]
  assign(paste0("articles_mod_kmodes_",i),setb)
}

for(i in 1:4){
  set               <- paste0("articles_mod_kmodes_",i)
  set               <- VCorpus(VectorSource(as.vector(get(set)$collapse)))
  set               <- tm_map(set, removeNumbers)
  set               <- tm_map(set, removeWords, stopwords("english"))
  set               <- tm_map(set, removeWords, my_stopwords)
  dtm_set           <- DocumentTermMatrix(set)
  freq              <- colSums(as.matrix(dtm_set))
  freq              <- freq[!(names(freq) %in% my_stopwords)]
  freq              <- freq[(names(freq) %in% freq_words_mod$word)]
  ord               <- order(freq, decreasing=T)
  
  articles          <- paste0("articles_mod_kmodes_",i) 
  articles          <- as.vector(get(articles)$title)
  data_articles     <- df2_ready[df2_ready$TItle %in% articles,]  
  
  assign(paste0("kmodes_",i),set) # Get words of the corpus
  assign(paste0("dtm_kmodes_",i),dtm_set) # Get dtm of the corpus
  assign(paste0("freq_kmodes_",i),freq) # Compute frequence of words
  assign(paste0('ord_kmodes_',i),ord) # Get most frequent words 
  assign(paste0("data_articles_kmodes_",i),data_articles) # Get data from the methodological variables
}

### II. ANALYSIS #####
## OCCURENCE DATA AND GRAPH #####
for(j in c(5,7,10,15,20,25,30,50,100,200)){
  for(i in 1:4){
    freq_loop           <- paste0('freq_kmodes_',i) #get the frequences of words for cluster i
    set                 <- data.frame(term= names(get(freq_loop)), occurences=get(freq_loop)) # turn into a dataframe
    
    set                 <- set %>% top_n(n = j, wt=occurences) # get top j words by cluster by occurence 
    # Graph the distribution and save it
    graph_mod           <- set %>% ggplot(aes(reorder(term, -occurences),occurences))+
      geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle=45))+labs(title = paste0("Top ",j," occurences for cluster : ", i))
    #ggsave(paste0("outputs/occurences_kmodes_",i,"_n",j,".pdf"), plot=graph_mod, width=15, height=10)
    print(graph_mod)
    dev.off()
    # Assign the data set for further analysis
    assign(paste0("outputs/occurences_kmodes_",i,"_n",j),graph_mod)
    set                 <- set %>% top_n(n = j, wt=occurences) %>% mutate(cluster = i)
    assign(paste0("data_kmodes_occ_",i,"_n",j), set)
  }
  # Merge all data_sets for j top words
  set_1                 <- get(paste0("data_kmodes_occ_1_n",j))
  set_2                 <- get(paste0("data_kmodes_occ_2_n",j))
  set_3                 <- get(paste0("data_kmodes_occ_3_n",j))
  set_4                 <- get(paste0("data_kmodes_occ_4_n",j))
  
  
  set                   <- rbind(set_1,set_2,set_3,set_4)
  # Get the terms in character
  set$term              <- as.character(set$term)
  # Compute the number of times each term is present
  set                   <- as.data.frame(table(set$term))
  colnames(set)         <- c("term","occurence")
  # Permute to descending order
  set                   <- set[order(set$occurence, decreasing = T),]
  # Get dataset of the sum of the dummy presence of each word in all groups for j topwords (ex: 3/j)
  assign(paste0("data_kmodes_n",j),set)
  
  # Find the common words : if presence=1 in each cluster for j topwords, it is a common word
  common_word           <- set[which(set$occurence==4),]
  common_word           <- cbind(common_word,j)
  colnames(common_word) <- c("term","occurence","cutoff")
  assign(paste0("common_word_kmodes_",j), common_word)
  common_word$term      <- as.character(common_word$term)
  
  # Find the (less) common words : if presence=1 in 4 (or 3) clusters for j topwords, it is a common word
  
  specific_word           <- set[which(set$occurence==1),]
  if(nrow(specific_word)==0){ #Avoid error in case there is no word that is just common to 4 clusters
    specific_word         <- NA
  } else {
    specific_word           <- cbind(specific_word,j)
    colnames(specific_word) <- c("term","occurence","cutoff")
    assign(paste0("specific_word_kmodes_",j), specific_word)
    specific_word$term      <- as.character(specific_word$term) 
  }
  
  # Less specific : 2 clusters

}

common_word     <- as.character(common_word_kmodes_50$term)
specific_word   <- as.character(specific_word_kmodes_50$term)


#### Plot for each cluster what is common, what is specific  ####
# Get the data for occurences and top words ####
for(j in 1:4){
  set            <- get(paste0("data_kmodes_occ_",j,"_n50")) # Get top 30 data for each cluster
  
  set[,4]        <- "nothing" #Set default value : for now, every word is specific
  
  indexes        <- which(set$term %in% common_word)  #Find index of words in common words
  set[indexes,4] <- "common"  #Assign common marker
  
  
  #indexes  <- which(set$term %in% common_word_3)
  #set[indexes,4] <- 'common_to_3'
  #set[indexes,4] <- "common"
  indexes         <- which(set$term %in% specific_word)
  set[indexes,4]  <- "specific"
  
  
  colnames(set)   <- c("term","occurence","cluster","key") 
  set[,4]         <- as.factor(set[,4])
  graph           <- set %>% top_n(n = 50, wt=occurence) %>% ggplot(aes(reorder(term, -occurence),occurence,fill=key))+ theme_bw()+
    scale_fill_manual(values=c("common"="gold","common_to_4"="khaki","nothing"="gray","specific"="navyblue","specific_2"="dodgerblue"), name="Legend",
                      labels("Common to all", "Common to 4 groups","Common to 3 groups", "Specific to 2 groups", "Specific to 1 group"))+
    geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle=90, hjust=1))+
    labs(title = paste0("Cluster : ", j)) + xlab(" ")+theme(legend.position = "none")
  
  assign(paste0("data_kmodes_occ",j,"_n50"),set)
  #ggsave(paste0("outputs/occurence_kmodes_",j,"_n50_common.pdf"), plot=graph, width=15, height=9)
}

lex_agri       <- c("Agriculture", "agriculture", "agricultural", "crop", "rangeland", "livestock", "forage", 
                    "fallow", "farmland", "grassland", "oats", "agri", "farmers", "grazing", "crop", "livestock",
                    "farming", "wheat", "crops", "farm", "cropping", "rangeland", "grazing", "stocking","alfalfa","wheat",
                    "agro","crofting","pastures","ranchers","range", "grasslands"
)

lex_invasive   <- c("Invasive species", "invasive", "rabies", "invasion", "invasivespecies", "invader", "coevolution",
                    "non-endemic", "nis", "eradication", "pest", "mountainpinebeetle", "weevil","disease","gypsymoth",
                    "weevil", "oats", "weed", "herbicide", "invader", "rabies", "pathogens", "invasivespecies",
                    "indigenous", "barrier", "infestation", "alien", "beaver","calvescens", "eradicate","host",
                    "resistance", "infestations", "pesticide", "pests", "invasions", "weed", "weeds", "pest","nonindigenous",
                    "pathogen", "invaders", "spartinaalterniflora", "spartina","beetle","endemic","emeraldashborer",
                    "beetles","avena","rodent","serratedtussock","tuberculosis","miconiacalvescens","vaccine","insects","spread",
                    "vector-borne","epidemiology","quarantine", "trap")

lex_forest       <- c("Trees", "stand", "tree", "forest", "forestry",  "basal", "spruce", 
                      "even-aged", "uneven-aged", "forests","timber", "diameter","wood","pine","faustmann",
                      "volume","reforestation","rotation","rotational","acacia","forested","lichen" )

lex_policy       <- c("Policy", "subsidy", "tax", "tradable", "subsidies", "instruments","policy","policies","payments","taxes",
                      "market","markets","incentives","payment","permits","taxes","incentive","funding","budget","budgets",
                      "conflict","conflicts","bonus", "planner", "taxation","property","market-based", "contracts",
                      "interventions","intervention","strategy","propertyrights", "taxsubsidy")

lex_scale        <- c("Agency", "landowner", "large scale", "small scale","local","global","agencies","public","scale","level", "regional"
)

lex_endangered   <- c("Endangered species", "remarkable", "trophy", "tiger", "endangeredspecies","warbler",
                      "moose", "illegal", "threatened", "threats", "endangered", "elephants","butterfly",
                      "wildlife", "game", "poachers", "wolf", "reindeer","poaching", "wolves","elephant","bushmeat",
                      "ivory","black","hunt","canislupus","hunters","bear","serengeti","tigers","deer","rhino", "extinction","endangered"
)

lex_risk         <- c("risk","uncertainty","insurance","markov","option","resilience","stochastic","probabilities",
                      "uncertain")

lex_mathematical <- c("minimize","maximization","algorithm","maximizing","viability", "constraint", "minimum", 
                      "variables", "constraint", "maximizes", "optimum", "nonlinear", "optimum", "optimizing", 
                      "discrete", "control", "parameters", "parameter", "equilibrium", "theory", "optimization", 
                      "numerical", "optimalcontrol", "maximize", "maximise", "programming", "simulations", "theoretical", 
                      "linear", "equation", "optimally", "optimisation", "numerically", "analytical", "analytically", 
                      "optimize", "integerprogramming", 'integer', "minimizing", "maximized", "coviability","empirically",
                      "solutions","factors","conditions", "functions", "modeling","solution", "threshold","properties",
                      "variable","differential", "constant","logistic","limit","optimal","nonconvexity","model", "nad", "maximal", 
                      "mathematical", "increasing","formula","equilibria","dynamic-programming","dynamic","constraints",
                      "steadystate")

lex_mathapplied  <- c("cost","costeffective", "spatial", "rate", "rates", "benefits", "benefit", "costeffectiveness", 
                      "marginal", "discount", "discounted", "utility", "preferences", "average", "marginal", "costbenefit",
                      "objectives","integrated","initial","criteria","estimated","estimate","estimates","heterogeneity",
                      "regime","criterion"
)

lex_harvesting   <- c("harvesting","harvests","harvest",'hunting')
lex_conservation <- c("conservation", "park","reserve", "sites","restoration", "planning", "conservationplanning")

lex              <- c("lex_agri","lex_invasive","lex_forest","lex_scale","lex_endangered","lex_mathematical",
                      "lex_risk","lex_mathapplied","lex_harvesting","lex_conservation")
table_lex <- data.frame(0,0,0,0,0,0,0,0,0,0,0,0)
colnames(table_lex) <- c("cluster", "agri", "endangered","forest","invasive","Policy","scale","mathematical",
                         "risk","math_applied","harvesting","conservation")

for(i in 1:4){
  names_cluster  <- paste0("freq_mod_",i) # Use all words present in freq_words_mod
  set            <- get(paste0("data_kmodes_occ_",i,"_n200"))
  set[,4]        <- "common"
  
  indexes        <- which(set$term %in% lex_agri)
  set[indexes,4] <- "agri"
  
  indexes        <- which(set$term %in% lex_invasive)
  set[indexes,4] <- "invasive"
  
  indexes        <- which(set$term %in% lex_forest)
  set[indexes,4] <- "forest"
  
  indexes        <- which(set$term %in% lex_scale )
  set[indexes,4] <- "scale"
  
  indexes        <- which(set$term %in% lex_policy)
  set[indexes,4] <- "Policy"
  
  indexes        <- which(set$term %in% lex_endangered)
  set[indexes,4] <- "endangered"
  
  indexes        <- which(set$term %in% lex_mathematical)
  set[indexes,4] <- "mathematical"
  
  indexes        <- which(set$term %in% lex_mathapplied)
  set[indexes,4] <- "math_applied"
  
  indexes        <- which(set$term %in% lex_risk)
  set[indexes,4] <- "risk"
  
  indexes        <- which(set$term %in% lex_conservation)
  set[indexes,4] <- "conservation"
  
  indexes        <- which(set$term %in% lex_harvesting)
  set[indexes,4] <- "harvesting"
  
  set            <- set %>% subset(V4 %in% c("agri", "endangered","forest","invasive","Policy","scale","mathematical",
                                             "math_applied","risk","conservation","harvesting"))
  
  assign(paste0("data_mod_occ_",i,"n200_lexical"), set)
  set            <- set %>% group_by(V4) %>% dplyr::summarise(occurences2 = sum(occurences)) 
  assign(paste0("lexi_mod_occ_n200_cluster_",i),set)
  set            <- set %>% mutate(cluster=i) %>% pivot_wider(names_from = V4, values_from = occurences2)
  assign(paste0("table_lexical_kmodes_row_",i),set)
  #  if(!("forest" %in% colnames(set) ){
  #    cbind(colnames(set),"forest")
  #  }
  # if(length(set)<7){
  #    set          <- cbind(set,0)
  #   colnames(set)<- c("cluster", "agri", "endangered","forest","invasive","Policy","scale")
  #  } else {
  #   set          <- set
  #  }
}
table_lex_kmodes <- rbind(table_lexical_kmodes_row_1,
                          table_lexical_kmodes_row_2,
                          table_lexical_kmodes_row_3,
                          table_lexical_kmodes_row_4)
#table_lex                      <- table_lex[-1,]

# Modify data for occurence per paper, to incorporate size effects in the original data ####
table_lex_kmodes                      <- table_lex_kmodes %>% mutate( size            = c(nrow(articles_mod_kmodes_1), 
                                                                                          nrow(articles_mod_kmodes_2),
                                                                                          nrow(articles_mod_kmodes_3),
                                                                                          nrow(articles_mod_kmodes_4)),
                                                                      # Normalized variables : number of occurences of lexical group/paper
                                                                      agri_norm       = agri/size,
                                                                      endangered_norm = endangered/size, 
                                                                      forest_norm     = forest/size,
                                                                      invasive_norm   = invasive/size, 
                                                                      Policy_norm     = Policy/size, 
                                                                      scale_norm      = scale/size)
table_lex_kmodes            <- table_lex_kmodes[1:4,1:13]
table_lex_kmodes$cluster    <- as.numeric(table_lex_kmodes$cluster)
table_lex_kmodes$agri       <- as.numeric(table_lex_kmodes$agri)
table_lex_kmodes$endangered <- as.numeric(table_lex_kmodes$endangered)
table_lex_kmodes$forest     <- as.numeric(table_lex_kmodes$forest)
table_lex_kmodes$invasive   <- as.numeric(table_lex_kmodes$invasive)
table_lex_kmodes$Policy     <- as.numeric(table_lex_kmodes$Policy)
table_lex_kmodes$scale      <- as.numeric(table_lex_kmodes$scale)
table_lex_kmodes$size       <- as.numeric(table_lex_kmodes$size)
table_lex_kmodes$conservation <- as.numeric(table_lex_kmodes$conservation)
table_lex_kmodes$harvesting <- as.numeric(table_lex_kmodes$harvesting)
table_lex_kmodes            <- table_lex_kmodes %>% mutate(agri_share = agri/sum(agri),
                                                           endangered_share = endangered/sum(endangered),
                                                           forest_share     = forest/sum(forest),
                                                           invasive_share   = invasive/sum(invasive),
                                                           Policy_share     = Policy/sum(Policy),
                                                           scale_share      = scale/sum(scale),
                                                           size_share       = size/sum(size),
                                                           math_share       = mathematical/sum(mathematical),
                                                           risk_share       = risk/sum(risk),
                                                           mathapp_share    = math_applied/sum(math_applied),
                                                           harvesting_share = harvesting/sum(harvesting),
                                                           conservation_share = conservation/sum(conservation))
write_xlsx(table_lex_kmodes, "data/table_lexical_groups_cluster_mod_lauriane.xlsx")
table_lex_kmodes$cluster     <- as.character(table_lex_kmodes$cluster)



table_lex_kmodes_long <- table_lex_kmodes %>% dplyr::select(cluster, agri_share, forest_share, 
                                                            invasive_share, endangered_share, 
                                                            Policy_share,
                                                            risk_share, harvesting_share, 
                                                            conservation_share) %>% 
  pivot_longer(!cluster, names_to="lexical_group",values_to = "count")
table_lex_kmodes_long$lexical_group <- factor(table_lex_kmodes_long$lexical_group, levels=unique(table_lex_kmodes_long$lexical_group))

for(i in 1:4){
  intercept_graph <- pull(table_lex_kmodes %>% subset(cluster==i) %>% dplyr::select(size_share))
  graph <- table_lex_kmodes_long %>% subset(cluster==i) %>%  ggplot(aes(x=lexical_group))+geom_col(aes(y=count),fill=colors_kmodes[i],size=2)+
    geom_abline(intercept=intercept_graph, slope=0, color="black", linetype="dashed")+
    scale_x_discrete(labels=c("agri_share"       = "Agriculture",
                              "forest_share"     = "Forest",
                              "endangered_share" = "Endangered species",
                              "invasive_share"   = "Invasive species",
                              "Policy_share"     = "Policy",
                              
                              "risk_share"       = 'Risk',
                              "conservation_share"="Conservation",
                              "harvesting_share" = "Harvesting"
    )) + theme(axis.text.x=element_text(angle =45, hjust=1),
               text = element_text(size = 20),
               legend.position="none",
               panel.background = element_rect(fill = "white"),
               axis.line = element_line(color='darkgrey',linetype="solid"))+ylab(" ")+xlab(" ")+
    labs(title= " ")+ylim(0,0.9)
  assign(paste0("data_graph_col_",i),intercept_graph)
  #ggsave(paste0("outputs/lexical_profile_cluster_kmodes_",i,".pdf"), plot=graph, width=10, height=7)
}

ggarrange()
### 
df1 <- df2_ready %>% subset(kmodes_result==1) %>% dplyr::select(TItle, Author, Year, journals_2)
df2 <- df2_ready %>% subset(kmodes_result==2) %>% dplyr::select(TItle, Author, Year, journals_2)
df3 <- df2_ready %>% subset(kmodes_result==3) %>% dplyr::select(TItle, Author, Year, keywords2, stoch_ecol, stoch_econ, habitat2)
df3b <- df3 %>% subset(stoch_ecol=="No"| stoch_econ=="No")
df4 <- df2_ready %>% subset(kmodes_result==4) %>% dplyr::select(TItle, Author, Year, keywords2,habitat2, stoch_ecol, stoch_econ)
df4b <- df4 %>% subset(stoch_ecol=="Yes" | stoch_econ=="Yes")
summary(df2$Year)
table(df3$keywords2)
table(df4$keywords2)
