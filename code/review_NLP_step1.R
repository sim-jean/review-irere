####################################################################################################################
###########  NATURAL LANGUAGE PROCESSING : GROUPS FROM TEXTUAL DATA ###########################################
####################################################################################################################
set.seed(123)
######### 0. PACKAGE SET-UP ####################################################################
setwd("/Users/simonjean/Desktop/Thèse/Review_Lauriane/data")

required_packages     <- c("readxl", "dplyr", "magrittr", "tidyr", "ggthemes","ggplot2", 
                           "plotrix","ggpubr","writexl","stargazer","udpipe","cluster","factoextra","tm","SnowballC",
                           "wordcloud","textreadr","stringr","qdapRegex", "FactoMineR","stopwords","svglite")

new_packages          <- required_packages[!(required_packages %in% installed.packages()[,"Package"]) ]

install.packages(new_packages,  repos = "http://cran.us.r-project.org")
invisible(lapply(required_packages, library, character.only = TRUE))
rm(list=ls())

######### I. DATA FORMATING ####################################################################

####### A. Load data ###########################################################################
df2_ready             <- read_excel("data_cleared.xlsx")

##### i. Original data ######
full_bib              <- read_rtf("base.rtf")
#full_bib              <- read_rtf("base_test.rtf")
full_bib              <- strsplit(full_bib,"@") # Separate each article into a list
# Slight modification for what comes next : 
# resources   -> resource
# models      -> model
# systems     -> system
# unevenaged  -> uneven-aged
# values      -> value
# zones       -> zone
# costs       -> cost
# harvesting  -> harvest
# dynamics    -> dynamic
# economics   -> economic
# strategies  -> strategy
# REMOVE DIFFERENT
##### ii. Modified data #####
# endangered-species
# bio-economic
# invasive-species
# ecosystem-service
# optimal-control
# dynamic-programming
# integer-programming
# reserve-design
# optimal-management
# landuse
# property rights
full_bib_mod          <- read_rtf("base_modified.rtf")
#full_bib_mod          <- read_rtf("base_test_modified.rtf")
full_bib_mod          <- strsplit(full_bib_mod, "@")


####### B. Create dataset necessary for analysis ##############################################
##### i. Original data ######

data                  <- data.frame(0,0,0,0,0) # initiate data frame to unlist and regex 
colnames(data)        <- c('title',"abstract","keywords","index","collapse")
for(i in c(2:318)){
  data[i-1,1]         <- rm_between(full_bib[[1]][i], "title = {",'},', extract=T)[[1]]
  data[i-1,2]         <- rm_between(full_bib[[1]][i], 'abstract = {', '},', extract=TRUE)[1]
  data[i-1,3]         <- rm_between(full_bib[[1]][i], "keywords = {", "},", extract=T)[[1]]
  data[i-1,4]         <- i-1
} 
data[,3]              <- apply(data[,3, drop=F], 2, function(x) gsub("([-])|[[:punct:]]", "\\1",x, perl=T)) #remove ponctuation
data[,5]              <- apply(data, 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = " ")); #paste all columns into collapse
data[,5]              <- apply(data[,5, drop=F],2, function(x) gsub("([-])|[[:punct:]]", "\\1", x,perl=T)) #remove punctuation

data[114,5]           <- NA # Remove outlier 
data                  <- data[-324,]
text_paper            <- as.vector(data[,5]) #Vectorize data for VCorpus 
rm(full_bib)                                 #Remove heavy database
text_paper            <- tolower(text_paper)

##### ii. Modified data #####
data_mod              <- data.frame(0,0,0,0,0,0)
colnames(data_mod)    <- c('title',"abstract","keywords","index","collapse","year")
for(i in c(2:318)){
  data_mod[i-1,1]     <- rm_between(full_bib_mod[[1]][i], "title = {",'},', extract=T)[[1]]
  data_mod[i-1,2]     <- rm_between(full_bib_mod[[1]][i], 'abstract = {', '},', extract=TRUE)[1]
  data_mod[i-1,3]     <- rm_between(full_bib_mod[[1]][i], "keywords = {", "},", extract=T)[[1]]
  data_mod[i-1,4]     <- i-1
  data_mod[i,5] <-       rm_between(full_bib_mod[[1]][i], "year = {",'},', extract=T)[[1]]
  

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

text_paper_data <- as.data.frame(text_paper)%>% mutate(index = 1:length(text_paper),
                                                       text_paper = as.character(text_paper)) 
text_paper_data <- text_paper_data %>% tidytext::unnest_tokens(input='text_paper', output='words', 
                                                     token='words') %>%
  filter(!words %in% my_stopwords)%>%
  filter(!words %in% numbers)
## Important : 
print(length(unique(text_paper_data$words)))

freq_words      <- data.frame(word=text_paper_data$words)%>%
  group_by(word) %>% #Group to make sure each word is considered, not the whole sample
  dplyr::mutate(n = n()) %>%#Give the number of occurences of each word
  unique()%>% # Make sure there is no double occurences of words
  ungroup() %>% # We no longer consider each word
  arrange(-n)%>% # arrange 
  mutate(word=as.character(word))

boxplot(freq_words$n)
summary(freq_words$n)
freq_words      <- freq_words[1:last(which(freq_words$n==5)),]

print(paste0("We take into account ", (nrow(freq_words)/length(unique(text_paper_data$words))*100), " % of the text data"))


text_paper_data       <- text_paper_data[text_paper_data$words %in% freq_words$word,]
text_ee               <- c()
for(i in 1:317){
  text <- text_paper_data %>% subset(index==i)
  text <- paste(text$words, collapse=' ')
  text_ee <- c(text_ee,text) 
}
# Modified data ###
text_paper_data_mod <- as.data.frame(text_paper_mod)%>% mutate(index = 1:length(text_paper_mod),
                                                       text_paper_mod = as.character(text_paper_mod)) 
text_paper_data_mod <- text_paper_data_mod %>% tidytext::unnest_tokens(input='text_paper_mod', output='words', 
                                                     token='words') %>%
  filter(!words %in% my_stopwords)%>%
  filter(!words %in% numbers)

print(length(unique(text_paper_data_mod$words)))

freq_words_mod      <- data.frame(word=text_paper_data_mod$words)%>%
  group_by(word) %>% #Group to make sure each word is considered, not the whole sample
  dplyr::mutate(n = n()) %>%#Give the number of occurences of each word
  unique()%>% # Make sure there is no double occurences of words
  ungroup() %>% # We no longer consider each word
  arrange(-n)%>% # Sort the words in descending number of occurences
  mutate(word=as.character(word))
summary(freq_words_mod$n)
mod_distrib               <- as.data.frame(table(freq_words_mod$n))
mod_distrib %>% ggplot(aes(x=Var1,y=Freq))+geom_point()
freq_words_mod            <- freq_words_mod[1:last(which(freq_words_mod$n==5)),]

print(paste0("We take into account ", (nrow(freq_words_mod)/length(unique(text_paper_data_mod$words))*100), " % of the text data"))


text_paper_data_mod       <- text_paper_data_mod[text_paper_data_mod$words %in% freq_words_mod$word,]



text_ee_mod               <- c()
for(i in 1:317){
  text <- text_paper_data_mod %>% subset(index==i)
  text <- paste(text$words, collapse=' ')
  text_ee_mod <- c(text_ee_mod,text) 
}

##### i. Original data ######

data_text             <- VCorpus(VectorSource(text_paper)) #Data structure for the K-Means algorithm
data_text             <- tm_map(data_text, removeNumbers) # remove numbers from data
data_text             <- tm_map(data_text, removeWords, stopwords("english")) # remove stopwords, pre-specidifed and mine
data_text             <- tm_map(data_text, removeWords, my_stopwords)

# Alternative data
data_text             <- VCorpus(VectorSource(text_ee))
##### ii. Modified data #####
data_text_mod         <- VCorpus(VectorSource(text_paper_mod))
data_text_mod         <- tm_map(data_text_mod, removeNumbers)
data_text_mod         <- tm_map(data_text_mod, removeWords, stopwords("english"))
data_text_mod         <- tm_map(data_text_mod, removeWords, my_stopwords)

# Alternative data #####
data_text_mod         <- VCorpus(VectorSource(text_ee_mod))
####### D. Stem data ############################################################################
# Stemming data is cutting words like : optimize, optimum, optimal -> optim

data_text_stem        <- tm_map(data_text,stemDocument) 
data_text_mod_stem    <- tm_map(data_text_mod, stemDocument)

####### E. Document term matrix #################################################################
dtm_stem              <- DocumentTermMatrix(data_text_stem) # Matrix of crossed-occurences in a document
dtm_mod_stem          <- DocumentTermMatrix(data_text_mod_stem)
####### F. Distances - computed between documents in a #word-dimension space #############################################################################
distance_norm_stem    <- dist(dtm_stem)                     # Compute the distance matrix for words and articles ??? A 
distance_mod_stem     <- dist(dtm_mod_stem)

distance_norm_stem    <- as.matrix(distance_norm_stem)
distance_mod_stem     <- as.matrix(distance_mod_stem)
######### II. Analysis ############################################################################
####### A. Elbow technique for optimal number of clusters ########################################
### Idea : try to minimize the within-cluster sum of squares between each point and the mean.
# -> Increase number of clusters until the reduction in WSS is small

clusters <- read_xlsx("data_clusters.xlsx") #Load clusters from previous runs
if(exists("clusters")==F){ # Loop : if those clusters already exist, no need to waste time and energy
  
  
  
##########  i. Find the optimal point for original basis #################
wss <- 2:150                 # Compute wss for all potential clusters. The larger the number of clusters, 
# the lower the variance. Indeed, if n_clustres=322, there is a cluster for 
# each paper, hence no more intra-cluster variance.
for(i in 2:150){
  wss[i]              <- sum(kmeans(distance_norm_stem, centers=i, nstart=50)$withinss)
}
### Design of an optimal rule in a descent/convergence style to find optimal cluster number

optimal_cluster_og_stem  <- 2 # Initiate cluster number
difference               <- 1 # Initiate difference
while(difference>0.045 ){ #Set difference at 20% reduction in WSS as cluster grows.
  # If difference is larger, try to increase cluster number.
  difference                       <- abs((wss[optimal_cluster_og_stem+1]-wss[optimal_cluster_og_stem])/wss[optimal_cluster_og_stem+1])
  # Absolute value of the percentage difference in WSS between i and i+1 clusters
  optimal_cluster_og_stem          <- optimal_cluster_og_stem+1
  # Increase the cluster count
}
print("Optimal cluster with original and stemmed data")
print(optimal_cluster_og_stem) # Optimal cluster level for this difference in wss

wss            <-  data.frame(wss, 1:150, as.character("none"))
colnames(wss)  <- c("value","index","key")
wss$key        <- as.character(wss$key)
wss[optimal_cluster_og_stem,3]       <- "key"

wss_plot <- wss %>% subset(index>1) %>% ggplot(aes(x=index)) + geom_point(aes(y=value,color=key), size=0.5)+scale_color_manual(values=c("red","black"))+
  theme_bw() + xlab('Number of clusters') + ylab("Within sum of squares")+ theme(legend.position='none')
ggsave("wss_og_data.pdf", plot=wss_plot, units='cm',height=8, width=14)

##########  ii. Find the optimal point for modified basis #################
rm(i)
wss <- 2:150                 # Compute wss for all potential clusters
for(i in 2:150){
  wss[i]              <- sum(kmeans(distance_mod_stem, centers=i, nstart=30)$withinss)
}
# Plot the distribution to see if there is an elbow



optimal_cluster_mod_stem  <- 2 # Initiate cluster number
difference                <- 1 # Initiate difference
while(difference>0.045 ){ #Set difference at 20% reduction in clusters
  difference                       <- abs((wss[optimal_cluster_mod_stem+1]-wss[optimal_cluster_mod_stem])/wss[optimal_cluster_mod_stem+1])
  optimal_cluster_mod_stem          <- optimal_cluster_mod_stem+1
}
print("Optimal cluster modified and stemmed : ")
print(optimal_cluster_mod_stem) # Optimal cluster level for this difference in wss
print("We DONE!")

wss            <-  data.frame(wss, 1:150, as.character("none"))
colnames(wss)  <- c("value","index","key")
wss$key        <- as.character(wss$key)
wss[optimal_cluster_mod_stem,3]       <- "key"

wss_plot <- wss %>% subset(index>1) %>% ggplot(aes(x=index)) + geom_point(aes(y=value,color=key), size=0.5)+scale_color_manual(values=c("red","black"))+
  theme_bw() + xlab('Number of clusters') + ylab("Within sum of squares")+ theme(legend.position='none')
ggsave("wss_modified_data.pdf", plot=wss_plot, units='cm',height=8, width=14)

}else{ # If the optimal clusters already exists
  optimal_cluster_mod_stem <- as.numeric(clusters[1,2]) #Load as numeric the clusters
  optimal_cluster_og_stem  <- as.numeric(clusters[1,1])
  print("Clusters already loaded")
}
####### B. K-means clustering with optimal clusters     ############
# Aim : given a number k and a set of points, the problem is to divide the set in k groups through the minimization of a certain
# objective function. 
# What it does : 
# a. Assign randomly documents to a bin
# b. Compute the location of the centroid of the bin
# c. Compute the distance between each document and each centroid
# d. Assign each document to the closest centroid
# e. Stop if no document moves, if not, go to step b. 
####### C. K-means clustering with the optimal cluster number ######
kfit                  <- kmeans(distance_norm_stem, optimal_cluster_og_stem, nstart=100)
kfit_mod              <- kmeans(distance_mod_stem, optimal_cluster_mod_stem, nstart=100)


####### D. Retrieve class of each paper ################################################
article_cluster <- data.frame(0,0,0)

for(i in 1:length(kfit$cluster)){ #For each cluster
  article_cluster[i,2]    <- i      
  article_cluster[i,3]    <- kfit$cluster[[i]]
  article_cluster[i,4]    <- kfit_mod$cluster[[i]]
}
article_cluster[,1]       <- data$title
colnames(article_cluster) <- c("title", "id","cluster_og","cluster_mod")

df2_ready$cluster_og      <- article_cluster$cluster_og
df2_ready$cluster_mod     <- article_cluster$cluster_mod

article_cluster_mod <- data.frame(0,0,0,0)
for(i in 1:length(kfit_mod$cluster)){ #For each cluster
  article_cluster_mod[i,2]    <- i      
  article_cluster_mod[i,3]    <- kfit$cluster[[i]]
  article_cluster_mod[i,4]    <- kfit_mod$cluster[[i]]
}
article_cluster_mod[,1]       <- data_mod$title

####### E. Save data #######
write_xlsx(df2_ready, "data_cleared_post_NLPstep1.xlsx") #save data
optimal_clusters <- data.frame(optimal_cluster_og_stem,optimal_cluster_mod_stem)
write_xlsx(optimal_clusters,"data_clusters.xlsx")
write_xlsx(article_cluster, "article_cluster.xlsx")
write_xlsx(article_cluster_mod, "article_cluster_mod.xlsx")

