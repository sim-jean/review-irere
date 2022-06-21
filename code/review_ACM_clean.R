#########################################################################################################
######### SUPPLEMENTARY ANALYSIS : MCA ##################################################################
#########################################################################################################

#setwd("/Users/simonjean/Desktop/Thèse/Review_Lauriane/data")

rm(list=ls())

set.seed(123)
### 0. Package set-up ####
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

colors_review <- c("Extinction"="firebrick","Conservation"="gray66","Ecosystem"="dodgerblue1", "Full"="red")
colors_kmodes <- c('1'="darkorchid3", "2"="forestgreen", "3"="gold", "4"="black")
##################################################################################################################
#### I. Data cleaning & preparation ####
####### A. Import #####
df2_ready                                     <- read_excel("data/data_cleared.xlsx", skip = 0)
df2_ready                                      <- df2_ready %>% subset(Journal!="Environmental Modelling & Assessment")

####### B. Optimal variable selection procedure ############
######### i. Initiate potential variables #####
potential_variables                           <- c("bio_monet","main_method","biodiv_objective","partial_general",
                                                   "biodiv_direct","biodiv_functional_historical",
                                                   "biodiv_indicator_level","biodiv_proxy","species","paper_empirical",
                                                   "method_static","method_resolution","spatial_economy","spatial_ecology",
                                                   "positive_normative","ecological_model","stoch_ecol",
                                                   "stoch_econ")
#potential_variables                           <- c("bio_monet","main_method","biodiv_objective","partial_general",
#                                                   "biodiv_direct")

######### ii. Eyeball the variables distribution, just to check.  ####
for(i in potential_variables){
  print(i)
  print(table(df2_ready[,i]))
}


######### iii. Compute all potential variable combination and corresponding explained variance  ######
j <- 0
k <- 0
for(i in 4:length(potential_variables)){
  k <- choose(18,i)
  j <- j+k
}
all_potential_comb <- j
### a. check if the data already exists, and if not, compute it : 
optimal_variables_selection <- read_excel("data/optimal_variables_selection.xlsx")


k <- 0
rm(j,i)
if(exists("optimal_variables_selection")==F){
  potential_variables_variance <- list()    # Set list of potential variables
  
  for(i in 4:length(potential_variables)){
    C <- as.matrix(CombSet(potential_variables, i, repl=F, ord=F)) # Matrix of all the potential combinations, with no replacement or order
    data_loop   <- data.frame()      # Create storage dataframe
    for(j in 1:nrow(C)){             # For each combination of variables : 
      variables <- c(C[j,])            # Get the variables,
      data_ok   <- df2_ready %>% dplyr::select(variables) # Select the appropriate data,
      res.mca   <- MCA(data_ok, ncp=5, graph = F)  # Perform MCA
      variance  <- res.mca$eig[2,3]                # Get explained variance of the 2 main axis
      data_loop[j,1] <- paste(variables, collapse=" ") # Collapse variables into dataframe
      data_loop[j,2] <- variance                       # Store explained variance
      data_loop[j,3] <- i
      print(i)
      percent_adv    <- (j+k)/all_potential_comb
      print(percent_adv)
    }
    k          <- k+j
    potential_variables_variance[[i-3]] <-  data_loop     # Store dataframe in list
  }
  
  optimal_variables_selection <- data.frame() # Data set for terminal storage
  for(k in 1:length(potential_variables_variance)){
    optimal_variables_selection <- rbind(optimal_variables_selection, potential_variables_variance[[k]]) #row-bind each list element into data.frame
  }
  write_xlsx(optimal_variables_selection,"data/optimal_variables_selection.xlsx") # save
} else{
  print("Optimal variables selection has already been computed")
}

######### iv.  Enveloppe of the explained variances with various variable combinations ####
opti_var_plot_data <-  data.frame(0,0,0) #Initiate storage for data
for(i in 4:length(potential_variables)){
  dataa                        <- optimal_variables_selection %>% subset(V3==i) # select data for i variables
  index_max                    <- which(dataa$V2==max(dataa$V2)) # Find index of the max variance explained 
  opti_var_plot_data[i-3,] <- dataa[index_max,] # Get max data
}
opti_var_plot_data            <- cbind(opti_var_plot_data, "max") # Assign "max" sticker value
colnames(opti_var_plot_data)  <- c("variables","variance_explained","number_var",'min_max') #Rename columns

opti <- data.frame(0,0,0) #Initiate storage of data
for(i in 4:length(potential_variables)){
  dataa                        <- optimal_variables_selection %>% subset(V3==i) #Select data for i variables
  index_min                    <- which(dataa$V2==min(dataa$V2)) # Find index of min
  opti[i-3,]                   <- dataa[index_min,] # Get min data
}
opti                       <- cbind(opti, "min") # Assign "min" sticker
colnames(opti)             <- c("variables","variance_explained","number_var",'min_max') #Set colnames

opti_var_plot_data         <- rbind(opti_var_plot_data,opti) # row-bind the two datasets for tidy data

variance_variables         <- ggplot(opti_var_plot_data, aes(x=number_var))+geom_line(aes(y=variance_explained, color=min_max))+geom_point(aes(y=variance_explained, color=min_max), shape=2, size=2)+
  theme_classic() +
  labs(x = "Number of variables", y = "Explained Variance (%)", fill = NULL, title = " ", color=" ")
#ggsave("outputs/variables_variance_enveloppe.pdf",plot=variance_variables,units="cm", width=14, height=8)
# Plot enveloppe

all_variance_variables     <- ggplot(optimal_variables_selection, aes(x=V3,y=V2))+geom_point(size=0.5, shape=4)+ # Plot overall - points plus petits
  theme_classic() +
  labs(x = "Number of variables", y = "Explained Variance (%)", fill = NULL, title = " ", color=" ")
#ggsave("outputs/variables_variance_all.pdf", plot=all_variance_variables, units="cm", width=14, height=8)
######### v.   Variable selection ######
### Must have a minimal number of variables : 

# If too few, no need to project variables on 2 axis. For example, if several variables are highly 
# correlated, and keep only these, can have a large share of variability that is explained. 
# On the other hand, not tacking into account other variables avoids a source of variation, and thus
# and thus reduces the means to argue for various groups. 

variables                                     <- c("bio_monet","main_method","biodiv_objective","partial_general",
                                                   "biodiv_direct","biodiv_functional_historical", "biodiv_proxy", "positive_normative",
                                                   "biodiv_indicator_level","species","paper_empirical",
                                                   "method_static","method_resolution","spatial_economy","spatial_ecology",
                                                   "ecological_model","stoch_ecol",
                                                   "stoch_econ")
# Could remove : "historical_functional", "biodiv_proxy", "positive_normative",
# -> Variable selection based on heuristics as well as eyeballing : remove the variables with too skewed
# distribution


# Combination for most explained variance (while keeping the meaningful variable)
number_var                                     <- 14
variables2                                     <- opti_var_plot_data[which(opti_var_plot_data$number_var==number_var & opti_var_plot_data$min_max=="max"),1] # Get opttimal variable combination
variables2                                     <- unlist(strsplit(variables2, split= " "))
print(variables2)
print(paste0(" explained variance in MCA : ", opti_var_plot_data[which(opti_var_plot_data$number_var==number_var & opti_var_plot_data$min_max=="max"),2]))


df2_ready_mca2                                 <- dplyr::select(df2_ready, variables2)
write_xlsx(df2_ready_mca2, "data/data_MCA_auto.xlsx")

#### II. Run the MCA analysis ######
####### A. Run MCA ####

res.mca2                                      <- MCA(df2_ready_mca2, ncp=5, graph=T)
ind2                                          <- get_mca_ind(res.mca2)

# This is very informative on the distribution of the values of the variables, and hence on the articles : an article on the
# the right hand side of the axis more likely features value_a and value_b of var1 and var2, while on the left-hand side it 
# is value_b and value_A of var1 and var2.
fviz_screeplot (res.mca2, addlabels = TRUE, ylim = c (0, 45))


fviz_contrib(res.mca2, choice = "var", axes = 1, top = 15)
fviz_contrib(res.mca2, choice = "var", axes = 2, top = 15)
fviz_contrib(res.mca2, choice = "var", axes = 3, top = 15)


# Plot of individuals and variables on the 2-dimensional space made of axis that retain the most variance
# fviz_mca_biplot (res.mca2, repel = TRUE, 
#                  ggtheme = theme_minimal())
# Plot of explained variance by the different axis.
# fviz_screeplot (res.mca2, addlabels = TRUE, ylim = c (0, 45))

# What variables are the most correlated with each of the 2 axis 
# fviz_mca_var(res.mca2, choice="mca.cor",repel=T) 
# Note that this tells us what variables correspond to each axis, 
# but not how, as what it plotted is the square of the correlation. One must investigate how the values of the variables
# rank on the 2-axis scheme.

####### B. Analysis of results : variables ######

fviz_mca_var(res.mca2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal(), title= " ",
) + xlab('Dimension 1 : 20.9%')+ ylab('Dimension 2 : 10%')+labs(color="Contribution (squared cosine)")
#ggsave("outputs/mca_variables_automated.pdf", plot=last_plot(), units="cm", width=30, height=18)

fviz_mca_var(res.mca2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal(), title= " " ,
             axes = c(1,3)
) 
#ggsave("outputs/mca_variables_automated_axes1.3.pdf", plot=last_plot(), units="cm", width=30, height=18)


####### D. Individual articles ######

colors_review <- colors_review[-4]
fviz_mca_ind (res.mca2,
              label = "ind", # masquer le texte des individus
              habillage = as.factor(df2_ready$keywords),
              palette = colors_review,
              addEllipses = TRUE, ellipse.type = "confidence",
              ellipse.level=0.95,
              ggtheme = theme_minimal (), 
              title=" ")
#ggsave("outputs/mca_individuals_keywords.pdf", plot=last_plot(), unit="cm", width=40, height=22)

fviz_mca_ind (res.mca2,
              label = "ind", # masquer le texte des individus
              habillage = as.factor(df2_ready$keywords),
              palette = colors_review,
              addEllipses = TRUE, ellipse.type = "confidence",
              ellipse.level=0.95,
              ggtheme = theme_minimal (), 
              axes = c(1,3),
              title=" ")
#ggsave("outputs/mca_individuals_keywords_axes1.3.pdf", plot=last_plot(), unit="cm", width=40, height=22)

#### III. K-modes clustering on the data #####
library(klaR)

####### A. Optimal cluster number selection ####
withindiff <- read_excel("data/withindiff_kmodes.xlsx")
if(exists("withindiff")==F){
  within_diff <- 2:80

for(i in 2:80){
  set.seed(123)
  check                  <- kmodes(df2_ready_mca2, modes=i)$withindiff
  within_diff[i]         <- sum(check)
  print(check)
}

withindiff            <- data.frame(within_diff, 1:80, as.character("none"))
withtindiff           <- withindiff[-1,]
colnames(withindiff)  <- c("value","index","key")
withindiff$key        <- as.character(withindiff$key)
withindiff[4,3]       <- "key"
write_xlsx(withindiff,"withindiff_kmodes.xlsx")
} else {
  print("Already computed, time is saved!")
}
withindiff_plot <- withindiff %>% subset(index>=2) %>%ggplot(aes(x=index)) + geom_point(aes(y=value), size=0.5)+scale_color_manual(values=c("red","black"))+
  theme_bw() + xlab('Number of clusters') + ylab("Within sum of squares")+ theme(legend.position='none')
#ggsave("outputs/kmodes_elbow.pdf", plot=withindiff_plot, units='cm',height=8, width=14)


####### B. Initialize modes to study stability ####
source("code/review_NLP_kmodes_selection.R")

ggMarginal(initial_modes_1, type="histogram") # Not too bad - Ok
ggMarginal(initial_modes_2, type="histogram") # Not too bad either - Ok
ggMarginal(initial_modes_3, type="histogram") # NO

ggMarginal(initial_modes_4, type="histogram") # NO
ggMarginal(initial_modes_5, type="histogram") # NO
ggMarginal(initial_modes_6, type="histogram") # NO
ggMarginal(initial_modes_7, type='histogram') # NO
ggMarginal(initial_modes_8, type="histogram") # Ok
# Randomly pick in 1,3,8
#set <- c(1,3,8)

#initial_modes            <- as.vector(lister[[sample(set,1)]])
initial_modes            <- lister[[8]]
initial_modes            <- df2_ready_mca2[initial_modes,]
####### C. Run the analysis with favored initial mode ####    
try                      <- kmodes(df2_ready_mca2, initial_modes, iter.max=100) #? nstart?

initial_modes9           <- df2_ready_mca2[c(11,169,287,292,229,26,32,24,68),]
try9                     <- kmodes(df2_ready_mca2,initial_modes9, iter.max=100)
kmodes9                  <- as.factor(try9$cluster)
kmodes_result            <- as.factor(try$cluster)
df2_ready$kmodes_result  <- kmodes_result

write_xlsx(df2_ready, "data/data_cleared_post_ACM.xlsx")
 df2_ready$kmodes9        <- kmodes9
#### IV. Graphical representation ####
####### A. Analysis ####

df2_ready_mca2                                 <- dplyr::select(df2_ready,variables2)
df2_ready_mca2                                 <- cbind(df2_ready_mca2, df2_ready$keywords, kmodes_result) %>% dplyr::rename("keywords"="df2_ready$keywords")

df2_ready_mca2$keywords                        <- as.factor(df2_ready_mca2$keywords)
df2_ready_mca2$kmodes_result                   <- as.factor(df2_ready_mca2$kmodes_result)

res.mca2                                       <- MCA(df2_ready_mca2, ncp=5, graph = F, 
                                                      quali.sup=c(which(colnames(df2_ready_mca2)%in%c("keywords","kmodes_result")))) #c((ncol(df2_ready_mca2)-4):ncol(df2_ready_mca2))

res.mca9                                       <- MCA(df2_ready_mca2, ncp=5, graph=F)
####### B. Variables ####
fviz_mca_var(res.mca2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())
#ggsave("outputs/mca_var_automated_supplementary.pdf", plot=last_plot(), units="cm", width=30, height=18)

fviz_mca_var(res.mca2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal(), 
             axes = c(1,3))
#ggsave("outputs/mca_var_automated_supplementary_axes1.3.pdf", plot=last_plot(), units="cm", width=30, height=18)

####### C. Individuals #####
fviz_mca_ind (res.mca2,
              label = "ind", # masquer le texte des individus
              habillage = kmodes_result,
              palette = colors_kmodes,
              addEllipses = TRUE, ellipse.type = "confidence",
              ggtheme = theme_minimal (), title= " ")+ xlab('Dimension 1 : 20.9%')+ ylab('Dimension 2 : 10%')
#ggsave("outputs/mca_ind_automated_kmodes.pdf", plot=last_plot(), units="cm", width=30, height=18)

fviz_mca_ind (res.mca9,
              label = "ind", # masquer le texte des individus
              habillage= kmodes9,
              palette = "ucscgb",
              addEllipses = TRUE, ellipse.type = "confidence",
              ggtheme = theme_minimal (), title= " ")+ xlab('Dimension 1 : 20.9%')+ ylab('Dimension 2 : 10%')
#ggsave("outputs/mca_ind_automated_kmodes9.pdf", plot=last_plot(), units="cm", width=30, height=18)
fviz_mca_ind (res.mca2,
              label = "ind", # masquer le texte des individus
              habillage = kmodes_result,
              palette = colors_kmodes,
              addEllipses = TRUE, ellipse.type = "confidence",
              ggtheme = theme_minimal (), 
              axes = c(1,3))
#ggsave("outputs/mca_ind_automated_kmodes_axes1.3.pdf", plot=last_plot(), units="cm", width=30, height=18)


####### D. Compared kmodes and pre-specified groups ####
fviz_ellipses(res.mca2, c("kmodes_result", "keywords"),
              geom = "point", 
              palette=c(colors_kmodes, colors_review))
#ggsave("outputs/mca_automated_keywords_kmodes.pdf", plot=last_plot(), units="cm", width=30, height=18)

fviz_ellipses(res.mca2, c("kmodes_result", "keywords"),
              geom = "point", 
              palette=c(colors_kmodes, colors_review), 
              axes = c(1,3))
#ggsave("outputs/mca_automated_keywords_kmodes_axes1.3.pdf", plot=last_plot(), units="cm", width=30, height=18)

#### V. Temporal representation of kmodes-cluster ####
###### A. Data prep   #######

distrib_time                         <- data.frame()
distrib_time[1,1]                    <- 'Year'
distrib_time[1,2]                    <- 'Full'
distrib_time[1,3]                    <- 'cluster1'
distrib_time[1,4]                    <- 'cluster2'
distrib_time[1,5]                    <- "cluster3"
distrib_time[1,6]                    <- "cluster4"

cluster1                             <- df2_ready%>% subset(kmodes_result==1)
cluster2                             <- df2_ready%>% subset(kmodes_result==2)
cluster3                             <- df2_ready%>% subset(kmodes_result==3)
cluster4                             <- df2_ready%>% subset(kmodes_result==4)

for(i in 0:47){
  distrib_time[i+2,2]                <- sum(df2_ready$Year==i+1973)
  distrib_time[i+2,3]                <- sum(cluster1$Year==i+1973)
  distrib_time[i+2,4]                <- sum(cluster2$Year==i+1973)
  distrib_time[i+2,5]                <- sum(cluster3$Year==i+1973)
  distrib_time[i+2,6]                <- sum(cluster4$Year==i+1973)
  distrib_time[i+2,1]                <- 1973+i
}
d                                    <- distrib_time[,1:2]
d[,3]                                <- "Full"
d                                    <- d[-1,]
colnames(d)                          <- c("Year", "Value", "Group")

d2                                   <- distrib_time[,c(1,3)]
d2[,3]                               <- "cluster1"
d2                                   <- d2[-1,]
colnames(d2)                         <- c("Year", "Value", "Group")

d3                                   <- distrib_time[,c(1,4)]
d3[,3]                               <- "cluster2"
d3                                   <- d3[-1,]
colnames(d3)                         <- c("Year", "Value", "Group")


d4                                   <- distrib_time[,c(1,5)]
d4[,3]                               <- "cluster3"
d4                                   <- d4[-1,]
colnames(d4)                         <- c("Year", "Value", "Group")

d5                                   <- distrib_time[,c(1,6)]
d5[,3]                               <- "cluster4"
d5                                   <- d5[-1,]
colnames(d5)                         <- c("Year", "Value", "Group")

distrib_time                         <- rbind(d,d2,d3,d4 ,d5)
distrib_time$Value                   <- as.numeric(distrib_time$Value)
distrib_time[,4]                     <- distrib_time[,2]/sum(distrib_time$Value)

rm(d,d2,d3,d4,d5)

plot                                 <- distrib_time %>% ggplot(aes(x=Year,y=V4, group=Group))
plot+stat_smooth(method="loess",se=F, aes(colour=Group))
plot+geom_line(aes(color=Group))
plot+stat_smooth(method='lm',formula= y~poly(x,10),aes(colour=Group), se=F)+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x=element_text(angle =45, vjust=0))+ labs(title=" Distribution of articles in the database ", y="Frequence", x=" Year ")



time_distrib <- plot+stat_smooth(method='loess',aes(colour=Group),size=1, se=F)+scale_color_manual(values= c('cluster1'="darkorchid3", "cluster2"="forestgreen", 
                                                                                                             "cluster3"="gold", "cluster4"="gray75","Full"= "black")) +
  theme(axis.text.x=element_text(angle =45, vjust=0),
        plot.title = element_text(size = 20),
        panel.background = element_rect(fill = "white"))+ grids(linetype="solid")+
  labs(title=" ", y="Freq.", x="")+ylim(0,0.02)

#ggsave("outputs/temporal_distribution_kmodes.pdf", plot=time_distrib, units="cm",width=30, height=18)


