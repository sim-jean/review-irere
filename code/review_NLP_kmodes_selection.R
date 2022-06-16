set.seed(22)

lister                   <- list(c(4,45,185,149), c(294,47,137,215), c(11,190,239,98),c(128,102,121,210),
                                 c(80,48,53,112),c(51,299,33,66),c(122,258,259,271),c(62,296,258,205))
# Initial modes comment : 
# 1 - square with variables below 1 on the x axis, with 4 groups
# 2 - furthest papers (eyeball), shape of diamond
# 3 - Other combination of papers apart
# 4 - closest to origin 
# 5 - On the Y-axis close to 1, close to the origin on X-axis
# 6 - All close together on the right hand side
# 7 - All close together on the left side
# 8 - Two on each side, close to origin on the Y-axis
for(i in 1:length(lister)){
  initial_modes          <- df2_ready_mca2[lister[[i]],]
  try                    <- kmodes(df2_ready_mca2, initial_modes, iter.max=100)
  kmodes_result          <- as.factor(try$cluster)
  
  df2_ready_mca2                                 <- dplyr::select(df2_ready,variables2)
  df2_ready_mca2                                 <- cbind(df2_ready_mca2, df2_ready$keywords, df2_ready$keywords2, kmodes_result)%>%
                                                                                      dplyr::rename("keywords2" = "df2_ready$keywords2",
                                                                                            "keywords" = "df2_ready$keywords"
                                                                                            )
  df2_ready_mca2$keywords                                 <- as.factor(df2_ready_mca2$keywords)
  df2_ready_mca2$keywords2                                <- as.factor(df2_ready_mca2$keywords2)
  df2_ready_mca2$kmodes_result                            <- as.factor(df2_ready_mca2$kmodes_result)
  df2_ready_mca2$initial                                  <- "no"
  df2_ready_mca2[lister[[i]],ncol(df2_ready_mca2)]        <- "initial"
  df2_ready_mca2$initial                                  <- as.factor(df2_ready_mca2$initial)
  res.mca2                                                <- MCA(df2_ready_mca2, ncp=5, graph = F, quali.sup=c((ncol(df2_ready_mca2)-5):ncol(df2_ready_mca2)))
  graph <- fviz_mca_ind (res.mca2,
                      label = "ind", # masquer le texte des individus
                      habillage = kmodes_result,
                      #jitter = list(width = 0.2, height = 0.2),
                      addEllipses = TRUE, ellipse.type = "confidence",
                      ggtheme = theme_minimal (),
                      title = paste0("Initial modes #",i))
  assign(paste0("initial_modes_",i),graph)
}


######### Interpretation of results #####
# When starting points are close to the origin on the X-axis, there are 3 groups 
# on the left hand side, and 1 on the right (ie cases 3 and 4)

# With #1 : 2 groups on the right hand side, and 2 groups, almost identical, on the left
# hand side. 

# With #2 : 2 on each side, but with elevated variance. Resembles more the results
# we worked on. 

# With #6 : one cluster for conservation and 3 with same centroid on extinction

# With #7 : all clusters close to origin

# With #8 : two clusters on each side, resembles #2

# -> Problem : need a rationale to explain the selection of the initial modes. 
# Could have several different rationales for it : we look for 4 clusters, by
# 1 - Picking the furthest apart to avoid effects of inertia resulting from a concentration of variables close by
# 2 - Pick papers that at least have something to say (i.e not to close to the origin)
#     and that somhow represent the distribution of the papers (i.e, not only random picking of numbers, 
#     but selection depends on the distribution of papers on the two axis)

