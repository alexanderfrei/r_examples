pkgs = c("arules","arulesViz","MASS","dplyr","mlogit","ChoiceModelR","vcdExtra","vcd","coefplot","ggplot2","cluster","mclust","e1071","pvclust","dplyr","ISLR","cluster","Rtsne","ggplot2","psych","FactoMineR","factoextra","gplots","graphics","corrplot","heplots","nFactors","GPArotation","RColorBrewer","semPlot","car","corrplot","gplots","lattice","rworldmap","car","gpairs","portfolio","treemap","htmlwidgets","forecast","lme4","nlme","MCMCpack","extrafont","igraph","png","network","animation","visNetwork","ndtv","maps","geosphere","gplots","zoo","mice","lavaan","semTools","plyr","car","Rtsne","psych","dbscan","dummies","plyr","rpart","rpart.plot","ape","WriteXLS","grid","RSGHB","data.table","bayesm","BayesLCA","poLCA","clue","coda","tidyr","stringr","semPLS","multcomp","BayesFactor","ggplot2y","rattle","randomForest")
for (pkg in pkgs){
  if (! pkg %in% rownames(installed.packages())){
    install.packages(pkg)
  }
}