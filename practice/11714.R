library(gplots)  
library(psych)
library(zoo)
library(mice)
library(lavaan)
library(dplyr)
library(semTools)
library(semPlot)
library(car)
library(corrplot)

df <- read.csv("data/11714.csv", sep=";")
sem.df <- df %>%
  filter(client_group < 6 ) %>% 
  select(client_group, matches("^r7_|^r10_|^r12_|^r14_|^r18_|^r21_|^p4_")) %>%
  select(num_range("r7_b_", 1:8), num_range("r7_d_", 9:10),
         num_range("r10_", 1:5),
         num_range("r12_b_", 1:5), num_range("r12_d_", 6:7),
         num_range("r14_b_", 1:7),
         num_range("r18_b_", 1:4), num_range("r18_d_", 5:6),
         num_range("r21_b_", 1:6), num_range("r21_d_", 7:11),
         num_range("p4_b_", 1:11), p4_d_12)

sem.df <- sem.df %>% 
  rename(r10_b_1=r10_1, r10_b_2=r10_2, r10_b_3=r10_3, r10_b_4=r10_4, 
         r10_b_5=r10_5)
  
# recode
feat <- names(sem.df)
drivers <- grep("._d_.",feat)
barriers <- grep("._b_.",feat)
sem.df[,drivers] <- apply(sem.df[,drivers], 2, 
                          function(x) {car::recode(x,"3:4=3;2=2;1=1;99=NA")}) 
sem.df[,barriers] <- apply(sem.df[,barriers], 2, 
                           function(x) {car::recode(x,"3:4=1;2=2;1=3;99=NA")}) 

# missings
# pmm.df <- mice(sem.df, method = "pmm", maxit = 10)
# sem.df <- na.aggregate(sem.df)

model <- "
block1 =~ r7_b_1 + r7_b_2 + r7_b_3 + r7_b_4 + r7_b_5 + r7_b_6 + r7_b_7 + r7_b_8 + r7_d_9 + r7_d_10 + 
r10_b_1 + r10_b_2 + r10_b_3 + r10_b_4 + r10_b_5 +
r12_b_1 + r12_b_2 + r12_b_3 + r12_b_4 + r12_b_5 + r12_d_6 + r12_d_7
block2 =~ r14_b_1 + r14_b_2 + r14_b_3 + r14_b_4 + r14_b_5 + r14_b_6 + r14_b_7
block3 =~ r18_b_1 + r18_b_2 + r18_b_3 + r18_b_4 + r18_d_5 + r18_d_6
block4 =~ r21_b_1 + r21_b_2 + r21_b_3 + r21_b_4 + r21_b_5 + r21_b_6 + r21_d_7 + r21_d_8 + r21_d_9 + r21_d_10 + r21_d_11
block5 =~ p4_b_1 + p4_b_2 + p4_b_3 + p4_b_4 + p4_b_5 + p4_b_6 + p4_b_7 + p4_b_8 + p4_b_9 + p4_b_10 + p4_b_11 + p4_d_12
all =~ block1 + block2 + block3 + block4 + block5
"

# fit <- sem(model, data=sem.df, Std.lv=F)
# fit <- sem(model, data=sem.df, missing='fiml', Std.lv=F)

########################################################################

b5 <- sem.df %>% select(contains("p4"))

b5$na <- apply(b5, 1, function(x) sum(is.na(x)))
b5 <- b5 %>% filter(b5$na < 12) %>% select(-na)

# b5 <- na.aggregate(b5)
imp <- mice(b5, method = "pmm", maxit = 10)
b5 <- complete(imp)

corrplot.mixed(corr=cor(b5, use="complete.obs"),
               diag = "n",
               tl.pos="lt", 
               tl.col="black",
               col = colorpanel(5, "blue", "grey", "red"))

model.b5.1 <- "
# measurements
group1 =~ p4_b_1 + p4_b_2 + p4_b_3 + p4_b_4
group2 =~ p4_b_5 + p4_b_6 + p4_b_7 + p4_b_8
group3 =~ p4_b_9 + p4_b_10 + p4_b_11
block5 =~ group1 + group2 + group3
# regressions
"

model.b5.2 <- "
# measurements
group1 =~ p4_b_1 + p4_b_2 + p4_b_3 + p4_b_4
group2 =~ p4_b_5 + p4_b_6 + p4_b_7 + p4_b_8
group3 =~ p4_b_9 + p4_b_10 + p4_b_11
block5 =~ group1 + group2 + group3 + p4_d_12
# regressions
"

model.b5.3 <- "
# measurements
group1 =~ p4_b_1 + p4_b_2 + p4_b_3 + p4_b_4
group2 =~ p4_b_5 + p4_b_6 + p4_b_7 + p4_b_8
group3 =~ p4_b_9 + p4_b_10 + p4_b_11
block5 =~ group1 + group2 + group3 + p4_d_12
# regressions

# residuals cov
group1 ~~ 0*group2 + 0*group3 + 0*p4_d_12
group2 ~~ 0*group3 + 0*p4_d_12
group3 ~~ 0*p4_d_12
"

fit.b5.1 <- sem(model.b5.1, data=b5, Std.lv=T)
fit.b5.2 <- sem(model.b5.2, data=b5, Std.lv=T)
fit.b5.3 <- sem(model.b5.3, data=b5, Std.lv=T)
fit.b5.4 <- sem(model.b5.1, data=b5, Std.lv=T, meanstructure = TRUE)
pdf("output/11714/block5.pdf", width=15)
semPaths(fit.b5.1, what="std", fade=T, residuals=FALSE, 
         edge.width = 0.3, edge.color="black", edge.label.cex = 0.8, 
         nCharNodes=7, sizeMan = 5, sizeLat = 7, label.prop = 0.8)
dev.off()
summary(fit.b5.4, fit.measures=TRUE)

AIC(fit.b5.1,fit.b5.2,fit.b5.3,fit.b5.4)
coef(fit.b5.1)



########################################################################

