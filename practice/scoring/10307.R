file = "W:/iMQ/X_Marketing/!Projects/=DANONE/Danone#10307_Actimel_segmentation/scoring/scoring.csv"
df = read.csv(file, sep=',')

library(dplyr)

df = df %>% select(-var34)
colnames(df)[1] = "segm"


df.1 = df.2 = df.3 = df
df.1$segm = ifelse(df.1$segm == 1, 1, 0)
df.2$segm = ifelse(df.2$segm == 2, 1, 0)
df.3$segm = ifelse(df.3$segm == 3, 1, 0)

model.1 <- glm(segm ~ ., data=df.1, family=binomial)
model.2 <- glm(segm ~ ., data=df.2, family=binomial)
model.3 <- glm(segm ~ ., data=df.3, family=binomial)



coefs = cbind(scale(exp(coef(model.1))), 
              scale(exp(coef(model.2))), 
              scale(exp(coef(model.3)))
              )
coefs



