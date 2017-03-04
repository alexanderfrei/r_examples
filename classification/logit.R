l <- read.table("data/logit.txt")
l.logit <- glm(data=l, V2 ~ V1, family=binomial)
summary(l.logit)

pokaz <- read.table("data/pokaz.txt")
pokaz.logit <- glm(data=pokaz, V3 ~ ., family=binomial)
summary(pokaz.logit)
