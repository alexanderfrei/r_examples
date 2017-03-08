vybory <- read.table("data/vybory.txt", h=TRUE)

attach(vybory)
DOLJA <- cbind(KAND.1, KAND.2, KAND.3) / IZBIR
JAVKA <- (DEJSTV + NEDEJSTV) / IZBIR
cor(JAVKA, DOLJA)
lm.1 <- lm(KAND.1/IZBIR ~ JAVKA)
lm.2 <- lm(KAND.2/IZBIR ~ JAVKA)
lm.3 <- lm(KAND.3/IZBIR ~ JAVKA)
lapply(list(lm.1, lm.2, lm.3), summary)

plot(KAND.3/IZBIR ~ JAVKA, xlim=c(0,1), ylim=c(0,1), 
     xlab="Явка", ylab="Доля проголосовавших за кандидата")
points(KAND.1/IZBIR ~ JAVKA, pch=2)
points(KAND.2/IZBIR ~ JAVKA, pch=3)
abline(lm.3)
abline(lm.2, lty=2)
abline(lm.1, lty=10)
legend("topleft", lty=c(10,2,1),
       legend=c("Кандидат 1","Кандидат 2","Кандидат 3"))
detach(vybory)

vybory2 <- cbind(JAVKA, stack(data.frame(DOLJA)))
names(vybory2) <- c("javka","dolja","kand")
ancova.v <- lm(dolja ~ javka * kand, data=vybory2)
summary(ancova.v)
