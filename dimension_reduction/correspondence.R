library(MASS)
library(ca)

caith.ru <- caith
row.names(caith.ru) <- abbreviate(c("голубоглазые",
                                    "сероглазые","кареглазые","черноглазые"), 10, method="both")
names(caith.ru) <- abbreviate(c("блондины","рыжие",
                                "русоволосые","шатены","брюнеты"), 10, method="both")

mytable <- caith.ru
fit <- ca(mytable)
print(fit) # basic results 
summary(fit) # extended results 
plot(fit) # symmetric map
plot(fit, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map
