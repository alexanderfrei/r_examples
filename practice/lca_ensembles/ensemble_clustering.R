library(dplyr)

# латентые классы 
library(poLCA)


# кластерные ансамбли
library(clue)

data("Cassini")
set.seed(1234)
CKME <- cl_boot(Cassini$x, 50, 3)

