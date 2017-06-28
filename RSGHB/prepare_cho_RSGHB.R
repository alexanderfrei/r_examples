prepare.cho.rsghb <- function(file, none=T, cont_list=NULL) {
  
  # id task item att1_1 att1_2 att2_1 .. cho
  
  library(stringr)
  library(dummies)
  library(dplyr)
  library(tidyr)

  data <- read.csv(file, sep=';')
  colnames(data)[1:3] = c("ID","task","item")
  
  cont_list <- cont_list
  cont_att = NULL
  if (length(cont_list)>0){
    cont_att <- paste("att", cont_list, sep = "")
  }
  
  cols <- colnames(data)
  one_hot <- cols[grep("att.*",cols)]
  one_hot <- one_hot[!one_hot %in% cont_att]
  
  data <- as.data.frame(dummy.data.frame(data, names=one_hot, sep="_"))
  data[, grepl(".*_0", colnames(data))] = NULL 

  colnames <- colnames(data)

  att <- colnames[grep("att._.*",colnames)]
  drop_match <- str_match_all(att, "(att.)(.*)")

  to_drop <- NULL

  for (i in 2:length(drop_match)){

    cur_var <- unlist(drop_match[i-1])[2]
    next_var <- unlist(drop_match[i])[2]
    if (cur_var != next_var) {
      to_drop <- c(to_drop, unlist(drop_match[i-1])[1])
    }
    if (i == length(drop_match)) {
      to_drop <- c(to_drop, unlist(drop_match[i])[1])
    }
  }

  attrib = data %>% as.data.frame %>% select(-cho)

  if (none) {
    attrib['none'] = as.numeric(attrib$item == max(attrib$item))
  }


  cho = data %>% select(ID, task, item, cho)
  # cho = cho %>% spread(item, cho)
  # cho = cho[, -c(1,2)]

  # return
  list("cho" = cho,
       "att" = attrib,
       "zero.levels" = to_drop, 
       "cont.attr" = cont_att 
       )
}

