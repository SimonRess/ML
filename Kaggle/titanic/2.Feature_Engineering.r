require(dplyr)
require(stringr)

split.Cabins = function(data) {
  data = data %>%
    mutate(cabin.nr = sapply(train$Cabin, 
                             \(x) str_sub(x,2,str_length(x)) %>% 
                               str_split(.," ") %>% 
                               unlist() %>% .[[1]]) %>% as.numeric(),
           cabin.area = str_sub(Cabin,1,1))  
  
  return(data)
}


create.family.size = function(data) {
  data = data %>%
    mutate(family.size = SibSp + Parch)
  
  return(data)
}




