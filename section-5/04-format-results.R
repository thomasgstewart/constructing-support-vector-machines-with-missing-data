require(data.table)
require(dplyr)
require(dtplyr)
source("rfuncs.txt")

mt_map <- data.frame(
  levels = 1:4
  , labels = c("MNAR X","MAR X","MNAR XY","MAR XY")
  , stringsAsFactors = FALSE
)

beta_map <- data.frame(
  levels = c(-5, -1, 0, 1, 5)
  , labels = c("-5", "-1", "\\phantom{-}0","\\phantom{-}1", "\\phantom{-}5")
  , stringsAsFactors = FALSE
)

ar <- readRDS("./results/all-results.RDS") %>% 
  rename(Predictors = D) %>% 
  mutate(`Bayes risk` = 1 - signal) %>% 
  mutate(`Missing type` = levelmap(missing_type, mt_map, type = "c")) %>% 
  mutate(`Missing prop` = missing_pct) %>% 
  mutate(`$\\beta$` = levelmap(beta, beta_map, type = "f")) %>% 
  arrange(Predictors, `Bayes risk`, `Missing prop`, `Missing type`, beta)
  

sf <- function(x){
  out <- list()
  out$columns = 2
  out$format = "rl"
  iqr <- quantile(x*100,c(.25,.5,.75))
  if(is.na(iqr[1])) browser()
  out$out <- sprintf("%2$ 1.1f&[%1$1.1f, %3$1.1f]", iqr[1], iqr[2], iqr[3])
  return(out)
}



for(p in c(2,20,200)){
  
  xyz_summary_table(
    as.data.frame(ar) %>% 
      filter(Predictors == p) #%>%
      #filter(beta != 0)
    , x_variables = c("Bayes risk","Missing prop","$\\beta$")
    , y_variable = "Missing type"
    , z_variable = "peao"
    , summary_function = sf
    , file_name = "big-table-p" %|% p %|% ".tex"
  )
  
  
  xyz_summary_table(
    as.data.frame(ar) %>% 
      filter(Predictors == p) #%>%
    #filter(beta != 0)
    , x_variables = c("Bayes risk","Missing prop","$\\beta$")
    , y_variable = "Missing type"
    , z_variable = "peao"
    , summary_function = function(x){tikz_bar_iqr3(100*x,max = 50)}
    , file_name = "tikz-big-table-p" %|% p %|% ".tex"
  )
}


sp <- function(x){browser()}



plot(x[[2]], x[[1]])

for(br in unique(ar$`Bayes risk`)){
  xyz_summary_table(
    as.data.frame(ar) %>% 
    filter(`Bayes risk` == br) #%>%
    #filter(beta != 0)
    , x_variables = c("Bayes risk","Missing prop","beta","Predictors")
    , y_variable = "Missing type"
    , z_variable = c("peao")
    , summary_function = function(x){tikz_bar_iqr2(x,max = .5)}
    , file_name = "tikz_bar" %|% (br*100) %|% ".tex"
  ) 
}

xyz_summary_table(
  as.data.frame(ar) %>% 
    filter(`Bayes risk` == 0.25) %>%
    filter(`Missing prop` == 0.4)
  , x_variables = c("Bayes risk","Missing prop","beta","Predictors")
  , y_variable = "Missing type"
  , z_variable = c("peao")
  , summary_function = function(x){tikz_bar_iqr2(100*x,max = 50)}
  , file_name = "tikz_bar_predictors.tex"
) 





for(p in c(2,20,200)){
  xyz_summary_table(
    as.data.frame(ar) %>% 
      filter(Predictors == p)#%>% 
      #filter(`Bayes risk` == br) #%>%
    #filter(beta != 0)
    , x_variables = c("Bayes risk","beta","Missing prop")
    , y_variable = "Missing type"
    , z_variable = c("peao")
    , summary_function = function(x){tikz_bar_iqr2(x,max = .5)}
    , file_name = "tikz_bar" %|% p %|% ".tex"
  ) 
}


xyz_summary_table(
  as.data.frame(ar) %>% 
    filter(`Bayes risk` == min(unique(ar$`Bayes risk`))) %>% 
    filter(beta %in% c(-5, 0))
  #filter(`Bayes risk` == br) #%>%
  #filter(beta != 0)
  , x_variables = c("Bayes risk","Predictors","beta","Missing prop")
  , y_variable = "Missing type"
  , z_variable = c("peao")
  , summary_function = function(x){tikz_bar_iqr2(100*x,max = 50)}
  , file_name = "tikz_bar_missingprop.tex"
)







