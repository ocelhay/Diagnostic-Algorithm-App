algo_run <- function(incidence_dis_1, incidence_dis_2){
  
  total_incidence <- incidence_dis_1 + incidence_dis_2
  
  return(data.frame("total_incidence" = total_incidence))
}