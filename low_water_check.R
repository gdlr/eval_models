#' lowflowmetrics
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @return annual_min_cor


low_water_check = function(m,o, month, day, year,wy) {

  flow = cbind.data.frame(m,o, month, day, year,wy)
  # first lets get minimum yearly values
  
  tmp = flow %>% 
    filter(month == c(8, 9, 10)) %>% 
      group_by(wy) %>% summarize(sumo=sum(o), summ=sum(m))

    annual_min_cor = nse(tmp$summ, tmp$sumo)
  
  
  return(annual_min_cor=annual_min_cor)
}
