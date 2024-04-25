# Functions -------

#create grid
CreateReferrals <- function(min_x,max_x,specialty,growth,starting_value){
  expand_grid('t'=min_x:max_x,'s'=specialty) %>%
    dplyr::mutate(open = (starting_value * (jitter(growth))^t)) %>%
    dplyr::mutate(i = -t)
}

#Function to output waitlist shape over time (by buckets of months waiting)
WaitList <- function(x,cap_el,result,df_a,df_c){
  
  #Set as DT
  data.table::setDT(result)

  #Where j
  for(j in 1:x){
    
    #cap i at 26

    result[i >= 26, i := 26]
    result <- result[,.(z=sum(z)),by=c('i','s')]
    
    #join on a
    result[as.data.frame(df_a),on=c('i','s'), a:=a]
    #join on c
    result[as.data.frame(df_c),on=c('i','s'), c:=c]
    #join on c
    result[as.data.frame(cap),on=c('s'), cap:=cap]
    
    #Calc sum by group t, s
    result[i>=0,z_c := (z*c)]
    result[i>=0,z_sum := sum(z_c),by =c('s')] 

    #Apply formula
    result[i >= 0,
           z := a * ( z - ((z*c*cap) / (z_sum) ))]
    
    #Add time period
    result[i >= 0,
           t := j]
    
  if(j == x){result$i <- result$i}else{result$i <- result$i + 1}
  }
  return(
    list(
      #return list of elements
      l = result[i>=0]
    ))
}

#Returns wait times, wait list size, and percent breaches
WaitTimes <- function(x,cap_el,result,breach,df_a,df_c){
  
  #Set as DT
  data.table::setDT(result)
  
  #Where j
  for(j in 1:x){
    
    #cap i at 26
    
    result[i >= 26, i := 26]
    result <- result[,.(z=sum(z)),by=c('i','s')]
    
    #join on a
    result[as.data.frame(df_a),on=c('i','s'), a:=a]
    #join on c
    result[as.data.frame(df_c),on=c('i','s'), c:=c]
    #join on c
    result[as.data.frame(cap),on=c('s'), cap:=cap]
    
    #Calc sum by group t, s
    result[,z_c := (z*c)]
    result[i>=0,z_sum := sum(z_c),by =c('s')] 
    
    #Apply formula
    result[i >= 0,
           z := a * ( z - ((z*c*cap) / (z_sum) ))]
    
    #Add time period
    result[i >= 0,
           t := j]
    
    if(j == x){result$i <- result$i}else{result$i <- result$i + 1}
  }
  return(
    list(
      #number of breaches
      b = sum(result['z'][result$i>=breach,]),
      #number of waiters
      w = sum(result['z'][result$i>=0,]),
      #average wait times (w)
      r = (sum(result['z'][result$i<=25,]*c(1:26))*(4.33))/sum(result['z'][result$i<=25,])
    ))
}

CreateCapacity <- function(x,specialties,growth,base_capacity){
  expand_grid(t=c(1:x),s=specialties) %>%
    dplyr::left_join(base_capacity,by=c('s')) %>%
    mutate(cap = cap_lq*(growth)^t)
}
