
# Functions -------

#create grid
CreateReferrals <- function(min_x,max_x,specialty,growth,starting_value,jitter_factor){
  expand_grid('t'=min_x:max_x,'s'=specialty) %>%
    dplyr::mutate(open = jitter(starting_value * ((growth))^t,factor=jitter_factor)) %>%
    dplyr::mutate(i = -t)
} %>%
  as.data.frame()

#Function to output waitlist shape over time (by buckets of months waiting)
WaitList <- function(x,capacity,result,df_a,df_c){
  
  #Set as DT
  data.table::setDT(result)
  data.table::setDT(capacity)
  
  #pre-allocate empty list of length x
  final_data <- vector('list',x)
  
  #Where j
  for(j in 1:x){
    
    #cap i at 24

    result[i >= 24, i := 24]
    result <- result[,.(z=sum(z)),by=c('i','s')]
    
    #join on a
    result[as.data.frame(df_a),on=c('i','s'), a:=a]
    #join on c
    result[as.data.frame(df_c),on=c('i','s'), c:=c]
    #join on c
    result[capacity[t==j,],on=c('s'),cap:=cap]
    
    #Calc sum by group t, s
    result[i>=0, z_c := (z*c)]
    result[i>=0, z_sum := sum(z_c),by =c('s')] 

    result[i >= 0,
           d := ((z*c*cap) / (z_sum)) ]
    
    #Apply formula
    result[i >= 0,
           z := a * ( z - d ) ]
    
    #Add time period
    result[i >= -1,
           t := j]
    
    result$i <- result$i+1
  
    #allocate result appropriately
    final_data[[j]] <- result[i>=0,]
  }
  #return func
  return(final_data)
}

CreateCapacity <- function(x,specialties,growth,base_capacity){
  expand_grid(t=c(1:x),s=specialties) %>%
    dplyr::left_join(base_capacity,by=c('s')) %>%
    mutate(cap = cap*(growth)^t)
}


CreateData <- function(df_data,ref_growth,cap_growth,policy,jitter_factor,breach_limit,a_lim){
  
  df_z_2 <- df_data %>%
    dplyr::ungroup()%>%
    #position as at latest
    dplyr::filter(t == max(t)) %>%
    dplyr::mutate(t=0) %>%
    select(t,s,open,i) %>%
    add_row(
      CreateReferrals(1,sim_time,
                      specialty=specs,
                      growth=ref_growth,
                      starting_value=starting_average,
                      jitter_factor=jitter_factor)
    ) %>%
    rename(z='open')
  
  df_c <- df_data %>%
    dplyr::mutate(c_a = completed/(open+completed))%>%
    dplyr::group_by(i,s)
  
  # Fixed drop-off rates
  df_a <- df_data %>%
    dplyr::group_by(i,s) %>%
    dplyr::summarise(a = quantile(a,a_lim,na.rm=T)) %>%
    #this is the mother of all evil: 
    #the drop-off is intensely consequential.
    rbind(data.frame('i'=27,'a'=0.97,'s'=unique(df_2$s))) %>%
    dplyr::mutate(a = case_when(is.nan(a)==TRUE ~ 0.5,
                                T ~ a),
                  i=i-1) %>%
    dplyr::filter(i>=0)
  
  capacity <- CreateCapacity(x=sim_time,
                             specialties=specs,
                             growth=cap_growth,
                             base_capacity=base_capacity)
  
  df_c <- df_c %>%
    dplyr::summarise(
      c = quantile(c_a,policy,na.rm=T),
    )
  

  #Waitlist over time
  data<- WaitList(x = sim_time,
                  capacity=capacity,
                  result=df_z_2,
                  df_a=df_a,
                  df_c=df_c) %>%
    data.table::rbindlist()

  breaches <- data %>%
    mutate(
      breach_flag = case_when(
        i > breach_limit ~ 'breach',
        TRUE ~ 'not_breach')) %>%
    ungroup()%>%
    group_by(t,breach_flag) %>%
    summarise(z = sum(z)) %>%
    pivot_wider(values_from=z,names_from=breach_flag) %>%
    mutate(tot = breach+not_breach)%>%
    ungroup()
    
  return(list('breaches'=breaches,'capacity'=capacity))
  
}

