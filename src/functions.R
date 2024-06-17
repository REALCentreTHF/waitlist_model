library(docstring)

# Functions -------

#create grid
CreateReferrals <- function(min_x,max_x,specialty,growth,starting_value,jitter_factor){
  expand_grid('t'=min_x:max_x,'s'=specialty) %>%
    dplyr::mutate(open = jitter(starting_value * ((growth))^t,factor=jitter_factor)) %>%
    dplyr::mutate(i = -t)} %>%
  as.data.frame()

#Function to output waitlist shape over time (by buckets of months waiting)
WaitList <- function(x,result,df_cap,df_a,df_c){
  
  #' This function simulates the amount of patients on the
  #' Elective Waitlist by waiting bucket over time based on 
  #' treatment prioritisation weights and drop-off assumptions.
  #' @param x integer Length of simulation in months
  #' @param result dataframe Dataframe of wait times by bucket
  #' @param df_cap dataframe Dataframe of capacity growth over time
  #' @param df_a dataframe Dataframe of drop-off by buckets
  #' @param df_c dataframe Dataframe of treatment assumptions
  
  data.table::setDT(result)
  data.table::setDT(df_cap)
  
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
    result[df_cap[t==j,],on=c('s'),cap:=cap]
    
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

CreateData <- function(df_data,ref_growth,capacity,policy,jitter_factor,breach_limit,a_lim){
  
  df_z_2 <- df_data %>%
    dplyr::ungroup()%>%
    #position as at latest (2024/03)
    dplyr::filter(t == 35) %>%
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
  
  df_c <- df_c %>%
    dplyr::summarise(
      c = quantile(c_a,policy,na.rm=T),
    )
  
  #Waitlist over time
  data<- WaitList(x = sim_time,
                  df_cap=capacity,
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
    
  return(list('breaches'=breaches,'capacity'=capacity,'full_data'=data))
  
}

#Function to generate productivity changes across time
CreateIndex <- function(w,d,r,prod,pay,drug,deflator){
  1 + (( (w*(pay-prod)) + (r*(drug-prod)) + (d*(1-prod) )))
}

GetBreachRatio <- function(c_growth, referrals,breach_limit){
  
  capacity <- CreateCapacity(x=sim_time,
                             specialties=specs,
                             growth=c_growth, #this is what needs to be fixed.
                             base_capacity=base_capacity)
  
  #Waitlist over time
  data <-  WaitList(x = sim_time,
                    df_cap=capacity,
                    result=referrals,
                    df_a=df_a,
                    df_c=df_c) %>%
    data.table::rbindlist()
  
  ratio <- sum(data[i >= breach_limit & t == max(t)]$z)/sum(data[i>=0 & t == max(t)]$z)
  
  return(ratio)
  
}

GetTotalWaitlist <- function(c_growth){
  
  capacity <- CreateCapacity(x=sim_time,
                             specialties=specs,
                             growth=c_growth, #this is what needs to be fixed.
                             base_capacity=base_capacity)
  
  #Waitlist over time
  data<- WaitList(x = sim_time,
                  df_cap=capacity,
                  result=df_z_2,
                  df_a=df_a,
                  df_c=df_c) %>%
    data.table::rbindlist()
  
  breaches <- data %>%
    group_by(t) %>%
    summarise(z = sum(z)) %>%
    filter(t == max(t))
  
  return(breaches$z)
}

SimulatePatients <- function(sim_n,risk_lambda,sigma_matrix,means){
  
  #' This function simulates elective patients by sex, age, severity and risk appetite
  #' 
  #' @description
    #' This function simulates patients needed for the underlying matrix
    #'  #' generating a data.table of patients by id containing age, severity, 
    #'  risk appetite,sex and deprivation.
    #'  #' model using a set of risk factors and a covariance matrix of
    #'  #' deprivation, age, and severity where sex is assumed a 50-50 chance. 
  #' @param sim_n integer Number of simulated patients
  #' @param risk_lambda integer Lambda variable for poisson distribution of risk where default is set to 4
  #' @param sigma_matrix matrix Covariance matrix of severity, age and deprivation
  #' @param means vector Vector of means 

  if(missing(risk_lambda)){
    risk_lambda <- 4
  } else {
    risk_lambda
  }
  
  #Sex is assumed independent for now, 50-50 chance
  sex <- sample(x=c('M','F'),size=sim_n,replace=T)
  #Risk appetite can either be assumed related to deprivation, sev? here rand
  risk_appetite <- rpois(n = sim_n, lambda = risk_lambda)
  
  #correlations
  sim_data_1  <- MASS::mvrnorm(n = sim_n, 
                               mu = means, 
                               Sigma = sigma_matrix)
  
  #chosen columns
  colnames(sim_data_1) <- c("deprivation", "age", "severity");
  #create pat id col
  sp <- data.table::data.table(
    risk_appetite,
    sex,
    sim_data_1
  )

  #generate id
  sp[,id := paste0('pat_',ids::random_id(sim_n,4))]
  
  return(sp)
}

SimulatePatients <- function(sim_n,risk_lambda,sigma_matrix,means){
  
  #' This function simulates elective patients by sex, age, severity and risk appetite
  #' 
  #' @description
  #' This function simulates patients needed for the underlying matrix
  #'  #' generating a data.table of patients by id containing age, severity, 
  #'  risk appetite,sex and deprivation.
  #'  #' model using a set of risk factors and a covariance matrix of
  #'  #' deprivation, age, and severity where sex is assumed a 50-50 chance. 
  #' @param sim_n integer Number of simulated patients
  #' @param risk_lambda integer Lambda variable for poisson distribution of risk where default is set to 4
  #' @param sigma_matrix matrix Covariance matrix of severity, age and deprivation
  #' @param means vector Vector of means 
  
  if(missing(risk_lambda)){
    risk_lambda <- 4
  } else {
    risk_lambda
  }
  
  #Sex is assumed independent for now, 50-50 chance
  sex <- sample(x=c('M','F'),size=sim_n,replace=T)
  #Risk appetite can either be assumed related to deprivation, sev? here rand
  risk_appetite <- rpois(n = sim_n, lambda = risk_lambda)
  
  #correlations
  sim_data_1  <- MASS::mvrnorm(n = sim_n, 
                               mu = means, 
                               Sigma = sigma_matrix)
  
  #chosen columns
  colnames(sim_data_1) <- c("deprivation", "age", "severity");
  #create pat id col
  sp <- data.table::data.table(
    risk_appetite,
    sex,
    sim_data_1
  )
  
  #generate id
  sp[,id := paste0('pat_',ids::random_id(sim_n,4))]
  
  return(sp)
}
