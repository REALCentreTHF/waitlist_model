# Functions -------

#Gets links from any single url; string matches
GetLinks <- function(url_name,string){
  files <- c()
  #this is inefficient and bad practice but it's a small vector.
  for(i in seq_along(url_name)){
    pg <- rvest::read_html(url_name[i])
    pg<-(rvest::html_attr(rvest::html_nodes(pg, "a"), "href"))
    files <- c(files,pg[grepl(string,pg)])
    files <- files %>% unique()
  }
  return(files)
}

#Read all csvs from urls; unz for zips
UnzipCSV <- function(files){
  #creates temp file to read in the data
  temp <- tempfile()
  download.file(files,temp)
  #This is needed because a zip file may have multiple files
  file_names <- unzip(temp,list=T)$Name
  data<- lapply(file_names,
                function(x){
                  da <- data.table::fread(unzip(temp,x))
                  #janitor to clean unruly names
                  names(da) <- names(da) %>% janitor::make_clean_names()  
                  return(da)
                })
  #unlink the temp file, important to do
  unlink(temp)
  data}

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

