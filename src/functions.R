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
  #Where j
  for(j in 1:x){
    
    #cap i at 26
    result <- result %>%
      dplyr::mutate(i = case_when(i > 26 ~ 26,
                                  TRUE ~ i)) %>%
      dplyr::group_by(i) %>%
      dplyr::summarise(z = sum(z,na.rm=T))
    
    #apply formula
    result['z'][result$i>=0,] <-
      #first multiple everything by ta
      (df_a['a']*
      #First term: z(i) * a(i)
      ((result['z'][result$i>=0,]) - 
      #second term: theta(i)*c
      (result['z'][result$i>=0,]*df_c['c']*cap_el)/(sum(result['z'][result$i>=0,]*df_c['c']))))
    if(j == x){result$i <- result$i}else{result$i <- result$i + 1}
  }
  return(
    list(
      #return list of elements
      l = data.frame('i'=0:26,'z'=result['z'][result$i>=0,],t=x)
      
    ))
}

#Returns wait times, wait list size, and percent breaches
WaitTimes <- function(x,cap_el,result,breach,df_a,df_c){
  
  for(j in 1:x){
    
    #cap i at 26
    result <- result %>%
      dplyr::mutate(i = case_when(i > 26 ~ 26,
                                  TRUE ~ i)) %>%
      dplyr::group_by(i) %>%
      dplyr::summarise(z = sum(z,na.rm=T))
    
    #apply formula
    result['z'][result$i>=0,] <-
      #first multiple everything by ta
      (df_a['a']*
         #First term: z(i) * a(i)
         ((result['z'][result$i>=0,]) - 
            #second term: theta(i)*c
            (result['z'][result$i>=0,]*df_c['c']*cap_el)/(sum(result['z'][result$i>=0,]*df_c['c']))))
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

