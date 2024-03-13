##Before running this script filter longitude, latitude and store fields 
## from the merged csv. 
## ten rows were filterd from the merge for this task

## This script uses the google places 



#libraries
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(googleway))

## read csv of places
search_locations <- read.csv("fortest.csv") %>%
  head(10)



# note: this is all of the possible destination types used by Google maps
place_types <- c("Subway",
                 "McDonalds",
                 "Dominoes",
                 "KFC",
                 "Hungry Jacks",
                 "Red Rooster",
                 "Nandos",
                 "Pizza Hut",
                 "Zambrero",
                 "Oporto",
                 "Grill’d",
                 "Guzman Y Gomez")


# read in the api key
key=read.table("api.txt")[1,1] %>%
  as.character()


## create an empty dataframe which will accept final lists after looping
resultsDF <- NULL
cat(paste0("querying the Google places API at ",nrow(search_locations)," search locations at ",Sys.time(),"\n"))



place_outputs = data.frame()


## Run a loop for every single row in dataframe using googleplaces to 
## query all 12 place types that fall within confines
##

for(i in 1:nrow(search_locations) ) {
  # for(i in 1:10 ) {
  id <- search_locations[i,]$store
  long <- search_locations[i,]$long %>%
    round(8)
  lat <- search_locations[i,]$lat %>%
    round(8)
  
  
  
  output = data.frame()
  out = lapply(place_types, function(x){
    token = 20
    while(token <= 60){
      if(token == 20){
        sub = google_places( search_string = x, 
                             location = c(lat, long),key= key)
      }else{
        err <- tryCatch(expr = {
          sub =  google_places( search_string = x, 
                                page_token = sub$next_page_token,
                                location = c(lat, long),key= key) }
          ,
          error=function(e) e, 
          warning=function(w) w)
        
        if(is(err, "warning")){
          sub = NULL
        }
      }
      
      if(!is.null(sub)){
        result = sub$results
        output = dplyr::bind_rows(output, result)
      }
      
      token = token + 20
    }
    placeTypes <- lapply(output$types,function(x) {paste(x,collapse='; ')}) %>%
      as.character()
    
    data.frame(address= output$formatted_address,
               lat= output$geometry$location$lat,
               long=output$geometry$location$lng,
               name=output$name,
               place_id=output$place_id,
               type=placeTypes)
  })
  
  resultsDF = do.call("rbind",out)
  resultDF_ = resultsDF[!duplicated(resultsDF),]
  
  place_outputs = bind_rows(place_outputs, resultDF_)
  cat("[INFO] ...completed location ", i, " of ", nrow(search_locations), "\n")
  
}

write.csv(place_outputs,"output_locationfirst_10.csv",row.names=F)



  