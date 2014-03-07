library(XML)
source("tomkit.R")

## Load Data Here
responses <- read.csv('./data/student_responses_test.csv')

## Build a better parser
Scrub <- function(x){

	x <- gsub('\\.', '', x)
	x <- gsub('\\,', '', x)
	x <- gsub('#', '', x)
	
	x <- strsplit(x, " ")[[1]]
	x <- subset(x, x != '')
	
	x <- paste(x, collapse="+")
	return(x)
}

GetMarInfo <- function(x){
    ## ping the API and get some data
    .addr <- Scrub(x)    
    .webcall <- "http://citizenatlas.dc.gov/newwebservices/locationverifier.asmx/findLocation?str=" %+% .addr
    .xmlobj <- xmlTreeParse(.webcall, useInternalNodes = TRUE)
    .xmlobj <- xmlToList(.xmlobj)
    
    if(!is.null(.xmlobj[[2]]$diffgram$NewDataSet$Table1)){
        .xmlobj[[2]]$diffgram$NewDataSet$Table1$InputString <- x
        return(.xmlobj[[2]]$diffgram$NewDataSet$Table1)
    }
}

geo_results <- sapply(responses$location, GetMarInfo)

geo_data <- lapply(geo_results, function(x){
    ## what do we need?
    ## turn the list into a data frame    
    return(data.frame(
        input_string = x$InputString,
        address_id = x$ADDRESS_ID,
        address_status = x$STATUS,
        full_address = x$FULLADDRESS,
        zip_code = x$ZIPCODE,
        ward = x$WARD,
        cluster = x$CLUSTER_,
        census_tract = x$CENSUS_TRACT,
        vote_precinct = x$VOTE_PRCNCT,
        latitude = x$LATITUDE,
        longitude = x$LONGITUDE,
        mar_confidence = x$ConfidenceLevel    
    ))
})

geo_data <- do.call("rbind", geo_data)

write.csv(geo_data, './data/output_test.csv', row.names=FALSE)

## How can we combine this with Google Maps API? Google Places API?