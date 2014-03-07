## USEFUL FUNCTIONS## 
options(scipen=20)
options(stringsAsFactors = FALSE)

## Operator Functions ##
`%notin%` <- function(x,y) !(x %in% y) 
`%+%` <- function(x,y) paste(x,y,sep="")

## Stolen from Harlan:
n20 <- function(x) if (is.null(x)) 0 else x
n2na <- function(x) if (is.null(x)) NA else x

## Wait for Key
readkey <- function()
{
    cat ("Press [enter] to continue", fill=TRUE)
    line0 <- readline()
}

## Generation of Indentation by Level
indent <- function(n){
	return(paste(rep("\t", n), collapse=''))
}

## ADD LEADING ZEROES to X Digits ## 
leadgr <- function(x, y){
	if(!is.na(x)){
		while(nchar(x)<y){
			x <- paste0("0",x)
		}
	}
	return(x)
}

## ADD TRAILING SPACES to X Digits ## 
endspace <- function(x, y){
	if(!is.na(x)){
		while(nchar(x)<y){
			x <- paste0(x," ")
		}
	}
	return(x)
}

trimall <- function(tstring){
	return(gsub("(^ +)|( +$)", "", tstring))
}

increment <- function(x)
{
	eval.parent(substitute(x <- x + 1))
}

up <- function(x){
	eval.parent(substitute(x <- x + 1))
}
down <- function(x){
	eval.parent(substitute(x <- x - 1))
}

## PLOT MULTIPLE DISTRIBUTIONS
plot.multi.dens <- function(s, toplb="")
{
	junk.x = NULL
	junk.y = NULL
	for(i in 1:length(s))
	{
		junk.x = c(junk.x, density(s[[i]],na.rm=TRUE)$x)
		junk.y = c(junk.y, density(s[[i]],na.rm=TRUE)$y)
	}
	xr <- range(junk.x)
	yr <- range(junk.y)
	plot(density(s[[1]],na.rm=TRUE), xlim = xr, ylim = yr, main = toplb)
	for(i in 1:length(s))
	{
		lines(density(s[[i]], na.rm=TRUE), xlim = xr, ylim = yr, col = i)
	}
}

## FINDS INTEGER MODE OF AN ARRAY
mhack <- function(x){
	temp <- table(as.vector(x))
	return(as.integer(names(temp)[temp == max(temp)]))
}

## FINDS STRING MODE OF A STRING SET
mhack2 <- function(x){
	temp <- table(as.vector(x))
	
	x <- subset(x, !is.na(x))
	
	if(length(x)==0){
		return(NA)
	} else{
		return(names(temp)[temp == max(temp)])
	}
}

## Exporting Stuff to JSON... check for NAs
checkna <- function(x){
	if(is.na(x)){
		return('null')
	}
	return(x)
}

checkna_str <- function(x){
	if(is.na(x)){
		return('null')
	}
	return('"' %+% x %+%'"')
}