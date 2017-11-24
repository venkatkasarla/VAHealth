library(dplyr)
library(ggplot2)


## 1-a.load the VA Health rda data
load_rda_object <- function(fname){
  e <- new.env(parent = parent.frame())
  load(fname, e)
  return(e[[ls(e)[1]]])
}

va.data <- load_rda_object('data/N-MHSS-2015-DS0001-bndl-data-r/N-MHSS-2015-DS0001-data/N-MHSS-2015-DS0001-data-r.rda')

# 1-b.state abbreviations without their counts
st.abbr <- unique(va.data$LST) %>% as.vector()
#cleaning state abbreviations to valid ones
st.abbr <- gsub(" ", "", st.abbr)

# 1-c Exclude Alaska - AK, Hawai - HI, US territory state code  AS, GU, PR, VI
# incude DC
st.abbr.filt <- st.abbr[st.abbr != "AK"]
st.abbr.filt <- st.abbr.filt[st.abbr.filt != "HI"]
st.abbr.filt <- st.abbr.filt[st.abbr.filt != "AS"]
st.abbr.filt <- st.abbr.filt[st.abbr.filt != "GU"]
st.abbr.filt <- st.abbr.filt[st.abbr.filt != "PR"]
st.abbr.filt <- st.abbr.filt[st.abbr.filt != "VI"]

va.data$LST <- gsub(" ", "", va.data$LST)
va.data.mainland <- select(va.data,LST,FACILITYTYPE) %>% filter(LST %in% st.abbr.filt ) %>% filter(grepl("Veterans Administration medical center",FACILITYTYPE))
va.data.cnt <- select(va.data.mainland,LST) %>% group_by(LST) %>% data.frame() 
#Counts of VA medical centers  by State
va.data.cnt <- va.data.cnt %>% group_by(LST) %>% summarise(total =n()) %>% data.frame() 

# 1-d VA Medical Centre Counts Bar Plot
ggplot(va.data.cnt, aes(x=LST, y=total,col=LST)) +  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, hjust=0)) + scale_linetype_discrete(name="VA Medical Centre Counts") + xlab("State") + ylab("Total Counts") + ggtitle("Counts of VA Medical Centers") + theme(plot.title = element_text(hjust = 0.5))

#2-a . read the statesize.csv and see the issue in LST , State abbreviation is cleaned in 1A to remove space in order to have valid state code and  merge
state.size <- read.csv("data/statesize.csv")

#2-b Merge the such that we can calculate the VS hospitals per thousand square miles
va.merged <- merge(x=state.size, y=va.data.cnt , by.x = "Abbrev" , by.y="LST")

#2-c calculate new variable VA hospitals per thousand square miles.
#initialize lowest decimal value per thousand sqmiles 
va.merged$va.per.thosand.sqmile <- 0.001
data.frame(lapply(va.merged, function(y) if(is.numeric(y)) round(y, 3) else y))
va.merged <- va.merged %>% mutate(va.per.thosand.sqmile = (total/SqMiles*1000))

#2-d ggplot state on x axis for every square thousand miles for VA
va.merged$logSqmile = log(va.merged$va.per.thosand.sqmile)
ggplot(va.merged, aes(x = reorder(Abbrev, -total), y = total,color=Abbrev)) +  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, hjust=0))
ggplot(va.merged, aes(x = reorder(va.per.thosand.sqmile, -total), y = total,color=Abbrev)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, hjust=0))