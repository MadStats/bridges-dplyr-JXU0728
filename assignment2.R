library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library(tidyverse)
library(ggplot2)

# Import data using the given code
dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
classes = sapply(tmp, class)
states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dest= rep("", 52)
for(i in 1:52){ 
  dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
}
x16 = ldply(dest, fread, colClasses = classes)  
M = x16
keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017", "TOLL_020" , "ADT_029"           ,      "YEAR_ADT_030" ,
         "YEAR_BUILT_027" , "DECK_COND_058" , "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060"  , "CHANNEL_COND_061","CULVERT_COND_062", "DATE_OF_INSPECT_090"   ,  "FRACTURE_092A"     ,      "UNDWATER_LOOK_SEE_092B" , "SPEC_INSPECT_092C"  )
x = select(M, one_of(keep))

# Select the data from Ohio.
oh = filter(x, STATE_CODE_001 == 39)

# We can find out busy bridge
min2dec = function(x) {
  as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}
oh = mutate(oh, lat = min2dec(LAT_016), lon = min2dec(LONG_017))
(quantile(oh$ADT_029,probs = 0.8)) #which is 7287, 28284-7287 approximate 21000
oh.busy = filter(oh, ADT_029 >= 21000)
ggplot(data = oh.busy) +geom_point(mapping = aes(y = lat, x = -lon, col =YEAR_BUILT_027))

# Relationship between the average condition of different parts and the level of busy
oh = mutate(oh, cond = pmin(DECK_COND_058, SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062, 
                              na.rm = T))
rateIt = function(cond){
  rate = rep("good", length(cond))
  rate[cond <5] = "bad"
  rate[cond <2]= "fail"
  return(rate)
}
oh$rate = rateIt(oh$cond)
oh %>% group_by(ADT_029) %>% summarize(prop = mean(rate=="good")) %>%
  ggplot(mapping = aes(x = ADT_029, y = prop)) + geom_point()

# Easy to find the number of bridges built in each year

buildyear <- oh %>% group_by(YEAR_BUILT_027) %>% dplyr::summarise(count = n())
ggplot(buildyear) + geom_line(mapping = aes(y = count, x = YEAR_BUILT_027), color = "red")

# Relationship between year of built and rate
oh %>% group_by(YEAR_BUILT_027) %>% summarize(prop = mean(rate=="good")) %>%
  ggplot(mapping = aes(x = YEAR_BUILT_027, y = prop)) + geom_point()
