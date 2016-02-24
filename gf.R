# setup your working directory 
setwd()

############### 
# preprocessing 
############### 
# Read in data
gf = read.csv("GF.csv")

#------------------------------------dealing with column 'country'---------------------------------------------
#Remove columns from dataframe where ALL values are NA
gf = gf[,colSums(is.na(gf))<nrow(gf)]
gf = gf[,-c(2,6,7,15,31)]

#leading and trailing whitespace in a data.frame
trim <- function (D){ 
  for (i in 1:length(D)){
    s = D[i]
    s1 = gsub("^\\s+|\\s+$", "", s)
    D[i] = s1
  }
 return (D)
}  
# none-relevent characters "\xca"  
move <- function (D) {
  for (i in 1:length(D)){
  s = D[i]
  s1 = gsub("[\xca]","",s)
  D[i] = s1
  }
  return (D)
}  
levels(gf$Country) = move(levels(gf$Country))
levels(gf$Country) = trim(levels(gf$Country))  

# spelling mistakes and inconsistent names
levels(gf$Country)[levels(gf$Country)==""] = NA
levels(gf$Country)[levels(gf$Country)=="#N/A"] = NA
levels(gf$Country)[levels(gf$Country)=="USA."] = "USA"
levels(gf$Country)[levels(gf$Country)=="Bangaldesh"] = "Bangladesh"
levels(gf$Country)[levels(gf$Country)=="Bangledesh"] = "Bangladesh"
levels(gf$Country)[levels(gf$Country)=="Boliva"] = "Bolivia"
levels(gf$Country)[levels(gf$Country)=="Bosnia"] = "Bosnia and Herzegovina"
levels(gf$Country)[levels(gf$Country)=="Bosnia and Herzogovina"] = "Bosnia and Herzegovina"
levels(gf$Country)[levels(gf$Country)=="Bosnia-Hercegovenia"] = "Bosnia and Herzegovina"
levels(gf$Country)[levels(gf$Country)=="Bosnia-Herzegovina"] = "Bosnia and Herzegovina"
levels(gf$Country)[levels(gf$Country)=="Burkino Faso"] = "Burkina Faso"
levels(gf$Country)[levels(gf$Country)=="Burma"] = "Myanmar"
levels(gf$Country)[levels(gf$Country)=="Burma/Myanmar"] = "Myanmar"
levels(gf$Country)[levels(gf$Country)=="Columbia"] = "Colombia"
levels(gf$Country)[levels(gf$Country)=="Comoros islands"] ="Comoros Islands"
levels(gf$Country)[levels(gf$Country)=="Cote d'Ivoire"] = "Ivory Coast"
levels(gf$Country)[levels(gf$Country)=="DR Congo"] ="Democratic Republic of the Congo"
levels(gf$Country)[levels(gf$Country)=="Democratic Republic Congo"] ="Democratic Republic of the Congo"
levels(gf$Country)[levels(gf$Country)=="Democratic Republic of Congo"] ="Democratic Republic of the Congo"
levels(gf$Country)[levels(gf$Country)=="Congo"] = "Democratic Republic of the Congo"
levels(gf$Country)[levels(gf$Country)=="Congo Republic"] = "Republic of the Congo"
levels(gf$Country)[levels(gf$Country)=="Domnican Republic"] = "Dominican Republic" 
levels(gf$Country)[levels(gf$Country)=="El Savador"] = "El Salvador"
levels(gf$Country)[levels(gf$Country)=="Inda"] = "India"
levels(gf$Country)[levels(gf$Country)=="Guatamala"] = "Guatemala"
levels(gf$Country)[levels(gf$Country)=="Kazahkstan"] = "Kazakhstan"
levels(gf$Country)[levels(gf$Country)=="Malayasia"] = "Malaysia"
levels(gf$Country)[levels(gf$Country)=="Moldava"] = "Moldova"
levels(gf$Country)[levels(gf$Country)=="Moldovo"] = "Moldova"
levels(gf$Country)[levels(gf$Country)=="Myanamar"] = "Myanmar"
levels(gf$Country)[levels(gf$Country)=="Papua New Gunea"] = "Papua New Guinea"
levels(gf$Country)[levels(gf$Country)=="Philipines"] = "Philippines"
levels(gf$Country)[levels(gf$Country)=="Philippine"] = "Philippines"
levels(gf$Country)[levels(gf$Country)=="Phillipines"] = "Philippines"
levels(gf$Country)[levels(gf$Country)=="Phillippines"] = "Philippines"
levels(gf$Country)[levels(gf$Country)=="Serbia-Montenegro"] = "Serbia and Montenegro"
levels(gf$Country)[levels(gf$Country)=="UK"] = "United Kingdom" 
levels(gf$Country)[levels(gf$Country)=="Uruguay,"] = "Uruguay"
levels(gf$Country)[levels(gf$Country)=="USSR"] = "Soviet Union"
levels(gf$Country)[levels(gf$Country)=="Viet Nam"] = "Vietnam"
levels(gf$Country)[levels(gf$Country)=="Zimbawe"] = "Zimbabwe" 
levels(gf$Country)[levels(gf$Country)=="Venezulea"] = "Venezuela"


#############################################################################
#clean main cause data
main.cause = as.character(gf$Main.cause)

main.cause[main.cause == "J\x9akulhlaup"] = "unknown causes"
main.cause[main.cause == "0"] = "unknown causes"
main.cause[main.cause == "#N/A"] = "unknown causes"
main.cause[main.cause == ""] = "unknown causes"

for(i in 1:length(main.cause)){
  main.cause[i] = tolower(main.cause[i])
}

# remove the specific storm/hurricane/cyclone/typhoon name
for(i in 1:length(main.cause)){
  if(grepl('^tropical storm', main.cause[i]))
    main.cause[i] = "tropical storm"
  if(grepl('^hurricane', main.cause[i]))
    main.cause[i] = "hurricane"
  if(grepl('^tropical cyclone', main.cause[i]))
    main.cause[i] = "tropical cyclone"
  if(grepl('^typhoon', main.cause[i]))
    main.cause[i] = "typhoon"
}

# if there are multiple causes, save as "multiple causes"
for(i in 1:length(main.cause)){
  if(grepl("and", main.cause[i]))
    main.cause[i] = "multiple causes"
  if(grepl(",", main.cause[i]))
    main.cause[i] = "multiple causes"
}

main.cause[main.cause == "avalance breach"] = "avalanche"
main.cause[main.cause == "avalanche related"] = "avalanche"
main.cause[main.cause == "brief torrential rain"] = "torrential rain"
main.cause[main.cause == "dam failure"] = "dam break"
main.cause[main.cause == "dam release"] = "dam break"
main.cause[main.cause == "dam/levy, break or release"] = "dam break"
main.cause[main.cause == "extra-tropical cyclone"] = "tropical cyclone"
main.cause[main.cause == "heavy  rain"] = "heavy rain"
main.cause[main.cause == "heay rain"] = "heavy rain"
main.cause[main.cause == "heavy monsoon rains"] = "monsoon rain"
main.cause[main.cause == "heavy rain, began with #4079"] = "heavy rain"
main.cause[main.cause == "heavy seasonal rains"] = "heavy rain"
main.cause[main.cause == "heavyrain"] = "heavy rain"
main.cause[main.cause == "heavy rain snowmelt dam b"] = "multiple causes"
main.cause[main.cause == "high tides"] = "storm surge"
main.cause[main.cause == "ice jam and ice melt"] = "ice jam"
main.cause[main.cause == "ice jam or ice break-up"] = "ice jam"
main.cause[main.cause == "ice jam/break-up"] = "ice jam"
main.cause[main.cause == "ice jams"] = "ice jam"
main.cause[main.cause == "monsoonal rain"] = "monsoon rain"
main.cause[main.cause == "monoonal rain"] = "monsoon rain"
main.cause[main.cause == "monsoonal rainfall"] = "monsoon rain"
main.cause[main.cause == "monsoon rain, began with #4078"] = "monsoon rain"
main.cause[main.cause == "monsoon rains"] = "monsoon rain"
main.cause[main.cause == "monsoonal ran"] = "monsoon rain"
main.cause[main.cause == "rainy season"] = "heavy rain"
main.cause[main.cause == "see notes"] = "unknown causes"
main.cause[main.cause == "snowmelt"] = "snow melt"
main.cause[main.cause == "tidal surge"] = "high tide"
main.cause[main.cause == "torrrential rain"] = "torrential rain"
main.cause[main.cause == " torrential rain"] = "torrential rain"
main.cause[main.cause == "torrential rains"] = "torrential rain"
main.cause[main.cause == "torrential rain; tropical storm gon"] = "multiple causes"
main.cause[main.cause == "tropical  storm fenshen"] = "tropical storm"
main.cause[main.cause == "tropical depressions"] = "tropical depression"
main.cause[main.cause == "tropical stroms pendring and nalgae"] = "tropical storm"

gf$Main.cause = main.cause



