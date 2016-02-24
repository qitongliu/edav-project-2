##### create dummy variables for main cause, analyze their relationships #############

gf.new = read.csv("GF.csv")
cause = as.character(gf.new$Main.cause)

cause[cause == "J\x9akulhlaup"] = "unknown causes"
cause[cause == "0"] = "unknown causes"
cause[cause == "#N/A"] = "unknown causes"
cause[cause == ""] = "unknown causes"

for(i in 1:length(cause)){
  cause[i] = tolower(cause[i])
}

# remove the specific storm/hurricane/cyclone/typhoon name
for(i in 1:length(cause)){
  if(grepl('^tropical storm', cause[i]))
    cause[i] = "tropical storm"
  if(grepl('^hurricane', cause[i]))
    cause[i] = "hurricane"
  if(grepl('^tropical cyclone', cause[i]))
    cause[i] = "tropical cyclone"
  if(grepl('^typhoon', cause[i]))
    cause[i] = "typhoon"
}

# replace "and" to "," in causes
cause = gsub(" and", ",", cause)
cause = gsub(";", ",", cause)

cause[cause == "avalance breach"] = "avalanche"
cause[cause == "avalanche related"] = "avalanche"
cause[cause == "brief torrential rain"] = "torrential rain"
cause[cause == "dam failure"] = "dam break"
cause[cause == "dam release"] = "dam break"
cause[cause == "dam/levy, break or release"] = "dam break"
cause[cause == "extra-tropical cyclone"] = "tropical cyclone"
cause[cause == "heavy  rain"] = "heavy rain"
cause[cause == "heay rain"] = "heavy rain"
cause[cause == "heavy monsoon rains"] = "monsoon rain"
cause[cause == "heavy rain, began with #4079"] = "heavy rain"
cause[cause == "heavy seasonal rains"] = "heavy rain"
cause[cause == "heavyrain"] = "heavy rain"
cause[cause == "heavy rain snowmelt dam b"] = "heavy rain, snowmelt, dam break"
cause[cause == "high tides"] = "storm surge"
cause[cause == "ice jam , ice melt"] = "ice jam"
cause[cause == "ice jam or ice break-up"] = "ice jam"
cause[cause == "ice jam/break-up"] = "ice jam"
cause[cause == "ice jams"] = "ice jam"
cause[cause == "monsoonal rain"] = "monsoon rain"
cause[cause == "monoonal rain"] = "monsoon rain"
cause[cause == "monsoonal rainfall"] = "monsoon rain"
cause[cause == "monsoon rain, began with #4078"] = "monsoon rain"
cause[cause == "monsoon rains"] = "monsoon rain"
cause[cause == "monsoonal ran"] = "monsoon rain"
cause[cause == "rainy season"] = "heavy rain"
cause[cause == "see notes"] = "unknown causes"
cause[cause == "snow melt"] = "snowmelt"
cause[cause == "tidal surge"] = "high tide"
cause[cause == "torrrential rain"] = "torrential rain"
cause[cause == " torrential rain"] = "torrential rain"
cause[cause == "torrential rains"] = "torrential rain"
cause[cause == "tropical  storm fenshen"] = "tropical storm"
cause[cause == "tropical depressions"] = "tropical depression"
cause[cause == "tropical stroms pendring, nalgae"] = "tropical storm"
cause[cause == "cylone tasha, monsoon rain, cyclone"] = "tropical cyclone, monsoon rain"
cause[cause == "dam failure, heavy rain"] = "dam break, heavy rain"
cause[cause == "dam release, heavy  ra"] = "dam break, heavy rain" 
cause[cause == "dam release, heavy  rain"] = "dam break, heavy rain"  
cause[cause == "dam releases, monsoonal rain"] = "dam break, monsoon rain" 
cause[cause == "heavy rain, snow"] = "heavy rain, snowmelt" 
cause[cause == "heavy rain, dam release"] = "heavy rain, dam break"
cause[cause == "heavy rain, tropical cycl"] = "heavy rain, tropical cyclone"
cause[cause == "heavy rain, tropical storm joaquin"] = "heavy rain, tropical storm"
cause[cause == "heavy rain,and dam release"] = "heavy rain, dam break"
cause[cause == "ice jam, ice melt"] = "ice jam"
cause[cause == "levee failure, early monsoonal rain"] = "levee failure, monsoon rain"
cause[cause == "monsoonal rain, offshore tropica"] = "monsoon rain, tropical storm"
cause[cause == "monsoonal rain, tropical storm k"] = "monsoon rain, tropical storm"
cause[cause == "monsoonal rain, tropical storm rust"] = "monsoon rain, tropical storm"
cause[cause == "rain, snow"] = "heavy rain, snowmelt"
cause[cause == "rain, snow melt"] = "heavy rain, snowmelt" 
cause[cause == "rain, snowmelt"] = "heavy rain, snowmelt"
cause[cause == "storm surge, heavy rai"] = "storm surge, heavy rain"
cause[cause == "torrential rain, hgh tides"] = "torrential rain, high tide"
cause[cause == "torrential rain, mudslides"] = "torrential rain, landslide"
cause[cause == "torrential rain, tropical storm gon"] = "torrential rain, tropical storm"

cause_list <- strsplit(as.character(cause), ", ")

# Create a list for all skill names
cause_name <- vector(mode="character", length=0)

cause.dummy = data.frame(matrix(vector(),nrow = nrow(gf)))
# Create dummies for skills, loop through samples and skills
for (i in 1:length(cause)) {
  
  for (j in cause_list[[i]]) {
    
    # append the names, create a new column if it is a new cause
    if (!is.element(j, cause_name)) {
      cause_name = append(cause_name, j)
      cause.dummy[, j] = 0
    }
    
    cause.dummy[i, j] = 1
  }
  
}


cause.count1 = as.data.frame(colSums(cause.dummy))
cause.count1 = cbind(row.names(cause.count1), cause.count1)
colnames(cause.count1) = c("main.cause", "count")
cause.count1 = cause.count1[order(cause.count1$main.cause),]
cause.count2 = as.data.frame(table(gf$Main.cause))
cause.count = merge(cause.count2, cause.count1)
colnames(cause.count) = c("main.cause", "all.count", "single.count")

library(ggplot2)

library(reshape2)
cause.count = melt(cause.count)
cause.count = cause.count[order(cause.count$value, decreasing = T),]

ggplot(cause.count[1:12,],aes(x=reorder(main.cause, value),value,fill=variable)) + 
  geom_bar(stat="identity",position="dodge") + xlab("main cause") + coord_flip()

ggplot(cause.count[13:nrow(cause.count),],aes(x=reorder(main.cause, value),value,fill=variable)) + 
  geom_bar(stat="identity",position="dodge") + xlab("main cause") + coord_flip()

library(corrplot)
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(cause.dummy, 0.95)
## specialized the insignificant value according to the significant level
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", 
                           "cyan", "#007FFF", "blue", "#00007F"))
cor.cause = cor(cause.dummy)
corrplot(cor.cause,p.mat = res1[[1]], insig = "blank" , sig.level = 0.05, diag = F, type = "lower", tl.col = "black", col = col4(50))
