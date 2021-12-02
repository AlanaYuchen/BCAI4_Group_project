### Import and clean the arousal data. 
#Import the individual valence dataset. 
library("xlsx")
arousal = read.xlsx("arousal.xlsx", sheetIndex =1, startRow=1)
summary(arousal)
#One point with an arousal of -1, which should be excluded. 
arousal1 = arousal[with(arousal,arousal>=1),]
summary (arousal1)

### Obtaining Picture memorability data. 
# Import the data for memoribility. 
library(R.matlab)
encoding_by_picture = readMat("encoding_by_picture.mat")
encoding_memorability = encoding_by_picture$memorability
encoding_memorability
ID=seq(1,72,1)
ID_encoding_memorability=data.frame(ID)
encoding_memorability_with_ID<-cbind(ID_encoding_memorability,encoding_memorability)
# Extract sequence of pictureID
pictureID = read.xlsx("pictureID.xlsx", sheetIndex =1, startRow=1)
ID=seq(1,72,1)
ID_pictureID=data.frame(ID)
pictureID_unique_with_ID <-cbind(ID_pictureID,pictureID)
#Combine the sequence of pictureID into the data.frame for memorability. 
combine_encoding_memorability = merge(pictureID_unique_with_ID, encoding_memorability_with_ID, by = "ID")
cleaned_combine_encoding_memorability = combine_encoding_memorability [,2:3]

### Obtaining averaged arousal data.
cleaned_arousal1 = arousal1[,-2]
mean_arousal = aggregate(.~pictureID, data= cleaned_arousal1, mean)
mean_arousal$rounded = round(mean_arousal$arousal)
mean_arousal$calibrated = 4- mean_arousal$rounded
mean_arousal

### Combining Picture memorability data with averaged arousal data. 
combine_encoding_memorability_with_arousal = merge(cleaned_combine_encoding_memorability, mean_arousal, by = "pictureID")
combine_encoding_memorability_with_arousal$rounded = as.factor(combine_encoding_memorability_with_arousal$rounded)
combine_encoding_memorability_with_arousal$calibrated = as.factor(combine_encoding_memorability_with_arousal$calibrated)
combine_encoding_memorability_with_arousal

### aov of encoding_memorability against arousal.  
model = aov(encoding_memorability~calibrated, data = combine_encoding_memorability_with_arousal)
summary(model)
TukeyHSD(model)

### plotting the encoding_memorability against arousal. 
plot1 = ggplot(combine_encoding_memorability_with_arousal, aes(x = calibrated, y = encoding_memorability)) 
plot2 = plot1 + stat_boxplot(geom = "errorbar",width=0.1)
plot3 = plot2 + geom_boxplot(width = .5, size = 1.2)
plot4 = plot3 + theme_classic()
plot5 = plot4 + theme(axis.line.x=element_line(linetype=1,color="black",size=1.2))
plot6 = plot5 + theme(axis.line.y=element_line(linetype = 1, color = "black", size = 1.2))
plot7 = plot6 + theme(axis.ticks=element_line(color="black",size=1.2,lineend = 2))
plot8 = plot7 + scale_x_discrete (labels = c ("Low", "Median", "High"))
plot9 = plot8 + theme(axis.text = element_text(size = 18, face = "bold"))
plot10 = plot9 + labs(x="", y="Free recall performance (%recalled)")
plot11 = plot10 + theme(axis.title = element_text(size = 18, face = "bold"))
plot12 = plot11 + coord_fixed (ratio=4)
plot12

#statistics

library(dplyr)
combine_encoding_memorability_with_arousal%>%
  group_by(calibrated)%>%
  summarise(y=mean(encoding_memorability)) -> df1
df1
combine_encoding_memorability_with_arousal%>%
  group_by(calibrated)%>%
  summarise(y=sd(encoding_memorability)) -> df2
df2

### plotting the proportion. 
nums = c(20, 41, 11)
Type = c("Low", "Median", "High")
data = data.frame(type=type, nums=nums)
p <- ggplot(data = data, mapping = aes(x = "Arousal", y = nums, fill = Type)) + 
  geom_bar(stat = 'identity', position = 'stack')+
  theme(axis.text = element_text(size = 20, face = "bold"))+
  theme_classic()
p
p1 = p + coord_polar(theta = 'y')
p2 = p1+labs(x = '', y = '', title = '')
p3 = p2+theme(axis.text = element_blank())
p3





