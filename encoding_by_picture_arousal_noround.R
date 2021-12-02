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
mean_arousal$reverted_arousal = 4-mean_arousal$arousal

### Combining Picture memorability data with averaged arousal data. 
combine_encoding_memorability_with_arousal = merge(cleaned_combine_encoding_memorability, mean_arousal, by = "pictureID")
combine_encoding_memorability_with_arousal$rounded = as.factor(combine_encoding_memorability_with_arousal$rounded)
combine_encoding_memorability_with_arousal

### plotting the encoding_memorability against valence. 
ggplot(data=combine_encoding_memorability_with_arousal,mapping=aes(x=reverted_arousal,y=encoding_memorability))+
  geom_smooth()+
  theme_classic()+
  geom_point() + 
  theme(axis.line.x=element_line(linetype=1,color="black",size=1.2))+
  theme(axis.line.y=element_line(linetype = 1, color = "black", size = 1.2))+
  theme(axis.ticks=element_line(color="black",size=1.2,lineend = 2))+ 
  theme(axis.text = element_text(size = 12, face = "bold"))+
  labs(x="Arousal", y="Recognition performance rating") +
  theme(axis.title = element_text(size = 15, face = "bold"))+
  theme(axis.text = element_text(size = 12, face = "bold"))+
  coord_fixed (ratio=4)
### spearman rank correlation.  
cor.test( ~ reverted_arousal + encoding_memorability, 
          data=combine_encoding_memorability_with_arousal,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

