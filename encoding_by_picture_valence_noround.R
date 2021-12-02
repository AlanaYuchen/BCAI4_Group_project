### Import and clean the valence data. 
#Import the individual valence dataset. 
library("xlsx")
Valence = read.xlsx("valence.xlsx", sheetIndex =1, startRow=1)
#One point with a valence of -1, which should be excluded. 
Valence1 = Valence[with(Valence,Valence>=1),]
Valence1
# Obtaining averaged valence data.
cleaned_Valence1 = Valence1[,-2]
cleaned_Valence1
mean_valence = aggregate(.~pictureID, data= cleaned_Valence1, mean)
mean_valence
mean_valence$balanced_valence = mean_valence$Valence-2
mean_valence

### Obtaining Picture memorability data. 
# Import the data for memorability. 
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
cleaned_combine_encoding_memorability


### Combining Picture memorability data with averaged valence data. 
combine_encoding_memorability_with_valence = merge(cleaned_combine_encoding_memorability, mean_valence, by = "pictureID")
combine_encoding_memorability_with_valence 

### plotting the encoding_memorability against valence. 
ggplot(data=combine_encoding_memorability_with_valence,mapping=aes(x=balanced_valence,y=encoding_memorability))+
        geom_smooth()+
        theme_classic()+
        geom_point() + 
  theme(axis.line.x=element_line(linetype=1,color="black",size=1.2))+
  theme(axis.line.y=element_line(linetype = 1, color = "black", size = 1.2))+
  theme(axis.ticks=element_line(color="black",size=1.2,lineend = 2))+ 
  theme(axis.text = element_text(size = 12, face = "bold"))+
  labs(x="Valence", y="Memorability (%)") +
  theme(axis.title = element_text(size = 15, face = "bold"))+
  theme(axis.text = element_text(size = 12, face = "bold"))+
  coord_fixed(ratio=4)





