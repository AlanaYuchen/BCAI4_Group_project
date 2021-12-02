### Import and clean the valence data. 
#Import the individual valence dataset. 
library("xlsx")
Valence = read.xlsx("valence.xlsx", sheetIndex =1, startRow=1)
#One point with a valence of -1, which should be excluded. 
Valence1 = Valence[with(Valence,Valence>=1),]
Valence1

### Obtaining performance data. 
# Import the data for performance 
library("xlsx")
recognition = read.xlsx("recognition.xlsx", sheetIndex =1, startRow=1)
summary(recognition)
#One point with a recognition of -1, which should be excluded. 
recognition1 = recognition[with(recognition,Recognition>=1),]
summary (recognition1)
recognition1
# Obtain averaged performance 
cleaned_recognition1 = recognition1[,-2]
mean_recognition = aggregate(.~pictureID, data= cleaned_recognition1, mean)
mean_recognition$rounded = round(mean_recognition$Recognition)
mean_recognition

### Obtaining averaged valence data.
cleaned_Valence1 = Valence1[,-2]
mean_valence = aggregate(.~pictureID, data= cleaned_Valence1, mean)
mean_valence$rounded_valence = round(mean_valence$Valence)
mean_valence
mean_valence$calibrated_mean_valence = mean_valence$Valence-2
mean_valence

### Combining performance data with averaged valence data. 
combine_encoding_recognition_with_valence = merge(mean_recognition, mean_valence, by = "pictureID")
combine_encoding_recognition_with_valence
combine_encoding_recognition_with_valence$rounded_valence = as.factor(combine_encoding_recognition_with_valence$rounded_valence)
combine_encoding_recognition_with_valence

### aov of encoding_memorability against arousal.  
model = aov(Recognition~rounded_valence, data = combine_encoding_recognition_with_valence)
summary(model)
TukeyHSD(model)

### plotting the encoding_memorability against arousal. 
# boxplot.
plot1 = ggplot(combine_encoding_recognition_with_valence, aes(x = rounded_valence, y = Recognition)) 
plot2 = plot1 + stat_boxplot(geom = "errorbar",width=0.1)
plot3 = plot2 + geom_boxplot(width = .5, size = 1.2)
plot4 = plot3 + theme_classic()
plot5 = plot4 + theme(axis.line.x=element_line(linetype=1,color="black",size=1.2))
plot6 = plot5 + theme(axis.line.y=element_line(linetype = 1, color = "black", size = 1.2))
plot7 = plot6 + theme(axis.ticks=element_line(color="black",size=1.2,lineend = 2))
plot8 = plot7 + scale_x_discrete (labels = c ("Negative", "Neutral", "Positive"))
plot9 = plot8 + theme(axis.text = element_text(size = 18, face = "bold"))
plot10 = plot9 + labs(x="", y="Recognition performance rating")
plot11 = plot10 + theme(axis.title = element_text(size = 18, face = "bold"))
plot11
plot12 = plot11 + coord_fixed(ratio = 6)
plot12

# lineplot 
library(ggplot2)
ggplot(combine_encoding_recognition_with_valence, aes(x =calibrated_mean_valence, y = Recognition)) +
  geom_smooth()+
  theme_classic()+
  geom_point() + 
  theme(axis.line.x=element_line(linetype=1,color="black",size=1.2))+
  theme(axis.line.y=element_line(linetype = 1, color = "black", size = 1.2))+
  theme(axis.ticks=element_line(color="black",size=1.2,lineend = 2))+ 
  theme(axis.text = element_text(size = 12, face = "bold"))+
  labs(x="Valence", y="Recognition performance rating") +
  theme(axis.title = element_text(size = 15, face = "bold"))+
  theme(axis.text = element_text(size = 12, face = "bold"))+
  coord_fixed(ratio=6)


hist(combine_encoding_recognition_with_valence$Recognition, breaks = c(0.5,1.5,2.5,3.5))
