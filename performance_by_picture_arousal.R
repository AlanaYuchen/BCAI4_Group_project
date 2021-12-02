### Import and clean the arousal data. 
#Import the individual arousal dataset. 
library("xlsx")
arousal = read.xlsx("arousal.xlsx", sheetIndex =1, startRow=1)
summary(arousal)
#One point with an arousal of -1, which should be excluded. 
arousal1 = arousal[with(arousal,arousal>=1),]
summary (arousal1)

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
mean_recognition

### Obtaining averaged arousal data.
cleaned_arousal1 = arousal1[,-2]
mean_arousal = aggregate(.~pictureID, data= cleaned_arousal1, mean)
mean_arousal$rounded_arousal = round(mean_arousal$arousal)
mean_arousal$calibrated = 4-mean_arousal$rounded_arousal
mean_arousal$calibrated_noround = 4-mean_arousal$arousal
mean_arousal

### Combining recognition data with averaged arousal data. 
combine_encoding_recognition_with_arousal = merge(mean_recognition, mean_arousal, by = "pictureID")
combine_encoding_recognition_with_arousal$rounded_arousal = as.factor(combine_encoding_recognition_with_arousal$rounded_arousal)
combine_encoding_recognition_with_arousal$calibrated = as.factor(combine_encoding_recognition_with_arousal$calibrated)
combine_encoding_recognition_with_arousal

### aov of recognition performance against arousal.  
model = aov(Recognition~rounded_arousal, data = combine_encoding_recognition_with_arousal)
summary(model)
TukeyHSD(model)

### plotting the recognition performance against arousal. 
plot1 = ggplot(combine_encoding_recognition_with_arousal, aes(x = rounded_arousal, y = Recognition)) 
plot2 = plot1 + stat_boxplot(geom = "errorbar",width=0.1)
plot3 = plot2 + geom_boxplot(width = .5, size = 1.2)
plot4 = plot3 + theme_classic()
plot5 = plot4 + theme(axis.line.x=element_line(linetype=1,color="black",size=1.2))
plot6 = plot5 + theme(axis.line.y=element_line(linetype = 1, color = "black", size = 1.2))
plot7 = plot6 + theme(axis.ticks=element_line(color="black",size=1.2,lineend = 2))
plot8 = plot7 + scale_x_discrete (labels = c ("High", "Median", "Low"))
plot9 = plot8 + theme(axis.text = element_text(size = 18, face = "bold"))
plot10 = plot9 + labs(x="", y="Recognition performance rating")
plot11 = plot10 + theme(axis.title = element_text(size = 18, face = "bold"))
plot12 = plot11 + coord_fixed(ratio=6)
plot12

library(ggplot2)
ggplot(combine_encoding_recognition_with_arousal, aes(x =calibrated_noround, y = Recognition)) +
  geom_smooth()+
  theme_classic()+
  geom_point() + 
  theme(axis.line.x=element_line(linetype=1,color="black",size=1.2))+
  theme(axis.line.y=element_line(linetype = 1, color = "black", size = 1.2))+
  theme(axis.ticks=element_line(color="black",size=1.2,lineend = 2))+ 
  theme(axis.text = element_text(size = 12, face = "bold"))+
  labs(x="Arousal", y="Recognition performance rating") +
  theme(axis.title = element_text(size = 18, face = "bold"))+
  theme(axis.text = element_text(size = 18, face = "bold"))+
  coord_fixed(ratio=6)

### Basic histogram
ggplot(combine_encoding_recognition_with_arousal, aes(x=Recognition)) + geom_histogram()+
  theme_classic()+
  xlim(1,3)+
  theme(axis.line.x=element_line(linetype=1,color="black",size=1.2))+
  theme(axis.line.y=element_line(linetype = 1, color = "black", size = 1.2))+
  theme(axis.ticks=element_line(color="black",size=1.2,lineend = 2))+ 
  theme(axis.text = element_text(size = 12, face = "bold"))+
  labs(x="Recognition performance rating", y="Count") +
  theme(axis.title = element_text(size = 15, face = "bold"))+
  theme(axis.text = element_text(size = 18, face = "bold"))+
  coord_fixed(ratio=.2)



