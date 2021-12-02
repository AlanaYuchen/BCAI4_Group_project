#reload data
write.csv(tryrun,'/Users/.../workshop9/finalrun.csv')
tryrun=tryrun[,-1]
shufflerun=read.csv('/Users/.../workshop9/finalshufflerun.csv')
shufflerun=shufflerun[,-1]

#rename colnames
tryrun=cbind(run1,run2,run3)
shufflerun=cbind(shuffle_run1,shuffle_run2,shuffle_run3)
colnames(tryrun) = c('loss1','acc1','loss2','acc2','loss3','acc3')
colnames(shufflerun) = c('loss1','acc1','loss2','acc2','loss3','acc3')
shufflerun=data.frame(shufflerun)
tryrun=data.frame(tryrun)

#two sample t test
prediction_significant=matrix(NA,nrow = 1792, ncol = 1)

for (i in 1:1790){
  samplelist=c(tryrun$acc1[i],tryrun$acc2[i],tryrun$acc3[i])
  shufflelist=c(shufflerun$acc1[i],shufflerun$acc2[i],shufflerun$acc3[i])
  
  if(samplelist[1]==samplelist[2] && samplelist[2]==samplelist[3] | shufflelist[1]==shufflelist[2] && shufflelist[2]==shufflelist[3]){
    #p_value = t.test(shufflelist,samplelist,alternative = 'less')$p.value
    
    prediction_significant[i,1]=NA 
  }else{
    p_value = t.test(samplelist,shufflelist,alternative = 'greater')$p.value
    prediction_significant[i,1]=p_value 
  }
  
}



prediction_significant_index=cbind(prediction_significant,seq(1,1792))
significant0.05 = subset(prediction_significant_index, prediction_significant_index[,1] <= 0.05)
significant0.05_isna = subset(isna_p,isna_p[,1] <= 0.05)

significant0.05_all = rbind(significant0.05,significant0.05_isna)

significantrun=cbind(tryrun[data,c(2,4,6)],shufflerun[data,c(2,4,6)],data)
#selct position
selectnumber=86
selectpos=matrix(NA,selectnumber,3)
for (j in 1:selectnumber) {
  index=significant0.05_all[j,2]
  #print(index)
  selectpos[j,]=position_array_t[index,]
}

#convert into MNI
selectpos_mni=matrix(NA,selectnumber,3)
for (k in 1:selectnumber){
  selectpos_mni[k,1]=(-2.75)*selectpos[k,1]+90.75
  selectpos_mni[k,2]=2.75*selectpos[k,2]-126.5
  selectpos_mni[k,3]=4*selectpos[k,3]-72
}

#last
significant.isna = subset(prediction_significant_index, is.na(prediction_significant_index[,1]))
isna_run=matrix(NA,113,7)
for (j in 1:113){
  index=significant.isna[j,2]
  samplelist=c(tryrun$acc1[index],tryrun$acc2[index],tryrun$acc3[index])
  shufflelist=c(shufflerun$acc1[index],shufflerun$acc2[index],shufflerun$acc3[index])
  
  isna_run[j,]= c(samplelist,shufflelist,significant.isna[j,2])
}


isna_p=matrix(NA,113,2)

for (m in 1:113){
  if(isna_run[m,1]==isna_run[m,2] && isna_run[m,2]==isna_run[m,3] && isna_run[m,4]==isna_run[m,5] && isna_run[m,5]==isna_run[m,6]){
    isna_p[m,2]=isna_run[m,7]
    print(m)
  }
  
  else if(isna_run[m,1]==isna_run[m,2] && isna_run[m,2]==isna_run[m,3]){
    p_value = t.test(isna_run[m,4:6],isna_run[m,1:3],alternative = 'less')$p.value
    isna_p[m,1]=p_value
    isna_p[m,2]=isna_run[m,7]
    
  }else if (isna_run[m,4]==isna_run[m,5] && isna_run[m,5]==isna_run[m,6]){
    p_value = t.test(isna_run[m,1:3],isna_run[m,4:6],alternative = 'greater')$p.value
    isna_p[m,1]=p_value
    isna_p[m,2]=isna_run[m,7]
    
  }
}


write.csv(selectpos_mni,'/Users/.../workshop9/selectpos_mni.csv')
