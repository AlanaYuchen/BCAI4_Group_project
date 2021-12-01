#individual valence without rounded
ind_valance = read.csv("~/Documents/BCAI4/material/week9/data/Individual_valences_cut.csv")
ind_valance_ave=aggregate(Individual.valence.rating..1.pos.2.neu.3.neg.~pictureID, data=ind_valance,FUN='mean')
colnames(ind_valance_ave)[2]='Individual.valence.rating'

# fMRI picture ID
fmri_picid=read.csv("~/Documents/BCAI4/material/week9/data/pictureIDs.csv",header = F)
colnames(fmri_picid)[1]='fmri.picid'

# fMRI data (write csv from encoding_by_picture.mat)
enc_pic_fmri=read.csv("~/Documents/BCAI4/material/week9/data/encoding_pic_fmri.csv",header = F);
na_flag=apply(is.na(enc_pic_fmri),2,sum)
enc_pic_fmri_cut=enc_pic_fmri[,which(na_flag==0)] # filter out non-NaN
enc_pic_fmri_cut$fmri_picid=fmri_picid$fmri.picid

library(dplyr)
fmri_picid_valance=left_join(fmri_picid,ind_valance_ave,by=c('fmri.picid'='pictureID'))
pval=c()
models=list()
for (i in 1:length(enc_pic_fmri_cut)) {
  value=enc_pic_fmri_cut[,i]
  ss1=cbind(value,fmri_picid_valance)
  model=summary(lm(value~Individual.valence.rating,data = ss1))
  models[[i]]=model
  pval=c(pval,model$coefficients[2,4])
}

adj.p=p.adjust(pval[,1])