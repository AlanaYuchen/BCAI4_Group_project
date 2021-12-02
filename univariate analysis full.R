data=read.csv("/Users/chenghui/Documents/BCAI4/material/week9/Phenos/picture recognition performance.csv")
length(unique(data$PictureID))
sum(is.na(data$Recognition..1.old.2.familiar.3.new.))

data=read.csv("/Users/chenghui/Documents/BCAI4/material/week9/Phenos/individual arousal.csv")
length(unique(data$pictureID))
sum(is.na(data$Recognition..1.old.2.familiar.3.new.))

#=======start univariate analysis====
#individual valence
ind_valance = read.csv("/Users/chenghui/Documents/BCAI4/material/week9/data/Individual_valences_cut.csv")
ind_valance_ave=aggregate(Individual.valence.rating..1.pos.2.neu.3.neg.~pictureID, data=ind_valance,FUN='mean')
ind_valance_ave$Individual.valence.rating..1.pos.2.neu.3.neg.=round(ind_valance_ave$Individual.valence.rating..1.pos.2.neu.3.neg.)
colnames(ind_valance_ave)[2]='Individual.valence.rating'

fmri_picid=read.csv("/Users/chenghui/Documents/BCAI4/material/week9/data/pictureIDs.csv",header = F)
colnames(fmri_picid)[1]='fmri.picid'

enc_pic_fmri=read.csv("/Users/chenghui/Documents/BCAI4/material/week9/data/encoding_pic_fmri.csv",header = F);
na_flag=apply(is.na(enc_pic_fmri),2,sum)
enc_pic_fmri_cut=enc_pic_fmri[,which(na_flag==0)]
enc_pic_fmri_cut$fmri_picid=fmri_picid$fmri.picid

library(dplyr)
fmri_picid_valance=left_join(fmri_picid,ind_valance_ave,by=c('fmri.picid'='pictureID'))
fmri_picid_valance$Individual.valence.rating=as.factor(fmri_picid_valance$Individual.valence.rating)
pval=data.frame()
models=list()
for (i in 1:length(enc_pic_fmri_cut)) {
  value=enc_pic_fmri_cut[,i]
  ss1=cbind(value,fmri_picid_valance)
  model=summary(lm(value~Individual.valence.rating,data = ss1))
  models[[i]]=model
  pval=rbind(pval,c(model$coefficients[2,4],model$coefficients[3,4]))
}

adj.p=p.adjust(pval[,1])


#=======univariate analysis 93 individual ====
#individual valence without rounded
ind_valance = read.csv("/Users/chenghui/Documents/BCAI4/material/week9/data/Individual_valences_cut.csv")
ind_valance_ave=aggregate(Individual.valence.rating..1.pos.2.neu.3.neg.~pictureID, data=ind_valance,FUN='mean')
colnames(ind_valance_ave)[2]='Individual.valence.rating'

fmri_picid=read.csv("/Users/chenghui/Documents/BCAI4/material/week9/data/pictureIDs.csv",header = F)
colnames(fmri_picid)[1]='fmri.picid'

library(dplyr)
fmri_picid_valance=left_join(fmri_picid,ind_valance_ave,by=c('fmri.picid'='pictureID'))
fmri_picid_valance$Individual.valence.rating=as.factor(round(fmri_picid_valance$Individual.valence.rating))

enc_pic_fmri=read.csv("/Users/chenghui/Documents/BCAI4/material/week9/data/encoding_pic_fmri.csv",header = F);

na_flag=apply(is.na(enc_pic_fmri),2,sum)
enc_pic_fmri_93=enc_pic_fmri[,which(na_flag==0)]

enc_pic_fmria=rbind(enc_pic_fmri,c(1:246560))
enc_pic_fmri_cuta=enc_pic_fmria[,which(na_flag==0)]
# enc_pic_fmri_cuta$fmri_picid=fmri_picid$fmri.picid


pval93=c()
models=list()
for (i in 1:length(enc_pic_fmri_93)) {
  value=enc_pic_fmri_93[,i]
  ss1=cbind(value,fmri_picid_valance)
  model=aov(value~Individual.valence.rating,data = ss1)
  p=summary(model)[[1]][1,5]
  model1=TukeyHSD(model)
  #  models[[i]]=model
  pval93=rbind(pval93,c(p,model1$Individual.valence.rating[1,1],model1$Individual.valence.rating[1,4],
                        model1$Individual.valence.rating[2,1],model1$Individual.valence.rating[2,4],
                        model1$Individual.valence.rating[3,1],model1$Individual.valence.rating[3,4]))
}
pval93=data.frame(pval93)

# significant voxel (fmri~continuous averaged valance): 
# 34364 41923 (in cutted data frome)
# 121024 137228
# relative coordinate (32,48,22) (12,64,26)
# MNI coordinate (2.75,5.5,16) (57.75,49.5,32)
# floor()向下取 ceiling()向上取 round()

#========================
adj.p=p.adjust(pval[,1])

voxelid=enc_pic_fmri_cuta[73,which(pval93$X3<0.01)]
voxelz=ceiling(voxelid/(67*80))
voxely=ceiling((voxelid-(voxelz-1)*67*80)/67)
voxelx=voxelid-(voxelz-1)*67*80-(voxely-1)*67
voxelco93=as.data.frame(t(rbind(voxelid,voxelx,voxely,voxelz)))
colnames(voxelco93)=c('voxel id','x','y','z')

voxelmni93=voxelco93
voxelmni93$x=-2.75*voxelco93$x+90.75
voxelmni93$y=2.75*voxelco93$y-126.5
voxelmni93$z=4*voxelco93$z-72

voxelmni93_pos_0.01=voxelmni93
pval_93_pos_0.01=pval93[which(pval93$X3<0.01),]
min_pval0.01_93_pos=cbind(voxelmni93_pos_0.01,pval_93_pos_0.01)
colnames(min_pval0.01_93_pos)[5:11]=c('all','1vs2diff','1vs2p','1vs3diff','1vs3p','2vs3diff','2vs3p')
scatterplot3d(voxelmni93$x,voxelmni93$y,voxelmni93$z)
plot(log2(abs(min_pval0.01_93_pos$`1vs2diff`)),min_pval0.01_93_pos$`1vs2p`,pch='.')

write.csv(min_pval0.01_93_pos,'/Users/chenghui/Documents/BCAI4/material/week9/data/voxelmni93_all_0.005.csv')

voxelid=enc_pic_fmri_cuta[73,which(pval93$X7<0.01)]
voxelz=ceiling(voxelid/(67*80))
voxely=ceiling((voxelid-(voxelz-1)*67*80)/67)
voxelx=voxelid-(voxelz-1)*67*80-(voxely-1)*67
voxelco93=as.data.frame(t(rbind(voxelid,voxelx,voxely,voxelz)))
colnames(voxelco93)=c('voxel id','x','y','z')

voxelmni93=voxelco93
voxelmni93$x=-2.75*voxelco93$x+90.75
voxelmni93$y=2.75*voxelco93$y-126.5
voxelmni93$z=4*voxelco93$z-72

voxelmni93_neg_0.005=voxelmni93
pval_93_neg_0.005=pval93[which(pval93$X7<0.01),]
min_pval0.005_93_neg=cbind(voxelmni93_neg_0.005,pval_93_neg_0.005)
colnames(min_pval0.005_93_neg)[5:11]=c('all','1vs2diff','1vs2p','1vs3diff','1vs3p','2vs3diff','2vs3p')
scatterplot3d(voxelmni93$x,voxelmni93$y,voxelmni93$z)

# scatterplot3d(voxelmni93$x,voxelmni93$y,voxelmni93$z)

write.csv(min_pval0.005_93_neg,'/Users/chenghui/Documents/BCAI4/material/week9/data/voxelmni93_pval0.005_93_neg.csv')

min_pval0.01_93_pos$Group='#FFCC9950'
min_pval0.01_93_neg=min_pval0.005_93_neg
min_pval0.01_93_neg$Group='#CCCCFF50'
pos_neg=rbind(min_pval0.01_93_pos,min_pval0.01_93_neg)
scatterplot3d(pos_neg$x,pos_neg$y,pos_neg$z,color=pos_neg$Group,pch=16,zlim = c(-80,80),xlim=c(-80,80),ylim=c(-80,80))
# =========univariate data with 100 individual============
library(R.matlab)
enc_pic_fmri_100 <- readMat("/Users/chenghui/Documents/BCAI4/material/week9/data/100individual_enc_pic_data.mat")
enc_pic_fmri_100ds=data.frame(enc_pic_fmri_100[[1]])

#remove NaN
na_flaga_100=apply(is.na(enc_pic_fmri_100ds),2,sum)
enc_pic_fmri_cut_100=enc_pic_fmri_100ds[,which(na_flaga_100==0)]

# prepare index for voxels
enc_pic_fmria_100=rbind(enc_pic_fmri_100ds,c(1:246560))
enc_pic_fmri_cuta_100=enc_pic_fmria_100[,which(na_flaga_100==0)]

library(dplyr)
fmri_picid_valance=left_join(fmri_picid,ind_valance_ave,by=c('fmri.picid'='pictureID'))
fmri_picid_valance$Individual.valence.rating=as.factor(round(fmri_picid_valance$Individual.valence.rating))
pval_100=c()
#models=list()
for (i in 1:length(enc_pic_fmri_cut_100)) {
  value=enc_pic_fmri_cut_100[,i]
  ss1=cbind(value,fmri_picid_valance)
  model=aov(value~Individual.valence.rating,data = ss1)
  p=summary(model)[[1]][1,5]
  model1=TukeyHSD(model)
#  models[[i]]=model
  pval_100=rbind(pval_100,c(p,model1$Individual.valence.rating[1,4],model1$Individual.valence.rating[2,4],model1$Individual.valence.rating[3,4]))
}

pval_100=data.frame(pval_100)

#adj.p=p.adjust(pval_100[,1])
voxelid=enc_pic_fmri_cuta_100[73,which(pval_100$`2vs3`<0.05)]
voxelz=ceiling(voxelid/(67*80))
voxely=ceiling((voxelid-(voxelz-1)*67*80)/67)
voxelx=voxelid-(voxelz-1)*67*80-(voxely-1)*67
voxelco100=as.data.frame(t(rbind(voxelid,voxelx,voxely,voxelz)))
colnames(voxelco100)=c('voxel id','x','y','z')


voxelmni100=voxelco100
voxelmni100$x=-2.75*voxelco100$x+90.75
voxelmni100$y=2.75*voxelco100$y-126.5
voxelmni100$z=4*voxelco100$z-72

write.csv(voxelmni100,'/Users/chenghui/Documents/BCAI4/material/week9/data/voxelmni100_aov_pval_neg0.05_2vs3.csv')

library(scatterplot3d)
scatterplot3d(voxelmni100$x,voxelmni100$y,voxelmni100$z,zlim = c(-60,80))

pval_100_neg_sig=pval_100[which(pval_100$`2vs3`<0.05),]
min_pval_100_neg_sig=cbind(voxelmni100,pval_100_neg_sig)
write.csv(min_pval_100_neg_sig,"/Users/chenghui/Documents/BCAI4/material/week9/data/voxelmni100_aov_pval_neg0.05_2vs3.csv")

min_pval_100_neg_sig$group='red'
min_pval_100_pos_sig$group='green'
min_pval_100_sig=rbind(min_pval_100_pos_sig,min_pval_100_neg_sig)
scatterplot3d(min_pval_100_sig$x,min_pval_100_sig$y,min_pval_100_sig$z,color = min_pval_100_sig$group)

# univariate for arousal vs encoding phase
ind_arousal=read.csv('/Users/chenghui/Documents/BCAI4/material/week9/data/Individual arousal.csv')
colnames(ind_arousal)[3]='arousal'
ind_arousal_ave=aggregate(arousal~pictureID,FUN='mean',data = ind_arousal)

fmri_picid_arou=left_join(fmri_picid,ind_arousal_ave,by=c('fmri.picid'='pictureID'))
fmri_picid_arou$arousal=as.factor(round(fmri_picid_arou$arousal))

pval=c()
models=list()
for (i in 1:length(enc_pic_fmri_cut_100)) {
  value=enc_pic_fmri_cut_100[,i]
  ss1=cbind(value,fmri_picid_arou)
  model=aov(value~arousal,data = ss1)
  p=summary(model)[[1]][1,5]
  model1=TukeyHSD(model)
  #  models[[i]]=model
  pval=rbind(pval,c(p,model1$arousal[1,4],model1$arousal[2,4],model1$arousal[3,4]))
}
pval=data.frame(pval)
colnames(pval)=c('all','1vs2','1vs3','2vs3')

# arousal 
voxelid=enc_pic_fmri_cuta_100[73,which(pval$all<0.0005)]
voxelz=ceiling(voxelid/(67*80))
voxely=ceiling((voxelid-(voxelz-1)*67*80)/67)
voxelx=voxelid-(voxelz-1)*67*80-(voxely-1)*67
voxelco100=as.data.frame(t(rbind(voxelid,voxelx,voxely,voxelz)))
colnames(voxelco100)=c('voxel id','x','y','z')


voxelmni100=voxelco100
voxelmni100$x=-2.75*voxelco100$x+90.75
voxelmni100$y=2.75*voxelco100$y-126.5
voxelmni100$z=4*voxelco100$z-72

pval_100_sig=pval[which(pval$all<0.0005),]
min_arousal101_pval0.0005_sig=cbind(voxelmni100,pval_100_sig)
write.csv(min_pval_sig,"/Users/chenghui/Documents/BCAI4/material/week9/data/voxelmni100_arousal_aov_p0.0005.csv")

# =====univariate for arousal vs encoding phase -- 93 individual=====
ind_arousal=read.csv('/Users/chenghui/Documents/BCAI4/material/week9/data/Individual arousal.csv')
colnames(ind_arousal)[3]='arousal'
ind_arousal_ave=aggregate(arousal~pictureID,FUN='mean',data = ind_arousal)

fmri_picid_arou=left_join(fmri_picid,ind_arousal_ave,by=c('fmri.picid'='pictureID'))
fmri_picid_arou$arousal=as.factor(round(fmri_picid_arou$arousal))

pval=c()
models=list()
for (i in 1:length(enc_pic_fmri_93)) {
  value=enc_pic_fmri_93[,i]
  ss1=cbind(value,fmri_picid_arou)
  model=aov(value~arousal,data = ss1)
  p=summary(model)[[1]][1,5]
  model1=TukeyHSD(model)
  #  models[[i]]=model
  pval=rbind(pval,c(p,model1$arousal[2,1],model1$arousal[2,4]))
}
pval=data.frame(pval)
colnames(pval)=c('all','1vs3diff','1vs3p')

# arousal 
voxelid=enc_pic_fmri_cuta[73,which(pval$all<0.001)]
voxelz=ceiling(voxelid/(67*80))
voxely=ceiling((voxelid-(voxelz-1)*67*80)/67)
voxelx=voxelid-(voxelz-1)*67*80-(voxely-1)*67
voxelco93=as.data.frame(t(rbind(voxelid,voxelx,voxely,voxelz)))
colnames(voxelco93)=c('voxel id','x','y','z')

voxelmni93=voxelco93
voxelmni93$x=-2.75*voxelco93$x+90.75
voxelmni93$y=2.75*voxelco93$y-126.5
voxelmni93$z=4*voxelco93$z-72

pval_93_sig=pval[which(pval$all<0.001),]
min_arousal93_pval0.001_sig=cbind(voxelmni93,pval_93_sig)
scatterplot3d(min_arousal93_pval0.001_sig$x,min_arousal93_pval0.001_sig$y,min_arousal93_pval0.001_sig$z,pch=16,
              zlim = c(-80,80),xlim=c(-80,80),ylim=c(-80,80),color = '#99CC3350',
              xlab = 'lateral',ylab = 'anterior--posterior',zlab = 'dorsal--ventral')
write.csv(min_arousal93_pval0.001_sig,"/Users/chenghui/Documents/BCAI4/material/week9/data/voxelmni93_arousal_aov_p0.001.csv")
