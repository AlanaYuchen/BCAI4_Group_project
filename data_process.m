csvwrite('encoding_pic_fmri.csv',enc_pic);

% count col
a=sum(enc_pic,1);
col=all(~isnan(a))

% filter out rows with NaNs
enc_pic_cut=enc_pic(:,all(~isnan(enc_pic),1));
csvwrite('encoding_pic_fmri_cut.csv',enc_pic_cut)

% fmri: 67x80x46

enc_pic_reshaped=reshape(enc_pic,[72 67 80 46]);
size(enc_pic_reshaped)

% reshape new data with 100 individuals
meanmapsre=reshape(meanmap,[72 246560]);

save enc_pic_reshape_93data.mat enc_pic_reshaped
