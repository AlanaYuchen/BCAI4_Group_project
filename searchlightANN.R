#import library
library(R.matlab)


#import 4D matrix
#shape 72*67*80*46
reshape4D_bypic = readMat('/Users/.../workshop9/enc_pic_reshaped.mat')
reshape4D_bypic_array = reshape4D_bypic[["enc.pic.reshaped"]] #72 67 80 46

#searchlight analysis-----------------------------------------------------
#loop for 72 pic in reshape4D_bypic with step 4*4*4 /3*3*3
#for 4*4*4 kernal that have 9 or 8 actual numbers, restore it
#store position information in encpic_kernal_position(list)
#store actual number in 72*(1215)*4*4*4 in encpic_kernal(list/array?)

#function input 3D array
#function output 2000 3D kernals in a list(array)
extract_kernal = function(encpic){
  #intialization
  count=0
  kernals = c()
  positions = c()
  
  #cleavage 67*80*46->66*78*45
  for (i in seq(1,66,3)){
    #j=1,4,7,10,13,..
    #3*78*45
    for (j in seq(1,78,3)){
      #3*3*45
      for (k in seq(1,45,3)){
        #3*3*3
        #extract kernal
        kernal = encpic[i:(i+2),j:(j+2),k:(k+2)]
        #select kernals with na less than 3
        if (sum(is.na(kernal)) == 0){
          count=count+1
          kernals=c(kernals,kernal)
          position=c(i,j,k)
          positions=c(positions,position)
        }
      }
    }
  }
  
  #return
  output_positions = array(positions,dim=c(3,count))
  output_kernals = array(kernals,dim = c(27,length(kernals)/27)) #27*1792
  list(output_kernals,output_positions,count)
  
}


#apply function and store 72 fMRI data with 1792 kernals in list
encpic_kernal=list()

for (i in 1:72){
  kernal_array = extract_kernal(reshape4D_bypic_array[i,,,])[[1]] #27*1792
  encpic_kernal[[i]]=kernal_array
}

#each pic has same
position_array = extract_kernal(reshape4D_bypic_array[1,,,])[[2]] #1792
position_array_t= t(position_array)

#build ANN---------------------------------------------------------------
library(keras)
library(neuralnet)

applyANN = function(input_matrix,trainlabel_cat,testlabel_cat){
  #add all model code here
  #first cut input matrix in to train and test
  trainset=input_matrix[1:54,]
  testset=input_matrix[55:72,]
  
  k_clear_session()
  
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 27, activation = 'relu', input_shape = c(27)) %>%
    layer_dense(units = 10, activation = 'relu') %>%
    layer_dense(units = 3,activation = 'softmax') 
  
  #complie
  model %>% compile(loss = 'categorical_crossentropy',
                    optimizer = 'nadam',
                    #optimizer_adam(lr = 0.2)
                    metrics = 'accuracy') 
  
  #traning data
  mymodel <- model %>%          
    fit(trainset,trainlabel_cat,
        epochs = 28,
        batch_size = 54,
        validation_split = 0.2, shuffle = T)
  
  #prediction using artificial neuron network
  result = keras::evaluate(model,testset, testlabel_cat)
  #return
  list(result$loss,result$acc)
  
  #return2 (hand calculation)
  #predmodel <- model %>% predict(testset)
  #pred_label=apply(predmodel, 1, function(x){order(x, decreasing=T)[1]})
  #sum(pred_label==groundtruth)
  
}



#use ann model--------------------------------------------------------------

#make label
library(dplyr)
pic_id=read.csv('/Users/.../workshop9/Phenos/pictureIDs.csv',header = F)
valences=read.csv('/Users/.../workshop9/Individual_valences_cut.csv')
colnames(pic_id) = c('pictureID')
colnames(valences) = c('pictureID','v')
avg_valences=aggregate(valences[,3],by=list(valences$pictureID),FUN=mean)
colnames(avg_valences) = c('pictureID','v')
label=inner_join(pic_id,avg_valences)

#seperate train/test label and convert to category for softmax function
index=seq(1,72)
label=cbind(label,index)
#important sequence
shuffle_id=sample(label$index,length(label$index))

samplelabel=c()
for (id in shuffle_id){
  #print(round(label[id,2]))
  samplelabel=c(samplelabel,samplelabel=round(label[id,2]))
}
trainlabel=round(samplelabel[1:54])
trainlabel_cat = to_categorical(trainlabel)[,-1]

testlabel=round(samplelabel[55:72])
testlabel_cat = to_categorical(testlabel)[,-1]

#chance level detection----------
#make shuffle label
shuffle_label=sample(label$v,length(label$v))
shuffle_label2=sample(label$v,length(label$v))
shuffle_label3=sample(label$v,length(label$v))

label = cbind(label,shuffle_label2,shuffle_label3)
#seperate shuffled label into train and test
shuffle_trainlabel=round(label[1:54,5])
shuffle_trainlabel_cat = to_categorical(shuffle_trainlabel)[,-1]

shuffle_testlabel=round(label[55:72,5])
shuffle_testlabel_cat = to_categorical(shuffle_testlabel)[,-1]


#for user running, start here--------------------------------------------

#scale here
encpic_kernal_scale=list()
for (g in 1:72) {
  encpic_kernal_scale[[g]]=scale(encpic_kernal[[g]])
}

#extract data 
running_cluster_number = 1792

#take 3 samples
for (loop in 1:3) {
  output_matrix = matrix(NA, nrow = running_cluster_number, ncol = 2)

  shuffle_trainlabel=round(label[1:54,loop+2])
  shuffle_trainlabel_cat = to_categorical(shuffle_trainlabel)[,-1]
  
  shuffle_testlabel=round(label[55:72,loop+2])
  shuffle_testlabel_cat = to_categorical(shuffle_testlabel)[,-1]
  
  
  for (n in 1:running_cluster_number){
    
    #for each n (cluster) conver into 72*27 input map
    input_matrix = matrix(NA,nrow = 72, ncol=27)
    pos=0
    #matching data and sequenced id
    for (id in shuffle_id){
      pos=pos+1
      excpic_kernal_1D = c(encpic_kernal_scale[[id]][,n]) #1*27
      input_matrix[pos,]=excpic_kernal_1D # ->72*27
    }
    
    
    #run ann one time
    return_list = applyANN(input_matrix,shuffle_trainlabel_cat,shuffle_testlabel_cat)
    return_loss = return_list[[1]]
    return_acc = return_list[[2]]
    output_matrix[n,]=c(return_loss,return_acc)
  }
  
  #write down output
  if (loop == 1){
    shuffle_run1 = output_matrix
  }else if (loop == 2){
    shuffle_run2 = output_matrix
  }else if (loop == 3){
    shuffle_run3 = output_matrix}
  
}

tryrun=cbind(run1,run2,run3)

