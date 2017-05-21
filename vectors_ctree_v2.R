{
options(digits.secs=3)
{
rm(list=ls())
  library(party)
  library(rpart)
  wind<-40 #window size for kmeans
  feat_wind<-5
options(digits.secs=5)

getwd()
}

getmode<-function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

  stringer<-c("GyroData_xyz.csv")
  combined_fin<-data.frame()
  combined_fin<-read.table(stringer, na.strings=c("","NA", " ", "0"), sep = ",", header = TRUE)

combined_fin$Event<-as.factor(combined_fin$Event)



#wind_df<-data.frame()


wind_ls<-list()


#which(combined_fin[,(levels(wind_df[,(wind*3+1)])[1]))]$km

#rm(event_based_list)
#event_based_list<-list()

event_types<-as.factor(levels(combined_fin$Event))


i<-1

spl_ev_inception<-list()
split_events<-split(combined_fin, combined_fin$Event)
split_events<-na.omit(split_events)
#for(i in 1: length(split_events))

k_split<-list()
c_split<-list()
b_split<-list()
t_split<-list()
s_split<-list()
br_split<-list()
z_split<-list()
}

i<-1
for(i in 1:length(split_events))
{
  spl_ev_inception<-list()
  j<-1  
  k<-1
  while(j<length(split_events[[i]]$id))
  {
    split_events[[i]]<-na.omit(split_events[[i]])
    if(!is.na(which((split_events[[i]]$id-split_events[[i]]$id[j])>140)[k]-(k+1)))
    {
      spl_ev_inception[[k]]<-split_events[[i]][(j:(which((split_events[[i]]$id-split_events[[i]]$id[j])>140)[k]-(k+1))),]
      j<-(which((split_events[[i]]$id-split_events[[i]]$id[j])>140)[k])
      k<-k+1
    }
    {}  
    if(is.na(which((split_events[[i]]$id-split_events[[i]]$id[j])>140)[k]-(k+1)))
    {  break()
    }
    
  }
  
  if(length(spl_ev_inception)==0)
  {
    i<-i+1
  }
  
  if(spl_ev_inception[[1]]$Event[[1]]=="k")
  {
    k_split<-spl_ev_inception
    
  }
  
  if(spl_ev_inception[[1]]$Event[[1]]=="c")
  {
    c_split<-spl_ev_inception
    
  }
  
  if(spl_ev_inception[[1]]$Event[[1]]=="b")
  {
    b_split<-spl_ev_inception
    
  }
  
  if(spl_ev_inception[[1]]$Event[[1]]=="br")
  {
    br_split<-spl_ev_inception
    
  }
  if(spl_ev_inception[[1]]$Event[[1]]=="t")
  {
    t_split<-spl_ev_inception
    
  }
  
  if(spl_ev_inception[[1]]$Event[[1]]=="s")
  {
    s_split<-spl_ev_inception
    
  }
  
  if(spl_ev_inception[[1]]$Event[[1]]=="z")
  {
    z_split<-spl_ev_inception
    
  }
  rm(spl_ev_inception)
}

{
  events_based_list<-list() # This sequence should be kept in mind for getting the required data
  events_based_list[[1]]<-t_split
  events_based_list[[2]]<-s_split
  events_based_list[[3]]<-c_split
  events_based_list[[4]]<-b_split
  events_based_list[[5]]<-br_split
  events_based_list[[6]]<-k_split
  events_based_list[[7]]<-z_split
  
}

{
  t_frame<-data.frame()
  s_frame<-data.frame()
  b_frame<-data.frame()
  br_frame<-data.frame()
  k_frame<-data.frame()
  c_frame<-data.frame()
  z_frame<-data.frame()
}

i<-1


wind_half<-wind/2


for (i in 1:length(events_based_list))
{
   sp_ev_wind<-list()  
  
  j<-1

  while(j <length(events_based_list[[i]]))
  {
    if(!(length(events_based_list[[i]][[j]]$id)<=(wind)))
    {event_mid<-(length(events_based_list[[i]][[j]]$id)/2)
    sp_ev_wind[[j]]<-events_based_list[[i]][[j]][(ceiling(event_mid-wind_half)):(ceiling(event_mid+wind_half-1)),]
    j<-j+1}else{j<-j+1}
    
  }
  
  {
    
    if(sp_ev_wind[[1]]$Event[[1]]=="t")
    {
      t_frame<-sp_ev_wind[[1]]
      for(j in 2:length(sp_ev_wind))
      {
        
        t_frame<-rbind.data.frame(t_frame, sp_ev_wind[[j]])
      }
    }
    
    
    
    
    
    if(sp_ev_wind[[1]]$Event[[1]]=="k")
    {
      k_frame<-sp_ev_wind[[1]]
      for(j in 2:length(sp_ev_wind))
      {
        
        k_frame<-rbind.data.frame(k_frame, sp_ev_wind[[j]])
      }
    }
    
    
    
    
    if(sp_ev_wind[[1]]$Event[[1]]=="c")
    {
      c_frame<-sp_ev_wind[[1]]
      for(j in 2:length(sp_ev_wind))
      {
        
        c_frame<-rbind.data.frame(c_frame, sp_ev_wind[[j]])
      }
    }
    
    
    
    
    if(sp_ev_wind[[1]]$Event[[1]]=="br")
    {
      br_frame<-sp_ev_wind[[1]]
      for(j in 2:length(sp_ev_wind))
      {
        
        br_frame<-rbind.data.frame(br_frame, sp_ev_wind[[j]])
      }
    }
    
    
    
    
    if(sp_ev_wind[[1]]$Event[[1]]=="b")
    {
      b_frame<-sp_ev_wind[[1]]
      for(j in 2:length(sp_ev_wind))
      {
        
        b_frame<-rbind.data.frame(b_frame, sp_ev_wind[[j]])
      }
    }
    
    
    
    
    if(sp_ev_wind[[1]]$Event[[1]]=="s")
    {
      s_frame<-sp_ev_wind[[1]]
      for(j in 2:length(sp_ev_wind))
      {
        
        s_frame<-rbind.data.frame(s_frame, sp_ev_wind[[j]])
      }
    }
    
    
    
    
    if(sp_ev_wind[[1]]$Event[[1]]=="z")
    {
      z_frame<-sp_ev_wind[[1]]
      if(length(sp_ev_wind)>wind)
      {
        for(j in 2:length(sp_ev_wind))
        {
          z_frame<-rbind.data.frame(z_frame, sp_ev_wind[[j]])
        }
        
      }
      
    }
    
    
  }
  
  
  
  
  rm(sp_ev_wind)
}

rm(combined_fin)

combined_fin<-data.frame()
combined_fin<-rbind.data.frame(t_frame,k_frame,b_frame, br_frame, s_frame, c_frame,z_frame)




wind_df<-combined_fin



event_types<-as.factor(levels(combined_fin$Event))



k<-1
for (k in 1:(floor(length(combined_fin$x)-k+wind-1)/wind))
  
{
  w_index<-k
  if(k>1)
  {
    k<-(k-1)*wind+1
  }
  
  wind_Event<-combined_fin$Event[[k]]
  
  #mode(c(1:20))
  X<-data.frame()
  Y<-data.frame()
  Z<-data.frame()
  #names(x)<-
  x<-combined_fin[k:(k+wind-1),]$x
  X<-cbind.data.frame(split(x,c(k:(k+wind-1))))
  colnames(X) <- paste("x", k:(k+wind-1), sep = "")
  
  y<-combined_fin[k:(k+wind-1),]$y
  Y<-cbind.data.frame(split(y,c(k:(k+wind-1))))
  colnames(Y) <- paste("y", k:(k+wind-1), sep = "")
  
  z<-combined_fin[k:(k+wind-1),]$z
  Z<-cbind.data.frame(split(z,c(k:(k+wind-1))))
  colnames(Z) <- paste("z", k:(k+wind-1), sep = "")
  
  gyro_vector<-cbind(X,Y,Z)
  
  
  
  
  
  wind_vector<-cbind(gyro_vector, wind_Event)
  wind_vector[1,]
  wind_ls[[w_index]]<-wind_vector[1,]
  
  
  remove(X)
  remove(Y)
  remove(Z)
  remove(wind_vector)
}

wind_df<-data.frame()


i<-1
j<-1
for(i in 1:length(wind_ls))
{
  
  for(j in 1:length(wind_ls[[1]]))
  {
    wind_df[i,j]<-wind_ls[[i]][[j]]
    
    
  }
  
}

names(wind_df)<-names(wind_ls[[1]])
wind_df<-na.omit(wind_df)


#------------------------Debugged Till This point---------------------------
#------------------------Problem detected in the following section




wind_df<-na.omit(wind_df)

# model<-rpart(wind_Event~., data = wind_df)
# tr<-ctree(wind_Event ~ . , wind_df)
# plot(model)
# 
#mean_wind<-data.frame(ncol(feat_wind))# plot(tr)

#feat_wind<-2
i<-1
j<-1
k<-1

mean_w_cmb<-list()
mean_wind<-data.frame(ncol(3*wind/feat_wind))
mean_names<-c(rep(0, 3*wind/feat_wind))

for(k in 1:length(wind_df$x1))
    {
      for(i in 1:3)
      {
        for(j in 1:(wind/feat_wind))
        {
          {
            indx<-1+((j-1)*feat_wind)+((i-1)*(wind))
            feat_w_tail<-(indx+feat_wind-1)
            if((feat_wind)>=2){#Only not true once when feat_wind=1
              mean_wind[k,(j+(i-1)*(wind/feat_wind))]<-mean(t(wind_df[k,indx:feat_w_tail]))
            }else{mean_wind[k,(j+(i-1)*(wind/feat_wind))]<-(wind_df[k,(j+(i-1)*(wind/feat_wind))])
            }
            if(k==length(wind_df$x1))
              mean_names[feat_w_tail/feat_wind] <- paste("Mean", colnames(wind_df[indx,])[(feat_w_tail)], sep = "_")
          }
        }
      }      
      
}

names(mean_wind)<-mean_names
rm(mean_names)


r_names<-c(rep(0, wind/feat_wind))

energies<-data.frame(matrix(ncol=(wind/feat_wind), nrow=(length(wind_df$x1))))
i<-1

for (i in 1:(wind/feat_wind))
{ 
  energies[,i]<-sqrt(mean_wind[,i]^2+mean_wind[,i+(wind/feat_wind)]^2+mean_wind[,i+(2*wind/feat_wind)]^2)
  i<-i
  }
 
  
energy_data<-cbind(wind_df$wind_Event,energies)

rm(wind_df)
names(energy_data)<-c("events_", "r1", "r2", "r3", "r4")



model<-rpart(events_~., energy_data)
plot(model)

mean_wind$wind_Event<-wind_df$wind_Event



mean_wind$wind_Event<-wind_df$wind_Event
dataS<-wind_df
set.seed(123)
trainSize <- round(nrow(dataS) * .7)
testSize <- nrow(dataS) - trainSize

training_indices <- sample(seq_len(nrow(dataS)), size=trainSize)
trainSet <- dataS[training_indices, ]
testSet <- dataS[-training_indices, ]



model<-ctree(wind_Event~., data = dataS, controls = ctree_control(  mincriterion =0.1))
plot(model)

p_for_event<-"c"
pind<-which(testSet$wind_Event==p_for_event)

k_predic<-predict(model, testSet[pind,])

k_total<-length(which(k_predic==p_for_event))
k_acc<-k_total/length(k_predic)

tpr<-c(rep(0,7))

for(i in 1:7)
{
  cur_ev<-levels(testSet$Event)[i]
  pind<-which(testSet$Event==cur_ev)
  
  predic<-predict(model2, testSet[pind,])
  
  tp<-length(which(predic==cur_ev))
  fn<-length(which(predic!=cur_ev))
  
  
  tpr[i]<-tp/(tp+fn)
  
  
}

