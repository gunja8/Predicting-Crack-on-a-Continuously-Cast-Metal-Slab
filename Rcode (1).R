
###################################     ABOUT THE PROJECT and DATA     #####################################

#--------------------------------------Data Challenge by SMS digital---------------------------------------#

# Casting slabs of quality steel is a demanding process that is highly sensitive to changes in its production
# and environmental parameters. There are several types of casting defects that reduce the quality of the end 
# product.End customers in the automotive industry for instance accept a rejection rate of 20% for some sorts
# of steel because the state-of-the-art casting process does not allow for better results. 
#
# The potential for optimization and the implied business opportunity are huge.

# OBJECTIVE:
# Locate longitudinal cracks on the slab's surface with a higher reliability than the pre-existing PCA.


#----------------------------------------HOW TO RUN THE CODE-----------------------------------------------#

# HOW TO RUN THE CODE
# To run the following code folow the simple steps:
# 1. Download the ZIP file from SMS Digital.
# 2. Remember to unZIP, both the first file and the folder inside to extract the core data files. USE EXTRACT HERE.
# 3. Create a Folder in Documents with name "POC1". Create Sub folders in POC1 as "train" and "test".
# 4. Coppy all data files from train and test data in POC1\train and POC1\test respectively.
# 5. RENAME the test data files. Use prefix test. For example "MEASUREMENT000" will become "testMEASUREMENT000"
# 6. Run the Code in Sequence. If you face problem importing the files, you can manually extract in RStudio 
#    enviornment using "Import Dataset" option in enviornment tab. Import all the tables as mentioned.
# 7. REMEMBER the default folder for output of any file is Documents. You might find an exported file there.

#----------------------------------------------------------------------------------------------------------#

####################################     CREATING "TRAINING DATA-SET"     ###################################

# About the training data:
# The trainig data was taken from the web sight of SMS digital competition and it contains followin tables:
# 1. HEAT
# 2. DEFECT
# 3. MEASUREMENT000
# 4. MEASUREMENT001
# 5. SENSOR
# 6. SLAB

# The HEAT, SENSOR, SLAB are the support tables which provide additional informaion about the arrangement
# done during data collection.
# The MEASUREMENT tables give the training data. And DEFECT table give the response variable.
# Thus, the step 1 is to create a unified table on which training can be done. Lets name the table "traindata".

#--------------------------------------------------IMPORTING DATA ------------------------------------------#

# Importing data when it is stored in folder ~/POC1/train.

DEFECT <- read.csv("~/POC1/train/DEFECT.csv", stringsAsFactors=FALSE)
HEAT <- read.csv("~/POC1/train/HEAT.csv", sep=";", stringsAsFactors=FALSE)
MEASUREMENT000 <- read.csv("~/POC1/train/MEASUREMENT000.csv", sep=";", stringsAsFactors=FALSE)
MEASUREMENT001 <- read.csv("~/POC1/train/MEASUREMENT001.csv", sep=";", stringsAsFactors=FALSE)
SENSOR <- read.csv("~/POC1/train/SENSOR.csv", stringsAsFactors=FALSE)
SLAB <- read.csv("~/POC1/train/SLAB.csv", sep=";", stringsAsFactors=FALSE)

#----------------------------------------------CREATING COLUMN NAMES-----------------------------------------#

# The Following code created a Keys vector of characters, these keys will be used as column names in traindata or 
# testdata sets.

t=colnames(MEASUREMENT000)
Keys=c(1:177)

Keys[1]=t[2]
Keys[2]=t[3]
Keys[3]=t[4]

for (i in 1:174){
  Keys[i+3]=MEASUREMENT000$Key[i]
}

print(Keys)
rm(t)

#---------------------------------------------removing useless data---------------------------------------#

# Some of the data columns in the MEASUREMENT data were not being used or have been taken in Keys vector as 
# created above. Hence deleting these columns and then merging the two MEASUREMENT tables to form just one 
# MEASUREMENT table. This is done using rbind function. Post this poeration, the 2nd MEASUREMENT table is of 
# use and we can delete that to clear the memory space.

MEASUREMENT000=MEASUREMENT000[-c(1,5,6,8,9,10)]

MEASUREMENT001=MEASUREMENT001[-c(1,5,6,8,9,10)]

MEASUREMENT000=rbind(MEASUREMENT000,MEASUREMENT001)
rm(MEASUREMENT001)

#--------------------------------------------check NA values--------------------------------------------#

# This code is the check if there is an NA Value in the SlabID column of the newly created MEASUREMENT000 
# table. It displays number of NA values and then also displays the row number of these NA values.
# In case these NA Value exist, just replace them manually with the SlabID or one above/bolow depending 
# upon the sequence number.
# Number of rows in MEASUREMENT000,x and Number of slabs,l values will be used later also.

x=length(MEASUREMENT000$SlabID)
l=length(SLAB$SlabID)


sum(is.na(MEASUREMENT000$SlabID))

for (i in 1:x){
  if(is.na(MEASUREMENT000$SlabID[i])){
    print(MEASUREMENT000$MeasurementID[i])
  }
}

#----------------------------------------checking the slab indices----------------------------------------------#

# To check where one slab ends, and other begins. The following code create a slab_end vector and then checks the 
# slabID in interval of every 5000 readings. If the next SalbID is different, the earlier one is saved. After doing
# over the complete MEASUREMENT table, we have an approximate slab_end vector. All we need to do is to check the
# next 5000 entries from this starting point available in approximate slab_end vector.
# The later loop doest the 2nd part and gives out exact slab_end vector.

slab_end=c(1:l)
k=1
for (i in seq(1,x,5000)){
  if (MEASUREMENT000$SlabID[min(i+5000,x)]!=MEASUREMENT000$SlabID[i]){
    slab_end[k]=i
    k=k+1
  }
}
slab_end[l]=x

for (i in 1:(l-1)){
  for (j in slab_end[i]:slab_end[i+1]){
    if (MEASUREMENT000$SlabID[j+1]!=MEASUREMENT000$SlabID[j]){
      slab_end[i]=j
      break()
    }
  }
}

# The following loop creates the sequenct of SlabID as in the MEASUREMENT table. The sequence is saved in vector
# called slab_vector.

slab_vector=c(1:l)
for (i in 1:l){
  slab_vector[i]=MEASUREMENT000$SlabID[slab_end[i]]
}
#--------------------------------------------------------------------------------------------------------------#

DEFECT["NewSlabID"]="NA"

for (i in 1:length(DEFECT$SlabID)){
  for (j in 1:133){
    if (DEFECT[i,2]==slab_vector[j])
    {
      DEFECT$NewSlabID[i]=j
      break()
    }
  }
}

#----------------------------------------------------------------------------------------------------------#

# The following code now creates the temp matrix called a. This a matrix contains the data as needed for any
# Lerning algorithm will need, all variables as columns. This matrix can use the column names as Keys Created
# start of the program. After a is constructed the file is exported to dafault folder, i.e documents folder, 
# by the name completedata.csv

# completedata is now imported back and a deleted. This makes the complate data as table. When we have
# completedata, a is no more needed and is deleted from the memory.

a=matrix(
  nrow=(x%/%174),
  ncol=177)

colnames(a)=Keys

for (i in c(1:(x%/%174))){
  
  print(MEASUREMENT000$SlabID[174*i-173])
  
  a[i,1]=MEASUREMENT000$SlabID[174*i-173]
  a[i,2]=MEASUREMENT000$HeatID[174*i-173]
  a[i,3]=MEASUREMENT000$MeasurementSequence[174*i-173]
  
  for (j in c(1:174)){
    a[i,j+3]=MEASUREMENT000$Value[174*(i-1)+j]}
}

write.csv(a,"completedata.csv")
completedata <- read.csv("~/completedata.csv", stringsAsFactors=FALSE)
rm(a)
rm(MEASUREMENT000)
#--------------------------------------------------------------------------------------------------------#

# Now, as we have complete data in a good format for training, all we need to do it mention response variable
# from the DEFECT table. 

# We are defining the crack to exist if the crack exist in the last belt of the sensors. That is, if the crack 
# existed in last belt, we may or may not find it in previous belts, but if it exists in presious belts, it
# must exist in last belt too.
# Defining sensor belt as sensors having same Y coordinate. We can obsrve that there are 5 belts.

# This is done by adding 3 responce columns to completedata table, namely 
# 1. CrackYN: This will hav BOOL value, if crack exist at that point 1, Else 0
# 2. CrackKloc: This is Xstart location of crack, after a little data browsing, Xstart=Xend, thus it is actual X
# 3. CrackSide: The Longitudnal cracks can be on two face sides only, this column will hold FS or LS value for
#    Fixed Side and Loose Side respecctively.

# The final Data Table is exported as "traindata.CSV"

completedata["CrackYN"]=0
completedata["CrackXloc"]="NA"
completedata["CrackSide"]="NA"

y=length(completedata$SlabID)
L=length(DEFECT$SlabID)

for (i in 1:y){
  if (any(DEFECT$SlabID==completedata$SlabID[i])){
    temp=completedata$ActCastLength[i]-0.72
    for (j in 1:L){
      if (temp>=DEFECT$DefectStart_Y[j] && temp<=DEFECT$DefectEnd_Y[j]){
        completedata$CrackYN[i]=1
        completedata$CrackXloc[i]=DEFECT$DefectStart_X[j]
        completedata$CrackSide[i]=DEFECT$DefectSlabSide[j]
      }
    }
  }
}

write.csv(completedata,"traindata.CSV")
traindata <- read.csv("~/traindata.CSV", stringsAsFactors=FALSE)
rm(completedata)


###########################     DIVIDING TRAINDATA into INTERNAL TEST & TRAIN    ###############################

defect_vector = c(1:l)

for (i in 1:l){
  if (any(DEFECT$SlabID==slab_vector[i])){
    defect_vector[i]=1
  }
  else defect_vector[i]=0
}

defected_slab = c(1:sum(defect_vector))
k=0
for (i in 1:l){
  if (defect_vector[i]==1) {
    defected_slab[k]=i
    k=k+1
  }
}
non_defected_slab = c(1:(l-sum(defect_vector)))
k=0
for (i in 1:l){
  if (defect_vector[i]==0) {
    non_defected_slab[k]=i
    k=k+1
  }
}

nondef = sample(non_defected_slab, 40, replace = FALSE)
def = sample(defected_slab, 3, replace = FALSE)

nondefslabs=c(1:40)
defslabs=c(1:3)

for (i in 1:40){
  nondefslabs[i]=slab_vector[nondef[i]]  
}

for (i in 1:3){
  defslabs[i]=slab_vector[def[i]]
}
nondefslabs=c(nondef,nondefslabs)

internal_train = matrix(ncol=180)
internal_test = matrix(ncol=180)

colnames(internal_test)=colnames(traindata)
colnames(internal_train)=colnames(traindata)

for (i in 1:y){
  if ( any(nondefslabs==traindata[i,1]) ){
    internal_test = rbind(internal_test,traindata[i,])
  }
  else {internal_train = rbind(internal_train, traindata[i,])}
  print(i)
}


write.csv(internal_test,"internal_test.csv")
write.csv(internal_train,"internal_train.csv")


#######################################     CREATING TEST DATA-SET    ########################################

# Refer to train code for explanation. The code does similar operation fro test data too.


#---------------------------------------------IMPORTING TEST DATA ------------------------------------------#

# Importing data when it is stored in folder ~/POC1/test.

testHEAT <- read.csv("~/POC1/test/testHEAT.csv", sep=";", stringsAsFactors=FALSE)
testMEASUREMENT000 <- read.csv("~/POC1/test/testMEASUREMENT000.csv", sep=";", stringsAsFactors=FALSE)
testSENSOR <- read.csv("~/POC1/test/testSENSOR.csv", stringsAsFactors=FALSE)
testSLAB <- read.csv("~/POC1/test/testSLAB.csv", sep=";", stringsAsFactors=FALSE)

#----------------------------------------------CREATING COLUMN NAMES-----------------------------------------#


t=colnames(testMEASUREMENT000)
Keys=c(1:177)

Keys[1]=t[2]
Keys[2]=t[3]
Keys[3]=t[4]

for (i in 1:174){
  Keys[i+3]=testMEASUREMENT000$Key[i]
}

print(Keys)
rm(t)

#---------------------------------------------removing useless data---------------------------------------#


testMEASUREMENT000=testMEASUREMENT000[-c(1,5,6,8,9,10)]

#--------------------------------------------check NA values--------------------------------------------#


x=length(testMEASUREMENT000$SlabID)
l=length(testSLAB$SlabID)


sum(is.na(testMEASUREMENT000$SlabID))

for (i in 1:x){
  if(is.na(testMEASUREMENT000$SlabID[i])){
    print(testMEASUREMENT000$MeasurementID[i])
  }
}

#----------------------------------------checking the slab indices----------------------------------------------#
slab_end=c(1:l)
k=1
for (i in seq(1,x,5000)){
  if (testMEASUREMENT000$SlabID[min(i+5000,x)]!=testMEASUREMENT000$SlabID[i]){
    slab_end[k]=i
    k=k+1
  }
}
slab_end[l]=x

for (i in 1:(l-1)){
  for (j in slab_end[i]:slab_end[i+1]){
    if (testMEASUREMENT000$SlabID[j+1]!=testMEASUREMENT000$SlabID[j]){
      slab_end[i]=j
      break()
    }
  }
}

# The following loop creates the sequenct of SlabID as in the MEASUREMENT table. The sequence is saved in vector
# called slab_vector.

slab_vector=c(1:l)
for (i in 1:l){
  slab_vector[i]=testMEASUREMENT000$SlabID[slab_end[i]]
}
#--------------------------------------------------------------------------------------------------------------#

a=matrix(
  nrow=(x%/%174),
  ncol=177)

colnames(a)=Keys

for (i in c(1:(x%/%174))){
  
  print(testMEASUREMENT000$SlabID[174*i-173])
  
  a[i,1]=testMEASUREMENT000$SlabID[174*i-173]
  a[i,2]=testMEASUREMENT000$HeatID[174*i-173]
  a[i,3]=testMEASUREMENT000$MeasurementSequence[174*i-173]
  
  for (j in c(1:174)){
    a[i,j+3]=testMEASUREMENT000$Value[174*(i-1)+j]}
}

write.csv(a,"testdata.csv")
testdata <- read.csv("~/testdata.csv", stringsAsFactors=FALSE)
rm(a)
rm(testMEASUREMENT000)


########################################     USING R-PART ALGORITHM     #########################################

# Final_Train <- df[ -c( 3:6) ]
# The data that we have now is in a format that can be used as an input file to the model.
# We have used an algorithm to train data that works on recursive partitioning.
# First of all we will install packages rpart.plot and rpart which are required for this algorithm to work.
# As we have three response variable in our model so we will train data three times in the following manner:
#   1. Train the data by taking only CrackYN as a response variable. 
#   2. Then we will train the data taking CrackXloc as response variable.
#   3. Then we will train data taking CrackSide as response variable.
# The trained model rpart.model that we get now is the one which is trained using three different response variables.
# Next we predict values of three response variable for test dataset using the fitted rpart model
# Complexity factor Cp=0.0055 is used for prruning the tree.
# Then we will Plot the Regression Tree


install.packages("rpart.plot")
library(rpart.plot)
library(rpart)

#train <- read.csv(choose.files(), header = T, stringsAsFactors = F)
#test <- read.csv(choose.files(), header = T, stringsAsFactors = F)

rpart.model <- rpart(train$CrackYN ~ .,data = train[,1:107], method = "anova",parms = list(split="information"))
#  1.Trains data taking CrackYN as response variable.

rpart.model <- rpart(train$CrackXloc ~ .,data = train[,1:107], method = "anova",parms = list(split="information"))

#  2.Trains data taking CrackXloc as response variable

rpart.model <- rpart(train$CrackSide ~ .,data = train[,1:107], method = "anova",parms = list(split="information"))
#  3.Trains data taking CrackSide as response variable

rpart.model

rpart.plot(annova.model)


rpart.prediction <- predict(rpart.model, test[1:107],method="anova")
# Predicts values of three response variable for test dataset using the fitted rpart model.

rpart.prediction
# Contains the values of the predicted response variables.



##############################      CREATING DEFECT TABLE from PREDECTED TEST DATA    ###########################

start=0
end=0
for ( i in 2:length(testdata$SlabID)){
  if (testdata$CrackYN[i]>0.75 && testdata$CrackYN[i-1]<0.750 && testdata$SlabID[i]==testdata$SlabID[i-1]){
    start=c(start,i)
  }
  if (testdata$CrackYN[i]<0.75 && testdata$CrackYN[i-1]>0.75 && testdata$SlabID[i]==testdata$SlabID[i-1]){
    end=c(end,i)
  }
}

testDEFECT = matrix(nrow=length(start)-1,ncol=10)
colnames(testDEFECT)=colnames(DEFECT)

start=start[2:length(start)]
end=end[2:length(end)]

for (i in 1:length(start)){
  testDEFECT[i,1]=i
  testDEFECT[i,2]=testdata$SlabID[start[i]]
  testDEFECT[i,3]=testdata$HeatID[start[i]]
  testDEFECT[i,4]="LFC"
  testDEFECT[i,6]="FALSE"
  if (testdata$CrackSide[start[i]]>1.5){
    testDEFECT[i,5]="LS"
  }
  else {testDEFECT[i,5]="FS"}
  testDEFECT[i,7]=testdata$CrackXloc[start[i]]
  testDEFECT[i,8]=testdata$ActCastLength[start[i]]+0.72
  testDEFECT[i,9]=testdata$CrackXloc[start[i]]
  testDEFECT[i,10]=testdata$ActCastLength[end[i]]+0.72
}

write.csv(testDEFECT,"test_DEFECT.csv")

####################################     END OF PROGRAM     #############################################

