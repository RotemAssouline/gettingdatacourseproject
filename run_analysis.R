library(dplyr)

## import data

        features <- read.table('data/features.txt')[[2]]
        features <- unlist(lapply(features,str_replace,'BodyBody','Body')) # correct mistake in feature names
        activities<- read.table('data/activity_labels.txt')
        Xtest <- read.table('data/test/X_test.txt',col.names = features,check.names = FALSE)
        ytest <- read.table('data/test/y_test.txt', col.names = 'activity label',check.names = FALSE)
        stest <- read.table('data/test/subject_test.txt', col.names = 'subject',check.names = FALSE)
        Xtrain <- read.table('data/train/X_train.txt', col.names = features,check.names = FALSE)
        ytrain <- read.table('data/train/y_train.txt', col.names = 'activity label',check.names = FALSE)
        strain <- read.table('data/train/subject_train.txt', col.names = 'subject',check.names = FALSE)
        
## construct main dataframe
        
        ## merge test and train data

        X <- rbind(Xtest,Xtrain)[,grep('mean\\(\\)|std\\(\\)',features)]
        y <- rbind(ytest,ytrain)
        subject <- as.factor(rbind(stest,strain)[,1])
        
        ## make variable names more explicit
        
        domain <- names(X) %>% lapply(str_sub,1,1) %>% lapply(str_replace_all,c('f' = 'freq','t' = 'time')) %>% unlist
        splitnames <- names(X) %>% lapply(str_sub,2) %>% lapply(str_split,'-')
        measurement <- splitnames %>% lapply(function(x) x[[1]][1]) %>% unlist
        measurement <- measurement %>% lapply(str_replace,'Mag','') %>% unlist
        measurement <- read.table('measurement_names.txt',row.names = 1)[measurement,]
        meanstd <- splitnames %>% lapply(function(x) str_sub(x[[1]][2],1,-3)) 
        coordinate <- splitnames %>% lapply(function(x) x[[1]][3]) %>% unlist
        coordinate[is.na(coordinate)] <- 'Mag'
        
        names(X) <- paste(domain,measurement,meanstd,coordinate,sep = '_')
        
        ## add subject and activity columns
        
        maindf <- cbind(subject,X) # merge measurment and subject tables
        maindf$activity <- factor(y[,1], labels = activities[[2]]) # add activity column
        
## calculate means for each activity and each subject
        means <- maindf %>% group_by(subject,activity) %>% summarise_all(mean) %>% ungroup()
        write.table(means,file = 'means.txt',row.names = FALSE)