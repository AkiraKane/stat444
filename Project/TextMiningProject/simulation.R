# dictionary <- read.csv("~/Dropbox/Academic notes/5 A/STAT 444/Project/dict.csv",sep=",")
# dictionary <- dictionary[,1:2]
# n <- nrow(dictionary)
# topic <- rep(1:10,n/10 +1)
# topic <- topic[1:n]
# dictionary <- data.frame(dictionary,topic=topic)
# W
words <-scan("~/Dropbox/Academic notes/5 A/STAT 444/Project/listingblob.txt",
             what="char", sep="\n")
blobi <-tolower(words)
blob.list<-strsplit(blobi, "\\W+", perl=TRUE)
blob.words.vector<-unlist(blob.list)
allwords <- unique(blob.words.vector)
W <- matrix(0,ncol=length(allwords),nrow=10)
#
for(i in 1:10){
        for(j in 1:length(names(blobs[[i]]))){
                W[i,which((names(blobs[[i]])[j] == allwords) ==1)] <- blobs[[i]][[j]]
                }
        }
}
#names(W) <- allwords
#
blob <-scan("~/Dropbox/Academic notes/5 A/STAT 444/Project/sample-listings.txt",
                          what="char", sep="\n")
blobs <- list(1,2,3,4,5,6,7,8,9,10)
for(i in 1:10){
blobi <-tolower(blob[i])
blob.list<-strsplit(blobi, "\\W+", perl=TRUE)
blob.words.vector<-unlist(blob.list)
blob.freq.list<-table(blob.words.vector)
print(length(blob.freq.list))
blobs[[i]] <- blob.freq.list
}

#
B <- list(1,2,3,4,5,6,7,8,9,10)
for(i in 1:10){
        blobi <-tolower(blob[i])
        blob.list<-strsplit(blobi, "\\W+", perl=TRUE)
        blob.words.vector <- unlist(blob.list)
        bigrams <- rep(0,length(blob.words.vector) -1)
        for(i in 1:length(blob.words.vector) -1){
                bigrams[i] <- paste(blob.words.vector[i],blob.words.vector[i+1] , sep = ",")
        }
        B[[i]] <- bigrams
}

for(i in 1:length(B)){
        print(table(B[i]))
}
B

B2 <- unlist(B[11:length(B)])
B3 <- unique(B2)
table(B2)[table(B2) > 1]

#View(sort(table(B2),decreasing =TRUE))
# Creating a dataframe to hold the bigrams that appeared and their respective counts - initializing all to 1 occurence
B.final <- data.frame(B3,count=rep(1,length(B3)))
table(B2)[table(B2) > 1]
# Adjusting the counts of bigrams that occurred more than once [hard-coded]
B.final[B3=="4,bedroom",2] <- 4
B.final[B3=="entrance,kitchen",2] <- 3
B.final[B3=="basement,detached",2] <- 2
B.final[B3=="hardwood,basement",2] <- 2
B.final[B3=="kitchen,basement",2] <- 2
B.final[B3=="maintained,detached",2] <- 2
B.final[B3=="sidewalk,interlock",2] <- 2
B.final[B3=="upgraded,ceramic",2] <- 2
# Data frame of all the bigrams and their rspective counts: data.frame(string int)
B.final <- B.final[with(B.final, order(B3)), ]

# Getting a full listing of all words in the corpus, disregarding different documents
words <-scan("~/Dropbox/Academic notes/5 A/STAT 444/Project/listingblob.txt",
            what="char", sep="\n")
# Changing the corpus to lower case
blobi <-tolower(words)
# Splitting the corpus into individual word tokens
blob.list<-strsplit(blobi, "\\W+", perl=TRUE)
# Changing the list of word tokens into a R vector data type
blob.words.vector<-unlist(blob.list)
# Extracting only the unique word tokens
blob.words.vector <- unique(blob.words.vector)
# Creating a data frame with all the possible bigrams for comparison with the list
# of bigrams that occurred in the text
for(i in 1:length(blob.words.vector)){
        for(j in 1:length(blob.words.vector)){
                if(j == 1){
                        column <- c()
                        column <- rbind(column,
                                        paste(blob.words.vector[i],
                                              blob.words.vector[j],
                                              sep = ","))
                } else {
                        column <- rbind(column,
                                        paste(blob.words.vector[i],
                                                     blob.words.vector[j],
                                                     sep = ","))
                }
        }
        if(i == 1){ 
                Bi <- data.frame(column)
        }
        else{ Bi <- data.frame(Bi,column)}
}
# Creating a matrix of same dimension as B to hold the counts of bigrams as the list calculated is
# compared against the data frame with all the possibilities
Bi2 <- matrix(0, ncol = 126, nrow = 126)
# Assigning each bigram count to its respective column in B
for(i in 1:nrow(Bi)){
        for(j in 1:ncol(Bi)){
                #B.final[,1] <- factor(B.final[,1], levels=levels(Bi[,j))
                if(is.na(match(Bi[i,j],B.final[,1]))){
                        Bi2[i,j] = 0
                } else{
                        Bi2[i,j] = B.final[match(Bi[i,j],B.final[,1]),2]
                }
        }
}
B <- Bi2
# Bi2 is the bigram matrix B
# 4,bedroom        4
# 2	entrance,kitchen	3
# 3	basement,detached	2
# 4	entrance,basement	2
# 5	hardwood,basement	2
# 6	kitchen,basement	2
# 7	maintained,detached	2
# 8	sidewalk,interlock	2
# 9	upgraded,ceramic	2
