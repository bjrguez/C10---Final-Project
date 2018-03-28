library(NLP)
library(tm)

create.corpus <- function(textDocs, 
                          rm_profanityWords=TRUE, 
                          rm_punctuation=FALSE, 
                          rm_stopwords=FALSE) {
  # # Combining all lines of text to make a single text document
  # doc <- paste(textDocs, collapse=" ")
  # Make a text source from the text document
  vecSource <- VectorSource(textDocs)
  # Make a corpus from the text source
  corpus <- VCorpus(vecSource)
  
  # 
  # Cleaning the corpus
  # 
  ## transform all text into lower-case text
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  ## remove bad words?
  if (rm_profanityWords) {
    corpus <- tm_map(corpus, removeWords, profanityWords)
  }
  
  corpus <- tm_map(corpus, to.space, '\\\"')
  corpus <- tm_map(corpus, to.space, "\\.\\.|\\.\\.\\.+")
  corpus <- tm_map(corpus, to.space, "[0-9]+.[0-9]+")
  ## remove tweets hashtags
  corpus <- tm_map(corpus, to.space, "\\#(.*?) ")
  corpus <- tm_map(corpus, to.space, "\\#(.*)")
  corpus <- tm_map(corpus, to.space, "\\#(.*)\\.")
  ## remove URLs
  corpus <- tm_map(corpus, to.space, "http(.*?) ")
  corpus <- tm_map(corpus, to.space, "http(.*)")
  corpus <- tm_map(corpus, to.space, "http(.*)\\.")
  corpus <- tm_map(corpus, to.space, "www\\.(.*?) ")
  corpus <- tm_map(corpus, to.space, "www\\.(.*)")
  corpus <- tm_map(corpus, to.space, "www\\.(.*)\\.")
  # 
  corpus <- tm_map(corpus, to.space, "\\&\\&+")
  corpus <- tm_map(corpus, to.space, "[0-9]+[a-z]+ ")
  #
  corpus <- tm_map(corpus, to.and, " \\& ")
  corpus <- tm_map(corpus, to.and, "\\&")
  ## remove punctuations
  if (rm_punctuation) {corpus <- tm_map(corpus, removePunctuation)}
  ## remove some symbols
  corpus <- tm_map(corpus, to.space, "\\!|\\£|\\$|\\%|\\^|\\'")
  corpus <- tm_map(corpus, to.space, "\\*|\\(|\\)|\\-|\\_")
  corpus <- tm_map(corpus, to.space, "\\{|\\}|\\[|\\]|\\+|\\=")
  corpus <- tm_map(corpus, to.space, "\\:|\\;|\\@|\\~|<|>|\\.|,")
  corpus <- tm_map(corpus, to.space, "\\||\\¬|\\`|\\?|\\/|\\\\")
  #
  corpus <- tm_map(corpus, to.I, " i |^i | i$|^i$")
  # 
  corpus <- tm_map(corpus, to.Iam, " im |^im | I m |^I m |I m$")
  #
  corpus <- tm_map(corpus, to.Iwould, " I d |^I d | I d$|I d$")
  # 
  corpus <- tm_map(corpus, to.will, " ll ")
  # 
  corpus <- tm_map(corpus, to.have, " ve ")
  # 
  corpus <- tm_map(corpus, to.are, " re ")
  #
  corpus <- tm_map(corpus, to.youare, " you re |^you re | youre |^youre ")
  #
  corpus <- tm_map(corpus, to.theyare, 
                   " they re |^they re | theyre |^theyre ")
  #
  corpus <- tm_map(corpus, to.weare, " we re |^we re ")
  # 
  corpus <- tm_map(corpus, to.itis, " it s |^it s ")
  # 
  corpus <- tm_map(corpus, to.sheis, " she s |^she s | shes |^shes ")
  # 
  corpus <- tm_map(corpus, to.heis, " he s |^he s | hes |^hes ")
  # 
  corpus <- tm_map(corpus, to.donot, " don t | dont |^don t |^dont ")
  # 
  corpus <- tm_map(corpus, to.doesnot, 
                   " doesn t | doesnt |^doesn t |^doesnt ")
  # 
  corpus <- tm_map(corpus, to.didnot, " didn t | didnt |^didn t |^didnt ")
  #
  corpus <- tm_map(corpus, to.isnot, " isnt |^isnt | isn t |^isn t ")
  # 
  corpus <- tm_map(corpus, to.wasnot, " wasnt |^wasnt | wasn t |^wasn t ")
  # 
  corpus <- tm_map(corpus, to.letus, " let s |^let s | lets |^lets ")
  # 
  corpus <- tm_map(corpus, to.cannot, " can t | ^can t | cant |^cant ")
  # 
  corpus <- tm_map(corpus, to.couldnot, 
                   " couldn t |^couldn t | couldnt |^couldnt ")
  # 
  corpus <- tm_map(corpus, to.wouldnot, 
                   " wouldn t |^wouldn t | wouldnt |^wouldnt ")
  # 
  corpus <- tm_map(corpus, to.shouldnot, 
                   " shouldn t |^shouldn t | shouldnt |^shouldnt ")
  # 
  corpus <- tm_map(corpus, to.thatis, " thats |^thats |^that s | that s ")
  # 
  corpus <- tm_map(corpus, to.whatis, " whats |^whats |^what s | what s ")
  # 
  corpus <- tm_map(corpus, to.whois, " whos |^whos |^who s | who s ")
  # 
  corpus <- tm_map(corpus, to.howis, " hows |^hows |^how s | how s ")
  # 
  corpus <- tm_map(corpus, to.whereis, 
                   " wheres |^wheres |^where s | where s ")
  # 
  corpus <- tm_map(corpus, to.whenis, " whens |^whens |^when s | when s ")
  # 
  corpus <- tm_map(corpus, to.whyis, " whys |^whys |^why s | why s ")
  ## remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, to.space, "[a-z]+([a-z])\\1{2,}")
  ## remove all single letters, except "i" and "a"
  corpus <- tm_map(corpus, to.space, " [b-z] |^[b-z] | [b-z]$")
  ## remove 'stopwords'?
  if (rm_stopwords) {corpus <- tm_map(corpus, removeWords, stopwords("en"))}
  ## remove whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}


strip <- function (str) gsub("^\\s+|\\s+$", "", str)


to.space <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " ", x))})

to.and <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " and ", x))})

to.I <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " I ", x))})

to.Iam  <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " I am ", x))})

to.Iwould  <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " I would ", x))})

to.will  <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " will ", x))})

to.have  <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " have ", x))})

to.are  <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " are ", x))})

to.youare <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " you are ", x))})

to.theyare <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " they are ", x))})

to.weare <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " we are ", x))})

to.itis <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " it is ", x))})

to.sheis <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " she is ", x))})

to.heis <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " he is ", x))})

to.donot  <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " do not ", x))})

to.doesnot  <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " does not ", x))})

to.didnot  <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " did not ", x))})

to.isnot <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " is not ", x))})

to.wasnot <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " was not ", x))})

to.letus <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " let us ", x))})

to.cannot <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " cannot ", x))})

to.couldnot <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " could not ", x))})

to.wouldnot <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " would not ", x))})

to.shouldnot <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " should not ", x))})

to.thatis <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " that is ", x))})

to.whatis <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " what is ", x))})

to.whois <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " who is ", x))})

to.howis <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " how is ", x))})

to.whereis <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " where is ", x))})

to.whenis <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " when is ", x))})

to.whyis <- content_transformer(
  function(x, pattern) {return(gsub(pattern, " why is ", x))})


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(stringr)

predictNextWord <- function(inputText) {
  # Clean input text
  input <- create.corpus(inputText)[[1]]$content
  input <- strip(input)
  
  inputTemp <- unlist(str_split(input," "))
  lenInput <- length(inputTemp)
  
  nGrams <- list(fiveGramDF, fourGramDF, triGramDF, biGramDF)
  
  predictions <- c()
  
  thr <- 6
  if (lenInput >= 4) {
    # first trail
    gram <- paste(inputTemp[(lenInput - 3):lenInput], collapse=" ")
    hits <- fiveGramDF[fiveGramDF$asInput == gram, ]
    i <- 2
    while (nrow(hits) < thr & i <= 4) {
      gram <- str_split_fixed(gram, " ", 2)[2]
      hits <- nGrams[[i]][nGrams[[i]]$asInput == gram, ]
      i <- i + 1
    }
  } else {
    gram <- paste(inputTemp, collapse=" ")
    if (lenInput == 3) {
      hits <- fourGramDF[fourGramDF$asInput == gram, ]
      i <- 3
      while (nrow(hits) < thr & i <= 4) {
        gram <- str_split_fixed(gram, " ", 2)[2]
        hits <- nGrams[[i]][nGrams[[i]]$asInput == gram, ]
        i <- i + 1
      }
    } else if (lenInput == 2) {
      hits <- triGramDF[triGramDF$asInput == gram, ]
      if (nrow(hits) < thr) {
        gram <- str_split_fixed(gram, " ", 2)[2]
        hits <- biGramDF[biGramDF$asInput == gram, ]
      }
    } else if (lenInput == 1) {
      hits <- biGramDF[biGramDF$asInput == gram, ]
    } else if (lenInput == 0) {
      hits <- data.frame()
    }
  }
  
  predictions <- as.vector(hits$nextWord)[1:6]
  
  if (nrow(hits) == 0) {
    predictions <- c("", "", "", "", "", "")
    moreHits <- "More options are not available!"
    if (inputText != "") {
      predictions[1] <- paste(
        '"Sorry, I do not understand your input!!!', 
        'Check with the free online dictionary via the', 
        'link provided below the input textbox.')
    }
  } else {
    predictions <- as.vector(hits$nextWord)[1:6]
    if (nrow(hits) <= 6) {
      moreHits <- "More options are not available!"
    } else if (nrow(hits) > 6 & nrow(hits) <= 30) {
      moreHits <- hits[, 2:3]
      # moreHits$nextWord <- factor(
      #     moreHits$nextWord, levels=moreHits$nextWord)
    } else {
      moreHits <- hits[1:30, 2:3]
      # moreHits$nextWord <- factor(
      #     moreHits$nextWord, levels=moreHits$nextWord)
    }
  }
  return(list(predictions, moreHits))
}


showUpdateHist <- function(moreHits) {
  if (is.data.frame(moreHits)) {
    if (nrow(moreHits) > 0) {
      moreHits <- moreHits[order(moreHits[,2]), ]
      moreHits$nextWord <- factor(
        moreHits$nextWord, levels=moreHits$nextWord)
      g <- ggplot(moreHits, aes(x=nextWord, y=Frequency))
      g <- g + geom_bar(stat="Identity", fill="#009E73", width=0.6)
      g <- g + theme(#axis.text.x=element_text(angle=45, hjust=1, size=17),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size=16, face="bold"),
        axis.title.y=element_text(size=16, face="bold"))
      g <- g + labs(x="Most likely next words", y="Frequency (for reference only)")
      g + coord_flip()
    } else {
      return(moreHits)
    }
  } else {
    return(moreHits)
  }
}
