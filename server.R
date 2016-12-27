# install.packages(c("tm", "SnowballC", "koRpus", "wordcloud", "shiny", "twitteR", "qdap", "XML", "qdapTools"))
library(shiny)
library(twitteR)
library(tm)
library(SnowballC)
library(koRpus)
library(wordcloud)
library(qdap)
library(RColorBrewer)
setup_twitter_oauth("tYi7ebomD7SJ2OJ8AAknAGQ7D", "6M168GhPrBhtYiWgXLZZJCpadeIG6jUYcpYLEi4eA8538mqvsn")
# defines the logic required to calculate all the variables to be displayed on the web app

shinyServer(function(input, output){
    tab <- reactive({input$tabs})
    ## function to generate output
    output$test <- renderText(genOutput(tab()))
    
    genOutput <- function(x){
      if(x == "user"){
       
      # search user
       user <- reactive({
         input$buttonUser
         isolate(getUser(input$search))}) 
      output$basic_info <- renderTable({
        df <- as.data.frame(user())
        t(df[-c(17)])
      })
      # profile picture
      output$profile <- renderText({paste("<img src = '", paste("' width=100, height=100/> <p>","</p>", sep = user()$name), sep = user()$profileImageUrl)})
      
      # to be used again
      
      # names of friends, followers
      output$follw <- renderTable({
            a17 <- user()$getFollowers(n=20)
            a<-NULL
            if(length(a17) != 0){
              a <- twListToDF(a17)
              a$id <-NULL
              rowames(a) <- c(1:20)
          }
          a
      })
        
      output$friends <- renderTable({
            a18 <- user()$getFriends(n=20)
            b<-NULL
            if(length(a18) != 0){
              b <- twListToDF(a18)
              b$id <- NULL
              rownames(b) <- c(1:20)
            }
            b
      })
      
      # timeline tweets and retweets
      res <- reactive({input$buttonUser
        isolate(preprocess(c(userTimeline(input$search, includeRts = T), user()$getFavorites())))})
      
      
      ## preprocess
      output$wordcloud <- renderImage({
        out <- tempfile(fileext = ".png")
        png(out, width = 400, height = 400, bg = "black")
        wordcloud(names(res()[-length(res())]), res()[-length(res())], min.freq = 2, scale =  c(4, 1), random.color = T, colors = brewer.pal(8, "Dark2"), max.words = 60)
        dev.off()
        list(src=out, width = 400, height = 400)}, deleteFile = T)
      
      output$optimism <- renderText(paste(res()[length(res())]*100,"%", sep = ""))
      
      }
      else if(x == "reviews"){
        res1 <- reactive({input$buttonReviews
          isolate(preprocess(searchTwitter(paste(input$review, " :)"))))})
        res2 <- reactive({input$buttonReviews
          isolate(preprocess(searchTwitter(paste(input$review, " :("))))})
        
        output$cloud1 <- renderImage({
          out1 <- tempfile(fileext = ".png")
          png(out1, width = 400, height = 400, bg = "black")
          wordcloud(names(res1()), res1(), min.freq = 2, scale = c(4, 1), random.color = T, colors = brewer.pal(8, "Dark2"), max.words = 60)
          dev.off()
          list(src=out1, width = 400, height = 400)}, deleteFile = T)
        
        output$cloud2 <- renderImage({
          out2 <- tempfile(fileext = ".png")
          png(out2, width = 400, height = 400, bg = "black")
          wordcloud(names(res2()), res2(), min.freq = 2, scale = c(4, 1), random.color = T, colors = brewer.pal(8, "Dark2"), max.words = 60)
          dev.off()
          list(src=out2, width = 400, height = 400)}, deleteFile = T)
        
      }
      
    else if(x == "hashtags"){
      cat(file = stderr(), "reached here")
      res <- reactive({input$buttonHash
        isolate(preprocess(c(searchTwitter(input$hashtags, lang = "en"))))})
      output$cloud3 <- renderImage({
        out <- tempfile(fileext = ".png")
        png(out, width = 400, height = 400, bg = "black")
        wordcloud(names(res()), res(), min.freq = 2, scale = c(4, 1), random.color = T, colors = brewer.pal(8, "Dark2"), max.words = 60)
        dev.off()
        list(src=out, width = 400, height = 400)}, deleteFile = T)
      
    }
    else if(x == "place"){
     res <- reactive({input$buttonPlace
       isolate(preprocess(searchTwitter(input$place, lang = "en")))})
      output$cloud4 <- renderImage({
        out <- tempfile(fileext = ".png")
        png(out, width = 400, height = 400, bg = "black")
        wordcloud(names(res()), res(), min.freq = 2, scale = c(4, 1), random.color = T, colors = brewer.pal(8, "Dark2"), max.words = 60)
        dev.off()
        list(src=out, width = 400, height = 400)}, deleteFile = T)
    }
    
      c("")
    }
preprocess <- function(t){
  library(tm)
  library(SnowballC)
  library(koRpus)
  library(wordcloud)
  library(qdap)
  df <- do.call(rbind, (lapply(t, as.data.frame)))
  rawData <- Corpus(VectorSource(df$text))
  
  f <- content_transformer(function(x) iconv(x, "ASCII", "ASCII", sub="")) ## needs to be defined only once
  rawData <- tm_map(rawData, f, lazy=T) ## change to lazy=F if possible
  
  
  rawData <- tm_map(rawData, removeNumbers)
  rawData <- tm_map(rawData, removePunctuation)  # rawData[[i]]$content to access the content
  
  polarity <- polarity(rawData$content)
  ratio <- sum(p$all$pos.words != "-")/(sum(p$all$neg.words != "-")+sum(p$all$pos.words != "-"))
  
  rawData <- tm_map(rawData, tolower)
  # & is being converted to amp here
  rawData <- tm_map(rawData, removeWords, c(stopwords(), "amp", "http")) ## access via rawData[[1]]
  
  ## tolower does not return a plain text document:: required for stemDocument
  rawData <- tm_map(rawData, PlainTextDocument)
  ## snowballC required
  rawData <- tm_map(rawData, stemDocument)
  rawData <- tm_map(rawData, stripWhitespace)
  
  dtm <- DocumentTermMatrix(rawData)
  tdm <- TermDocumentMatrix(rawData)
  
  ft <- findFreqTerms(dtm, lowfreq = 2)
  ## assoc <- findAssocs(dtm, inputString, corlimit=0.90) ## get the input string
  
  dtms <- removeSparseTerms(dtm, 0.95)
  freq <- colSums(as.matrix(dtms))
  ord <- order(freq) 
  c(freq, ratio)
}
})