# (1) 爬網擷取：提供三個看板供使用者選擇、可以設定資料擷取的時間範圍、選擇資料擷取範圍（是否包含標題）
# (2) 資料分析：使用者可選擇是否過濾中斷詞與多餘詞彙、是否將分析結果輸出、選擇輸出資料數
# (3) 圖表展示：提供四種圖表供使用者選擇、並選擇是否將圖片儲存
# 介面可以重複執行、直到使用者選擇結束

rm( list = ls() )

# 安裝需要的套件
# rvest：網路解析處理套件
# tidyverse：內含資料處理套件(dplyr)和繪圖套件(ggplot2)
# stringr：字串處理
# jiebaR：文字斷詞
# tmcn：文字字庫
# lubridate：時間計算
# wordcloud：繪製文字雲
# RColorBrewer：色彩選擇工具
# xlsx：將資料轉存為xlsx檔案
if ( !require( "rvest" ) ) install.packages( "rvest" )
library( rvest )
if ( !require( "tidyverse" ) ) install.packages( "tidyverse" )
library( tidyverse )
if ( !require( "stringr" ) ) install.packages( "stringr" )
library( stringr )
if ( !require( "jiebaR" ) ) install.packages( "jiebaR" )
library( jiebaR )
if ( !require( "tmcn" ) ) install.packages( "tmcn" )
library( tmcn )
if ( !require( "lubridate" ) ) install.packages( "lubridate" )
library( lubridate )
if ( !require( "lattice" ) ) install.packages( "lattice" )
library( lattice )
if ( !require( "wordcloud" ) ) install.packages( "wordcloud" )
library( wordcloud )
if ( !require( "RColorBrewer" ) ) install.packages( "RColorBrewer" )
library( RColorBrewer )
if ( !require( "xlsx" ) ) install.packages( "xlsx" )
library( xlsx )

timeFilter <- function( temp.links.article, overTimeRange ) {
  tempLink <- strsplit( temp.links.article, split = ".", fixed = T ) 
  
  # 刪除超過時間範圍的連結
  for ( i in length( temp.links.article ) : 1 ) {
    if ( as.numeric( tempLink[[i]][2] ) < timeRange ) {
      temp.links.article <- temp.links.article[-i]
      overTimeRange <<- T
    }
  }
  return( temp.links.article )
}

# 除了內建的中斷詞，有一些詞彙也是不需要算入統計的
unnecessaryWord <- c( "com", "imgur", "com", "https", "沒有", "jpg", "真的", "知道", "現在", 
                      "覺得", "目前", "大家", "可能", "不是", "已經", "不會", "一下", "請問", 
                      "www", "問題", "之前", "不要", "有人", "發現", "tw", "from", "my", "只是", "應該",
                      "http", "比較", "使用", "the", "that", "should", "and", "a", "of", "for", "in",
                      "flic", "kr", "mm", "大概", "感謝", "Re", "XD", "謝謝", "其實", "有點", 
                      "前輩", "jpghttps", "置底", "內容", "以板規", "Line", "line", "隨意", "閱讀", 
                      "連結", "一張" )

setTimeRange <- function( timeRange ) {
  # 先設定資料搜尋的範圍(使用timestamp)
  cat( "請輸入分析時間範圍（建議搜尋大於3日的範圍，圖表較有分析意義）：\n" )
  cat( "請先選擇單位（輸入數字）：\n1.日\n2.週\n3.年" )
  input <- scan()
  switch( input,
          cat( "請輸入--您想分析最近___日的文章（輸入數字）：" ),
          cat( "請輸入--您想分析最近___週的文章（輸入數字）：" ),
          cat( "請輸入--您想分析最近___年的文章（輸入數字）：" ) )
  num <- scan()
  switch ( input,
           timeRange <- Sys.Date() - ddays( num - 1 ),
           timeRange <- Sys.Date() - dweeks( num ) + ddays( 1 ),
           timeRange <- Sys.Date() - dyears( num ) + ddays( 1 ) )
  timeRange <- as.numeric( as.POSIXct( timeRange ) ) - 60 * 60 * 8  # 時區問題

    switch( input,
          timeRangeStr <<- paste( num, "日", sep = "" ),
          timeRangeStr <<- paste( num, "週", sep = "" ),
          timeRangeStr <<- paste( num, "年", sep = "" ) )
  
  cat( "您選擇分析", timeRangeStr, "內的文章\n" )
  return( timeRange )
}

pttTextMining <- function( timeRange ) {
  timeRange <<- setTimeRange( timeRange )

  # 開始爬蟲
  ptt.url <- "https://www.ptt.cc"
  board <- html_session( url = board.url )
  
  # 找最新的頁數
  page.latest <- read_html( board ) %>%
    xml_find_all( "//div[@class='btn-group btn-group-paging']/a[2]" ) %>%
    str_subset( "index[0-9]{2,}\\.html" ) %>%
    str_extract( "[0-9]+" ) %>%
    as.numeric()
  
  links.article <- NULL  # 存放每則文章的網址
  overTimeRange <<- F
  page.index <- page.latest
  
  # 開始利用規則處理，取得每頁的文章網址，直到超過指定的時間範圍
  while ( overTimeRange == F ) {
    links.temp <- NULL
    
    # 取得「上一頁」的頁數
    link <- str_c( board.url, "/index", page.index, ".html" )
    print( link )
    links.temp <- c(
      board %>%
        jump_to( link ) %>%
        html_nodes( "a" ) %>%
        html_attr( "href" ) %>%
        str_subset( "[A-z]\\.[0-9]+\\.[A-z]\\.[A-z0-9]+\\.html" )
    )
    
    links.temp <- timeFilter( links.temp, overTimeRange )
    if ( overTimeRange == T ) {
      links.article <- c( links.article, links.temp )
      break
    } else {
      links.article <- c( links.article, links.temp )
    }
    
    page.index <- page.index - 1
  }
  
  # 存放文章的table
  article.table <- tibble()
  
  # 開始訪問每篇文章的網址，並將訪問的資訊存下來
  cat( "\n*正在抓取資料，請稍後*\n" )
  for ( temp.link in links.article ) {
    cat( "=" )
    
    # 首先取得每篇文章網址，並訪問該頁面
    article.url <- str_c( ptt.url, temp.link )
    temp.html <- board %>% 
      jump_to( article.url )
    
    # 擷取文章開頭作者、標題、發文時間的資訊
    article.header <- temp.html %>%
      html_nodes( "span.article-meta-value" ) %>% # 開頭部分元素
      html_text()
    article.title <- article.header[3]
    article.datetime <- article.header[4]
    
    # 取得文章內容
    article.content <- temp.html %>%
      html_nodes( 
        xpath = '//div[@id="main-content"]/node()[not(self::div|self::span[@class="f2"])]'
      ) %>%
      html_text( trim = TRUE ) %>%
      str_c( collapse = "" )
    
    # 合併文章資料
    article.table <- article.table %>%
      bind_rows(
        tibble(
          datetime = article.datetime,
          title = article.title,
          content = article.content
        )
      )
  }
  cat( "\n處理完畢\n" ) 
  
  cat( "\n是否要將文章標題合併分析？ Y/N" )
  input <- scan( what = "character" )
  if( input == "Y" )
    # 標題也是要分析的文字，因此將標題和內文合併
    article.table$content <- paste( article.table$title, article.table$content, sep = "" )
  
  # 整理格式：將有NA(不完整)的資訊清除、新增日期欄位(年-月-日)
  article.table <- article.table %>%
    mutate( 
      datetime = str_sub( datetime, 5 ) %>% parse_datetime( "%b %d %H:%M:%S %Y" ),
      month = format(datetime, "%m"),
      day = format(datetime, "%d"),
      date = format(datetime, "%Y-%m-%d") %>% parse_datetime("%Y-%m-%d")
    ) %>%
    filter_all(
      all_vars(!is.na(.))
    )

  jieba.worker <- worker()
  
  cat( "是否過濾中斷詞與無助於資料分析的詞彙（例如：www、你、沒有、http...等等）？ Y/N\n" )
  command <- scan( what = "character" )
  if ( command == "Y" ) {
    # 開始進行文章斷詞
    # 並過濾中斷詞、非必要詞彙、單個字的助詞
    # 斷詞的同時，計算該詞彙出現的頻率
    words.timeRange <<- article.table %>%
      do( ( function( input ) {
        freq( segment( input$content, jieba.worker ) ) %>% 
          filter(
            !( char %in% toTrad( stopwordsCN() ) ),
            !( char %in% unnecessaryWord ),
            !str_detect( char, "[0-9]" ),
            nchar( char ) > 1
          ) %>%
          arrange( desc( freq ) )
      } )( . ) ) 
    
    # 將斷出來的詞依照日期分組
    words.date <<- article.table %>%
      group_by(date) %>%
      do( ( function( input ) {
        freq( segment( input$content, jieba.worker ) ) %>% 
          filter(
            !( char %in% toTrad( stopwordsCN() ) ),
            !( char %in% unnecessaryWord ),
            !str_detect( char, "[0-9]" ),
            nchar( char ) > 1
          ) %>%
          arrange( desc( freq ) ) %>%
          return
      } )( . ) )
  } else {
    # 開始進行文章斷詞
    # 不過濾多餘詞彙
    # 斷詞的同時，計算該詞彙出現的頻率
    words.timeRange <<- article.table %>%
      do( ( function( input ) {
        freq( segment( input$content, jieba.worker ) ) %>% 
          filter(
            !str_detect( char, "[0-9]" ),
            nchar( char ) > 1
          ) %>%
          arrange( desc( freq ) )
      } )( . ) ) 
    
    # 將斷出來的詞依照日期分組
    words.date <<- article.table %>%
      group_by(date) %>%
      do( ( function( input ) {
        freq( segment( input$content, jieba.worker ) ) %>% 
          filter(
            !str_detect( char, "[0-9]" ),
            nchar( char ) > 1
          ) %>%
          arrange( desc( freq ) ) %>%
          return
      } )( . ) )    
  }
  
  chartInterface()
}

generateTotalBarChart <- function() {
  # 前十名詞彙長條圖
  temp <- data.frame( words.timeRange )
  temp <- temp[c( 1 : 10 ),]
  temp$char <- factor( temp$char, levels = temp$char[order( temp$freq )] )
  totalBarChart <<- ggplot( data = temp, aes( x = char, y = freq ) ) + 
    geom_col( aes( fill = freq ) ) +
    labs( fill = "", x = "詞彙", y = "出現次數", title = paste(  timeRangeStr, "內出現次數前十名的詞彙", sep = "" ) ) + 
    theme( axis.text.x = element_text( angle = 90, hjust = 1 ),  plot.title = element_text( hjust = 0.5 ) )
  plot( totalBarChart )
  
  cat( "是否儲存此張圖表? Y/N")
  input <- scan( what = "character" )
  if ( input == "Y" ) {
    # 避免覆蓋掉同檔名的舊檔案
    dir <- getwd()
    files <- list.files()
    i <- 1
    while ( T ) {
      if ( !( paste( "totalBarChart_", i, ".png", sep = "" ) %in% files ) ) {
        fileName <- paste( dir, "/totalBarChart_", i, ".png", sep = "" )
        break
      }
      i <- i + 1
    }
    png( fileName )
    plot( totalBarChart )
    dev.off()
    cat( "已儲存圖表，路徑為：", fileName, "\n" )
  }
}

generateSingleLineChart <- function() {
  # 單一詞彙折線圖
  cat( timeRangeStr, "內出現次數前十名的詞彙有：\n")
  for ( i in 1 : 10 ) {
    cat( i, ":", words.timeRange[i, ]$char, "\n" )
  }
  cat( "\n請選擇想分析的詞彙（輸入數字）：" )
  i <- scan()
  
  word.filtered <- data.frame( filter( words.date, char %in% words.timeRange[i, 1] ) )
  singleLineChart <<- ggplot( data = word.filtered, aes( x = date, y = freq, group = 1 ) ) +
    geom_line( color = "#9fbfdf", size = 1 ) +
    geom_point( size = 3, aes( colour = freq ) ) +
    scale_colour_gradient( low = "#006699" ) +
    labs( colour = "", x = "時間", y = "出現次數", title = paste(  "「",words.timeRange[i, 1], "」在", timeRangeStr, "內出現次數", sep = "" ) ) +
    theme( plot.title = element_text( hjust = 0.5 ) )
  plot( singleLineChart )

  cat( "是否儲存此張圖表? Y/N")
  input <- scan( what = "character" )
  if ( input == "Y" ) {
    # 避免覆蓋掉同檔名的舊檔案
    dir <- getwd()
    files <- list.files()
    i <- 1
    while ( T ) {
      if ( !( paste( "singleLineChart_", i, ".png", sep = "" ) %in% files ) ) {
        fileName <- paste( dir, "/singleLineChart_", i, ".png", sep = "" )
        break
      }
      i <- i + 1
    }
    png( fileName )
    plot( singleLineChart )
    dev.off()
    cat( "已儲存圖表，路徑為：", fileName, "\n" )
  }
}

generateTotalLineChart <- function() {
  # 前十名詞彙折線圖
  words.date.dataframe <- NULL
  for ( i in 1 : 10 ) {
    temp <- filter( words.date, char %in% words.timeRange[i,1] )
    words.date.dataframe <- rbind( words.date.dataframe, temp )
  }
  dateRange <- as.character( ceiling( as.numeric( ( unique( words.date$date )[length( unique( words.date$date ) )] - unique( words.date$date )[1] ) / 10 ) ) )
  if ( dateRange %in% "0" ) dateRange <- "1"
  totalLineChart <<- ggplot( data = words.date.dataframe, aes( x = as.Date( date ), y = freq ) ) + 
    geom_line( aes( colour = char ) ) + 
    geom_point( size = 1, aes( colour = char ) ) +
    scale_x_date( date_minor_breaks = "1 day", date_labels = "%Y-%m-%d", date_breaks = paste( dateRange, " days", sep = "" ) ) + 
    theme( axis.text.x = element_text( angle = 90, hjust = 1 ), plot.title = element_text( hjust = 0.5 ) ) +
    labs( colour = "詞彙", x = "時間", y = "出現次數", title = paste(  "在", timeRangeStr, "內出現次數前十名的詞彙", sep = "" ) )  
  plot( totalLineChart )
  
  cat( "是否儲存此張圖表? Y/N")
  input <- scan( what = "character" )
  if ( input == "Y" ) {
    # 避免覆蓋掉同檔名的舊檔案
    dir <- getwd()
    files <- list.files()
    i <- 1
    while ( T ) {
      if ( !( paste( "totalLineChart_", i, ".png", sep = "" ) %in% files ) ) {
        fileName <- paste( dir, "/totalLineChart_", i, ".png", sep = "" )
        break
      }
      i <- i + 1
    }
    png( fileName )
    plot( totalLineChart )
    dev.off()
    cat( "已儲存圖表，路徑為：", fileName, "\n" )
  }
}

generateWordCloud<- function() {
  pal <- brewer.pal( 9, "RdBu" )
  wordcloud( words = words.timeRange[c( 1 : 100 ),]$char, freq = words.timeRange[c( 1 : 100 ),]$freq, colors = pal  )
  
  cat( "是否儲存此張圖表? Y/N\n")
  cat( "*注意：請將RStudio的Plot Windows（右下角視窗）拉大，以利文字雲產生*" )
  input <- scan( what = "character" )
  if ( input == "Y" ) {
    # 避免覆蓋掉同檔名的舊檔案
    dir <- getwd()
    files <- list.files()
    i <- 1
    while ( T ) {
      if ( !( paste( "wordCloud_", i, ".png", sep = "" ) %in% files ) ) {
        fileName <- paste( dir, "/wordCloud_", i, ".png", sep = "" )
        break
      }
      i <- i + 1
    }
    png( fileName )
    wordcloud( words = words.timeRange[c( 1 : 100 ),]$char, freq = words.timeRange[c( 1 : 100 ),]$freq, colors = pal )
    
    dev.off()
    cat( "已儲存圖表，路徑為：", fileName, "\n" )
  }
}

outputFile <- function() {
  cat( "請輸入希望儲存幾筆資料（輸入數字，資料依照出現次數遞減、最大值為", nrow( words.date ), "）：" )
  i <- scan()
  if ( i > nrow( words.date ) ) 
    i <- words.date
  outputData <- data.frame( 日期 = as.character( words.date$date ), 詞彙 = words.date$char, 出現次數 = words.date$freq )
  outputData <- arrange( outputData, desc( 出現次數 ) )[c( 1 : i ), ]
  
  # 避免覆蓋掉同檔名的舊檔案
  dir <- getwd()
  files <- list.files()
  i <- 1
  while ( T ) {
    if ( !( paste( "pttTextMining_", i, ".xlsx", sep = "" ) %in% files ) ) {
      fileName <- paste( dir, "/pttTextMining_", i, ".xlsx", sep = "" )
      break
    }
    i <- i + 1
  }
  
  write.xlsx( outputData, fileName, row.names = F )
  cat( "已儲存數據，路徑為：", fileName, "\n" )
}

chartInterface <- function() {
  cat( "\n請選擇想觀看的分析圖表圖表（輸入數字）：\n" )
  cat( "1. 長條圖--", timeRangeStr, "內出現次數最高的前10名詞彙比較\n" )
  cat( "2. 折線圖--", timeRangeStr, "內單一詞彙出現次數與時間關係\n" )
  cat( "3. 折線圖--", timeRangeStr, "內出現次數最高的前10名詞彙與時間關係\n" )
  cat( "4. 文字雲--", timeRangeStr, "內出現次數最高的前100名詞彙\n\n" )
  input <- scan()
  
  switch( input,
          generateTotalBarChart(),
          generateSingleLineChart(),
          generateTotalLineChart(),
          generateWordCloud() )     
}

chooseBoard <- function() {
  # 開始進行爬蟲
  # 首先設定url
  cat( "請選擇想分析的看板（輸入數字）：\n" )
  cat( "1. DC_SALE：單眼相機板\n" )
  cat( "2. DSLR：數位相機交易板\n" )
  cat( "3. MobileComm：手機板\n" )
  input <- scan()
  switch( input,
          board.url <<- "https://www.ptt.cc/bbs/DC_SALE", 
          board.url <<- "https://www.ptt.cc/bbs/DSLR",
          board.url <<- "https://www.ptt.cc/bbs/MobileComm" )
}

showMenu <- function() {
  cat( "\n請選擇操作（輸入數字）：\n" )
  cat( "1. 再次產生圖表（相同數據）\n" )
  cat( "2. 儲存此次搜尋數據（日期、詞彙、出現次數）\n")
  cat( "3. 重新分析數據\n")
  cat( "4. 結束\n" )
  input <- scan()
  return( input )
}

userInterface <- function() {
  chooseBoard()
  pttTextMining()
  
  input <- showMenu()
  while( input != 4 ) {
    switch( input,
            chartInterface(),
            outputFile(),
            { chooseBoard()
              pttTextMining() }
            )
    
    input <- showMenu()
  }
}

##########################################################################

# 選擇看板時請注意，MobileComm是最熱門的看板、每日文章的量非常大
# 因此若想分析長一點的時間範圍，建議選擇DSLR、DC_SALE板，避免搜尋時間很久
userInterface()

