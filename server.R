library(plyr)
library(dplyr)
library(ggplot2)
library(qcc)
library(reshape2)
library(shinydashboard)
library(FrF2)
library(ggrepel)
library(extrafont)
library(Rttf2pt1)
library(extrafontdb)
library(Cairo)



server <- function(input, output, session) {
  
  d2 <- c(1.414, 1.912, 2.239, 2.481, 2.673, 2.830, 2.963, 3.078, 3.179, 3.269, 3.350, 3.424, 3.491, 3.553,
          1.279, 1.805, 2.151, 2.405, 2.604,2.768,2.906,3.025,3.129,3.221,3.305,3.380,3.449,3.513,
          1.231, 1.769, 2.120, 2.379, 2.581,2.747,2.886,3.006,3.112,3.205,3.289,3.366,3.435,3.499,
          1.206, 1.750,	2.105,	2.366,	2.570,	2.736,	2.877,	2.997,	3.103,	3.197,	3.282,	3.358,	3.428,	3.492,
          1.191,	1.739,	2.096,	2.358,	2.563,	2.730,	2.871,	2.992,	3.098,	3.192,	3.277,	3.354,	3.424,	3.488,
          1.181,	1.731,	2.090,	2.353,	2.558,	2.726,	2.867,	2.988,	3.095,	3.189,	3.274,	3.351,	3.421,	3.486,
          1.173,	1.726,	2.085,	2.349,	2.555,	2.723,	2.864,	2.986,	3.092,	3.187,	3.272,	3.349,	3.419,	3.484,
          1.168,	1.721,	2.082,	2.346,	2.552,	2.720,	2.862,	2.984,	3.090,	3.185,	3.270,	3.347,	3.417,	3.482,
          1.164,	1.718,	2.080,	2.344,	2.550,	2.719,	2.860,	2.982,	3.089,	3.184,	3.269,	3.346,	3.416,	3.481,
          1.160,	1.716,	2.077,	2.342,	2.549,	2.717,	2.859,	2.981,	3.088,	3.183,	3.268,	3.345,	3.415,	3.480,
          1.157,	1.714,	2.076,	2.340,	2.547,	2.716,	2.858,	2.980,	3.087,	3.182,	3.267,	3.344,	3.415,	3.479,
          1.155,	1.712,	2.074,	2.3439,	2.546,	2.715,	2.857,	2.979,	3.086,	3.181,	3.266,	3.343,	3.414,	3.479,
          1.153,	1.710,	2.073,	2.338,	2.545,	2.714,	2.856,	2.978,	3.085,	3.180,	3.266,	3.343,	3.413,	3.478,
          1.151,	1.709,	2.072,	2.337,	2.545,	2.714,	2.856,	2.978,	3.085,	3.180,	3.265,	3.342,	3.413,	3.478,
          1.150,	1.708,	2.071,	2.337,	2.544,	2.713,	2.855,	2.977,	3.084,	3.179,	3.265,	3.342,	3.412,	3.477
  )
  
  d2.tabela<-matrix(d2,ncol=14,nrow=15, byrow=TRUE)
  rownames(d2.tabela)<-1:15
  colnames(d2.tabela)<-2:15
  
  A2 <-	c(1.88,1.23,0.729,0.577,0.483,0.419,0.373,0.337,0.308,0.285,0.266,0.249,0.235,0.223,0.212,0.203,0.194,0.187,0.180,
          0.173,0.167,0.162,0.157,0.153)
  D3 <- c(0,0,0,0,0,0.076,0.136,0.184,0.223,0.256,0.283,0.307,0.328,0.347,0.363,0.378,0.391,0.403,0.415,0.425,0.434,0.443,0.451,0.459)
  D4 <- c(3.267,2.574,2.282,2.114,2.004,1.924,1.864,1.816,1.777,1.744,1.717,1.693,1.672,1.653,1.637,1.622,1.608,1.597,1.585,1.575,1.566,1.557,1.548,1.541)
  
  
  #generuj plik do MSE
  
  generujPlik <- reactive ({
    if (is.null(input$pomiary) || is.null(input$czesci) || is.null(input$operatorzy))
      return(NULL) 
    pomiary <- matrix(rep(1:input$pomiary, times = input$operatorzy*input$czesci, each = 1), ncol=1)
    czesci <- matrix(rep(1:input$czesci, times = input$operatorzy, each = input$pomiary), ncol=1)
    operatorzy <- matrix(rep(1:input$operatorzy, times = 1, each = input$czesci*input$pomiary))
    tabela <- cbind(operatorzy, czesci, pomiary)
    row.names(tabela) <- NULL
    colnames(tabela) = c('Operator', 'Czesc', 'Pomiar')
    tabela <- as.data.frame(tabela)
    tabela[,"Wynik"] <- ""
    return(tabela)
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("MSE_",input$operatorzy, 'x', input$czesci, 'x', input$pomiary, '.csv', sep='') 
    },
    content = function(file) {
      write.csv2(generujPlik(), file, row.names = FALSE, na = "NA")
    }
  )
  
  
  
  #przygotowanie tabeli do analizy praktycznej
  praktyczna.tabela <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)  
    praktyczna.tabela <- read.csv2(inFile$datapath, header=TRUE, sep = ";", quote = "\"",dec=",")
    #praktyczna.tabela <-  melt(praktyczna.tabela,id.var = c('Operator','Czesc'), variable.name = 'Pomiar')
    praktyczna.tabela <- praktyczna.tabela %>%
      #  arrange(Operator,Czesc, Pomiar) %>%
      mutate(Numer_wiersza = row_number())
  })
  
  
  praktyczna.tabela.min.max <- reactive({
    if (is.null(praktyczna.tabela()))
      return(NULL)
    n <- length(unique(praktyczna.tabela()$Pomiar))
    #n <- length(unique(praktyczna.tabela()$Numer_wiersza)) / 
    #  (length(unique(praktyczna.tabela()$Czesc))*length(unique(praktyczna.tabela()$Operator)))
    #parts <- length(unique(praktyczna.tabela()$Czesc))
    dane <- praktyczna.tabela()
    praktyczna.tabela.min.max <- dane %>%
      select(-Pomiar, -Numer_wiersza) %>%
      group_by(Operator,Czesc) %>%
      summarize(min.measurement=min(Wynik), max.measurement=max(Wynik))
    praktyczna.tabela.min.max <- tbl_df(praktyczna.tabela.min.max)
    praktyczna.tabela.min.max <- praktyczna.tabela.min.max %>%
      arrange(Operator,Czesc,min.measurement,max.measurement) %>%
      mutate(Numer_wiersza = row_number() * n - n + 1)
    
  })
  
  tolerancje.max <- reactive({
    if (is.null(praktyczna.tabela.min.max()))
      return(NULL)
    tolerancje.max <- 1.1*max(praktyczna.tabela.min.max()$max.measurement)
  })
  tolerancje.min <- reactive({
    if (is.null(praktyczna.tabela.min.max()))
      return(NULL)
    tolerancje.min <- 0.9*min(praktyczna.tabela.min.max()$min.measurement) 
  })
  
  output$tolerancje.DGT<- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(tolerancje.min()))
      return()
    # Create the checkboxes and select them all by default
    numericInput("DGT", label = h5("Dolna granica tolerancji"), value = tolerancje.min(), step=0.1)
  })
  
  output$tolerancje.GGT<- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(tolerancje.max()))
      return()
    # Create the checkboxes and select them all by default
    numericInput("GGT", label = h5("GÄÅrna granica tolerancji"), value = tolerancje.max(), step=0.1)
  })
  
  
  praktyczna.tabela.average <- reactive({
    if (is.null(praktyczna.tabela()))
      return(NULL)
    dane <- praktyczna.tabela()
    praktyczna.tabela.average <- dane %>%
      select(-Pomiar, -Numer_wiersza) %>%
      group_by(Operator,Czesc) %>%
      summarize(Mean = mean(Wynik)) %>%
      ungroup() %>%
      mutate(Numer_wiersza = row_number(), mR = abs(Mean - lag(Mean)))
  })
  
  praktyczna.tabela.range <- reactive({
    if (is.null(praktyczna.tabela.min.max()))
      return(NULL)
    dane <- praktyczna.tabela.min.max()
    praktyczna.tabela.range <- dane %>%
      select(-Numer_wiersza) %>%
      group_by(Operator,Czesc) %>%
      summarize(Range = max.measurement-min.measurement) %>%
      ungroup() %>%
      mutate(Numer_wiersza = row_number())
  })
  
  average.operator <- reactive({
    if (is.null(praktyczna.tabela.average()))
      return(NULL)
    dane <- praktyczna.tabela.average()
    average.operator <- dane %>%
      select(-Czesc) %>%
      group_by(Operator) %>%
      summarize(Mean.operator = mean(Mean))
    average.operator <-max(average.operator$Mean.operator) - min(average.operator$Mean.operator)
  })
  
  average.part <- reactive({
    if (is.null(praktyczna.tabela.average()))
      return(NULL)
    dane <- praktyczna.tabela.average()
    average.part <- dane %>%
      select(-Operator) %>%
      group_by(Czesc) %>%
      summarize(Mean.part = mean(Mean))
    average.part <-max(average.part$Mean.part) - min(average.part$Mean.part)
  })
  
  
  average.range.part <- reactive ({
    if (is.null(praktyczna.tabela.min.max()))
      return(NULL)
    dane <- praktyczna.tabela.min.max()
    range.measurement <- dane %>%
      select(-Numer_wiersza) %>%
      group_by(Operator, Czesc) %>%
      summarize(range.part=max(max.measurement) - min(min.measurement))
    range <- mean(range.measurement$range.part)
  })
  
  average.range.operator <- reactive({
    if (is.null(praktyczna.tabela.average()))
      return(NULL)
    dane <- praktyczna.tabela.average()
    range.operator <- dane %>%
      select(-Czesc) %>%
      group_by(Operator) %>%
      summarize(max.Mean=max(Mean), min.Mean=min(Mean))
    range.operator <- range.operator %>%
      group_by(Operator) %>%
      summarize(range.part=max.Mean-min.Mean)
    
    range <- mean(range.operator$range.part)
  })
  
  ilosciowa.pomiary <- reactive({
    if (is.null(praktyczna.tabela()))
      return(NULL)
    d2 <- c(1, 1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.259, 3.336, 3.407)
    n <- length(unique(praktyczna.tabela()$Pomiar))
    k <- length(unique(praktyczna.tabela()$Czesc))
    o <- length(unique(praktyczna.tabela()$Operator))
    x <- (average.range.part()/d2[n]) * (average.range.part()/d2[n]) #wariancja pomiarÄÂÃâÄâÃâÄÂÃâÄâÃÂÄÂÃÂÄâÃâÄÂÃâ¦ÄâÃâw
    y <- (average.range.operator()/d2[k]) * (average.range.operator()/d2[k]) - x/n
    if(x<0){x=0}
    if(y<0){y=0}
    z <- x + y
    ilosciowa.pomiary <- round(x/z *100, digits=2)
    
    return(ilosciowa.pomiary)
  })
  
  ilosciowa.czesci<- reactive({
    if (is.null(praktyczna.tabela()))
      return(NULL)
    d2 <- c(1, 1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.259, 3.336, 3.407)
    n <- length(unique(praktyczna.tabela()$Pomiar))
    k <- length(unique(praktyczna.tabela()$Czesc))
    x <- (average.range.part()/d2[n]) * (average.range.part()/d2[n]) #wariancja pomiarÄÂÃâÄâÃâÄÂÃâÄâÃÂÄÂÃÂÄâÃâÄÂÃâ¦ÄâÃâw.
    y <- (average.range.operator()/d2[k]) * (average.range.operator()/d2[k]) - x/n
    if(x<0){x=0}
    if(y<0){y=0}
    z <- x + y
    ilosciowa.czesci <- round(y/z *100, digits=2)
    
    return(ilosciowa.czesci)
  })
  
  #okreÄÂÃÂÄâÃâÄÂÃâÄâÃâ¦ÄÂÃÂÄâÃËÄÂÃËÄâÃâÄâÃÂ¬ÄÂÃâ¦ÄâÃÅºlenie min i max pomiarÄÂÃÂÄâÃâÄÂÃËÄâÃâ¬ÄâÃÅ¡ÄÂÃâÄâÃÄÄÂÃËÄâÃâ¬ÄâÃÅ¡w dla kaÄÂÃÂÄâÃâÄÂÃâÄâÃâ¦ÄÂÃÂÄâÃâÄÂÃâ¹ÄâÃÅ¥dej czÄÂÃâÄâÃâÄÂÃËÄâÃâ¬ÄâÃÅ¾ÄÂÃÂÄâÃËÄÂÃËÄâÃâ¬ÄâÃÅ¾ÄÂÃâ¹ÄâÃÂÄÂÃÂÄâÃâÄÂÃâÄâÃâ¦ÄÂÃÂÄâÃËÄÂÃËÄâÃâÄâÃÂ¬ÄÂÃâ¦ÄâÃÅºci
  
  praktyczna.tabela.min.max.operator <- reactive({
    if (is.null(praktyczna.tabela()))
      return(NULL)
    n <- length(unique(praktyczna.tabela()$Pomiar))
    parts <- length(unique(praktyczna.tabela()$Czesc))
    dane <- praktyczna.tabela()
    praktyczna.tabela.min.max.operator <- dane %>%
      select(-Pomiar, -Czesc, -Numer_wiersza) %>%
      group_by(Operator) %>%
      summarize(min.measurement.operator=min(Wynik), max.measurement.operator=max(Wynik))
    praktyczna.tabela.min.max.operator <- tbl_df(praktyczna.tabela.min.max.operator)
    praktyczna.tabela.min.max.operator <- praktyczna.tabela.min.max.operator %>%
      arrange(Operator,min.measurement.operator,max.measurement.operator) %>%
      mutate(Numer_wiersza = row_number() * n*parts - n*parts + 1)
  })
  
  # individual value plot
  output$practical <- renderPlot({
    if (is.null(praktyczna.tabela()))
      return(NULL)
    if (is.null(praktyczna.tabela.min.max()))
      return(NULL)
    praktyczna <- praktyczna.tabela()
    praktyczna.min.max <- praktyczna.tabela.min.max()
    praktyczna.min.max.operator <- praktyczna.tabela.min.max.operator()
    intercept <- praktyczna.tabela.min.max()$max.measurement
    n <- length(unique(praktyczna.tabela()$Pomiar))
    parts <- length(unique(praktyczna.tabela()$Czesc))
    
    if (0 %in% input$rysuj.praktyczna) {
      
      i <- ggplot()
      i <- i + geom_point(data=praktyczna, aes(x=Numer_wiersza, y=Wynik), size = 2.5, shape = 16)
      i <- i + geom_hline(aes(yintercept = input$DGT), size = 0.5, alpha = 1, linetype = "dashed")
      i <- i + geom_hline(aes(yintercept = input$GGT), size = 0.5, alpha = 1, linetype = "dashed")
      i <- i + geom_text(data=praktyczna, aes(x=max(Numer_wiersza), y=input$DGT, label=paste("",input$DGT)), family = "Open Sans",size=4,  vjust = -0.5)
      i <- i + geom_text(data=praktyczna, aes(x=max(Numer_wiersza), y=input$GGT, label=paste("",input$GGT)), family = "Open Sans",size=4,  vjust = -0.5)
      i <- i + scale_y_continuous( breaks = waiver(), minor_breaks = waiver(), expand = c(0.2, 0))
      i <- i + scale_x_continuous('Części', breaks = NULL, labels=NULL)
      #      i <- i + geom_text(data=praktyczna.min.max, aes(x=Numer_wiersza+((Numer_wiersza + n - 1)-Numer_wiersza)/2, y=max.measurement, label=Czesc), size=4, vjust= -0.5 )
      i <- i + theme_tufte(base_size = 13, base_family = "Open Sans")
    }
    
    else if(1 %in% input$rysuj.praktyczna){
      i <- ggplot()
      i <- i + geom_rect(data=praktyczna.min.max, mapping=aes(xmin=Numer_wiersza, xmax=Numer_wiersza + n - 1, ymin=min.measurement, ymax=max.measurement), size = 0.25, alpha = 1 , fill = "white", linetype = "solid", color = "black")
      i <- i + geom_point(data=praktyczna, aes(x=Numer_wiersza, y=Wynik), size = 2.5, shape = 16)
      i <- i + geom_hline(aes(yintercept = input$DGT), size = 0.5, alpha = 1, linetype = "dashed")
      i <- i + geom_hline(aes(yintercept = input$GGT), size = 0.5, alpha = 1, linetype = "dashed")
      i <- i + geom_text(data=praktyczna, aes(x=max(Numer_wiersza), y=input$DGT, label=paste("",input$DGT)), size=4,  vjust = -0.5)
      i <- i + geom_text(data=praktyczna, aes(x=max(Numer_wiersza), y=input$GGT, label=paste("",input$GGT)), size=4,  vjust = -0.5)
      i <- i + scale_y_continuous(breaks = waiver(), minor_breaks = waiver(), expand = c(0.2, 0))
      i <- i + scale_x_continuous('Części', breaks = NULL, labels=NULL)
      i <- i + geom_text(data=praktyczna.min.max, aes(x=Numer_wiersza+((Numer_wiersza + n - 1)-Numer_wiersza)/2, y=max.measurement, label=Czesc), size=4, vjust= -0.5 )
      i <- i + theme_tufte(base_size = 13, base_family = "Open Sans")
      
    }
    else if(2 %in% input$rysuj.praktyczna){
      i <- ggplot()
      i <- i + geom_rect(data=praktyczna.min.max.operator, mapping=aes(xmin=Numer_wiersza, xmax=Numer_wiersza + n*parts - 1, ymin=min.measurement.operator, ymax=max.measurement.operator), size = 0.25, alpha = 1 , fill = "white", linetype = "dashed", color = "black")
      i <- i + geom_rect(data=praktyczna.min.max, mapping=aes(xmin=Numer_wiersza, xmax=Numer_wiersza + n - 1, ymin=min.measurement, ymax=max.measurement), size = 0.25, alpha = 1 , fill = "white", linetype = "solid", color = "black")
      i <- i + geom_point(data=praktyczna, aes(x=Numer_wiersza, y=Wynik), size = 2.5, shape = 16)
      i <- i + geom_hline(aes(yintercept = input$DGT), size = 0.5, alpha = 1, linetype = "dashed")
      i <- i + geom_hline(aes(yintercept = input$GGT), size = 0.5, alpha = 1, linetype = "dashed")
      i <- i + geom_text(data=praktyczna, aes(x=max(Numer_wiersza)-0.5, y=input$DGT, label=paste("",input$DGT)), family = "Open Sans",size=4,  vjust = -0.5)
      i <- i + geom_text(data=praktyczna, aes(x=max(Numer_wiersza)-0.5, y=input$GGT, label=paste("",input$GGT)), family = "Open Sans",size=4,  vjust = -0.5)
      i <- i + geom_text(data=praktyczna.min.max.operator, aes(x=Numer_wiersza+((Numer_wiersza + n*parts - 1)-Numer_wiersza)/2, y=max.measurement.operator, label=paste("Operator ",Operator)), size=4,  vjust = -0.5)
      i <- i + scale_y_continuous( breaks = waiver(), minor_breaks = waiver(), expand = c(0.2, 0))
      i <- i + scale_x_continuous('Części', breaks = NULL, labels=NULL)
      i <- i + geom_text(data=praktyczna.min.max, aes(x=Numer_wiersza+((Numer_wiersza + n - 1)-Numer_wiersza)/2, y=max.measurement, label=Czesc), size=4, vjust= -0.5 )
      i <- i + theme_tufte(base_size = 13, base_family = "Open Sans")
      
      
    }
    else if(3 %in% input$rysuj.praktyczna){
      i <- ggplot()
      i <- i + geom_rect(data=praktyczna.min.max.operator, mapping=aes(xmin=Numer_wiersza, xmax=Numer_wiersza + n*parts - 1, ymin=min.measurement.operator, ymax=max.measurement.operator), size = 0.25, alpha = 1 , fill = "white", linetype = "dashed", color = "black")
      i <- i + geom_rect(data=praktyczna.min.max, mapping=aes(xmin=Numer_wiersza, xmax=Numer_wiersza + n - 1, ymin=min.measurement, ymax=max.measurement), size = 0.25, alpha = 1 , fill = "white", linetype = "solid", color = "black")
      i <- i + geom_line(data=praktyczna, aes(x=Numer_wiersza, y=Wynik, group = interaction(Operator, Czesc)), linetype = "solid", colour = "#8b9dc3", size = 0.25)
      i <- i + geom_point(data=praktyczna, aes(x=Numer_wiersza, y=Wynik), size = 2.5, shape = 16)
      i <- i + geom_hline(aes(yintercept = input$DGT), size = 0.5, alpha = 1, linetype = "dashed")
      i <- i + geom_hline(aes(yintercept = input$GGT), size = 0.5, alpha = 1, linetype = "dashed")
      i <- i + geom_text(data=praktyczna, aes(x=max(Numer_wiersza)-0.5, y=input$DGT, label=paste("",input$DGT)), family = "Open Sans",size=4,  vjust = -0.5)
      i <- i + geom_text(data=praktyczna, aes(x=max(Numer_wiersza)-0.5, y=input$GGT, label=paste("",input$GGT)), family = "Open Sans",size=4,  vjust = -0.5)
      i <- i + scale_y_continuous( breaks = waiver(), minor_breaks = waiver(), expand = c(0.2, 0))
      i <- i + scale_x_continuous('Części', breaks = NULL, labels=NULL)
      i <- i + geom_text(data=praktyczna.min.max, aes(x=Numer_wiersza+((Numer_wiersza + n - 1)-Numer_wiersza)/2, y=max.measurement, label=Czesc), size=4, vjust= -0.5 )
      i <- i + theme_tufte(base_size = 13, base_family = "Open Sans")
      i <- i + geom_text(data=praktyczna.min.max.operator, aes(x=Numer_wiersza+((Numer_wiersza + n*parts - 1)-Numer_wiersza)/2, y=max.measurement.operator, label=paste("Operator ",Operator)), size=4,  vjust = -0.5)
      
    }
    
    
    return(i)
  })
  
  # karta X
  output$graphicalX <- renderPlot({
    if (is.null(graficzna.tabela.label()))
      return(NULL)
    qcc.options(bg.margin = "#FFFFFF")
    qA <- qcc(graficzna.tabela(),type="xbar", plot = FALSE, labels=graficzna.tabela.label()$Czesc)
    X <- plot(qA, add.stats=FALSE,  title="CzÄâÃâ¢Äâ¦ÃâºÄâÃâ¡-do-czÄâÃâ¢Äâ¦Ãâºci", xlab="CzÄâÃâ¢Äâ¦ÃâºÄâÃâ¡-do-czÄâÃâ¢Äâ¦Ãâºci", ylab="Äâ¦ÃÅ¡rdednia pomiarÄÂÃÅw", restore.par=TRUE)
    return(X)
  })
  
  # karta R
  output$graphicalR <- renderPlot({
    if (is.null(graficzna.tabela.label()))
      return(NULL)
    qcc.options(bg.margin = "black")
    qA <- qcc(graficzna.tabela(),type="R", plot = FALSE, labels=graficzna.tabela.label()$Czesc)
    R <- plot(qA, add.stats=FALSE, title="Pomiar-do-pomiaru", xlab="CzÄâÃâ¢Äâ¦ÃâºÄâÃâ¡-do-czÄâÃâ¢Äâ¦Ãâºci", ylab="RozstÄâÃâ¢p pomiarÄÂÃÅw", restore.par=TRUE)
    return(R)
  })
  
  output$graficznaX <- renderPlot({
    if (is.null(praktyczna.tabela()))
      return(NULL)
    n <- length(unique(praktyczna.tabela()$Pomiar))
    dane <- praktyczna.tabela.average()
    Range.avg <- mean(praktyczna.tabela.range()$Range, na.rm = TRUE)
    X.avg <- mean(praktyczna.tabela.average()$Mean, na.rm = TRUE)
    UCL <- X.avg + A2[n-1] * Range.avg
    LCL <- X.avg - A2[n-1] * Range.avg
    UCL.annotate <- round(UCL, digits = 2)
    LCL.annotate <- round(LCL, digits = 2)
    X.avg.annotate <- round(X.avg, digits = 2)
    
    g <- ggplot(data=dane)
    g <- g + geom_line(aes(x=Numer_wiersza, y=Mean), linetype = "solid", colour = "black", size=0.25)
    g <- g + geom_point(aes(x=Numer_wiersza, y=Mean), size = 2.5, shape = 16, colour = "black")
    g <- g + geom_hline(aes(yintercept = UCL), size = 0.25, colour = "black" , alpha = 1, linetype = "dashed", show.legend = TRUE)
    g <- g + geom_hline(aes(yintercept = LCL), size = 0.25, colour = "black" , alpha = 1, linetype = "dashed" , show.legend = TRUE)
    g <- g + geom_hline(aes(yintercept = X.avg), size = 0.25, colour = "black" , alpha = 1, linetype = "solid", show.legend = TRUE)
    g <- g + scale_y_continuous('Średnia próbek', breaks = waiver(), minor_breaks = waiver(), expand = c(0.2, 0))
    g <- g + scale_x_continuous('Części', breaks = NULL, minor_breaks = NULL)
    g <- g + geom_text(aes(max(praktyczna.tabela.range()$Numer_wiersza), UCL, label=UCL.annotate), size = 3.5, color = "black", vjust= -0.5)
    g <- g + geom_text(aes(max(praktyczna.tabela.range()$Numer_wiersza), LCL, label=LCL.annotate), size = 3.5, color = "black", vjust= 1.5)
    g <- g + geom_text(aes(max(praktyczna.tabela.range()$Numer_wiersza), X.avg, label=X.avg.annotate), size = 3.5, color = "black", vjust= 1.5)
    g <- g + theme_tufte()
    g <- g + theme(axis.title=element_blank())
    return(g)
    
  })
  
  output$graficznaR <- renderPlot({
    if (is.null(praktyczna.tabela()))
      return(NULL)
    dane <- praktyczna.tabela.range()
    n <- length(unique(praktyczna.tabela()$Pomiar))
    Range.avg <- mean(praktyczna.tabela.range()$Range, na.rm = TRUE)
    UCL <- D4[n-1] * Range.avg
    LCL <- D3[n-1] * Range.avg
    UCL.annotate <- round(UCL, digits = 2)
    LCL.annotate <- round(LCL, digits = 2)
    Range.avg.annotate <- round(Range.avg, digits = 2)
    
    r <- ggplot(data=dane)
    r <- r + geom_line(aes(x=Numer_wiersza, y=Range), linetype = "solid", colour = "black", size=0.25)
    r <- r + geom_point(aes(x=Numer_wiersza, y=Range), size = 2.5, shape = 16, colour = "black")
    r <- r + geom_hline(aes(yintercept = UCL), size = 0.25, colour = "black" , alpha = 1, linetype = "dashed", show.legend = TRUE)
    r <- r + geom_hline(aes(yintercept = LCL), size = 0.25, colour = "black" , alpha = 1, linetype = "dashed" , show.legend = TRUE)
    r <- r + geom_hline(aes(yintercept = Range.avg), size = 0.25, colour = "black" , alpha = 1, linetype = "solid", show.legend = TRUE)
    r <- r + scale_y_continuous('Średnia próbek', breaks = waiver(), minor_breaks = waiver(), expand = c(0.2, 0))
    r <- r + scale_x_continuous('Części', breaks = NULL, minor_breaks = NULL)
    r <- r + geom_text(aes(max(praktyczna.tabela.range()$Numer_wiersza), UCL, label=UCL.annotate), size = 3.5, color = "black", vjust= -0.5)
    r <- r + geom_text(aes(max(praktyczna.tabela.range()$Numer_wiersza), LCL, label=LCL.annotate), size = 3.5, color = "black", vjust= 1.5)
    r <- r + geom_text(aes(max(praktyczna.tabela.range()$Numer_wiersza), Range.avg, label=Range.avg.annotate), size = 3.5, color = "black", vjust= 1.5)
    r <- r + theme_tufte()
    r <- r + theme(axis.title=element_blank())
    return(r)  
  })
  
  output$contents <- DT::renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=TRUE, sep = ";", quote = "\"")
  })
  
  
  
  output$tabela <- DT::renderDataTable({
    praktyczna.tabela()
    # head(range.operator(), n = 40)
  })
  
  output$GageRNR_wariancja <- renderTable({
    if (is.null(tabela.wariancja()))
      return(NULL)
    tabela.wariancja()
    # head(range.operator(), n = 40)
  }, digits = 4)
  
  output$GageRNR_odchylenie <- renderTable({
    if (is.null(tabela.odchylenie()))
      return(NULL)
    head(tabela.odchylenie(), n = 40)
    # head(range.operator(), n = 40)
  }, digits = 4)
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", icon = icon("calendar"))
    )
  })
  
  
  output$ilosciowa.procenty.pomiary <- renderInfoBox({
    
    infoBox( "Info ",repeatability() , icon = icon("thumbs-o-up"),color = "green")
    
  })
  output$ilosciowa.procenty.czesci <- renderInfoBox({
    i <- ilosciowa.czesci()
    if ( (!is.null(i))&&(i<=90))  {infoBox( "Wariancja procesu [%]", paste(i, "%"), icon = icon("thumbs-o-down"),color = "red")}
    else if ((!is.null(i))&&(i>90)) { infoBox( "Wariancja procesu [%]", paste(i, "%"), icon = icon("thumbs-o-up"),color = "green")}
    else {
      infoBox( "Wariancja procesu [%]", "N/A", icon = icon("hand-paper-o"),color = "green")}
  })
  
  #Gage R&R
  
  tabela.wariancja <- reactive({
    if (is.null(repeatability()))
      return(NULL)
    dane <- c(0,0,0,0,0,0,0,0,0,0) 
    tabela<-matrix(dane,ncol=2,nrow=5, byrow=TRUE)
    rownames(tabela)<-c("Total Gage R&R","Repeatability", "Reproducibility", "Part-To-Part", "Total Variation")
    colnames(tabela)<-c("VarComp","%Contribution")
    #repeatability <- repeatability()
    #reproducibility <- reproducibility()
    tabela["Total Gage R&R","VarComp"] <- total.gage()
    tabela["Repeatability","VarComp"] <- repeatability()
    tabela["Reproducibility","VarComp"] <- reproducibility()
    tabela["Part-To-Part","VarComp"] <- part.to.part()
    tabela["Total Variation","VarComp"] <- part.to.part() + total.gage()
    
    tabela["Total Gage R&R","%Contribution"] <- total.gage() / (part.to.part() + total.gage())*100
    tabela["Repeatability","%Contribution"] <- repeatability() / (part.to.part() + total.gage())*100
    tabela["Reproducibility","%Contribution"] <- reproducibility() / (part.to.part() + total.gage())*100
    tabela["Part-To-Part","%Contribution"] <- part.to.part() / (part.to.part() + total.gage())*100
    tabela["Total Variation","%Contribution"] <- (part.to.part() + total.gage()) / (part.to.part() + total.gage())*100
    tabela <- as.table(tabela)
    
    return(tabela)
  })
  
  tabela.odchylenie <- reactive({
    if (is.null(repeatability()))
      return(NULL)
    dane <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) 
    tabela<-matrix(dane,ncol=4,nrow=5)
    rownames(tabela)<-c("Total Gage R&R","Repeatability", "Reproducibility", "Part-To-Part", "Total Variation")
    colnames(tabela)<-c("StdDev (SD)","Study Var", "%Study Var", "%Tolerance" )
    
    tabela["Total Gage R&R","StdDev (SD)"] <- sqrt(total.gage())
    tabela["Repeatability","StdDev (SD)"] <- sqrt(repeatability())
    tabela["Reproducibility","StdDev (SD)"] <- sqrt(reproducibility())
    tabela["Part-To-Part","StdDev (SD)"] <- sqrt(part.to.part())
    tabela["Total Variation","StdDev (SD)"] <- sqrt(part.to.part() + total.gage())
    
    tabela["Total Gage R&R","Study Var"] <- sqrt(total.gage()) * input$StudyVar
    tabela["Repeatability","Study Var"] <- sqrt(repeatability()) * input$StudyVar
    tabela["Reproducibility","Study Var"] <- sqrt(reproducibility()) * input$StudyVar
    tabela["Part-To-Part","Study Var"] <- sqrt(part.to.part()) * input$StudyVar
    tabela["Total Variation","Study Var"] <- sqrt(part.to.part() + total.gage()) * input$StudyVar
    
    tabela["Total Gage R&R","%Study Var"] <- (sqrt(total.gage()) / sqrt(part.to.part() + total.gage())) * 100
    tabela["Repeatability","%Study Var"] <- (sqrt(repeatability()) / sqrt(part.to.part() + total.gage())) * 100
    tabela["Reproducibility","%Study Var"] <- (sqrt(reproducibility()) / sqrt(part.to.part() + total.gage())) * 100
    tabela["Part-To-Part","%Study Var"] <- (sqrt(part.to.part()) / sqrt(part.to.part() + total.gage())) * 100
    tabela["Total Variation","%Study Var"] <- (sqrt(part.to.part() + total.gage()) / sqrt(part.to.part() + total.gage())) * 100
    
    tabela["Total Gage R&R","%Tolerance"] <- (sqrt(total.gage()) * input$StudyVar)*100 / (input$GGT - input$DGT)
    tabela["Repeatability","%Tolerance"] <- (sqrt(repeatability()) * input$StudyVar)*100 / (input$GGT - input$DGT)
    tabela["Reproducibility","%Tolerance"] <- (sqrt(reproducibility()) * input$StudyVar)*100 / (input$GGT - input$DGT)
    tabela["Part-To-Part","%Tolerance"] <- (sqrt(part.to.part()) * input$StudyVar)*100 / (input$GGT - input$DGT)
    tabela["Total Variation","%Tolerance"] <- (sqrt(part.to.part() + total.gage())*100 * input$StudyVar) / (input$GGT - input$DGT)
    
    tabela <- as.table(tabela)
    return(tabela)
    
  })
  
  repeatability <- reactive({
    if (is.null(praktyczna.tabela()))
      return(NULL)
    d2 <- c(1, 1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.259, 3.336, 3.407)
    n <- length(unique(praktyczna.tabela()$Pomiar))
    k <- length(unique(praktyczna.tabela()$Czesc))
    o <- length(unique(praktyczna.tabela()$Operator))
    if (k*o <= 15) {
      x <- (average.range.part()/d2.tabela[k*o,n-1]) * (average.range.part()/d2.tabela[k*o,n-1]) #wariancja pomiarÄÂÃâÄâÃâÄÂÃâÄâÃÂÄÂÃÂÄâÃâÄÂÃâ¦ÄâÃâw
    } else {
      x <- (average.range.part()/d2[n]) * (average.range.part()/d2[n]) #wariancja pomiarÄÂÃâÄâÃâÄÂÃâÄâÃÂÄÂÃÂÄâÃâÄÂÃâ¦ÄâÃâw  
    }
    return(x)
  })
  
  reproducibility<- reactive({
    if (is.null(praktyczna.tabela()))
      return(NULL)
    d2 <- c(1, 1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.259, 3.336, 3.407)
    n <- length(unique(praktyczna.tabela()$Pomiar))
    k <- length(unique(praktyczna.tabela()$Czesc))
    o <- length(unique(praktyczna.tabela()$Operator))
    y <- (average.operator()/d2.tabela[1,o-1]) * (average.operator()/d2.tabela[1,o-1]) #wariancja pomiarÄÂÃâÄâÃâÄÂÃâÄâÃÂÄÂÃÂÄâÃâÄÂÃâ¦ÄâÃâw.
    x <- repeatability()
    z <- y - (1/(n*k))*x
    if (z <0) {z = 0}
    return(z)
  })
  
  total.gage<- reactive({
    if (is.null(praktyczna.tabela()))
      return(NULL)
    total.gage <- repeatability() + reproducibility()
    return(total.gage)
  })
  
  part.to.part <- reactive({
    if (is.null(praktyczna.tabela()))
      return(NULL)
    k <- length(unique(praktyczna.tabela()$Czesc))
    y <- (average.part()/d2.tabela[1,k-1]) * (average.part()/d2.tabela[1,k-1]) #wariancja pomiarÄÂÃâÄâÃâÄÂÃâÄâÃÂÄÂÃÂÄâÃâÄÂÃâ¦ÄâÃâw.
    return(y)
  })
  
  
  output$distinct.categories <- renderValueBox({
    if (is.null(praktyczna.tabela()))
      return(valueBox("0", "Distinct categories  ",icon = icon("hand-paper-o"),color = "yellow"))
    i <- 0
    ndc <- round(1.41 * (sqrt(part.to.part())/sqrt(total.gage())), digits = 0)
    if  ( ndc<=5 && ndc>0)  {
      valueBox( ndc, "Distinct categories  ", icon = icon("thumbs-o-down"),color = "red")
    } else if ( ndc>5) { 
      valueBox( ndc, "Distinct categories  ", icon = icon("thumbs-o-up"),color = "green")
    }
    
    
  })
  
  output$process.tolerance <-renderValueBox({
    if (is.null(praktyczna.tabela()))
      return(valueBox("N/A", "Process Tolerance  ",icon = icon("hand-paper-o"),color = "light-blue"))
    process.tolerance <- input$GGT - input$DGT
    valueBox(process.tolerance, "Process Tolerance  ",icon = icon("hand-paper-o"),color = "light-blue")
    
  })
  
  
  # DOE DOE DOE DOE DOE
  # DOE DOE DOE DOE DOE
  # DOE DOE DOE DOE DOE
  
  # pomocniczy wydruk tabeli
  output$tabela.pomocnicza <- renderDataTable({
    dane <- ANOG()
  })
  
  # (1) Tabela ANOG
  ANOG <- reactive ({
    if (is.null(DOE.Wynik.tabela()))
      return(NULL)
    dane <- DOE.Wynik.tabela()
    dane <- dane %>%
      arrange(Wynik) %>%
      mutate(Numer_wiersza = row_number())
  })
  
  output$rysuj.ANOG <- renderPlot({
    if (is.null(wynik.DOE()))
      return(NULL)
    dane <- ANOG()
    ANOG <- ggplot()
    ANOG <- ANOG + geom_line(data=dane, aes(x=Numer_wiersza, y=Wynik, group = 1), linetype = "solid", colour = "#2d2d2d", size = 0.5)
    ANOG <- ANOG + geom_point(data=dane, aes(x=Numer_wiersza, y=Wynik), size = 3, shape = 15, colour = "#2d2d2d")
    ANOG <- ANOG + scale_x_continuous('Pomiar', breaks = NULL, labels=NULL)
    ANOG <- ANOG + theme_bw()
    ANOG <- ANOG + theme(legend.position="none", panel.grid.major = element_line(colour = "grey98"))
    return(ANOG)
  })  
  
  output$rysuj.SCATTER <- renderPlot({
    if (is.null(wynik.DOE()))
      return(NULL)
    dane <- ANOG()
    SCATTER <- ggplot()
    SCATTER <- SCATTER + geom_line(data=dane, aes(x=run.no, y=Wynik, group = 1), linetype = "solid", colour = "#2d2d2d", size = 0.5)
    SCATTER <- SCATTER + geom_point(data=dane, aes(x=run.no, y=Wynik), size = 3, shape = 15, colour = "#2d2d2d")
    SCATTER <- SCATTER + scale_x_continuous('Pomiar', breaks = dane$run.no, labels=dane$run.no)
    SCATTER <- SCATTER + theme_bw()
    SCATTER <- SCATTER + theme(legend.position="none", panel.grid.major = element_line(colour = "grey98"))
    return(SCATTER)
  })
  
  output$MEPlot <- renderPlot({
    if (is.null(wynik.DOE()))
      return(NULL)
    Design <- generuj.DOE()
    Wynik <- wynik.DOE()
    Wynik <- Wynik[,"Wynik"]
    Design <- add.response(Design,Wynik)
    response.names(Design) <- "Wynik"
    
    MEPlot(Design, abbrev=7, main = NULL, cex.lab = 1.5, cex.yax = 1.5, cex.xax = 1.5)
  })
  
  output$NormalPlot <- renderPlot({
    if (is.null(wynik.DOE()))
      return(NULL)
    Design <- generuj.DOE()
    Wynik <- wynik.DOE()
    Wynik <- Wynik[,"Wynik"]
    Design <- add.response(Design,Wynik)
    response.names(Design) <- "Wynik"
    
    DanielPlot(Design, code = TRUE, half = FALSE, alpha = 0.2)
  })
  
  output$IEPlot <- renderPlot({
    if (is.null(wynik.DOE()))
      return(NULL)
    Design <- generuj.DOE()
    Wynik <- wynik.DOE()
    Wynik <- Wynik[,"Wynik"]
    Design <- add.response(Design,Wynik)
    response.names(Design) <- "Wynik"
    
    IAPlot(Design, show.alias = FALSE, main = NULL, cex = 2, cex.lab = 1.5, cex.yax = 1.5, cex.xax = 1.5)
  })
  
  output$ParetoPlot <- renderPlot({
    if (is.null(efekty()))
      return(NULL)
    dane <- efekty()
    Par <- ggplot(data=dane, aes(x=reorder(Czynnik,Abs.Efekt), y=Abs.Efekt))
    Par <- Par + geom_bar(stat="identity", fill="#BEAD6B")
    Par <- Par + coord_flip() + theme_bw()
    return(Par)
  })
  
  LenthPSE <- reactive({
    dane <- efekty()
    #S <- 1.7
    S <-  1.5 * median(dane$Abs.Efekt)
    dane <- dane %>%
      filter(Abs.Efekt < 2.5 * S) %>%
      group_by(Abs.Efekt)
    LenthPSE <- 1.5 * median(dane$Abs.Efekt)
    
    
  })
  
  output$PSE <- renderValueBox({
    LenthPSE <- LenthPSE()
    valueBox(LenthPSE, "Distinct categories  ", icon = icon("thumbs-o-up"),color = "green")
    
  })
  
  generuj.DOE <- reactive({
    FF <- FrF2(16,7, default.levels = c("-", "+"), randomize = TRUE, seed = 6285)
    return(FF)
  })
  
  DOE.Wynik.tabela <- reactive({
    Design <- generuj.DOE()
    RunOrder <- run.order(generuj.DOE())
    Design <- tbl_df(Design)
    RunOrder <- tbl_df(RunOrder)
    
    if(is.null(wynik.DOE())) {
      Tabela.DOE <- cbind(RunOrder,Design)
    } else {
      Wynik <- wynik.DOE()
      Wynik <- tbl_df(Wynik)
      Wynik <- Wynik %>%
        select(Wynik)
      Tabela.DOE <- cbind(RunOrder,Design,Wynik)  
    }
    return(Tabela.DOE)
    
  })
  
  wynik.DOE <- reactive({
    inFile <- input$fileWynik
    if (is.null(inFile))
      return(NULL)
    Wynik <- read.csv2(inFile$datapath, header=TRUE, sep = ";", quote = "\"", dec=",")
  })
  
  # czytaj plik zrodlowy DOE
  output$tabela.DoE <- DT::renderDataTable({
    dane <- DOE.Wynik.tabela()
    dane <- dane %>%
      select(-run.no.std.rp, -run.no.in.std.order)
  })
  
  output$tabela.efekty <- DT::renderDataTable({
    dane <- efekty()
    #dane <- LenthPSE()
  })
  
  
  efekty <- reactive({
    if (is.null(wynik.DOE()))
      return(NULL)
    Design <- generuj.DOE()
    Wynik <- wynik.DOE()
    Wynik <- Wynik[,"Wynik"]
    Design <- add.response(Design,Wynik)
    response.names(Design) <- "Wynik"
    
    efekty <- as.data.frame(lm(Design)$coefficients*2)
    efekty <- tbl_df(add_rownames(efekty))
    data_frame <- setNames(efekty, c("Czynnik","Efekt"))
    
    data_frame <- data_frame %>%
      filter(Czynnik != "(Intercept)", Efekt != 0)
    dane <- data_frame %>%
      mutate(Abs.Efekt=abs(Efekt)) %>%
      arrange(desc(Abs.Efekt))
    
  })
  
}
