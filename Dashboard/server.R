# TODO: OUTLIERS HANDLING
# TODO: FILL N/A OR OUTLIERS-> WITH REGRESSION, WITH MEAN, MEDIAN
# TODO: NORM TEST



# IMPORTS #######################
library(mice)
library(shiny)
library(shinyjs)
library(dplyr)
library(psych)
library(ggplot2)
library(outliers)
library(cleaner)
library(tseries)

# LOAD UTILS ##################

pickDecimal<-function(input){
    if(input == "comma")
        return(",")
    else
        return(".")
}
pickSeparator<-function(input){
    if(input == "comma"){
        return(",")
        
    }
    else if(input == "space"){
        return(" ")
        
    }
    else if(input == "semicolon"){
        return(";")
        
    }
    else{
        return("\t")
    } 
}
current_data<-0

shinyServer(function(input, output, session) {
    
    
    
    write.table(iris,file="temp.csv",row.names = F,sep = ",", dec=".")
    ## Plots rendering
    
    invalidPlot<-FALSE
    tmp_exists<-FALSE
    
    ## setup local storage
    
    
    output$scatterUI<- renderUI({
        plotOutput("scatterplot")
    })
    
    output$scatterplot <- renderPlot({
        dat<-dataset()
        if(input$variable2scatterSelect && 
           input$variable1scatterSelect && 
           input$variable3scatterSelect){
            dat<-dat %>% select(!!!c(input$variable1scatter, 
                                     input$variable2scatter, 
                                     input$variable3scatter))
            V<-"Variable"
            V2<-"Variable2"
            V3<-"Variable3"
            assign(V, names(dat)[1])
            assign(V2, names(dat)[2])
            assign(V3, names(dat)[3])
            fill<-factor(dat[,3])
            ggplot(dat, aes_string(x=Variable, y=Variable2))+
                geom_point(aes_string(colour = fill), size=2)+
                labs(colour=Variable3)
        }
        else if(input$variable1scatterSelect && input$variable2scatterSelect){
            dat<-dat %>% select(!!!c(input$variable1scatter, 
                                     input$variable2scatter))
            V<-"Variable"
            V2<-"Variable2"
            assign(V, names(dat)[1])
            assign(V2, names(dat)[2])
            ggplot(dat, aes_string(x=Variable, y=Variable2))+geom_point(size=2)
        }
        else if(input$variable1scatterSelect){
            dat<-dat %>% select(!!!input$variable1scatter)
            V<-"Variable"
            assign(V, names(dat)[1])
            ggplot(dat, aes_string(x=Variable, y=rep(0,nrow(dat))))+geom_point(size=2)
        }
        else{
            ggplot()+ggtitle("Data incorrect")+theme(plot.title = element_text(vjust = - 20, hjust=0.5,size = 40, face = "bold")) 
        }
    })
    
    output$lineUI<- renderUI({
        plotOutput("lineplot")
    })
    
    output$lineplot <- renderPlot({
        dat<-dataset()
        dat<-dat %>% select(!!!input$variablesLine)
        selectedColumn<-as.character(input$dateVariable)[1]
        if(!is.null(input$dateFormat) && input$dateFormat != "" && input$datevariableSelect){
            dat[,selectedColumn]<-
                as.Date(clean_Date(dat[,selectedColumn], format = input$dateFormat))
        }
        if(length(names(dat))>0){
            V<-"Variable"
            assign(V, names(dat)[1])
            a<-ggplot(dat, aes_string(x=Variable))
            if(length(names(dat))>1){
                for(i in c(2:length(names(dat)))){
                    assign(V, names(dat)[i])
                    a<-a+geom_line(aes_string(y=Variable, colour=shQuote(Variable)))
                }
            }
            a<-a+labs(colour="Variable")
            a
        }
        else{
            ggplot()+ggtitle("Data incorrect")+theme(plot.title = element_text(vjust = - 20, hjust=0.5,size = 40, face = "bold"))
        }
    })
    
    output$histUI<- renderUI({
        plotOutput("hist")
    })
    
    output$hist <- renderPlot({
        dat<-dataset()
        dat<-dat %>% select(!!!input$variableHist)
        x <- dat
        a <- seq(min(x), max(x), length.out = input$bins + 1)
        V<-"VariableHist"
        assign(V, names(dat)[1])
        if(length(names(dat)) != 1) {
            ggplot()+ggtitle("Data incorrect")+theme(plot.title = element_text(vjust = - 20, hjust=0.5,size = 40, face = "bold")) 
        }
        else {
            bw<-(max(dat)-min(dat))/input$bins
            ggplot(dat, aes_string(x=VariableHist))+geom_histogram(binwidth = bw)+geom_density(aes(y=bw*..count..))
        }
    })
    
    output$panelsUI<- renderUI({
        plotOutput("panels")
    })
    
    output$panels <- renderPlot({
        dat<-dataset()
        dat<-dat %>% select(!!!input$variablesPanels)
        if(length(names(dat)) < 2) {
            ggplot()+ggtitle("Data incorrect")+theme(plot.title = element_text(vjust = - 20, hjust=0.5,size = 40, face = "bold")) 
        }
        else{
            pairs.panels(dat,
                         method = "pearson",
                         hist.col = "#00AFBB",
                         density = T,
                         lm=T)
        }
    })
    
    output$boxplotUI<- renderUI({
        plotOutput("boxplot")
    })
    
    output$boxplot <- renderPlot({
        dat<-dataset()
        if(input$variable2boxSelect && 
           input$variable1boxSelect && 
           input$variable3boxSelect){
            dat<-dat %>% select(!!!c(input$variable1box, 
                                     input$variable2box, 
                                     input$variable3box))
            V<-"Variable"
            V2<-"Variable2"
            V3<-"Variable3"
            assign(V, names(dat)[1])
            assign(V2, names(dat)[2])
            assign(V3, names(dat)[3])
            fill<-factor(dat[,3])
            ggplot(dat, aes_string(x=Variable, y=Variable2))+
                geom_boxplot(aes_string(colour = fill))+
                labs(colour=Variable3)
        }
        else if(input$variable1boxSelect && input$variable2boxSelect){
            dat<-dat %>% select(!!!c(input$variable1box, 
                                     input$variable2box))
            V<-"Variable"
            V2<-"Variable2"
            assign(V, names(dat)[1])
            assign(V2, names(dat)[2])
            ggplot(dat, aes_string(x=Variable, y=Variable2))+geom_boxplot()
        }
        else if(input$variable1boxSelect){
            dat<-dat %>% select(!!!input$variable1box)
            V<-"Variable"
            assign(V, names(dat)[1])
            ggplot(dat, aes_string(x=Variable))+geom_boxplot()
        }
        else{
            ggplot()+ggtitle("Data incorrect")+theme(plot.title = element_text(vjust = - 20, hjust=0.5,size = 40, face = "bold")) 
        }
    })
    
    output$barplotUI<- renderUI({
        plotOutput("barplot")
    })
    
    output$barplot <- renderPlot({
        dat<-dataset()
        if(input$variable2barSelect && 
           input$variable1barSelect && 
           input$variable3barSelect){
            dat<-dat %>% select(!!!c(input$variable1bar, 
                                     input$variable2bar, 
                                     input$variable3bar))
            V<-"Variable"
            V2<-"Variable2"
            V3<-"Variable3"
            assign(V, names(dat)[1])
            assign(V2, names(dat)[2])
            assign(V3, names(dat)[3])
            fills<-factor(dat[,3])
            ggplot(data, aes_string(x=Variable, y=Variable2, fill=Variable3))+
                geom_bar(stat = "identity", position="dodge")+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
                labs(color = Variable3)
        }
        else if(input$variable1barSelect && input$variable2barSelect){
            dat<-dat %>% select(!!!c(input$variable1bar, 
                                     input$variable2bar))
            V<-"Variable"
            V2<-"Variable2"
            assign(V, names(dat)[1])
            assign(V2, names(dat)[2])
            ggplot(dat, aes_string(x=Variable, y=Variable2))+
                geom_bar(stat="identity", position="dodge")+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        }
        else if(input$variable1barSelect){
            dat<-dat %>% select(!!!input$variable1bar)
            V<-"Variable"
            assign(V, names(dat)[1])
            ggplot(dat, aes_string(x=Variable))+
                geom_bar(position="dodge")+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        }
        else{
            ggplot()+ggtitle("Data incorrect")+theme(plot.title = element_text(vjust = - 20, hjust=0.5,size = 40, face = "bold")) 
        }
    })
    
    output$missingDataChart <- renderPlot({
        dat<-dataset()
        md.pattern(dat)
    })
    
    
    
    
    
    #####################################
    ## Table rendering
    
    output$dataUI<- renderUI({
        tableOutput("data")
    })
    
    output$testOutputsUI<- renderUI({
        verbatimTextOutput("GrubbsTest")
        verbatimTextOutput("DixonTest")
    })
    
    output$GrubbsTest<- renderPrint({
        dat<-dataset()
        dat<-dat %>% select(input$variableToCleanSelect)
        tested_variable<-dat[,1]
        grubbs.test(tested_variable)
    })
    
    output$GrubbsTest1<- renderPrint({
        dat<-dataset()
        dat<-dat %>% select(input$variableToCleanSelect)
        tested_variable<-dat[,1]
        grubbs.test(tested_variable, opposite = T)
    })
    
    output$GrubbsTest2<- renderPrint({
        dat<-dataset()
        dat<-dat %>% select(input$variableToCleanSelect)
        tested_variable<-dat[,1]
        grubbs.test(tested_variable, type=11, opposite = F)
    })
    
    output$GrubbsTest21<- renderPrint({
        dat<-dataset()
        dat<-dat %>% select(input$variableToCleanSelect)
        tested_variable<-dat[,1]
        grubbs.test(tested_variable, type=11, opposite = T)
    })
    
    output$GrubbsTest3<- renderPrint({
        dat<-dataset()
        dat<-dat %>% select(input$variableToCleanSelect)
        tested_variable<-dat[,1]
        grubbs.test(tested_variable, type=20)
    })
    
    output$GrubbsTest31<- renderPrint({
        dat<-dataset()
        dat<-dat %>% select(input$variableToCleanSelect)
        tested_variable<-dat[,1]
        grubbs.test(tested_variable, type=20, opposite = T)
    })
    
    output$DixonTest<- renderPrint({
        dat<-dataset()
        dat<-dat %>% select(input$variableToCleanSelect)
        tested_variable<-dat[,1]
        dixon.test(tested_variable, opposite = T)
    })
    
    output$DixonTest2<- renderPrint({
        dat<-dataset()
        dat<-dat %>% select(input$variableToCleanSelect)
        tested_variable<-dat[,1]
        dixon.test(tested_variable, opposite = F)
    })
    
    output$normTest<- renderPrint({
        dat<-dataset()
        dat<-dat %>% select(input$variableToCleanSelect)
        tested_variable<-dat[,1]
        shapiro.test(tested_variable)
    })
    
    output$normTest2<- renderPrint({
        dat<-dataset()
        dat<-dat %>% select(input$variableToCleanSelect)
        tested_variable<-dat[,1]
        jarque.bera.test(tested_variable)
    })
    
    output$IQRoutliers<- renderPrint({
        dat<-dataset()
        dat<-dat %>% select(input$variableToCleanSelect)
        boxplot(dat[,1],plot=F)$out
    })
    
    
    
    output$data <- renderTable({
        dat<-dataset()
        head(dat)
        if (length(input$variables) == 0) return(dat)
        dat %>% select(!!!input$variables) 
    }, rownames = TRUE)
    
    output$dataPreviewTypes <- renderPrint({
        dat<-dataset()
        return(str(dat))
    })
    
    output$summary <- renderPrint({
        dat<-dataset()
        return((summary(dat)))
    })
    
    output$dataPreviewUI<- renderUI({
        tableOutput("dataPreviev")
    })
    
    output$dataPreview <- renderTable({
        dat<-dataset()
        dat %>% slice(1:10)
    }, rownames = TRUE, colnames = TRUE)
    
    #####################################
    ## OBSERVABLES
    invalidPlots<-reactive({
        return(invalidPlot)
    })
    
    dataset <- reactive({
        dummyClick<-input$trigger
        header<-input$cast
       newData<-read.table("temp.csv", sep=",", dec=".",header=T)
       updateVarSelectInput(session,inputId = "variable1box", data = newData)
       updateVarSelectInput(session,inputId = "variable2box", data = newData)
       updateVarSelectInput(session,inputId = "variable3box", data = newData)
       updateVarSelectInput(session,inputId = "variablesLine", data = newData)
       updateVarSelectInput(session,inputId = "variable1bar", data = newData)
       updateVarSelectInput(session,inputId = "variable2bar", data = newData)
       updateVarSelectInput(session,inputId = "variable3bar", data = newData)
       updateVarSelectInput(session,inputId = "variablesTable", data = newData)
       updateVarSelectInput(session,inputId = "variable1scatter", data = newData)
       updateVarSelectInput(session,inputId = "variable2scatter", data = newData)
       updateVarSelectInput(session,inputId = "variable3scatter", data = newData)
       updateVarSelectInput(session,inputId = "variableHist", data = newData)
       updateVarSelectInput(session,inputId = "variablesPanels", data = newData)
       updateVarSelectInput(session,inputId = "variableToCleanSelect", data = newData)
       updateVarSelectInput(session,inputId = "variableToCastSelect", data = newData)
       updateVarSelectInput(session,inputId = "dateVariable", data = newData)
       updateVarSelectInput(session,inputId = "variableToFilterSelect", data = newData)
       return(newData)
    })
    
    observeEvent(input$upload,{
        separator <- pickSeparator(input$separator)
        decimal <- pickDecimal(input$decimal)
        if(!is.null(input$file1$datapath)){
            newData<-read.csv(input$file1$datapath, sep=separator, dec=decimal, header=input$header)
            write.table(newData,file="temp.csv",row.names = F,sep = ",", dec=".")
        }
        js$ref()
    })
    
    observeEvent(input$applyFilter,{
        dat<-dataset()
        selectedColumn<-as.character(input$variableToCastSelect)[1]
        if(input$filter == "equal to") {
            dat<-dat[dat[,selectedColumn] == input$filtervalue,]
        }
        else if(input$filter == "less than") {
            dat<-dat[dat[,selectedColumn] < input$filtervalue,]
        }
        else {
            dat<-dat[dat[,selectedColumn] > input$filtervalue,]
        }
        write.table(dat,file="temp.csv",row.names = F,sep = ",", dec=".")
        js$ref()
    })
    
    observeEvent(input$cast,{
        dat<-dataset()
        selectedColumn<-as.character(input$variableToCastSelect)[1]
        switch(input$type, 
               numeric={
                   dat[,selectedColumn]<-
                       clean_double(dat[,selectedColumn]) 
               },
               character={
                   dat[,selectedColumn]<-
                       as.character(dat[,selectedColumn])
               }) 
        write.table(dat,file="temp.csv",row.names = F,sep = ",", dec=".")
    })
    
    observeEvent(input$naClean,{
        dat<-dataset()
        dat<-na.omit(dat)
        write.table(dat,file="temp.csv",row.names = F,sep = ",", dec=".")
        js$ref()
    })
    
    observeEvent(input$naFill, {
        dat<-dataset()
        columns<-names(dat)
        switch(input$fillMissing, {
            mean={
                for(i in c(1:length(columns))){
                    dat[is.na(dat[,i]),i]<-mean(na.omit(dat[,i]))
                } 
            }
            median={
                for(i in c(1:length(columns))){
                    dat[is.na(dat[,i]),i]<-median(na.omit(dat[,i]))                } 
            }
            previous={
                dat<-fill(dat, "down")
            }
            'next'={
                dat<-fill(dat, "up")
            }
            zero={
                for(i in c(1:length(columns))){
                    dat[is.na(dat[,i]),i]<-0
                } 
            }
        })
        write.table(dat,file="temp.csv",row.names = F,sep = ",", dec=".")
        js$ref()
    })
    
    observeEvent(input$naFill, {
        dat<-dataset()
        columns<-names(dat)
        switch(input$fillMissing, {
            mean={
                for(i in c(1:length(columns))){
                    dat[is.na(dat[,i]),i]<-mean(na.omit(dat[,i]))
                } 
            }
            median={
                for(i in c(1:length(columns))){
                    dat[is.na(dat[,i]),i]<-median(na.omit(dat[,i]))                } 
            }
            previous={
                dat<-fill(dat, "down")
            }
            'next'={
                dat<-fill(dat, "up")
            }
            zero={
                for(i in c(1:length(columns))){
                    dat[is.na(dat[,i]),i]<-0
                } 
            }
        })
        write.table(dat,file="temp.csv",row.names = F,sep = ",", dec=".")
        js$ref()
    })
    
    output$download <- downloadHandler(
        filename = function(){"exported.csv"}, 
        content = function(fname){
            write.table(dataset(), fname)
        }
    )
})

readUrl <- function(url) {
    out <- tryCatch(
        {
            # try
        },
        error=function(cond) {
            return(NULL)
        },
        warning=function(cond) {
            return(NULL)
        },
        finally={
            message(paste("Processed URL:", url))
            message("Some other message at the end")
        }
    )    
    return(out)
}