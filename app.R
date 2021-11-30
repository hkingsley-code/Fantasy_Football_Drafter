library(shiny)
library(XML)
library(DT)
library(shinythemes)
########################################
#############PRE PROCESSING############
########################################

all<-readHTMLTable("http://games.espn.com/ffl/tools/projections?&leagueId=122576&seasonTotals=true&seasonId=2017")[[2]]
all<-all[-1,]
seq<-seq(40,280,40)
###loop that scrapes espn data and comples it into a data frame 
for(i in 1:length(seq)){
  first<-readHTMLTable(sprintf("http://games.espn.com/ffl/tools/projections?&leagueId=122576&seasonTotals=true&seasonId=2017&startIndex=%s",seq[i]))[[2]]
  first<-first[-1,]
  all<-rbind(first,all)
}
colnames(all)<-c("RNK","PLAYER, TEAM POS","TYPE","ACTION","C/A","YDS","TD","INT","RUSH","YDS","TD","REC","YDS","TD","PTS")


####finds the Position of the player and creates a new column with this variable 
all$POS<-0
all[,2]<-as.character(all[,2])
for(i in 1:length(all$RNK)){
  if(grepl("RB",all[i,2])){all[i,'POS']<-"RB" }
  else if(grepl("QB",all[i,2])){all[i,'POS']<-"QB" }
  else if(grepl("WR",all[i,2])){all[i,'POS']<-"WR" }
  else if(grepl("D/ST",all[i,2])){all[i,'POS']<-"D/ST" }
  else if(grepl("TE",all[i,2])){all[i,'POS']<-"TE" }
  else if(grepl("K",all[i,2])){all[i,'POS']<-"K" }
  ####finds the comma in the Name section and takes the charachter before that  
  if(all[i,'POS']!='D/ST'){
    locate<-gregexpr(",",all[i,2]) 
    all[i,2]<-as.character(substr(all[i,2],1,as.numeric(locate)-1))
  }
  else{ locate<-gregexpr("/",all[i,2]) 
  all[i,2]<-as.character(substr(all[i,2],1,as.numeric(locate[[1]][1])-3))}
}
all<-all[,-c(3,4)]
################SHINY START ###################
ui<-   fluidPage( 
  theme = shinytheme("flatly"),
  titlePanel("Fantasy Football Auto-Drafter and Draft Aid"),
  ####side panel 
  sidebarPanel(
    
    
    actionButton(inputId = "submit",label = "submit"),
    actionButton(inputId = "autodraft",label ="Autodraft"),
    #numericInput(inputId = "pos",label = "Enter your draft Position",value = 1),
    selectInput("teams","Team to view",choices = c(1,2,3,4,5,6,7,8,9,10,11,12)),
    tabsetPanel(
      tabPanel("Best choices",DT::dataTableOutput("choices")),
      tabPanel("My team",DT::dataTableOutput("team")),
      tabPanel("Autodraft",DT::dataTableOutput("auto")))
  ),
  
  ##main panel
  mainPanel(
    h4("You can use the 'Team to View' bar to look at everyones teams"),
    h4("Once a player has been drafted, select his name and hit submit"),
    h4("Select autodraft for a generated version of the whole draft"),
    DT::dataTableOutput('x1')))

server<-function(input,output){
  order<-rep(c(seq(1,12),seq(12,1)),8)
  cnt<-reactiveValues(x=0)
  data<-reactiveValues()
  all<- all[order(all[,'PTS'],decreasing = TRUE),]
  data$values<-all
  output$x1 = DT::renderDataTable(data$values[,c(1,2,13,14)])
  myteam<-reactiveValues() 
  myteam$values<-data.frame(matrix(1, ncol = 12, nrow = 17))
  output$team<-DT::renderDataTable(data.frame(myteam$values[,as.numeric(input$teams)]),rownames=c("QB","RB1","RB2","WR1","WR2","TE","Flex","D/ST","K","Bench1","Bench2","Bench3","Bench4","Bench5","Bench6","Bench7","Total"),
                                   colnames=c("POS","Players"))
  choices<-reactiveValues()
  choices$values<-data.frame(rep(NA,6),rep(NA,6))
  output$choices<-DT::renderDataTable(choices$values,colnames=c("Player","Value"),rownames=c("QB","RB","WR","TE","K","D/ST"))
  
  auto<-reactiveValues()
  auto$values<-data.frame(rep(NA,192))
  output$auto<-DT::renderDataTable(auto$values,colnames=c("Player"))
  
  
  
  ################AFTER SUBMIT#############
  observeEvent(input$submit,{
    ####puts player on my team
    cnt$x<-cnt$x+1
    #if(input$pos==order[cnt$x]){
    if(data$values[input$x1_rows_selected,"POS"]=="QB"){
      row_seq<-c(1,10,11,12,13,14,15,16)
    }
    else if(data$values[input$x1_rows_selected,"POS"]=="RB"){
      row_seq<-c(2,3,7,10,11,12,13,14,15,16)
    }  
    else if(data$values[input$x1_rows_selected,"POS"]=="WR"){
      row_seq<-c(4,5,7,10,11,12,13,14,15,16) 
    }   
    else if(data$values[input$x1_rows_selected,"POS"]=="TE"){
      row_seq<-c(6,7,10,11,12,13,14,15,16)
    }
    else if(data$values[input$x1_rows_selected,"POS"]=="D/ST"){
      row_seq<-c(8,10,11,12,13,14,15,16)
    }
    else if(data$values[input$x1_rows_selected,"POS"]=="K"){
      row_seq<-c(9,10,11,12,13,14,15,16)
    }
    counter<-1
    for(i in 1:length(row_seq)){
      x<-myteam$values[row_seq[i],order[cnt$x]] 
      
      if(nchar(x)<2 && counter==1  ){
        myteam$values[row_seq[i],order[cnt$x]]<-data$values[input$x1_rows_selected,2] 
        #adds points 
        if(row_seq[i]<10){
          myteam$values[17,order[cnt$x]]<-as.numeric(as.character((myteam$values[17,order[cnt$x]])))+as.numeric(as.character(data$values[input$x1_rows_selected,13]))}
        
        
        counter<-counter+1
      }
      
    }
    
    
    
    #####searches through teams and adjusts value weights based on who was alrady drafted 
    #QB
    if(nchar(myteam$values[1,order[cnt$x+1]])<2){
       choices$values[1,2]<-1}
  else    { choices$values[1,2]<-.1}
    #TE
    if(nchar(myteam$values[6,order[cnt$x+1]])<2){
      choices$values[4,2]<-1}
    else    { choices$values[4,2]<-.1}
    #D/ST
    if(nchar(myteam$values[8,order[cnt$x+1]])<2){
      choices$values[6,2]<-.5}
    else    { choices$values[6,2]<-.1}
    #K
    if(nchar(myteam$values[9,order[cnt$x+1]])<2){
      choices$values[5,2]<-.5}
    else    { choices$values[5,2]<-.1}
  ####RB
    if(nchar(myteam$values[3,order[cnt$x+1]])>2){choices$values[2,2]<-.5}
    else if(nchar(myteam$values[2,order[cnt$x+1]])>2){choices$values[2,2]<-1.5}
    else {choices$values[2,2]<-2.5}
   ##WR
     if(nchar(myteam$values[5,order[cnt$x+1]])>2){choices$values[3,2]<-5}
    else if(nchar(myteam$values[4,order[cnt$x+1]])>2){choices$values[3,2]<-1.5}
    else {choices$values[3,2]<-2.5}
    
    if(nchar(myteam$values[2,order[cnt$x+1]])>2 && nchar(myteam$values[3,order[cnt$x+1]])>2 && nchar(myteam$values[7,order[cnt$x+1]])>2 ){
      choices$values[2,2]<-.3
    }
    if(nchar(myteam$values[4,order[cnt$x+1]])>2 && nchar(myteam$values[5,order[cnt$x+1]])>2 && nchar(myteam$values[7,order[cnt$x+1]])>2 ){
      choices$values[3,2]<-.3
    }
    ###if all positions are filled then dont adjust values anymore 
    C<-0
    for( i in 1:9){
      if(nchar(myteam$values[i,order[cnt$x+1]])>2){
      C<-C+1
    }}
    if(C==9){
    choices$values[1,2]<-1
    choices$values[2,2]<-1
    choices$values[3,2]<-1
    choices$values[4,2]<-1
    choices$values[5,2]<-1
    choices$values[6,2]<-1
  
    }
    choices$values[1,2]<-1
    choices$values[2,2]<-1
    choices$values[3,2]<-1
    choices$values[4,2]<-1
    choices$values[5,2]<-1
    choices$values[6,2]<-1
    
    
        ##subsets the dataframe 
    data$values<-data$values[-input$x1_rows_selected,]
    data$values<- data$values[order(data$values[,'PTS'],decreasing = TRUE),]
    
    #adjust the best choices dataframe
    choices$values[1,1]<-data$values[,2][which(data$values[,"POS"]=="QB")][1]
    choices$values[1,2]<-choices$values[1,2]*(as.numeric(as.character(data$values[,13])[which(data$values[,"POS"]=="QB")][1])-251)#mean(as.numeric(as.character(data$values[,"PTS"][which(data$values[,"POS"]=="QB")][2:6]))))
    choices$values[2,1]<-data$values[,2][which(data$values[,"POS"]=="RB")][1]
    choices$values[2,2]<-choices$values[2,2]*(as.numeric(as.character(data$values[,13])[which(data$values[,"POS"]=="RB")][1])-53.4)#mean(as.numeric(as.character(data$values[,"PTS"][which(data$values[,"POS"]=="RB")][2:6]))))
    choices$values[3,1]<-data$values[,2][which(data$values[,"POS"]=="WR")][1]
    choices$values[3,2]<-choices$values[3,2]*(as.numeric(as.character(data$values[,13])[which(data$values[,"POS"]=="WR")][1])-70.8)#mean(as.numeric(as.character(data$values[,"PTS"][which(data$values[,"POS"]=="WR")][2:6]))))
    choices$values[4,1]<-data$values[,2][which(data$values[,"POS"]=="TE")][1]
    choices$values[4,2]<-choices$values[4,2]*(as.numeric(as.character(data$values[,13])[which(data$values[,"POS"]=="TE")][1])-67)#mean(as.numeric(as.character(data$values[,"PTS"][which(data$values[,"POS"]=="TE")][2:6]))))
    choices$values[5,1]<-data$values[,2][which(data$values[,"POS"]=="K")][1]
    choices$values[5,2]<-choices$values[5,2]*(as.numeric(as.character(data$values[,13])[which(data$values[,"POS"]=="K")][1])-124.4)#mean(as.numeric(as.character(data$values[,"PTS"][which(data$values[,"POS"]=="K")][2:6]))))
    choices$values[6,1]<-data$values[,2][which(data$values[,"POS"]=="D/ST")][1]
    choices$values[6,2]<-choices$values[6,2]*(as.numeric(as.character(data$values[,13])[which(data$values[,"POS"]=="D/ST")][1])-106.4)#mean(as.numeric(as.character(data$values[,"PTS"][which(data$values[,"POS"]=="D/ST")][2:6]))))
    
   
    
    
    
    
    
     
  })
  
  
  ##########AUTO DRAFT
  observeEvent(input$autodraft,{
    
    draft<-all
    everyteam<-data.frame(matrix(0, ncol = 12, nrow = 16))
    
    for(i in 1:192){
      draft<- draft[order(draft[,'PTS'],decreasing = TRUE),]
      if(length(which(draft[,"POS"]=="QB"))<6  ){QB<-1}
      else {
        QB<-as.numeric(as.character(draft[,13])[which(draft[,"POS"]=="QB")][1])-251#mean(as.numeric(as.character(draft[,"PTS"][which(draft[,"POS"]=="QB")][2:6])))
      }
      if(length(which(draft[,"POS"]=="RB"))<6  ){RB<-1}
      else {RB<-as.numeric(as.character(draft[,13])[which(draft[,"POS"]=="RB")][1])-53.4#mean(as.numeric(as.character(draft[,"PTS"][which(draft[,"POS"]=="RB")][2:6])))
      } 
      if(length(which(draft[,"POS"]=="WR"))<6  ){WR<-1}
      else {WR<-as.numeric(as.character(draft[,13])[which(draft[,"POS"]=="WR")][1])-70.8#mean(as.numeric(as.character(draft[,"PTS"][which(draft[,"POS"]=="WR")][2:6])))
      }
      if(length(which(draft[,"POS"]=="TE"))<6  ){TE<-1}
      else {TE<-as.numeric(as.character(draft[,13])[which(draft[,"POS"]=="TE")][1])-67#mean(as.numeric(as.character(draft[,"PTS"][which(draft[,"POS"]=="TE")][2:6])))
        }
    if(length(which(draft[,"POS"]=="K"))<6  ){K<-1}  
    else {K<-as.numeric(as.character(draft[,13])[which(draft[,"POS"]=="K")][1])-124.4#mean(as.numeric(as.character(draft[,"PTS"][which(draft[,"POS"]=="K")][2:6])))
    }  
    if(length(which(draft[,"POS"]=="D/ST"))<6  ){DST<-1}
    else{  DST<-as.numeric(as.character(draft[,13])[which(draft[,"POS"]=="D/ST")][1])-106.4#mean(as.numeric(as.character(draft[,"PTS"][which(draft[,"POS"]=="D/ST")][2:6])))
    }
      
  ##adjustments so positions dont get overdrafted 
 # if(everyteam[1,order[i]]>0){QB<-QB*.1}    
  #if(everyteam[2,order[i]]==0){RB<-RB*2.5}
  #else if(everyteam[2,order[i]]==1){RB<-RB*1.5}
  #else if(everyteam[2,order[i]]==2){RB<-RB*.5}    
  #else if(everyteam[2,order[i]]>2){RB<-RB*.1}    
  #if(everyteam[3,order[i]]==0){WR<-WR*2.5}
  #else if(everyteam[3,order[i]]==1){WR<-WR*1.5}
  #else if(everyteam[3,order[i]]==2){WR<-WR*.5}    
  # else if(everyteam[3,order[i]]>2){WR<-WR*.1}    
  #  if(everyteam[4,order[i]]>0){TE<-TE*.1}    
  #  if(everyteam[5,order[i]]==0){K<-K*.5}    
  #  else  if(everyteam[5,order[i]]>0){K<-K*.1}    
  #    if(everyteam[6,order[i]]==0){DST<-DST*.5}    
  #    else  if(everyteam[6,order[i]]>0){DST<-DST*.1}   
      
      
      
      
      ####puts autodrafter player into dataframe
      M<- max(QB,RB,WR,TE,K,DST)
      if(M==QB){
        auto$values[i,1]<- draft[,2][which(draft[,"POS"]=="QB")][1] 
        draft<-draft[-which(draft[,"POS"]=="QB")[1],] 
        everyteam[1,order[i]]<- everyteam[1,order[i]]+1} 
      else if(M==RB){
        auto$values[i,1]<- draft[,2][which(draft[,"POS"]=="RB")][1] 
        draft<-draft[-which(draft[,"POS"]=="RB")[1],] 
        everyteam[2,order[i]]<- everyteam[2,order[i]]+1}  
      else if(M==WR){
        auto$values[i,1]<- draft[,2][which(draft[,"POS"]=="WR")][1] 
        draft<-draft[-which(draft[,"POS"]=="WR")[1],] 
        everyteam[3,order[i]]<- everyteam[3,order[i]]+1}   
      else if(M==TE){
        auto$values[i,1]<- draft[,2][which(draft[,"POS"]=="TE")][1] 
        draft<-draft[-which(draft[,"POS"]=="TE")[1],] 
        everyteam[4,order[i]]<- everyteam[4,order[i]]+1}  
      else if(M==K){
        auto$values[i,1]<- draft[,2][which(draft[,"POS"]=="K")][1] 
        draft<-draft[-which(draft[,"POS"]=="K")[1],] 
        everyteam[5,order[i]]<- everyteam[5,order[i]]+1}  
      else if(M==DST){
        auto$values[i,1]<- draft[,2][which(draft[,"POS"]=="D/ST")][1] 
        draft<-draft[-which(draft[,"POS"]=="D/ST")[1],] 
        everyteam[6,order[i]]<- everyteam[6,order[i]]+1}  
    }
  })    
  
  
  
}





shinyApp(ui=ui,server = server)