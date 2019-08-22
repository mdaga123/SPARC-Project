#install.packages("dplyr")
#setwd('E:\\Spring\\RA Work All\\RA work 3\\Modularize')
########Loading libraries###############
library(shiny)
library(DT)
library(dplyr)
library(shinycssloaders)
library(readxl)
library(shinyjs)
library(ggplot2)
library(png)
library(spatstat)
library(shinyBS) #popify function
library(nortest)

require(tibble)
require(magrittr)
require(dplyr)
require(multcomp)
require(emmeans)
require(readxl)
library(httr)
library(plotly)
require(ggfortify)
source('rshiny_functions.R')

#####UI Functions########

h<-densityUI()
t<-tukeyUI()
b<-barUI()
s<-scatterUI()
spat<-spatUI_sb()
spat_m<-spatUI_mp()
loaddata<-loaddataUI()

######UI Function Start for Shiny##############
ui <-navbarPage(title="RShinyApp", windowTitle = "Data Visualization", theme = shinythemes::shinytheme("cerulean"),selected = "Load Data",
#####Reset Code#########                
                tags$a(href="javascript:history.go(0)",
                       popify(tags$i(class="fa fa-refresh fa-5x"),
                              title = "Reload",
                              content = "Click here to restart the Shiny session",
                              placement = "right")),
                
                tags$style(type="text/css", "#value{ height: 50px; font-family: monospace;}"),
                

             
#########Statistical Analysis Tab UI######################                
               tabPanel(title="Statistical Analysis", #1st Tab Panel Start
                           fluidPage(
                                   sidebarLayout(
                                       sidebarPanel( 
                                         
                                         wellPanel(selectInput("plottype",label = "Choose Visualization Plot Type",choices = list(Histogram = "density", Barplot = "bar",Tukey="tukey",Scatter="scatter"), selected = "bar")
                                         ),#WellPanel
                                         
                                         conditionalPanel(condition = h$condition, img(src=h$src),wellPanel(radioButtons("varstabh","Variance Stablisation for Normality",choices = c("None","Log","Square-Root"),selected = "None")),
                                                          wellPanel(radioButtons("dennum","Histogram Plot Type",choices = c("Single","Comparision"),selected = "Single")),
                                                          conditionalPanel(condition = h$choicecond1,wellPanel(selectInput("densityx", h$label,choices = NULL))),
                                                          conditionalPanel(condition = h$choicecond2,wellPanel(selectInput("densityx1", h$label1 ,choices = NULL),
                                                                           selectInput("densityx2", h$label2 ,choices = NULL)))
                                                                           ),#conditionalPAnel
                                         
                                        
                                        conditionalPanel(condition = t$condition, img(src=t$src),
                                                         wellPanel(selectInput("tukeyy", t$ylabel,choices = NULL),radioButtons("varstabt","Variance Stablisation for Normality",choices = c("None","Log","Square-Root"),selected = "None")),
                                                                   wellPanel(radioButtons("tukeycatvar",t$choicelabel,choices = c("One","Two (Interaction)"),selected = "One"),
                                                                   selectInput("tukeyx1", t$x1label,choices = NULL),
                                                                   conditionalPanel(condition = t$tukeychoicecond,selectInput("tukeyx2", t$x2label,choices = NULL)))),#conditionalPAnel
                                        
                                        conditionalPanel(condition=b$condition,img(src=b$src),
                                                         wellPanel(selectInput("bary", b$ylabel,choices=NULL),radioButtons("varstabb","Variance Stablisation for Normality",choices = c("None","Log","Square-Root"),selected = "None")),
                                                                   wellPanel(selectInput("barx", b$xlabel,choices=NULL)),
                                                                   wellPanel(checkboxInput("barfiltercheck",HTML(b$filterlabel),value = FALSE),
                                                                   conditionalPanel(b$filtercond,selectInput("barfiltervariable",b$filterlabel_var,choices=NULL),
                                                                                    selectInput("barfiltervalue",  b$filterlabel_val,choices=NULL)))),#conditionalPanel
                                        
                                        
                                        conditionalPanel(condition=s$condition,img(src=s$src),
                                                         wellPanel(selectInput("scattery", s$ylabel,choices=NULL),radioButtons("varstabsy","Variance Stablisation for Normality",choices = c("None","Log","Square-Root"),selected = "None")),
                                                         wellPanel(selectInput("scatterx", s$xlabel,choices=NULL),radioButtons("varstabsx","Variance Stablisation for Normality",choices = c("None","Log","Square-Root"),selected = "None")),
                                                         wellPanel(checkboxInput("scattercolorcheck",HTML(s$colorcheck),value = FALSE),
                                                                   conditionalPanel(s$colorcond,selectInput("scatterc", s$colorlabel,choices=NULL))),
                                                         wellPanel(checkboxInput("scattersizecheck",HTML(s$sizecheck),value = FALSE),
                                                                   conditionalPanel(s$sizecond,selectInput("scatters", s$sizelabel,choices=NULL))),
                                                         wellPanel(checkboxInput("scatterfiltercheck",HTML(s$filterlabel),value = FALSE),
                                                                   conditionalPanel(s$filtercond,selectInput("scatterfiltervariable",s$filterlabel_var,choices=NULL),
                                                                                    selectInput("scatterfiltervalue",  s$filterlabel_val,choices=NULL)))
                                                                   
                                                                   
                                                                   
                                                                   ),#conditionalPanel
                                                                  
                                                                                    
                                        
                                         wellPanel(textInput("plottitle", "Plot Title :"))
                                         
                                       ),#sideBarPanel
                                       
                                       
                                       mainPanel(wellPanel(actionButton("plotclick","Plot"), downloadButton('downloadPlotDesc','Download Plot')),
                                         textOutput('pvalue'),
                                         plotlyOutput('plot1'),
                                        # plotlyOutput('plot1'),
                                         tableOutput('text1')
                                        
                                          #textOutput("text1")
                                       )#mainpanel
                                   )##Sidebarlayout
                           )#Fluidpage
                 ), # 1st tabpanel
                
#########Spatial Analysis Tab UI#######################                
                tabPanel(title="Spatial Analysis",  #2nd Tab Panel Start
                         fluidPage(useShinyjs(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       fileInput(inputId = "pic", label = spat$contourlabel, accept = c(".png",".jpeg",".bmp")),
                                       wellPanel(selectInput("spatgraphtype", spat$spatplottype, choices = c("Black & White Point Pattern", "Colored Point Pattern","Density Map")),
                                                 conditionalPanel(condition =  spat$bwcond,img(src= spat$bwsrc)),
                                                 conditionalPanel(condition = spat$colorcond,img(src=spat$ccsrc)),
                                                 conditionalPanel(condition = spat$densitycond,img(src= spat$dsrc)),
                                       selectInput("left",  spat$xcoord,choices = NULL),
                                       selectInput("bottom",  spat$ycoord,choices = NULL)),#End Well Panel
                                       wellPanel(selectInput("feature", spat$numvar,choices = NULL),checkboxInput("spatfiltercheck", spat$filterlabel,value = FALSE),
                                                 conditionalPanel( spat$spatfiltercond,selectInput("spatvariable",  spat$spatcatvar,choices=NULL),selectInput("spatvalue",  spat$spatcatval,choices=NULL)))#End well Panel
                                       
                                     ),#End Sidebarpanel
                                     
                                     mainPanel(tags$h3(spat_m$title,align="center"),
                                       wellPanel(radioButtons(inputId = "spatplotlevel",spat_m$radiobuttonslabel, choices = c("Two","Three"), selected = "Three", inline=TRUE),
                                                 HTML(spat_m$desc),
                                                 actionButton("spatplotclick","Plot")),#End WellPanel   
                                        
                                          
                                       withSpinner(plotOutput("plot2")),
                                       verbatimTextOutput("blankvalues")
                                       #textOutput({"text5"})
                                               
                                      # tableOutput('tablespat')
                                               )#End Main Panel Output
                                   )#End SideBarLaypout
                                   )#End FluidPage Spatial Analysis Tabpanel
                         
                ), #2nd Tab Panel End 

#############Load Data Tab UI############################                
                tabPanel(title="Load Data", #3rd Tab Panel Start,
                         
                         fluidPage(useShinyjs(),
                                   
                                  
                                   sidebarLayout(
                                     sidebarPanel(
                                       wellPanel(checkboxGroupInput("filetype", loaddata$uploadlabel,
                                                                    choices = c("CSV"="csv"),selected = "csv")),
                                      
                                       conditionalPanel(condition = loaddata$loadcond,
                                                        
                                                        wellPanel(checkboxInput(inputId = 'header', label =loaddata$headerlabel, value = FALSE),
                                                                  tags$p(HTML(loaddata$special_char_label))),#Header Well Panel
                                                                                                                                                  
                                                        
                                                        fileInput(inputId = "file", label = "Upload File", accept = c(".csv",".xlsx"))
                                                        
                                       ),#End of conditional panel
                                       
                                       uiOutput("sheetnames"),
                                       #conditionalPanel(condition = "(input.filetype=='excel')&(!is.null(input.file))",uiOutput("sheetnames")),
                                       #wellPanel(actionButton(inputId = 'reset',label = "Reset"))
                                       wellPanel(htmlOutput("totalrowstext")),
                                       wellPanel(tableOutput("blankcellscount"))
                                       
                                       
                                       
                                     ),#sidebarpanel
                                     
                                     mainPanel(
                                       # h3("Data Table"),
                                       withSpinner(dataTableOutput("contents"))
                                       
                                       
                                     )  
                                     
                                   )#SideBarLayout
                                   
                                   
                         )#FluidPage End
                         
                         
                ) #3rd Tab Panel End
        
              
)#navbarpage
                                
##########Server Function Start for Shiny###############################
server <-function(input,output,session){

#####Load Data Server Side##########
  
  rv<-reactiveValues(data=NULL,xlorcsv='csv',head=FALSE,sheet=NULL,features=NULL)
  
 
  
 # observeEvent(input$filetype,{if(input$filetype=='csv'){rv$xlorcsv<-'csv'} 
  #  else if(input$filetype=='excel'){rv$xlorcsv<-'excel'}})
  
  observeEvent(input$header, {rv$head<-input$header })
  
 
  observeEvent(input$sheetnames,rv$sheet<-input$sheetnames)
  
  
  
  observe({  
    
      if((!is.null(rv$xlorcsv))&(!is.null(input$file))){
      if(rv$xlorcsv=='csv'){
      rv$data<-read.csv(input$file$datapath, header = rv$head, na.strings = "")
      rv$features<-colnames(rv$data)
      
      rowdetail<-rowdetail(reactive({rv$data}))
      output$totalrowstext<-renderText(rowdetail$totalrowstext)
      output$blankcellscount<- renderTable({rowdetail$blankrowsdf},digits = 0,striped = TRUE)
      }
      # else if (rv$xlorcsv=='excel'){ 
      #   if(is.null(rv$sheet)){rv$data<-read_excel(input$file$datapath,sheet =excel_sheets(input$file$datapath)[1], col_names = rv$head)}
      #   else {rv$data<-read_excel(input$file$datapath,sheet =rv$sheet, col_names = rv$head)
      #   rv$features<-colnames(rv$data)}}
      
    }
    })
  
  # output$sheetnames<-renderUI({
  #   if((is.null(rv$xlorcsv))|(is.null(input$file))){return(NULL)}
  #   if((rv$xlorcsv=='excel')&(!is.null(input$file))){selectInput("sheetnames","Select sheet to load",choices = excel_sheets(path = input$file$datapath))}
  # })
  
  
  output$contents<-renderDataTable({
    
    dt<-datatable(rv$data,
    options = list(paging=TRUE,pageLength=100,
                   autoWidth = TRUE,
                   columnDefs = list(list(width = '50px', targets = "_all"))
                   
                   )#List End
                  )#End DataTable
    
    
    })#End renderDataTable
                                   
                                  
  
  
######Getting selectInputs updated#############
  
  observe({if((is.null(input$file))){return(NULL)}
    else if(!is.null(input$file)){
      sortedx<-sort(rv$features)
      
      
      if (input$plottype=='density'){
        updateSelectInput(session, "densityx",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
        
        
        
        updateSelectInput(session, "densityx1",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
        
        updateSelectInput(session, "densityx2",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
        
        
      } #density plot if
      
      if (input$plottype=='tukey'){
        updateSelectInput(session, "tukeyx1",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
        
        
        updateSelectInput(session, "tukeyx2",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
        
        updateSelectInput(session, "tukeyy",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
      } #tukey plot if
      
      if(input$plottype=='bar'){
        updateSelectInput(session, "barx",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
        
        updateSelectInput(session, "bary",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
        
        updateSelectInput(session, "barfiltervariable",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
        
        
        
      }#bar plot if
      
      if(input$plottype=='scatter'){
        updateSelectInput(session, "scatterx",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
        
        updateSelectInput(session, "scattery",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
     
        
        
        updateSelectInput(session, "scatterc",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
        
        updateSelectInput(session, "scatters",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
        
        updateSelectInput(session, "scatterfiltervariable",
                          
                          choices = sortedx,
                          selected = tail(sortedx, 1))
        
         }#scatter plot if
      
      #Spatial Stats Tab Column update
      
      updateSelectInput(session, "left",
                        
                        choices = sortedx,
                        selected = tail(sortedx, 1))
      
      updateSelectInput(session, "bottom",
                        
                        choices = sortedx,
                        selected = tail(sortedx, 1))
      
      updateSelectInput(session, "spatvariable",
                        
                        choices = sortedx,
                        selected = tail(sortedx, 1))
      
      updateSelectInput(session, "feature",
                        
                        choices = sortedx,
                        selected = tail(sortedx, 1))

      
    }#else if
  })#observe
  
  
  #Barplot Filter Variable
  
  
  observeEvent(input$barfiltervariable,{ 
     if (is.null(input$barfiltervariable)){filtervaluesbar<-NULL}
     else {filtervaluesbar<-sort(unique(rv$data[,input$barfiltervariable]))}
     
     
     updateSelectInput(session, "barfiltervalue",
                       choices = filtervaluesbar ,
                       selected = tail(filtervaluesbar, 1))
    
    })#input$barfiltervariable
  
  observeEvent(input$scatterfiltervariable,{ 
    if (is.null(input$scatterfiltervariable)){filtervaluesscatter<-NULL}
    else {filtervaluesscatter<-sort(unique(rv$data[,input$scatterfiltervariable]))}
    
    
    updateSelectInput(session, "scatterfiltervalue",
                      choices = filtervaluesscatter ,
                      selected = tail(filtervaluesscatter, 1))
    
  })#input$scatterfiltervariable
  
  
  
  
  
  observeEvent(input$spatvariable,{
    if (is.null(input$spatvariable)){spatvalues<-NULL}
    else {spatvalues<-sort(unique(rv$data[,input$spatvariable]))}
    
    
    updateSelectInput(session, "spatvalue",
                      choices = spatvalues ,
                      selected = tail(spatvalues, 1))
    
  })#input$spatvariable
  
  
  
#########Statistical Tab Server Side Start#################
  
  
  
  
  observeEvent(input$plotclick, {
    
    tryCatch({
    
    
#####Histogram Plot###########    
   
  if (input$plottype=="density"){
    
  switch(input$dennum, 
         "Single"={
  xvar<-reactive({rv$data[,input$densityx]})
  dfunc<-densityserver1(xvar,reactive({rv$data}),reactive({input$varstabh}),reactive({input$densityx}),reactive({input$plottitle}))  
  output$pvalue<-renderText({dfunc$pvaluetext})
  p<-dfunc$p }, #Single End
  
  "Comparision"={
    xvar1<-reactive({rv$data[,input$densityx1]})
    xvar2<-reactive({rv$data[,input$densityx2]})
    dfunc<-densityserver2(xvar1,xvar2,reactive({rv$data}),reactive({input$varstabh}),reactive({input$densityx1}),reactive({input$densityx2}),reactive({input$plottitle}))  
    output$pvalue<-renderText({dfunc$pvaluetext})
    p<-dfunc$p
  }#End Comparision
  
  )#End Switch
 # output$text1<-renderTable({dfunc$hist_table})
  
  }#End if Density Plot
    
    
#####Tukey Plot#################    
 
    if (input$plottype=='tukey'){
      
      output$pvalue<-NULL
      tfunc<-tukeyserver(reactive({rv$data}),reactive({input$tukeyy}),reactive({input$tukeycatvar}),
                         reactive({input$tukeyx1}),reactive({input$tukeyx2}),reactive({input$varstabt}),
                         reactive({input$plottitle}))
      
      output$text1<-renderTable({round_df(tfunc$t,digits=0)},digits = 0)
      par(mar = c(tfunc$intlength, tfunc$intlength, 1, 1),oma = c(0, 0, 0, 0), pty = "s")
      p<-tfunc$p
         
      }#Tukey End
      
######Bar Plot################
    
    if (input$plottype=='bar'){
      
      output$pvalue<-NULL
      
      bfunc<-barserver(reactive({rv$data}),reactive({input$bary}),reactive({input$barx}),
                       reactive({input$barfiltercheck}),reactive({input$barfiltervariable}),reactive({input$barfiltervalue}),
                       reactive({input$varstabb}),reactive({input$plottitle}))
      
      
      output$text1<-renderTable({bfunc$fit.em.tb})
      p<-bfunc$p
     
    }#Barplot End
    
#########Scatter Plot##################
      
      if (input$plottype=='scatter'){
        output$pvalue<-NULL
        
        sfunc<-scatterserver(reactive({rv$data}),reactive({input$scatterx}),reactive({input$scattery}),
                             reactive({input$scatterc}),reactive({input$scatters}),
                             reactive({input$scattercolorcheck}),reactive({input$scattersizecheck}),
                             reactive({input$varstabsx}),reactive({input$varstabsy}),
                             reactive({input$scatterfiltercheck}),reactive({input$scatterfiltervariable}),reactive({input$scatterfiltervalue}),
                             reactive({input$plottitle}))
                                                                                     
        p<-sfunc$p
        
      }
   
    
#####Common code for all statistical tab plots#########
    # p<-p+ labs(x = input$xtitle, y = input$ytitle)+ggtitle(input$plottitle)+
    #   theme(
    #     text = element_text(size = 20, color = "black", face="bold"),
    #     axis.text.x = element_text(size = 10, color = "black", face = "bold"),
    #     axis.text.y = element_text(size = 10, color = "black", face = "bold"),
    #     axis.title = element_text(size = 25, color = "black", face = "bold"),
    #     plot.title=element_text(size=30,color="black", face="bold",hjust = 0.5)
    #   )#Theme
    # 
   
   
     # output$plot1<-renderPlotly({p})
    output$plot1<-renderPlotly({p})  
    
    },#TRyCatch
error=function(e){output$errortext<-renderText("Error! Reload the App again")},
finally=print("Error in inputs! Reload the app again!")
      )#TryCatch End

  })#observeEvent End Plotting the graphs
  
  
  #Download Plot for Statistical Tab
  output$downloadPlotDesc <- downloadHandler(
    filename = function() { paste(input$plottype, ".png", sep="") },
    content = function(file) {
      ggsave(filename = file,device = "png",limitsize = FALSE)
    }
  )
  
######Spatial Tab Server Side Start###############
 
   observeEvent(input$spatplotclick,{
     if((is.null(input$pic))){return(NULL)}
     else if(!is.null(input$pic)){
      
       # Plotting the outer contour
       output$plot2<-renderPlot({
         
         req(input$pic)
         inp<-input$pic
         featurevariable<-as.character(input$feature)
         #output$text5<-renderText({inp$datapath})
         #imgpath<-"Stomach03.png"
         #img<-readPNG(imgpath)
         img<-readPNG(inp$datapath)
         img1<-img[,,1]
         stom.win.mask <- matrix(ncol=ncol(img1), nrow=nrow(img1), data=as.logical(img1), byrow=F)
         stom.owin <- owin(mask=stom.win.mask)
         stom.poly <- as.polygonal(stom.owin)
         #plot(stom.poly)
         
         spat_pp<-spat_point_pattern(reactive({rv$data}),reactive({input$left}),reactive({input$bottom}),
                                     reactive({input$feature}),reactive({input$spatfiltercheck}),reactive({input$spatvariable}),
                                     reactive({input$spatvalue}),reactive({stom.poly}),reactive({input$spatplotlevel}))
         
         output$blankvalues<-renderText({spat_pp$output_blankvalues_text})
         spat.test.cut<-spat_pp$spat.test.cut
        
        #Plotting the spatial graph
         
         switch(input$spatgraphtype,
                "Black & White Point Pattern"={plot(spat.test.cut)},
                "Colored Point Pattern"={
                  if(input$spatplotlevel=="Two"){
                    par(mar = c(0, 0, 3, 0),oma = c(0, 0, 0, 0), pty = "m")
                    plot(spat.test.cut, bg = c(4, 2),  pch = 21,  main = "", main.cex = 2, cex = 2 )     
                  }#End if
                  
                if(input$spatplotlevel=="Three"){
                  par(mar = c(0, 0, 3, 0),oma = c(0, 0, 0, 0), pty = "m")
                  plot(spat.test.cut,bg = c(4, 3, 2), pch = 21,main = "",main.cex = 2,cex = 2)
                }#End If
                  
                },#Colored Point PAttern End


                "Density Map"={
                  
                  if(input$spatplotlevel=="Two"){
                    par(mar = c(0, 0, 0, 0),oma = c(0, 0, 0, 1),pty = "m")
                    plot(density(split(spat.test.cut)), main = featurevariable,useRaster = T, log = F,col = colorRampPalette(c("blue", "red"))(120),cex.main = 2)          
                  }#End If
                  
                  if(input$spatplotlevel=="Three"){
                    par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 1), pty = "m")
                    plot(density(split(spat.test.cut)), main = featurevariable,useRaster = T,log = F,col = colorRampPalette(c("blue", "yellow", "red"))(120),cex.main = 2)
                  }#End If
                  
                }#End Density Map

                )#End Switch

         })#End RenderPlot
      
       } #else if end
   }) #End observeEvent Spatplotclick
  

}#Server End

shinyApp(server=server, ui=ui)

