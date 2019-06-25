#install.packages("dplyr")
#setwd('W:\Research Assisstant Work\Post Vacation')
library(shiny)
library(DT)
library(dplyr)
library(shinycssloaders)
library(readxl)
library(shinyjs)
library(ggplot2)
library(png)
library(spatstat)

require(tibble)
require(magrittr)
require(dplyr)
require(multcomp)
require(emmeans)
require(readxl)
library(httr)
require(ggfortify)

###Functions

##Function to round-off numeric columns in a dataframe
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  return(df)
}


#### Just a bunch of different ways of formatting the axis, choose one you like. It is all about aestethics!

library(scales)

scientific_10 <-
  function(x) {
    ifelse(x == 0, "0", parse(text = gsub(
      "[+]", "", gsub("e", " %*% 10^", scientific_format()(x))
    )))
  }

scientific_10_1 <-
  function(x) {
    ifelse(x == 0, "0", parse(text = gsub("e", " %*% 10^", scientific_format()(x))))
  }

scientific_10_2 <-
  function(x) {
    ifelse(x == 0, "0", (
      ifelse(abs(log10(abs(x))) <= 2, x, parse(text = gsub(
        "[+]", "", gsub("e", " %*% 10^", scientific_format()(x))
      )))))
  }

scientific_10_3 <-
  function(x) {
    ifelse(x==0, "0", 
           ifelse(abs(log10(abs(x))) <= 2, x, sfsmisc::pretty10exp(x, sub10 = T, drop.1 = F))
    )
  }

###############################################################################################




ui <-navbarPage(title="RShinyApp", windowTitle = "Data Visualization", theme = shinythemes::shinytheme("cerulean"),selected = "Load Data",
                
          
                 
               tabPanel(title="Statistical Analysis", #1st Tab Panel Start
                           fluidPage(
                                   sidebarLayout(
                                       sidebarPanel( 
                                         
                                         wellPanel(selectInput("plottype",label = "Type of Plot",choices = list(Density = "density", Barplot = "bar",Tukey="tukey"), selected = "bar"),
                                         radioButtons("varstab","Variance Stablisation",choices = c("None","Log","Square-Root"),selected = "None")),#WellPanel
                                         
                                         
                                         conditionalPanel(condition = "input.plottype=='density'", wellPanel(selectInput("densityx", "Select X variable",choices = NULL))),#conditionalPAnel
                                         
                                         conditionalPanel(condition = "input.plottype=='tukey'", wellPanel(radioButtons("tukeycatvar","Select the number of categorical variables",choices = c("One","Two"),selected = "One"),selectInput("tukeyy", "Select Y variable",choices = NULL),selectInput("tukeyx1", "Select interaction variable (X1)",choices = NULL),conditionalPanel(condition = "input.tukeycatvar=='Two'",selectInput("tukeyx2", "Select interaction variable (X2)",choices = NULL)))),#conditionalPAnel
                                         
                                         conditionalPanel(condition="input.plottype=='bar'",wellPanel(selectInput("bary", "Select Continuous Variable",choices=NULL),selectInput("barx", "Select Categorical Variable",choices=NULL),checkboxInput("barfiltercheck","Need a filter?",value = FALSE),
                                                                                                      conditionalPanel("input.barfiltercheck==1",selectInput("barfiltervariable", "Select Filter Variable",choices=NULL),selectInput("barfiltervalue", "Select Filter Value",choices=NULL)))),
                                         wellPanel(textInput("plottitle", "Plot Title :"),textInput("xtitle", "X Axis Title :"),textInput("ytitle", "Y Axis Title:"))
                                         
                                       ),#sideBarPanel
                                       mainPanel(wellPanel(actionButton("plotclick","Plot"), downloadButton('downloadPlotDesc','Download Plot')),
                                         
                                         plotOutput('plot1'),
                                         tableOutput('text1')
                                          #textOutput("text1")
                                       )#mainpanel
                                   )##Sidebarlayout
                           )#Fluidpage
                 ),# 1st tabpanel
                
                
                tabPanel(title="Spatial Analysis",  #2nd Tab Panel Start
                         fluidPage(useShinyjs(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       fileInput(inputId = "pic", label = "Upload Image File", accept = c(".png",".jpeg",".bmp")),
                                       wellPanel(selectInput("spatgraphtype","Type of Plot", choices = c("Black & White PP", "Colored PP","Density Map")),
                                       selectInput("left", "Select Left Co-ordinate Column",choices = NULL),
                                       selectInput("bottom", "Select Bottom Co-ordinate Column",choices = NULL)),#End Well Panel
                                       wellPanel(selectInput("feature", "Select Feature Numeric Variable",choices = NULL),checkboxInput("spatfiltercheck","Need a filter?",value = FALSE),
                                                 conditionalPanel("input.spatfiltercheck==1",selectInput("spatvariable", "Select Spatial Categorical Variable",choices=NULL),selectInput("spatvalue", "Select Spatial Value",choices=NULL)))#End well Panel
                                       
                                     ),#End Sidebarpanel
                                     
                                     mainPanel(wellPanel(actionButton("spatplotclick","Plot"), #downloadButton('downloadPlotSpat','Download Plot'),
                                       radioButtons(inputId = "spatplotlevel","Number of Levels", choices = c("Two","Three"), selected = "Three", inline=TRUE)), #End WellPanel
                                              
                                       withSpinner(plotOutput("plot2"))
                                              # textOutput("text2"),
                                      # tableOutput('tablespat')
                                               )#End Main Panel Output
                                   )#End SideBarLaypout
                                   )#End FluidPage Spatial Analysis Tabpanel
                         
                ), #2nd Tab Panel End 
                
                tabPanel(title="Load Data", #3rd Tab Panel Start,
                         
                         fluidPage(useShinyjs(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       wellPanel(checkboxGroupInput("filetype", "Choose filetype to upload:",
                                                                    choices = c("CSV"="csv", "Excel"="excel"))),
                                       
                                       conditionalPanel(condition = "(input.filetype=='csv')|(input.filetype=='excel')",
                                                        
                                                        wellPanel(checkboxInput(inputId = 'header', label = 'Header', value = FALSE)),
                                                        
                                                        fileInput(inputId = "file", label = "Upload File", accept = c(".csv",".xlsx"))
                                                        
                                       ),#End of conditional panel
                                       
                                       uiOutput("sheetnames"),
                                       #conditionalPanel(condition = "(input.filetype=='excel')&(!is.null(input.file))",uiOutput("sheetnames")),
                                       
                                       wellPanel(actionButton(inputId = 'reset',label = "Reset"))
                                       
                                       
                                       
                                       
                                     ),#sidebarpanel
                                     
                                     mainPanel(
                                       # h3("Data Table"),
                                       withSpinner(tableOutput("contents"))
                                       
                                       
                                     )  
                                     
                                   )#SideBarLayout
                                   
                                   
                         )#FluidPage End
                         
                         
                ) #3rd Tab Panel End
        
              
)#navbarpage
                                

server <-function(input,output,session){

  
  
  
  
  
  
  
    
  ###########Load Data Tab#######################
  
  rv<-reactiveValues(data=NULL,xlorcsv=NULL,head=FALSE,sheet=NULL,features=NULL)
  
  observeEvent(input$reset,{
    rv$xlorcsv=NULL
    rv$data=NULL
    shinyjs::reset('filetype')
    shinyjs::reset('file')
    shinyjs::reset('header')})
  
  
  observeEvent(input$filetype,{if(input$filetype=='csv'){rv$xlorcsv<-'csv'} 
    else if(input$filetype=='excel'){rv$xlorcsv<-'excel'}})
  
  observeEvent(input$header, rv$head<-input$header)
  
  observeEvent(input$sheetnames,rv$sheet<-input$sheetnames)
  
  
  
  observe(
    {if((!is.null(rv$xlorcsv))&(!is.null(input$file))){
      if(rv$xlorcsv=='csv'){rv$data<-read.csv(input$file$datapath, header = rv$head, na.strings = "")
      rv$features<-colnames(rv$data)      }
      else if (rv$xlorcsv=='excel'){ 
        if(is.null(rv$sheet)){rv$data<-read_excel(input$file$datapath,sheet =excel_sheets(input$file$datapath)[1], col_names = rv$head)}
        else {rv$data<-read_excel(input$file$datapath,sheet =rv$sheet, col_names = rv$head)
        rv$features<-colnames(rv$data)}}
      
    }
    })
  
  output$sheetnames<-renderUI({
    if((is.null(rv$xlorcsv))|(is.null(input$file))){return(NULL)}
    if((rv$xlorcsv=='excel')&(!is.null(input$file))){selectInput("sheetnames","Select sheet to load",choices = excel_sheets(path = input$file$datapath))}
  })
  
  
  output$contents<-renderTable({rv$data})
  
  
  ####################Descriptive Statistics Page######################################################
  
  #########DensityPlot Module##################################
  #Getting selectInputs variable data updated
  observe({if((is.null(input$file))){return(NULL)}
    else if(!is.null(input$file)){
      sortedx<-sort(rv$features)
      
      if (input$plottype=='density'){
    updateSelectInput(session, "densityx",
                      
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
  
  
  observeEvent(input$barfiltervariable,{ 
     if (is.null(input$barfiltervariable)){filtervaluesbar<-NULL}
     else {filtervaluesbar<-sort(unique(rv$data[,input$barfiltervariable]))}
     
     
     updateSelectInput(session, "barfiltervalue",
                       
                       choices = filtervaluesbar ,
                       selected = tail(filtervaluesbar, 1))
    
    })#input$barfiltervariable
  
  observeEvent(input$spatvariable,{
    if (is.null(input$spatvariable)){spatvalues<-NULL}
    else {spatvalues<-sort(unique(rv$data[,input$spatvariable]))}
    
    
    updateSelectInput(session, "spatvalue",
                      
                      choices = spatvalues ,
                      selected = tail(spatvalues, 1))
    
  })#input$spatvariable
  
  
  
  #Plotting The graphs
  
  
  
  
  observeEvent(input$plotclick, {
    
    
    #Plotting Density Plot
    if (input$plottype=="density"){
  xvar<-reactive({rv$data[,input$densityx]})
  denbin<-as.numeric(input$bins)
  {p <- ggplot(data=rv$data, aes(x=xvar())) + geom_histogram(aes(y =..density..,col="black",alpha=1))+ geom_density(col=2, size=2, fill=2, alpha=.2)+scale_y_continuous(labels = scientific_10_2)}
  
  if (input$varstab=="Log"){p<-p+scale_x_log10(#paste("",x_lab, sep=""),
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)))}
  if (input$varstab=="Square-Root"){p<-p+scale_x_sqrt(#paste("",x_lab, sep=""),
    breaks = trans_breaks("sqrt", function(x) x^2),
    labels = trans_format("sqrt", math_format(.x^2)))}
  
  if (input$varstab=="None"){p<-p+scale_x_continuous(#paste("\n",input$xtitle, sep=""), 
    labels = scientific_10)}
  
  p<-p+scale_y_continuous(labels = scientific_10_1)
  p<-p+theme(legend.position = "none")
  output$text1<-renderTable({xvar()})
  
  }#End if Density Plot
    
    
    
  #Plotting tukey plot
    if (input$plottype=='tukey'){
      
      df<-as.data.frame(rv$data)
       yvar<-input$tukeyy
       
       
       
       if (input$tukeycatvar=="One"){ 
      xvar1<-as.character(input$tukeyx1)
       df$int<-df[,xvar1]  
       }#End input$tukeycatvar=="One"
         
        
       if (input$tukeycatvar=="Two"){
         xvar1<-reactive({rv$data[,input$tukeyx1]})   
       xvar2<-reactive({rv$data[,input$tukeyx2]})
      df$int<-with(df,interaction(xvar1(),xvar2()))
       }#End tukeycatvar=="Two"
      
      
      contrasts(df$int)<-contr.poly   #some error here
      intlength<-length(unique(df$int))
      intvalues<-unique(df$int)
      
        ##### for some feature the transformation should be "sqrt"
      if(input$varstab=='Log'){ 
        fit <-    lm(log(get(yvar)) ~ int - 1,data = df) 
        fit <-    update(ref_grid(fit), tran = "log")}
      
      else if (input$varstab=='Square-Root'){ fit <-    lm(sqrt(get(yvar)) ~ int - 1,data = df) 
        fit <-    update(ref_grid(fit), tran = "sqrt")}
      
      else if (input$varstab=='None'){fit <-    lm((get(yvar)) ~ int - 1,data = df)
        fit <-    update(ref_grid(fit))}
      
      fit.em <- emmeans(fit, c("int"))
      t<-as.data.frame(summary(fit.em, type = "response"))
      #colnames(t)[1]<-"Categorical_Variable"
      
      output$text1<-renderTable({round_df(t,digits=0)},digits = 0)
      
      par(mar = c(intlength, intlength, 1, 1),
          oma = c(0, 0, 0, 0),
          pty = "s")
      
      p <- plot(fit.em, comparisons = TRUE, type = "response") +
                          scale_y_discrete(labels = t$int)
      
      
      if (input$varstab=="Log"){p<-p+scale_x_log10(#paste("",x_lab, sep=""),
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x)))}
      if (input$varstab=="Square-Root"){p<-p+scale_x_sqrt(#paste("",x_lab, sep=""),
        breaks = trans_breaks("sqrt", function(x) x^2),
        labels = trans_format("sqrt", math_format(.x^2)))}
      
      if (input$varstab=="None"){p<-p+scale_x_continuous(#paste("\n",input$xtitle, sep=""), 
        labels = scientific_10)}
      
      
      
          }#Tukey End
    
    if (input$plottype=='bar'){
      
      dispformat<-reactive({input$axisdispformat})
      filtervariable<-as.character(input$barfiltervariable)
      filtervalue<-as.character(input$barfiltervalue)
      xvar<-input$barx
      yvar<-input$bary
      df<-as.data.frame(rv$data)
      
      
      
      if (input$barfiltercheck==1)
        #{df<- df[df[,filtervariable]==filtervalue,]} Old expression that was being used (Commented)
      {df<-df[df[filtervariable]==filtervalue,]}
      #df_spat<- df_spat[df_spat[spatialvariable]==spatialvalue,] For reference from Spatial (Verified and Tested)
      
      
      predictors = paste(input$barx,collapse="+")
      
      
      if(input$varstab=='Log'){ 
        fml = as.formula(sprintf('%s ~ %s', paste0("log(",input$bary,")"), predictors))
        fit = lm(fml, data=df)
        fit <-    update(ref_grid(fit), tran = "log")
              }
      else if (input$varstab=='Square-Root'){
        fml = as.formula(sprintf('%s ~ %s', paste0("sqrt(",input$bary,")"), predictors))
        fit = lm(fml, data=df)
        fit <-    update(ref_grid(fit), tran = "sqrt")
        
      }
      else if (input$varstab=='None'){
        fml = as.formula(sprintf('%s ~ %s',input$bary, predictors))
        fit = lm(fml, data=df)
        fit <-    update(ref_grid(fit),tran = "NULL")
        }
      
                                       
      fit.em<-fit <- emmeans(fit,input$barx)
      fit.em.sum <- summary(fit.em, type = "response")
      fit.em.tb <- data.frame(Xvariable=fit.em.sum[,1], means=fit.em.sum$response, lower.CL=fit.em.sum$lower.CL,upper.CL=fit.em.sum$upper.CL)
      
      
      output$text1<-renderTable({fit.em.sum})
      
      p<-ggplot(data = fit.em.tb,aes(
        x = Xvariable,
        y = means,
        fill = as.factor(Xvariable)
      ))+
        geom_bar(stat = "identity",
                 alpha = 0.7,
                 width = 0.7) +
        scale_fill_brewer(guide = FALSE, palette = "Set1") +
        geom_errorbar(
          aes(x = Xvariable, ymin = lower.CL * 1, ymax = upper.CL * 1),
          width = 0.2,
          colour = "darkblue",
          alpha = 0.9,
          size = 2
        )+ scale_y_continuous(labels = scientific_10_1)
      
      #baryaxis","Y-Axis Display Format",choices = c("Scientific","Decimal")
      #if(input$baryaxis=="Scientific"){p<-p+scale_y_continuous(labels = scientific_10_1)}
      #if(input$baryaxis=="Decimal"){p<-p+scale_y_continuous(labels = scientific_10)} 
      
      
      
    }#Barplot End
    
   
    
#Common code for plots
    p<-p+ labs(x = input$xtitle, y = input$ytitle)+ggtitle(input$plottitle)+
      theme(
        text = element_text(size = 20, color = "black", face="bold"),
        axis.text.x = element_text(size = 10, color = "black", face = "bold"),
        axis.text.y = element_text(size = 10, color = "black", face = "bold"),
        axis.title = element_text(size = 25, color = "black", face = "bold"),
        plot.title=element_text(size=30,color="black", face="bold",family="courier",hjust = 0.5)
      )#Theme
    
   
      output$plot1<-renderPlot({p})
    #
  
  })#observeEvent End Plotting the graphs
  
#Spatial Analysis Tab Starts
 
   observeEvent(input$spatplotclick,{
     if((is.null(input$pic))){return(NULL)}
     else if(!is.null(input$pic)){
      
       # Plotting the outer contour
       output$plot2<-renderPlot({
         req(input$pic)
         inp<-input$pic
         img<-readPNG(inp$datapath)
         img1<-img[,,1]
         stom.win.mask <- matrix(ncol=ncol(img1), nrow=nrow(img1), data=as.logical(img1), byrow=F)
         stom.owin <- owin(mask=stom.win.mask)
         stom.poly <- as.polygonal(stom.owin)
         #plot(stom.poly)
         
         # #Creating Point Patterns and filtering for a specific variable
         left_coord<-reactive({rv$data[,input$left]}) #X-coordinate
         bottom_coord<-reactive({rv$data[,input$bottom]}) #Y-Coordinate
         spatial.data.xy <- data.frame(X=round(left_coord()*930.05/100+25,0), Y=round(bottom_coord()*700.05/100+82,0)) #Generating X-Y pairs
         featurevariable<-as.character(input$feature) #Feature to study
         
         df_spat<-as.data.frame(rv$data) # Original Dataframe
         
         
         if(input$spatfiltercheck==1){
         spatialvariable<-as.character(input$spatvariable) #Filtervariable
         spatialvalue<-as.character(input$spatvalue) #Filter Value
         df_spat_filtered<- df_spat[df_spat[spatialvariable]==spatialvalue,] #Subsetting the dataframe based on filtervalues
         sp.xy.subset <- subset(spatial.data.xy, df_spat[,spatialvariable]==spatialvalue) #Subsetting the X-Y coordinates for values of filtered variable
         }
         else
         {
           df_spat_filtered<- df_spat #No subsetting required
           sp.xy.subset <- spatial.data.xy# No Subsetting the X-Y coordinates required
        }
         
         

         

         output$text2<-renderText({dim(df_spat_filtered)})
         output$tablespat<-renderTable({df_spat_filtered})



          spat.test <- ppp(x=sp.xy.subset[,1], y=sp.xy.subset[,2], marks=log(df_spat_filtered[,featurevariable]), window=stom.poly, checkdup=F) #Generating point pattern
          #plot(spat.test)


          if(input$spatplotlevel=="Two"){

            breaks.hist <-quantile(log(df_spat[,featurevariable]), probs = c(0, 0.5, 1))   #### generate tertiles
            spat.test.cut <- cut.ppp(spat.test, breaks = (breaks.hist), include.lowest = T)
            levels(spat.test.cut$marks) <- c("low", "high")

            if (input$spatgraphtype=="Black & White PP") {plot(spat.test.cut)}

            else if(input$spatgraphtype=="Colored PP"){

              par(mar = c(0, 0, 3, 0),
                  oma = c(0, 0, 0, 0),
                  pty = "m")
              plot(
                spat.test.cut,
                bg = c(4, 2),
                pch = 21,
                main = "",
                main.cex = 2,
                cex = 2
              )
            } # End Else if

            else if(input$spatgraphtype=="Density Map") {

              #Plotting Density Plots
              par(mar = c(0, 0, 0, 0),
                  oma = c(0, 0, 0, 1),
                  pty = "m")
              plot(density(split(spat.test.cut)),
                   main = featurevariable,
                   useRaster = T,
                   log = F,
                   col = colorRampPalette(c("blue", "red"))(120),
                   cex.main = 2)
            } # End Else If





          }#End if spatplotlevel==Two


          else if(input$spatplotlevel=="Three"){
            breaks.hist <-quantile(log(df_spat[,featurevariable]), probs = c(0, 0.33, 0.66, 1))   #### generate tertiles
            spat.test.cut <- cut.ppp(spat.test, breaks = (breaks.hist), include.lowest = T)
            levels(spat.test.cut$marks) <- c("low", "medium", "high")


            #selectInput("spatgraphtype","Type of Plot", choices = c("Black & White PP", "Colored PP","Density Map")),
            if (input$spatgraphtype=="Black & White PP") {plot(spat.test.cut)}

            else if(input$spatgraphtype=="Colored PP"){

              par(mar = c(0, 0, 3, 0),
                  oma = c(0, 0, 0, 0),
                  pty = "m")
              plot(
                spat.test.cut,
                bg = c(4, 3, 2),
                pch = 21,
                main = "",
                main.cex = 2,
                cex = 2
              )
            } # End Else if

            else if(input$spatgraphtype=="Density Map") {

              #Plotting Density Plots
              par(mar = c(0, 0, 0, 0),
                  oma = c(0, 0, 0, 1),
                  pty = "m")
              plot(density(split(spat.test.cut)),
                   main = featurevariable,
                   useRaster = T,
                   log = F,
                   col = colorRampPalette(c("blue", "yellow", "red"))(120),
                   cex.main = 2)
            } # End Else If

          }#End else if spatplotlevel==Three


       })#End RenderPlot
      
      
      
       } #else if end
   }) #End observeEvent Spatplotclick
   
   output$downloadPlotDesc <- downloadHandler(
     filename = function() { paste(input$plottype, ".png", sep="") },
     content = function(file) {
       ggsave(filename = file,device = "png",limitsize = FALSE)
     }
   )

   
   
  
}#Server End

                                        
                    

shinyApp(server=server, ui=ui)

