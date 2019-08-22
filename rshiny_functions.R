#####Generic Aesthetic Functions###############
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



get_hist_table <- function(p) {
  d <- ggplot_build(p)$data[[1]]
  data.frame(X = d$x, Xmin = d$xmin, Xmax = d$xmax, Density_Value = d$y,Observations_Count=d$count)
}
#############UI FUnctions##################################################################################




densityUI<-function(){
  
  condition<-"input.plottype=='density'"
  src<-"histogram_img.png"
  xlabel<-"Select parameter for analysis"
  label1<-"Select first parameter for analysis"
  label2<-"Select second parameter for analysis"
  choicecond1<-"input.dennum=='Single'"
  choicecond2<-"input.dennum=='Comparision'"
  densityUIvalues<-list("condition"=condition,"src"=src,"label"=xlabel,"label1"=label1,"label2"=label2,"choicecond1"=choicecond1,"choicecond2"=choicecond2)
  return(densityUIvalues)
}

tukeyUI<-function(){
  
  condition<-"input.plottype=='tukey'"
  src<-'tukey_img.png'
  ylabel<-"Select Continuous variable"
  choicelabel<-"Select the number of categorical variables"
  x1label<-"Select categorical variable (X1)"
  tukeychoicecond<-"input.tukeycatvar=='Two (Interaction)'"
  x2label<-"Select categorical variable (X2)"
  tukeyUIvalues<-list("condition"=condition,"src"=src,"ylabel"=ylabel,"choicelabel"=choicelabel,"x1label"=x1label,"tukeychoicecond"=tukeychoicecond,"x2label"=x2label)
  
  return(tukeyUIvalues)
}



barUI<-function(){
  
  condition<-"input.plottype=='bar'"
  src<-'barplot_img.png'
  ylabel<-"Select Continuous Variable (Parameter you want to analyze)"
  xlabel<-"Select Category for comparision of the Parameter"
 filterlabel<-"Need a filter?<br/> (Eg. Circ vs Long layers)"
 filtercond<-"input.barfiltercheck==1"
 filterlabel_var<-"Select Filter Variable"
 filterlabel_val<-"Select Filter Value"
 
 barUIvalues<-list("condition"=condition,"src"=src,"ylabel"=ylabel,"xlabel"=xlabel,"filterlabel"=filterlabel,"filtercond"=filtercond,"filterlabel_var"=filterlabel_var,"filterlabel_val"=filterlabel_val)
  
  return(barUIvalues)
}

scatterUI<-function(){
  condition<-"input.plottype=='scatter'"
  src<-'scatter_img.png'
  ylabel<-"Select Continuous Variable on y-axis"
  xlabel<-"Select Continuous Variable on x-axis"
  colorcond<-"input.scattercolorcheck==1"
  colorcheck<-"Require to color data points based on a specific variable? "
  colorlabel<-"Select Variable to color the data points"
  sizecheck<-"Require to size data points based on a specific continuous variable? "
  sizecond<-"input.scattersizecheck==1"
  sizelabel<-"Select Continuous Variable to determine the size of data points"
  filterlabel<-"Need a filter?<br/> (Eg. Circ vs Long layers)"
  filtercond<-"input.scatterfiltercheck==1"
  filterlabel_var<-"Select Filter Variable"
  filterlabel_val<-"Select Filter Value"
  
  
  scatterUIvalues<-list("condition"=condition,"src"=src,"ylabel"=ylabel,"xlabel"=xlabel,
                        "colorcond"=colorcond,"colorcheck"=colorcheck,"colorlabel"=colorlabel,
                        "sizecheck"=sizecheck,"sizecond"=sizecond,"sizelabel"=sizelabel,
                        "filterlabel"=filterlabel,"filtercond"=filtercond,"filterlabel_var"=filterlabel_var,
                        "filterlabel_val"=filterlabel_val)
  return(scatterUIvalues)
}


spatUI_sb<-function(){
  
  contourlabel<-"Upload contour outline"
  spatplottype<-"Visualization Plot Type"
  bwcond<-"input.spatgraphtype=='Black & White Point Pattern'"
  colorcond<-"input.spatgraphtype=='Colored Point Pattern'"
  densitycond<-"input.spatgraphtype=='Density Map'"
  bwsrc<-'bwpp_img.png'
  ccsrc<-'coloredpp_img.png'
  dsrc<-'densitypp_img.png'
  xcoord<-"Select X Co-ordinate Column"
  ycoord<-"Select Y Co-ordinate Column"
  numvar<-"Select Numeric Variable to plot"
  filterlabel<-"Need a filter?"
  spatfiltercond<-"input.spatfiltercheck==1"
  spatcatvar<-"Select Categorical Variable"
  spatcatval<-"Select Value for Categorical Variable"
  
  spatUIvalues<-list("contourlabel"=contourlabel,"spatplottype"=spatplottype,"bwcond"=bwcond,"colorcond"=colorcond,
                     "densitycond"=densitycond,"bwsrc"=bwsrc,"ccsrc"=ccsrc,"dsrc"=dsrc,"xcoord"=xcoord,"ycoord"=ycoord,
                     "numvar"=numvar,"filterlabel"=filterlabel,"spatfiltercond"=spatfiltercond,"spatcatvar"=spatcatvar,"spatcatval"=spatcatval)
  
  return(spatUIvalues)
}


spatUI_mp<-function(){
  title<-"Spatial Distribution Segments of the data"
  radiobuttonslabel<-"Number of Levels"
  desc<-"<b>Two: Spatial Distribution of Lower and Upper halves of the data<br/>Three: Spatial Distribution of Lower, Middle and Upper terciles of the data<br/><br/>To download the plot, right click and choose 'Save Image As'</b><br/><br/>"
  
  spatUIvalues<-list("title"=title,"radiobuttonslabel"=radiobuttonslabel,"desc"=desc)
  
  return(spatUIvalues)
}

loaddataUI<-function(){
  
  uploadlabel<-"Choose filetype to upload :"
  loadcond<-"(input.filetype=='csv')|(input.filetype=='excel')"
  headerlabel<-'Columns include Header?'
  special_char_label<-" No special/Greek characters in Header </b>(Space,!,@,#,$,%,^,&,*) "
  
  loaddataUIvalues<-list("uploadlabel"=uploadlabel,"loadcond"=loadcond,"headerlabel"=headerlabel,"special_char_label"=special_char_label)
}


#foo <- 12
#bar <- c("a", "b", "e")
#newList <- list("integer" = foo, "names" = bar)

###Statistical Tab Server side FUnctions#####


rowdetail<-function(data){
  totalrows<-round(nrow(data()),0)
  totalrowstext<-paste("<b>Total Number of Rows: ",totalrows,"</b>")
  
  blankrowsdf<-data.frame(colSums(is.na(data())))
  blankrowsdf<-tibble::rownames_to_column(blankrowsdf,"Column_Names")
  colnames(blankrowsdf)<-c("Column_Names","Missing_Values")
  #blankrowsdf$Missing_Values<-sprintf('%1.0f',blankrowsdf$Missing_Values)
  
  
  rowdetailvalues<-list("totalrowstext"=totalrowstext,"blankrowsdf"=blankrowsdf)
  return(rowdetailvalues)
  
}

#Plot Title Font
# f <- list(
#   family = "Arial, sans-serif",
#   size = 20,
#   color = "#7f7f7f")


#For Single Histogram option 
densityserver1<-function(xvar,data,varstab,xvariable,plottitle){
 
  
  if (varstab()=="Log"){
    
    norm.test<-(ad.test(log10(xvar()))$p.value)
    p <- plot_ly(data = data(),x = ~log10(xvar()),alpha = 0.6,
                 type = "histogram",
                 histnorm = "count") %>% layout(title=plottitle(),#font=f,
                                                  xaxis=list(zeroline = FALSE,tickprefix="10^",title=as.character(xvariable())),
                                                yaxis=list(zeroline = FALSE,title="Count"))

    
    
    # p<-plot_ly(alpha = 0.6) %>% add_histogram(x=~xvar())%>% layout(title=plottitle(),#font=f,
    #                                                                xaxis=list(zeroline = FALSE,tickprefix="10^",title=as.character(xvariable())),
    #                                                                yaxis=list(zeroline = FALSE,title="Count"))
    # 
                                                
  }
    
  if (varstab()=="Square-Root"){
    
    norm.test<-(ad.test(sqrt(xvar()))$p.value)
    p <- plot_ly(data = data(),x = ~sqrt(xvar()),alpha = 0.6,
                 type = "histogram",
                 histnorm = "count") %>% layout(title=plottitle(),
                   xaxis=list(zeroline = FALSE,ticksuffix="^2",title=as.character(xvariable())),
                                                yaxis=list(zeroline = FALSE,title="Count"))
                                               
  }
  
  if (varstab()=="None"){ 
    norm.test<-(ad.test(xvar())$p.value)
    p <- plot_ly(data = data(),x = ~xvar(),alpha = 0.6,
                                       type = "histogram",
                                       histnorm = "count") %>%
                                     layout(title=plottitle(),
                                       xaxis=list(zeroline = FALSE,title=as.character(xvariable())),
                                     yaxis=list(zeroline = FALSE,title="Count"))
  
  }
  
 p<-p%>% layout(yaxis=list(zeroline = FALSE,showexponent = "all", exponentformat = "power"))
 
 norm.test<-formatC(norm.test,format = "g",digits=2)
 pvaluetext<-paste0("Anderson-Darling Normality test P-value for the selected parameter: ",gsub("e"," X 10^",norm.test))
 #pvaluetext<-paste0("Anderson-Darling Normality test P-value for the selected parameter: ",gsub("e"," X 10^",norm.test))
 #pvaluetext<-paste0("P-value for Anderson Darling Normality Test for the selected Parameter is ",norm.test$p.value)
 #pvaluetext<-paste0("Anderson-Darling Normality test P-value for the selected parameter: ",sprintf('%1.2e',norm.test))
 #pvaluetext<-paste0("Anderson-Darling Normality test P-value for the selected parameter: ",scientific_10_3(norm.test()))
 

  
 # hist_table<-get_hist_table(p)
 hist_table<-'' 
 
 
 
  densityvalues<-list("pvaluetext"=pvaluetext,"p"=p,"hist_table"=hist_table)
  return(densityvalues)
  
}#End densityserver1

#For Comparision option (Histogram Overlay)
densityserver2<-function(xvar1,xvar2,data,varstab,xvariable1,xvariable2,plottitle){
  

  if (varstab()=="Log"){
    
    norm.test1<-(ad.test(log10(xvar1()))$p.value)
    norm.test2<-(ad.test(log10(xvar2()))$p.value)
    
    p<-plot_ly(alpha = 0.6,histnorm="probability") %>% 
      add_histogram(x=~log10(xvar1()),name=as.character(xvariable1()))%>% 
      add_histogram(x=~log10(xvar2()),name=as.character(xvariable2())) %>%
    layout(barmode = "overlay",title=plottitle(),#font=f,
           xaxis=list(zeroline = FALSE,tickprefix="10^",title="Values"),
           yaxis=list(zeroline = FALSE,title="Probability"))
          
  }
  
  if (varstab()=="Square-Root"){
    
    norm.test1<-(ad.test(sqrt(xvar1()))$p.value)
    norm.test2<-(ad.test(sqrt(xvar2()))$p.value)
    
    p<-plot_ly(alpha = 0.6,histnorm="probability") %>% 
      add_histogram(x=~sqrt(xvar1()),name=as.character(xvariable1()))%>% 
      add_histogram(x=~sqrt(xvar2()),name=as.character(xvariable2())) %>%
      layout(barmode = "overlay",title=plottitle(),#font=f,
             xaxis=list(zeroline = FALSE,ticksuffix="^2",title="Values"),
             yaxis=list(zeroline = FALSE,title="Probability"))
    
    
  }
  
  if (varstab()=="None"){
    
    norm.test1<-(ad.test(xvar1())$p.value)
    norm.test2<-(ad.test(xvar2())$p.value)
    
    p<-plot_ly(alpha = 0.6,histnorm="probability") %>%
      add_histogram(x=~xvar1(),name=as.character(xvariable1()))%>% 
      add_histogram(x=~xvar2(),name=as.character(xvariable2())) %>%
      layout(barmode = "overlay",title=plottitle(),#font=f,
             xaxis=list(zeroline = FALSE,title="Values"),
             yaxis=list(zeroline = FALSE,title="Probability"))
    
    
  }
  
 
  # hist_table<-get_hist_table(p)
  hist_table<-'' 
  
  norm.test1<-formatC(norm.test1,format = "g",digits=2)
  norm.test2<-formatC(norm.test2,format = "g",digits=2)
  
  pvaluetext<-paste0("Anderson-Darling Normality test P-values for the selected parameters: ",
                     "First Parameter: ",gsub("e"," X 10^",norm.test1),",  ",
                     "Second Parameter: ",gsub("e"," X 10^",norm.test2)
  )
  
  densityvalues<-list("pvaluetext"=pvaluetext,"p"=p,"hist_table"=hist_table)
  return(densityvalues)
  
  
  
}#End densityserver2







tukeyserver<-function(data,tukeyy,tukeycatvar,tukeyx1,tukeyx2,varstab,plottitle){
  
 
  df<-as.data.frame(data())
  yvar<-tukeyy()
  
  
  
  
  
  if (tukeycatvar()=="One"){ 
    xvar1<-as.character(tukeyx1())
    df$int<-df[,xvar1]  
  }#End input$tukeycatvar=="One"
  
  
  if (tukeycatvar()=="Two (Interaction)"){
    xvar1<-data()[,tukeyx1()]   
    xvar2<-data()[,tukeyx2()]
    df$int<-with(df,interaction(xvar1,xvar2))
  }#End tukeycatvar=="Two (Interaction)"
  
  
  contrasts(df$int)<-contr.poly   #some error here
  intlength<-length(unique(df$int))
  intvalues<-unique(df$int)
  
  ##### for some feature the transformation should be "sqrt"
  if(varstab()=='Log'){ 
    fit <-    lm(log(get(yvar)) ~ int - 1,data = df) 
    fit <-    update(ref_grid(fit), tran = "log")}
  
  else if (varstab()=='Square-Root'){ fit <-    lm(sqrt(get(yvar)) ~ int - 1,data = df) 
  fit <-    update(ref_grid(fit), tran = "sqrt")}
  
  else if (varstab()=='None'){fit <-    lm((get(yvar)) ~ int - 1,data = df)
  fit <-    update(ref_grid(fit))}
  
  fit.em <- emmeans(fit, c("int"))
  t<-as.data.frame(summary(fit.em, type = "response"))
  colnames(t)[1]<-"Categorical_Variable"
  print(fit.em)
  
  p <- plot(fit.em, comparisons = TRUE, type = "response",ylab = "Categorical Variable/s",xlab = as.character(yvar),main = plottitle())   
  # + scale_y_discrete(labels = t$int)
  

   tukeyservervalues<-list("t"=t,"p"=p,"intlength"=intlength)
  
  return(tukeyservervalues)
  
  }


barserver<-function(data,bary,barx,barfiltercheck,barfiltervariable,barfiltervalue,varstab,plottitle){
  
  filtervariable<-as.character(barfiltervariable())
  filtervalue<-as.character(barfiltervalue())
  #xvar<-barx()
  #yvar<-bary()
  df<-as.data.frame(data())
  
  if (barfiltercheck()==1)
    #{df<- df[df[,filtervariable]==filtervalue,]} Old expression that was being used (Commented)
  {df<-df[df[filtervariable]==filtervalue,]}
  #df_spat<- df_spat[df_spat[spatialvariable]==spatialvalue,] For reference from Spatial (Verified and Tested)
  
  
  predictors = paste(barx(),collapse="+")
  
  
  if(varstab()=='Log'){ 
    fml = as.formula(sprintf('%s ~ %s', paste0("log(",bary(),")"), predictors))
    fit = lm(fml, data=df)
    fit <-    update(ref_grid(fit), tran = "log")
  }
  else if (varstab()=='Square-Root'){
    fml = as.formula(sprintf('%s ~ %s', paste0("sqrt(",bary(),")"), predictors))
    fit = lm(fml, data=df)
    fit <-    update(ref_grid(fit), tran = "sqrt")
    
  }
  else if (varstab()=='None'){
    fml = as.formula(sprintf('%s ~ %s',bary(), predictors))
    fit = lm(fml, data=df)
    fit <-    update(ref_grid(fit),tran = "NULL")
  }
  
  
  fit.em<-fit <- emmeans(fit,barx())
  fit.em.sum <- summary(fit.em, type = "response")
  
  fit.em.tb <- data.frame(Xvariable=fit.em.sum[,1], means=fit.em.sum$response,SE=fit.em.sum$SE, lower.CL=fit.em.sum$lower.CL,upper.CL=fit.em.sum$upper.CL)
  
  # SE_log=log10(fit.em.tb$SE)*1.996
  
 # #Plotly Try with variance stablisation options (Not working)
 #  
  # if(varstab()=='Log'){
  #   p<-plot_ly(data=fit.em.tb,x=~Xvariable,y=~log10(means),type='bar',error_y=~list(array=SE_log,color = '#000000')) %>% layout(yaxis=list(tickprefix="10^",hoverformat = '.4f'))
  # 
  # }
 # 
 #  else if (varstab()=='Square-Root'){
 #    p<-plot_ly(data=fit.em.tb,x=~Xvariable,y=~sqrt(means),type='bar',error_y=~list(array=SE_sqrt,color = '#000000')) %>% layout(yaxis=list(ticksuffix="^2",hoverformat = '.4f'))}
 #     
 # 
 #  else if (varstab()=='None'){
 #    p<-plot_ly(data=fit.em.tb,x=~Xvariable,y=~means,type='bar',error_y=~list(array=SE*1.996,color = '#000000')) %>%     layout(yaxis=list(showexponent = "all", exponentformat = "power",hoverformat = '.4f'))}
 # 

 
  #ggplot code
  
  # p<-ggplot(data = fit.em.tb,aes(
  #   x = Xvariable,
  #   y = means,
  #   fill = as.factor(Xvariable)
  # ))+
  #   geom_bar(stat = "identity",
  #            alpha = 0.7,
  #            width = 0.7) +
  #   scale_fill_brewer(guide = FALSE, palette = "Set1") +
  #   geom_errorbar(
  #     aes(x = Xvariable, ymin = lower.CL * 1, ymax = upper.CL * 1),
  #     width = 0.2,
  #     colour = "darkblue",
  #     alpha = 0.9,
  #     size = 2
  #   )#+ scale_y_continuous(labels = scientific_10_1)
  # 
  # 
  # p<-ggplotly(p) %>% layout(yaxis=list(showexponent = "all", exponentformat = "power"))
  
  
  p<-plot_ly(data=fit.em.tb,x=~Xvariable,y=~means,type='bar',color=~Xvariable,error_y=~list(array=SE*1.996,color = '#000000')) %>%
    layout(title=plottitle(),
      yaxis=list(showexponent = "all", exponentformat = "power",hoverformat = '.4f',title=bary(),zeroline=FALSE),
           xaxis=list(title=barx(),zeroline=FALSE))

  barservervalues<-list("fit.em.tb"=fit.em.tb,"p"=p)
  
  return(barservervalues)
  
}


scatterserver<-function(data,scatterx,scattery,scatterc,scatters,
                        scattercolorcheck,scattersizecheck,varstabx,varstaby,
                        scatterfiltercheck,scatterfiltervariable,scatterfiltervalue,plottitle){
  
  #df<-as.data.frame(data())
  df<-as.data.frame(data())
  
  filtervariable<-as.character(scatterfiltervariable())
  filtervalue<-as.character(scatterfiltervalue())
  
  if (scatterfiltercheck()==1)
    #{df<- df[df[,filtervariable]==filtervalue,]} Old expression that was being used (Commented)
  {df<-df[df[filtervariable]==filtervalue,]}
  
  print(df)
  
  
  if (scattercolorcheck()==1 & scattersizecheck()==1){
        p<-scattercolorsize11(df,scatterx(),scattery(),scatterc(),scatters(),varstabx(),varstaby(),plottitle()) }
   
  else if (scattercolorcheck()==1 & scattersizecheck()==0){
    p<-scattercolorsize10(df,scatterx(),scattery(),scatterc(),varstabx(),varstaby(),plottitle()) }
   
  else if (scattercolorcheck()==0 & scattersizecheck()==1){
    p<-scattercolorsize01(df,scatterx(),scattery(),scatters(),varstabx(),varstaby(),plottitle()) }
   
  else if (scattercolorcheck()==0 & scattersizecheck()==0){
    p<-scattercolorsize00(df,scatterx(),scattery(),varstabx(),varstaby(),plottitle()) }
   
  scatterservervalues<-list("p"=p)
  return(scatterservervalues)
  
}#ScatterServer


scattercolorsize11<-function(df,scatterx,scattery,scatterc,scatters,varstabx,varstaby,plottitle)
{
#  "None","Log","Square-Root"
  
if (varstabx=="None"){
  
  switch(varstaby,
         
         "None"={p<-plot_ly(df,x=df[,scatterx],y=df[,scattery],
                            marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                            color = df[,scatterc],
                            size=df[,scatters],
                            type='scatter' ) %>%
        
           layout(title = plottitle,
                  yaxis = list(zeroline = FALSE,title=scattery),
                  xaxis = list(zeroline = FALSE,title=scatterx))
           
         }, #End "None" varstaby
         
         
      "Log"={p<-plot_ly(df,x=df[,scatterx],y=log10(df[,scattery]),
                        marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                        color = df[,scatterc],
                        size=df[,scatters],
                        type='scatter' ) %>%
        
        layout(title = plottitle,
               yaxis = list(zeroline = FALSE,tickprefix="10^",title=scattery),
               xaxis = list(zeroline = FALSE,title=scatterx))
        
        
      },#End "Log" varstaby
         
      "Square-Root"={p<-plot_ly(df,x=df[,scatterx],y=sqrt(df[,scattery]),
                                marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                                color = df[,scatterc],
                                size=df[,scatters],
                                type='scatter' ) %>%
        
        layout(title = plottitle,
               yaxis = list(zeroline = FALSE,ticksuffix="^2",title=scattery),
               xaxis = list(zeroline = FALSE,title=scatterx))
        
      } #End ""Square-Root" varstaby    
         
         
         
         )#End Switch
  
}#End if "None" x
  
 else if(varstabx=="Log") {
   
   switch(varstaby,
          
          "None"={p<-plot_ly(df,x=log10(df[,scatterx]),y=df[,scattery],
                             marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                             color = df[,scatterc],
                             size=df[,scatters],
                             type='scatter' ) %>%
            
            layout(title = plottitle,
                   yaxis = list(zeroline = FALSE,title=scattery),
                   xaxis = list(zeroline = FALSE,tickprefix="10^",title=scatterx))
          
          }, #End "None" varstaby
          
          
          "Log"={p<-plot_ly(df,x=log10(df[,scatterx]),y=log10(df[,scattery]),
                            marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                            color = df[,scatterc],
                            size=df[,scatters],
                            type='scatter' ) %>%
            
            layout(title = plottitle,
                   yaxis = list(zeroline = FALSE,tickprefix="10^",title=scattery),
                   xaxis = list(zeroline = FALSE,tickprefix="10^",title=scatterx))
          
          
          },#End "Log" varstaby
          
          "Square-Root"={p<-plot_ly(df,x=log10(df[,scatterx]),y=sqrt(df[,scattery]),
                                    marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                                    color = df[,scatterc],
                                    size=df[,scatters],
                                    type='scatter' ) %>%
            
            layout(title = plottitle,
                   yaxis = list(zeroline = FALSE,ticksuffix="^2",title=scattery),
                   xaxis = list(zeroline = FALSE,tickprefix="10^",title=scatterx))
          
          } #End ""Square-Root" varstaby    
          
          
          
   )#End Switch
   
 }#End if "Log" x
  
  
  
  else if(varstabx=="Square-Root"){
    
    switch(varstaby,
           
           "None"={p<-plot_ly(df,x=sqrt(df[,scatterx]),y=df[,scattery],
                              marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                              color = df[,scatterc],
                              size=df[,scatters],
                              type='scatter' ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,title=scattery),
                    xaxis = list(zeroline = FALSE,ticksuffix="^2",title=scatterx))
           
           }, #End "None" varstaby
           
           
           "Log"={p<-plot_ly(df,x=sqrt(df[,scatterx]),y=log10(df[,scattery]),
                             marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                             color = df[,scatterc],
                             size=df[,scatters],
                             type='scatter' ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,tickprefix="10^",title=scattery),
                    xaxis = list(zeroline = FALSE,ticksuffix="^2",title=scatterx))
           
           
           },#End "Log" varstaby
           
           "Square-Root"={p<-plot_ly(df,x=sqrt(df[,scatterx]),y=sqrt(df[,scattery]),
                                     marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                                     color = df[,scatterc],
                                     size=df[,scatters],
                                     type='scatter' ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,ticksuffix="^2",title=scattery),
                    xaxis = list(zeroline = FALSE,ticksuffix="^2",title=scatterx))
           
           } #End ""Square-Root" varstaby    
           
           
           
    )#End Switch
    
    
    
    
  }#End if "Square-Root" x
  

return(p)

}#scattercolorsize11




scattercolorsize10<-function(df,scatterx,scattery,scatterc,varstabx,varstaby,plottitle){
  
  
  
  
  
  if (varstabx=="None"){
    
    switch(varstaby,
           
           "None"={p<-plot_ly(df,x=df[,scatterx],y=df[,scattery],
                              marker = list(size = 10, line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                              color = df[,scatterc],
                              type='scatter') %>%
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,title=scattery),
                    xaxis = list(zeroline = FALSE,title=scatterx))
           
           }, #End "None" varstaby
           
           
           "Log"={p<-plot_ly(df,x=df[,scatterx],y=log(df[,scattery]),
                             marker = list(size = 10, line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                             color = df[,scatterc],
                             type='scatter') %>%
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,tickprefix="10^",title=scattery),
                    xaxis = list(zeroline = FALSE,title=scatterx))
           
           
           },#End "Log" varstaby
           
           "Square-Root"={p<-plot_ly(df,x=df[,scatterx],y=sqrt(df[,scattery]),
                                     marker = list(size = 10, line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                                     color = df[,scatterc],
                                     type='scatter') %>%
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,ticksuffix="^2",title=scattery),
                    xaxis = list(zeroline = FALSE,title=scatterx))
           
           } #End ""Square-Root" varstaby    
           
           
           
    )#End Switch
    
  }#End if "None" x
  
  else if(varstabx=="Log") {
    
    switch(varstaby,
           
           "None"={p<-plot_ly(df,x=log10(df[,scatterx]),y=df[,scattery],
                              marker = list(size = 10, line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                              color = df[,scatterc],
                              type='scatter') %>%
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,title=scattery),
                    xaxis = list(zeroline = FALSE,tickprefix="10^",title=scatterx))
           
           }, #End "None" varstaby
           
           
           "Log"={p<-plot_ly(df,x=log10(df[,scatterx]),y=log10(df[,scattery]),
                             marker = list(size = 10, line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                             color = df[,scatterc],
                             type='scatter') %>%
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,tickprefix="10^",title=scattery),
                    xaxis = list(zeroline = FALSE,tickprefix="10^",title=scatterx))
           
           
           },#End "Log" varstaby
           
           "Square-Root"={p<-plot_ly(df,x=log10(df[,scatterx]),y=sqrt(df[,scattery]),
                                     marker = list(size = 10, line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                                     color = df[,scatterc],
                                     type='scatter') %>%
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,ticksuffix="^2",title=scattery),
                    xaxis = list(zeroline = FALSE,tickprefix="10^",title=scatterx))
           } #End ""Square-Root" varstaby    
           
           
           
    )#End Switch
    
  }#End if "Log" x
  
  
  
  else if(varstabx=="Square-Root"){
    
    switch(varstaby,
           
           "None"={p<-plot_ly(df,x=sqrt(df[,scatterx]),y=df[,scattery],
                              marker = list(size = 10, line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                              color = df[,scatterc],
                              type='scatter') %>%
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,title=scattery),
                    xaxis = list(zeroline = FALSE,ticksuffix="^2",title=scatterx))
           
           }, #End "None" varstaby
           
           
           "Log"={p<-plot_ly(df,x=sqrt(df[,scatterx]),y=log10(df[,scattery]),
                             marker = list(size = 10, line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                             color = df[,scatterc],
                             type='scatter') %>%
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,tickprefix="10^",title=scattery),
                    xaxis = list(zeroline = FALSE,ticksuffix="^2",title=scatterx))
           
           
           },#End "Log" varstaby
           
           "Square-Root"={p<-plot_ly(df,x=sqrt(df[,scatterx]),y=sqrt(df[,scattery]),
                                     marker = list(size = 10, line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                                     color = df[,scatterc],
                                     type='scatter') %>%
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,ticksuffix="^2",title=scattery),
                    xaxis = list(zeroline = FALSE,ticksuffix="^2",title=scatterx))
           
           } #End ""Square-Root" varstaby    
           
           
           
    )#End Switch
    
    
    
    
  }#End if "Square-Root" x
  
  
  
  return(p)
  
  
}#scattercolorsize10



scattercolorsize01<-function(df,scatterx,scattery,scatters,varstabx,varstaby,plottitle)
{
 
  
  if (varstabx=="None"){
    
    switch(varstaby,
           
           "None"={ p<-plot_ly(df,x=df[,scatterx],y=df[,scattery],
                               marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2), color = 'rgba(255, 182, 193, .9)'),
                               size=df[,scatters],
                               type='scatter' ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,title=scattery),
                    xaxis = list(zeroline = FALSE,title=scatterx))
           
           
             
           }, #End "None" varstaby
           
           
           "Log"={ p<-plot_ly(df,x=df[,scatterx],y=log10(df[,scattery]),
                              marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2), color = 'rgba(255, 182, 193, .9)'),
                              size=df[,scatters],
                              type='scatter' ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,tickprefix="10^",title=scattery),
                    xaxis = list(zeroline = FALSE,title=scatterx))
           
           
             
             
           },#End "Log" varstaby
           
           "Square-Root"={ p<-plot_ly(df,x=df[,scatterx],y=sqrt(df[,scattery]),
                                      marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2), color = 'rgba(255, 182, 193, .9)'),
                                      size=df[,scatters],
                                      type='scatter' ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,ticksuffix="^2",title=scattery),
                    xaxis = list(zeroline = FALSE,title=scatterx))
           
           
             
           } #End ""Square-Root" varstaby    
           
           
           
    )#End Switch
    
  }#End if "None" x
  
  else if(varstabx=="Log") {
    
    switch(varstaby,
           
           "None"={ p<-plot_ly(df,x=log10(df[,scatterx]),y=df[,scattery],
                               marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2), color = 'rgba(255, 182, 193, .9)'),
                               size=df[,scatters],
                               type='scatter' ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,title=scattery),
                    xaxis = list(zeroline = FALSE,tickprefix="10^",title=scatterx))
           
           
             
           }, #End "None" varstaby
           
           
           "Log"={ p<-plot_ly(df,x=log10(df[,scatterx]),y=log10(df[,scattery]),
                              marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2), color = 'rgba(255, 182, 193, .9)'),
                              size=df[,scatters],
                              type='scatter' ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,tickprefix="10^",title=scattery),
                    xaxis = list(zeroline = FALSE,tickprefix="10^",title=scatterx))
           
           
             
             
           },#End "Log" varstaby
           
           "Square-Root"={ p<-plot_ly(df,x=log10(df[,scatterx]),y=sqrt(df[,scattery]),
                                      marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2), color = 'rgba(255, 182, 193, .9)'),
                                      size=df[,scatters],
                                      type='scatter' ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,ticksuffix="^2",title=scattery),
                    xaxis = list(zeroline = FALSE,tickprefix="10^",title=scatterx))
           
           
           } #End ""Square-Root" varstaby    
           
           
           
    )#End Switch
    
  }#End if "Log" x
  
  
  
  else if(varstabx=="Square-Root"){
    
    switch(varstaby,
           
           "None"={ p<-plot_ly(df,x=sqrt(df[,scatterx]),y=df[,scattery],
                               marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2), color = 'rgba(255, 182, 193, .9)'),
                               size=df[,scatters],
                               type='scatter' ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,title=scattery),
                    xaxis = list(zeroline = FALSE,ticksuffix="^2",title=scatterx))
           
           
             
           }, #End "None" varstaby
           
           
           "Log"={ p<-plot_ly(df,x=sqrt(df[,scatterx]),y=log10(df[,scattery]),
                              marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2), color = 'rgba(255, 182, 193, .9)'),
                              size=df[,scatters],
                              type='scatter' ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,tickprefix="10^",title=scattery),
                    xaxis = list(zeroline = FALSE,ticksuffix="^2",title=scatterx))
           
           
             
             
           },#End "Log" varstaby
           
           "Square-Root"={ p<-plot_ly(df,x=sqrt(df[,scatterx]),y=sqrt(df[,scattery]),
                                      marker=list(line = list(color = 'rgba(152, 0, 0, .8)',width=2), color = 'rgba(255, 182, 193, .9)'),
                                      size=df[,scatters],
                                      type='scatter' ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,ticksuffix="^2",title=scattery),
                    xaxis = list(zeroline = FALSE,ticksuffix="^2",title=scatterx))
           
           
             
           } #End ""Square-Root" varstaby    
           
           
           
    )#End Switch
    
    
    
    
  }#End if "Square-Root" x
  
  
  
  
  
  
  return(p)
}#scattercolorsize01


scattercolorsize00<-function(df,scatterx,scattery,varstabx,varstaby,plottitle){
  
 
  
  
  if (varstabx=="None"){
    
    switch(varstaby,
           
           "None"={ p<-plot_ly(df,x=df[,scatterx],y=df[,scattery],
                               marker=list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                               type='scatter'
           ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,title=scattery),
                    xaxis = list(zeroline = FALSE,title=scatterx))
           
             
           }, #End "None" varstaby
           
           
           "Log"={ p<-plot_ly(df,x=df[,scatterx],y=log10(df[,scattery]),
                              marker=list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                              type='scatter'
           ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,tickprefix="10^",title=scattery),
                    xaxis = list(zeroline = FALSE,title=scatterx))
           
             
             
           },#End "Log" varstaby
           
           "Square-Root"={ p<-plot_ly(df,x=df[,scatterx],y=sqrt(df[,scattery]),
                                      marker=list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                                      type='scatter'
           ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,ticksuffix="^2",title=scattery),
                    xaxis = list(zeroline = FALSE,title=scatterx))
           
             
           } #End ""Square-Root" varstaby    
           
           
           
    )#End Switch
    
  }#End if "None" x
  
  else if(varstabx=="Log") {
    
    switch(varstaby,
           
           "None"={ p<-plot_ly(df,x=log10(df[,scatterx]),y=df[,scattery],
                               marker=list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                               type='scatter'
           ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,title=scattery),
                    xaxis = list(zeroline = FALSE,tickprefix="10^",title=scatterx))
           
             
           }, #End "None" varstaby
           
           
           "Log"={ p<-plot_ly(df,x=log10(df[,scatterx]),y=log10(df[,scattery]),
                              marker=list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                              type='scatter'
           ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,tickprefix="10^",title=scattery),
                    xaxis = list(zeroline = FALSE,tickprefix="10^",title=scatterx))
           
             
             
           },#End "Log" varstaby
           
           "Square-Root"={ p<-plot_ly(df,x=log10(df[,scatterx]),y=sqrt(df[,scattery]),
                                      marker=list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                                      type='scatter'
           ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,ticksuffix="^2",title=scattery),
                    xaxis = list(zeroline = FALSE,tickprefix="10^",title=scatterx))
           
           } #End ""Square-Root" varstaby    
           
           
           
    )#End Switch
    
  }#End if "Log" x
  
  
  
  else if(varstabx=="Square-Root"){
    
    switch(varstaby,
           
           "None"={ p<-plot_ly(df,x=sqrt(df[,scatterx]),y=df[,scattery],
                               marker=list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                               type='scatter'
           ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,title=scattery),
                    xaxis = list(zeroline = FALSE,ticksuffix="^2",title=scatterx))
           
             
           }, #End "None" varstaby
           
           
           "Log"={ p<-plot_ly(df,x=sqrt(df[,scatterx]),y=log(df[,scattery]),
                              marker=list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                              type='scatter'
           ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,tickprefix="10^",title=scattery),
                    xaxis = list(zeroline = FALSE,ticksuffix="^2",title=scatterx))
           
             
             
           },#End "Log" varstaby
           
           "Square-Root"={ p<-plot_ly(df,x=sqrt(df[,scatterx]),y=sqrt(df[,scattery]),
                                      marker=list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width=2)),
                                      type='scatter'
           ) %>%
             
             layout(title = plottitle,
                    yaxis = list(zeroline = FALSE,ticksuffix="^2",title=scattery),
                    xaxis = list(zeroline = FALSE,ticksuffix="^2",title=scatterx))
           
             
           } #End ""Square-Root" varstaby    
           
           
           
    )#End Switch
    
    
    
    
  }#End if "Square-Root" x
  
  
  
  return(p)
  
}#scattercolorsize00



##########Spatial Tab Functions###############################

spat_point_pattern<-function(data,left,bottom,feature,spatfiltercheck,spatvariable,spatvalue,stom.poly,spatplotlevel){
  
  # #Creating Point Patterns and filtering for a specific variable
  left_coord<-data()[,left()] #X-coordinate
  bottom_coord<-data()[,bottom()] #Y-Coordinate
  spatial.data.xy <- data.frame(X=round(left_coord*930.05/100+25,0), Y=round(bottom_coord*700.05/100+82,0)) #Generating X-Y pairs
  featurevariable<-as.character(feature()) #Feature to study
  
  df_spat<-as.data.frame(data()) # Original Dataframe
  
  
  if(spatfiltercheck()==1){
    
    spatialvariable<-as.character(spatvariable()) #Filtervariable
    spatialvalue<-as.character(spatvalue()) #Filter Value
    df_spat_filtered<- df_spat[df_spat[spatialvariable]==spatialvalue,] #Subsetting the dataframe based on filtervalues
    sp.xy.subset <- subset(spatial.data.xy, df_spat[,spatialvariable]==spatialvalue) #Subsetting the X-Y coordinates for values of filtered variable
    
    #Checking if the filter variable has blanks
    sp.xy.subset <- subset(spatial.data.xy, !is.na(df_spat_filtered[spatialvariable]))
    spatblankfiltervalues<-colSums(is.na(df_spat_filtered[spatialvariable]))
    df_spat_filtered<-df_spat_filtered[!is.na(df_spat_filtered[spatialvariable]),]
    
    #spatblankvalues<-colSums(is.na(df_spat_filtered[featurevariable]))  #Checking for blank cells in feature variable "Used -1 becoz R introduces a blank row while subsetting"
  }
  
  else
  {
    df_spat_filtered<- df_spat #No subsetting required
    sp.xy.subset <- spatial.data.xy# No Subsetting the X-Y coordinates required
    #spatblankvalues<-colSums(is.na(df_spat_filtered[featurevariable])) #Checking for blank cells in feature variable
  }
  
  
  #Checking for blank cells in feature variable
  spatblankvalues<-colSums(is.na(df_spat_filtered[featurevariable]))
  if (spatblankvalues!=0){
    df_spat_filtered<-df_spat_filtered[!is.na(df_spat_filtered[featurevariable]),]
    sp.xy.subset <- subset(spatial.data.xy, !is.na(df_spat_filtered[featurevariable]))
    
  }
  
  
  if(spatfiltercheck()==1){output_blankvalues_text<-paste0("There are ",spatblankvalues," blank cells for the numeric variable and ", spatblankfiltervalues," blank cells for the categorical variable")}
  else {output_blankvalues_text<-paste0("There are ",spatblankvalues," blank cells for the numeric variable")}
  
  #Generating point pattern
  spat.test <- ppp(x=sp.xy.subset[,1], y=sp.xy.subset[,2], marks=log(df_spat_filtered[,featurevariable]), window=stom.poly(), checkdup=F) 
  #plot(spat.test)
  
  #Creating feature_df
 # feature_df<-df_spat_filtered[,featurevariable]
  
  if(spatplotlevel()=="Two"){
    
    breaks.hist <-quantile(log(df_spat_filtered[,featurevariable]), probs = c(0, 0.5, 1))   #### generate tertiles
    spat.test.cut <- cut.ppp(spat.test, breaks = (breaks.hist), include.lowest = T)
    levels(spat.test.cut$marks) <- c("low", "high")}
  
  else if(spatplotlevel()=="Three"){
    breaks.hist <-quantile(log(df_spat_filtered[,featurevariable]), probs = c(0, 0.33, 0.66, 1))   #### generate tertiles
    spat.test.cut <- cut.ppp(spat.test, breaks = (breaks.hist), include.lowest = T)
    levels(spat.test.cut$marks) <- c("low", "medium", "high")}
  
  
  
  
  spat_point_patternvalues<-list("output_blankvalues_text"=output_blankvalues_text,"spat.test.cut"=spat.test.cut)
  
  return(spat_point_patternvalues)
  
}#spat_point_pattern function end



