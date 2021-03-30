

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(cowplot)

setwd('this directory')

all_order_results=readRDS('all_order_results.rds')

title_list= c('HIRHAM Fine','HIRHAM Coarse', 'MAR Fine','MAR Coarse','MERRA2 Fine','MERRA2 Coarse','RACMO2 Fine','RACMO2 Coarse')
xlabel_list=c(rep(' ',6),'Time (hr)','Time (hr)')

plotsaver= list(length(all_order_results)/2)
count=0
for (i in c(1,2,5,6,9,10,13,14)){
  
  count=count+1
  
  
  
  
  model1=all_order_results[[i]] %>%
    mutate(model ="Hillslope") %>%
    rename(Model=model)
  
  model2=all_order_results[[i+2]]  %>%
    mutate(model ="Non Hillslope")%>%
    rename(Model=model)
  
  # model3=left_join(model1,model2,by= c('time','order',"upper_CI","lower_CI",'model','flow_mean')) %>%
  model3=rbind(model1,model2) %>%
    rename(Order=order)%>%
    filter( Order ==4 | Order ==5 )
  
  current_title=title_list[count]
  if (count ==1){current_legend=T
  }else{current_legend=F}
  
  
  
  plot_current= ggplot(model3) +
    geom_ribbon(aes(x=time,ymin=lower_CI, ymax=upper_CI,fill=Order, linetype=Model),alpha=0.2,show.legend = current_legend)+
    geom_line(aes(x=time,y=flow_mean,col=Order,linetype=Model), lwd=0.5,show.legend = current_legend)+
    scale_linetype_manual(values=c('solid','dotted'))+
    guides(colour=guide_legend(nrow=1),legend.text=element_text(size=6))+
    guides(linetype=guide_legend(nrow=1),legend.text=element_text(size=6))+    
    guides(fill=guide_legend(nrow=1))+  
    ylab(expression(paste("Q (",m^3, "/s)", sep="")))+
    xlab(xlabel_list[count]) +
    theme_bw() +
    ggtitle(current_title)+
    # theme(legend.text=element_text(size=15),
    theme(legend.text=element_text(size=6),
          legend.title=element_text(size=6),
          #legend.position = 'left',
          axis.text= element_text(size=8),
          axis.title= element_text(size=8),
          plot.title = element_text(color="black", size=8, face="bold.italic"),
          legend.position =  c(0.5,-0.30),
          legend.justification='center',
          legend.box ='horizontal',
          legend.direction='horizontal',
          legend.key.size =unit(4, "mm"),
          plot.margin=grid::unit(c(0,0,0,0), "mm"),
          legend.background = element_rect(fill=alpha('white',0.8)))
  
  
  plotsaver[[count]]=plot_current
}
figure1= cowplot::plot_grid(plotlist=plotsaver,nrow=4)


plotsaver= list(length(all_order_results)/2)
count=0
for (i in c(1,2,5,6,9,10,13,14)){
  count=count+1
  
  
  
  
  model1=all_order_results[[i]] %>%
    mutate(model ="Hillslope") %>%
    rename(Model=model)
  
  model2=all_order_results[[i+2]]  %>%
    mutate(model ="Non Hillslope")%>%
    rename(Model=model)
  
 # model3=left_join(model1,model2,by= c('time','order',"upper_CI","lower_CI",'model','flow_mean')) %>%
  model3=rbind(model1,model2) %>%
    rename(Order=order)%>%
    filter( Order ==3 | Order ==2 | Order ==1  )
  
  current_title=title_list[count]
  if (count ==1){current_legend=T
  }else{current_legend=F}
  
  
  plot_current= ggplot(model3) +
    geom_ribbon(aes(x=time,ymin=lower_CI, ymax=upper_CI,fill=Order, linetype=Model),alpha=0.2,show.legend = current_legend)+
    geom_line(aes(x=time,y=flow_mean,col=Order,linetype=Model), lwd=0.5,show.legend = current_legend)+
    scale_linetype_manual(values=c('solid','dotted'))+
    guides(colour=guide_legend(nrow=1),legend.text=element_text(size=6))+
    guides(linetype=guide_legend(nrow=1),legend.text=element_text(size=6))+    
    guides(fill=guide_legend(nrow=1))+  
    ylab(expression(paste("Q (",m^3, "/s)", sep="")))+
    xlab(xlabel_list[count]) +
    theme_bw() +
    ggtitle(current_title)+
    # theme(legend.text=element_text(size=15),
    theme(legend.text=element_text(size=6),
          legend.title=element_text(size=6),
          #legend.position = 'left',
          
          axis.text= element_text(size=8),
          axis.title= element_text(size=8),
          plot.title = element_text(color="black", size=8, face="bold.italic"),
          legend.position =  c(0.5,-0.30),
          legend.justification='center',
          legend.box ='horizontal',
          legend.direction='horizontal',
          legend.key.size =unit(2.15, "mm"),
          plot.margin=grid::unit(c(0,0,0,0), "mm"),
          legend.background = element_rect(fill=alpha('white',0.8)))
  

  plotsaver[[count]]=plot_current
}
figure2= cowplot::plot_grid(plotlist=plotsaver,nrow=4)





ggsave(filename="order1_3delay.jpg", 
       plot=figure2,
       width=6, 
       height=5, 
       dpi= 500)

ggsave(filename="order4_5delay.jpg", 
       plot=figure1,
       width=6, 
       height=5, 
       dpi= 500)






















# test=select(gis_layer,HRRID,geometry,i)%>%
#   rename(HRRID=HRRID,geometry=geometry,Flow=i)
# 
# #plot controls
# time_base=ymd_hm('2015-07-20 11:00')
# plotbreaks_flow= c(0.001,0.01,0.1,0.5,1,3,10,30)
# plotbreaks_Incision= c(-5,-4,-3,-2)
# # plotbreaks_Incision= c(1e-5,1e-4,1e-3,1e-2)
# 
# plot1=ggplot(test) +
#   geom_sf(aes(size=Flow,geometry=geometry),col='blue',show.legend = FALSE) +
#   geom_line(aes(x=-170000,y=-2508000,size=Flow),col='blue')+
#   annotate('text',x=-170000,y=-2508000, label= format(time_base+ strtoi(i)*3600,"%Y-%m-%d %H:%M"), size=10) +
#   scale_size_continuous( name=expression(paste("Discharge (",m^3, "/s)", sep="")),
#                          breaks = plotbreaks_flow,
#                          limits=range(plotbreaks_flow)) +
#   ylab('')+
#   xlab('') +
#   theme_bw() +
#   theme(legend.text=element_text(size=20),
#         legend.title=element_text(size=20))
# 
# plot(plot1)
# 
# 
# 
# 
# if (flow_movie==1){
#   
#   saveGIF({for (i in 1:num_hours){
#     temp_df=filter(master_DF,Time==i)%>%
#       #  filter(HRRID < 100) %>%
#       mutate(Incision_rate_mm=log10(Incision_rate_mm))
#     
#     hydrograph_df=filter(master_DF,HRRID==max(HRRID,na.rm=TRUE))%>%
#       filter(Time<=i) %>%
#       select(-geometry) %>%
#       left_join(R_in2,by='Time') %>%
#       distinct()     %>%
#       select(Time,Flow,Qlat,Q.cms) %>%
#       gather('source','value',-Time)
#     
#     cumulative_df= filter(master_DF,HRRID==max(HRRID,na.rm=TRUE))%>%
#       filter(Time<=i) %>%
#       select(-geometry) %>%
#       left_join(R_in2,by='Time') %>%
#       distinct()     %>%
#       mutate(cumulative_flow=cumsum(Flow),cumulative_Qlat=cumsum(Qlat),cumulative_true=cumsum(Q.cms)) %>%
#       select(Time,cumulative_flow,cumulative_Qlat,cumulative_true) %>%
#       gather('source','value',-Time)
#     
#     
#     
#     plot1=ggplot(temp_df) +
#       geom_sf(aes(size=Flow),col='blue',show.legend = FALSE) +
#       geom_line(aes(x=-170000,y=-2508000,size=Flow),col='blue')+
#       annotate('text',x=-170000,y=-2508000, label= format(time_base+ i*3600,"%Y-%m-%d %H:%M"), size=10) +
#       scale_size_continuous( name=expression(paste("Discharge (",m^3, "/s)", sep="")),
#                              breaks = plotbreaks_flow,
#                              limits=range(plotbreaks_flow)) +
#       ylab('')+
#       xlab('') +
#       theme_bw() +
#       theme(legend.text=element_text(size=20),
#             legend.title=element_text(size=20))
#     
#     
#     
#     
#     plot2 = ggplot(cumulative_df)+
#       geom_line(aes(x= Time, y=value,col=source ),lwd=2) +
#       scale_color_manual(values=c('blue','magenta','black'),labels=c('Routing','No Routing','ADCP'))+
#       xlim(c(0,72))+
#       ylim(c(0,1600))+
#       ylab(expression(paste("Q (",m^3, "/s)", sep="")))+
#       xlab('') +
#       theme_bw() +
#       theme(legend.text=element_text(size=20),
#             legend.title=element_blank(),
#             legend.position = c(0.1,.7),
#             axis.text= element_text(size=14),
#             axis.title= element_text(size=14),
#             legend.background = element_rect(fill=alpha('white',0.8)))
#     
#     
#     
#     
#     plot3 = ggplot(hydrograph_df)+
#       geom_line(aes(x= Time, y=value,col=source ),lwd=2, show.legend=FALSE) +
#       scale_color_manual(values=c('blue','black','magenta'),labels=element_blank())+
#       xlim(c(0,72))+
#       ylim(c(0,70))+
#       ylab(expression(paste("Q (",m^3, "/s)", sep="")))+
#       xlab('Time (hr)') +
#       theme_bw() +
#       theme(axis.text= element_text(size=14),
#             axis.title= element_text(size=14))
#     
#     
#     
#     figure1= plot_grid(plot2,plot3,plot1,align='hv', axis='lt', rel_heights=c(1/4,1/4,1),nrow=3)
#     
#     print(figure1)
#     
#     ani.options(interval=0.4)
#   }
#   },movie.name="Flow_CNH.gif",ani.width = 1000, ani.height = 1000)
#   
#   
#   
#   
# }


