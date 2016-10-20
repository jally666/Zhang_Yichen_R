data("diamonds")
library("grid")
library("ggplot2")

#Q2:
weight <- diamonds$carat
price <- diamonds$price #take out these two variables
ggplot(diamonds,aes(weight,price,color=factor(color)))+geom_point()+ #use ggplot to draw graph
  ggtitle("Diamonds- Weight to Price by color")+ #give title to graph
  theme(plot.title=element_text(color="blue")) #change title's color


#Q3:
ggplot(diamonds,aes(log(carat),log(price),color=factor(color)))+ #transfer the two variable into log
  xlab("Weight")+ylab("Price")+ #rename the labels
  ggtitle('Diamonds- Weight to Price by color(Linear)')+ #give title to graph
  theme(plot.title=element_text(color="blue"))+ #change title's color
  geom_point()

#Q4:
lm1 <- lm(log(price)~1+log(weight)) #do linear regression
residuals <- lm1$residuals # take out all residuals from linear regression
new_diamonds <- data.frame(diamonds,residuals) # build a new diamonds dataframe adding residuals
ggplot(new_diamonds,aes(log(carat),residuals,color=factor(color)))+
  xlab("Weight")+ylab("Price residuals")+
  ggtitle('Diamonds- Weight to Price by Color')+
  geom_point()+ #plot the residuals and weight
  theme(plot.title=element_text(color="blue"),legend.position = "top") #put the legend of factor on the top

#Q5:
grid.newpage()
lm1 <- lm(log(price)~1+log(weight)) #do linear regression
residuals <- lm1$residuals # take out all residuals from linear regression
new_diamonds <- data.frame(diamonds,residuals) # build a new diamonds dataframe adding residuals
p1 <- ggplot(new_diamonds,aes(log(carat),residuals))+
      xlab("Weight")+ylab("Price residuals")+
      ggtitle('Diamonds- Weight to Price by Color')+ #give the title
      geom_point(aes(colour=factor(color)))+ #plot the residuals and weight
      theme(plot.title=element_text(color="blue"),legend.position = "top")+ #put the legend of factor on the top and change title's color
      guides(col=guide_legend(nrow=1)) #make the legend in one row

p2 <- ggplot(diamonds,aes(x=carat,color=color,..density..))+ 
      geom_histogram(binwidth = 0.027)+ #draw the histogram of "carat"
      theme(legend.position = "none",
            axis.title=element_blank(),#cancel the legend, title and labels
            plot.margin=unit(c(0,0,0,0),"mm")) #cancel the border

p3 <- ggplot(diamonds,aes(x=price,colour=color,..density..))+
      geom_histogram(binwidth = 45)+  #draw the histogram of "price"
      theme(legend.position = "none",
            axis.title=element_blank(),#cancel the legend, title and labels
            plot.margin=unit(c(0,0,0,0),"mm")) #cancel the border

view2 <- viewport(width=0.4,height=0.2,x=0.79,y=0.65) #use "viewpoint" function in "grid" to locate the postions of two histograms 
view3 <- viewport(width=0.4,height=0.2,x=0.3,y=0.21) 

print(p1)
print(p2,vp=view2)
print(p3,vp=view3)#draw three graphs together

#Q6
grid.newpage()

view1 <- viewport(width=0.7,height=0.7,x=0.6,y=0.65)
view2 <- viewport(width=0.6,height=0.23,x=0.63,y=0.15)
view3 <- viewport(width=0.4,height=0.23,x=0.15,y=0.6,angle=90) #create three graphs' location

p1 <- ggplot(new_diamonds,aes(log(carat),residuals,color=factor(color)))+
      xlab("Weight")+ylab("Price residuals")+
      ggtitle('Diamonds- Weight to Price by Color')+
      geom_point()+ #plot the residuals and weight
      theme(plot.title=element_text(color="blue"),legend.position = "top")+ #put the legend of factor on the top and change title's
      guides(col=guide_legend(nrow=1)) #make the legend in one row

p2 <- ggplot(diamonds,aes(x=carat,color=color,..density..))+
      geom_histogram(binwidth = 0.027)+  #draw the histogram of "carat"
      theme(legend.position = "none",axis.title=element_blank()) 

p3 <- ggplot(diamonds,aes(x=price,colour=color,..density..))+
      geom_histogram(binwidth = 45)+  #draw the histogram of "price"
      theme(legend.position = "none",axis.title=element_blank())

print(p1,vp=view1)
print(p2,vp=view2)
print(p3,vp=view3)# draw three graphs
