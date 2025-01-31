#  various temporal noise exposure profiles

library(ggplot2)
library(ggthemes)


# 3 days ------------------------------------------



########## creating data

## three day

noise <- data.frame( time = seq( from = 1 , to = 3*8*60 , by = 1) )


# task strategy

# T1 from 0 to 2h
# T2 from 2h to 3H30
# T3 from 4 to 8h

noise$task <- numeric( length( noise[,1] ) ) 

noise$T1 <- rep( c( rep(1 , 120)  , rep( 0 , 360 ) ) , 3 )

noise$T2 <- rep( c( rep(0 , 120)  , rep( 1 , 90 ) , rep( 0 , 270) ) , 3 )

noise$T3 <- rep( c( rep(0 , 240)  , rep( 1 , 240 )) , 3 )

noise$task_mean <- 75 *  noise$T1 + 95 * noise$T2 + 80 * noise$T3

noise$day <- c( rep( "jour1" , 480 ) , rep( "jour2" , 480 ) , rep( "jour3" , 480 ))

noise$task <- noise$task_mean + rnorm( length( noise[,1] ) , 0 , 1 )

p <- ggplot(data=noise, aes( x=time , y = task , color = day))

p <- p + geom_point()

p <- p + theme_calc()

p <- p +  theme(axis.text.x=element_text(size=14),axis.title.x=element_text(size=16,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
  theme(axis.ticks = element_line(size = 1) , axis.ticks.length.x = unit(.25, "cm"), axis.ticks.length.y = unit(.25, "cm"))+
  labs(x=expression ( "Temps (min)") , y="dBA") + ylim( c(50,100))

p

# function strategy

noise$metier <- 80 + rnorm( length( noise[,1] ) , 0 , 1.5 )

p <- ggplot(data=noise, aes( x=time , y = metier , color = day))

p <- p + geom_point()

p <- p + theme_calc()

p <- p +  theme(axis.text.x=element_text(size=14),axis.title.x=element_text(size=16,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
  theme(axis.ticks = element_line(size = 1) , axis.ticks.length.x = unit(.25, "cm"), axis.ticks.length.y = unit(.25, "cm"))+
  labs(x=expression ( "Temps (min)") , y="dBA")+ ylim( c(50,100))

p

# full day strategy

noise$D1 <- c( rep(1 , 480)  , rep( 0 , 480 )  , rep( 0 , 480 ) ) 

noise$D2 <- c( rep(0 , 480)  , rep( 1 , 480 )  , rep( 0 , 480 ) ) 

noise$D3 <- c( rep(0 , 480)  , rep( 0 , 480 )  , rep( 1 , 480 ) ) 

noise$day_mean <- 81 *  noise$D1 + 84 * noise$D2 + 77 * noise$D3

noise$fullday <- noise$day_mean + rnorm( length( noise[,1] ) , 0 , 1.3 )

p <- ggplot(data=noise, aes( x=time , y = fullday , color = day))

p <- p + geom_point()

p <- p + theme_calc()

p <- p +  theme(axis.text.x=element_text(size=14),axis.title.x=element_text(size=16,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
  theme(axis.ticks = element_line(size = 1) , axis.ticks.length.x = unit(.25, "cm"), axis.ticks.length.y = unit(.25, "cm"))+
  labs(x=expression ( "Temps (min)") , y="dBA")+ ylim( c(50,100))

p



# 5 days ------------------------------------------

### INRS R259 values of 1min data variability

# fraiseurs 4.5
# monteurs 8.4, 6 intra tache
# menuisiers 8.7  7 intra tache
# extrudeurs 3.5
# conducteurs 5.2
# regleurs 6.4


########## creating data


noise <- data.frame( time = seq( from = 1 , to = 5*8*60 , by = 1) )


# task strategy

# T1 from 0 to 2h
# T2 from 2h to 3H30
# T3 from 4 to 8h

noise$task <- numeric( length( noise[,1] ) ) 

noise$T1 <- rep( c( rep(1 , 120)  , rep( 0 , 360 ) ) , 5 )

noise$T2 <- rep( c( rep(0 , 120)  , rep( 1 , 90 ) , rep( 0 , 270) ) , 5 )

noise$T3 <- rep( c( rep(0 , 240)  , rep( 1 , 240 )) , 5 )

noise$task_mean <- 75 *  noise$T1 + 95 * noise$T2 + 80 * noise$T3

noise$day <- c( rep( "jour1" , 480 ) , rep( "jour2" , 480 ) , rep( "jour3" , 480 ),rep( "jour4" , 480 ),rep( "jour5" , 480 ))

noise$task <- noise$task_mean + rnorm( length( noise[,1] ) , 0 , 2 )

p <- ggplot(data=noise, aes( x=time , y = task , color = day))

p <- p + geom_point()

p <- p + theme_calc()

p <- p +  theme(axis.text.x=element_text(size=14),axis.title.x=element_text(size=16,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
  theme(axis.ticks = element_line(size = 1) , axis.ticks.length.x = unit(.25, "cm"), axis.ticks.length.y = unit(.25, "cm"))+
  labs(x=expression ( "Temps (min)") , y="dBA") + ylim( c(65,100))

p

# function strategy - TBD

noise$metier <- 80 + rnorm( length( noise[,1] ) , 0 , 1.5 )

p <- ggplot(data=noise, aes( x=time , y = metier , color = day))

p <- p + geom_point()

p <- p + theme_calc()

p <- p +  theme(axis.text.x=element_text(size=14),axis.title.x=element_text(size=16,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
  theme(axis.ticks = element_line(size = 1) , axis.ticks.length.x = unit(.25, "cm"), axis.ticks.length.y = unit(.25, "cm"))+
  labs(x=expression ( "Temps (min)") , y="dBA")+ ylim( c(50,100))

p

# full day strategy - TBD

noise$D1 <- c( rep(1 , 480)  , rep( 0 , 480 )  , rep( 0 , 480 ) ) 

noise$D2 <- c( rep(0 , 480)  , rep( 1 , 480 )  , rep( 0 , 480 ) ) 

noise$D3 <- c( rep(0 , 480)  , rep( 0 , 480 )  , rep( 1 , 480 ) ) 

noise$day_mean <- 81 *  noise$D1 + 84 * noise$D2 + 77 * noise$D3

noise$fullday <- noise$day_mean + rnorm( length( noise[,1] ) , 0 , 1.3 )

p <- ggplot(data=noise, aes( x=time , y = fullday , color = day))

p <- p + geom_point()

p <- p + theme_calc()

p <- p +  theme(axis.text.x=element_text(size=14),axis.title.x=element_text(size=16,vjust=+0.55))+
  theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
  theme(axis.ticks = element_line(size = 1) , axis.ticks.length.x = unit(.25, "cm"), axis.ticks.length.y = unit(.25, "cm"))+
  labs(x=expression ( "Temps (min)") , y="dBA")+ ylim( c(50,100))

p

