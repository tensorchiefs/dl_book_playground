setwd("~/Documents/workspace/dl_book_playground")
library(readr)
X = read_csv("n3_lr_2e-3_woff2.csv")
dat = read_csv('data_blr_3.csv')
library(ggplot2)
library(dplyr)
library(tidyr)

# Rendering of the losses
losses = select(X, epoch, loss, loss_kl,loss_mse)
losses$loss_tot = losses$loss
losses$loss = NULL
d = gather(losses, key='type', value = 'loss', -'epoch')
d$epochs_all = d$epoch
d$epoch = NULL
unique(d$type)
ggplot(d) + geom_line(aes(x=epochs_all, y=loss, color=type), alpha=0.8) + ylim(0,100)

epochs = c(1:25, seq(25,75,2), seq(75,150,4),seq(150,1000,10),seq(1000,5000,50))
library(gganimate)
X$toprint = paste0("epoch ", X$epoch, " loss ", X$loss)
last_frame = length(epochs)
#ggplot(X[210:222,1:13]) +
left = ggplot(X[epochs,]) +
  geom_line(data=data.frame(x=X$w0, y=X$w2),aes(x=x,y=y), size = 0.5, col='grey')+
  geom_point(aes(x=w0,y=w2), col='blue', alpha = 0.6, size = 4)+
  geom_segment(aes(x=w0,y=w2,xend=w0-0.01*wg0,yend=w2-0.01*wg2)) +
  geom_label(aes(x=w0,y=w2,label=toprint), hjust = -0.3) + 
  ylab('Slope') +
  xlab('Intercept') +
  theme_classic(base_size = 22) + 
  transition_manual(epoch) 

left_gif = animate(left,nframes=last_frame)
#left_gif = animate(left,nframes=last_frame)
left_gif
anim_save('grads_n3_lr_2e-3_woff2.gif', left_gif)

m = ggplot(X[epochs,]) +
  geom_segment(aes(x=epoch, xend=epoch+1), y=0, yend=40, size=1.5)+
  geom_line(data=d,aes(x=epochs_all, y=loss, color=type), alpha=0.7) +
  ylim(0,40) +
  theme_classic() +
  transition_manual(epoch)  
m_gif = animate(m,nframes=last_frame)
m_gif
anim_save('losses_n3_lr_2e-3_woff2.gif', m_gif)

right = ggplot(X[epochs,]) +
  geom_point(data=dat, aes(x=x, y=y), col='black', size=3) +
  geom_segment(aes(y=(-3)*w2+w0,yend=w0+w2*3),size=1.5, col='blue',x=-3, xend=3)+ 
  theme_classic(base_size = 22) + 
  transition_manual(epoch) 
#right_gif = animate(right, nframes=last_frame,renderer = ffmpeg_renderer())
right_gif = animate(right, nframes=last_frame)
right_gif
anim_save('fit_n3_lr_2e-3_woff2.gif', right_gif)

library(magick)
a_mgif <- image_read(left_gif)
b_mgif <- image_read(right_gif)
m_mgif <- image_read(m_gif)
new_gif<-image_append(c(a_mgif[1], m_mgif[1],b_mgif[1]))
for(i in 2:330){
  combined <- image_append(c(a_mgif[i], m_mgif[i], b_mgif[i]))
  new_gif<-c(new_gif,combined)
}
#new_gif

anim_save('anim.gif', new_gif)

