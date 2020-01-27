#devtools::install_github("rstudio/keras")
#library(keras)
#install_keras()
#install.packages('tfautograph')

#library(keras)

#install.packages("tfprobability")
library(tfprobability)

library(tensorflow)
# tf_version()
# tf$version
tf_probability()
tf$compat$v2$enable_v2_behavior()

init_beta_dist_for_h = function(len_theta){
  beta_dist = tfd_beta(1:len_theta, len_theta:1)
  return (beta_dist)
}

init_beta_dist_for_h_dash = function(len_theta){
  M = len_theta - 1
  beta_dist = tfd_beta(1:M, M:1)
  return (beta_dist)
}

# # determine h' ((MLT Paper p.12))
# # h' is needed to get densities: f_y(y)=f_z(h(y))*h'(y)
# h_dash <- function(theta, y, r=1) {
#   M = length(theta)-1 #Order 
#   dd = rep(0, length(y)) 
#   for (m in 0:(M-1)){
#     print(m)
#     dd = dd + (theta[m+2] - theta[m+1]) * dbeta(y,m+1,M-m)   
#   }
#   #dd = dd * M / ((M + 1.)*r)  # so ists im paper, wird aber falsch
#   dd = dd  / r  # so wirds richtig
#   return (dd)
# }

eval_h_dash = function(theta, y){
  y = tf$clip_by_value(y,1E-5, 1.0-1E-5)
  by = beta_dist_h_dash$prob(y) 
  dtheta = (theta[,2:(ncol(theta))]-theta[,1:(ncol(theta)-1)])
  return (tf$reduce_sum(by * dtheta, axis=1L))
}

eval_h = function(theta, y){
  y = tf$clip_by_value(y,1E-5, 1.0-1E-5)
  by = beta_dist_h$prob(y) 
  return (tf$reduce_mean(by * theta, axis=1L))
}

# ######## function that turns pre_theta from NN ouput to theta

to_theta = function(pre_theta){
  #Trivially calclutes theta_1 = h_1, theta_k = theta_k-1 + exp(h_k)
  d = tf$concat( c(tf$zeros(c(nrow(pre_theta),1L)),
                   tf$slice(pre_theta, begin = c(0L,0L), size = c(nrow(pre_theta),1L)),
                   tf$math$exp(pre_theta[,2:ncol(pre_theta)])),axis=1L)
  #d = tf.concat( (tf.zeros((h.shape[0],1)), h[:,0:1], tf.math.softplus(h[:,1:h.shape[1]])),axis=1)
  return (tf$cumsum(d[,2L:ncol(d)], axis=1L))
}


# # testing:
# #############################################
# # plain R functions from mlt_utils.r
# theta = matrix(c(
#   0.1,1.1,4,12,
#   2,3,4,5,
#   6,7,8,9,
#   10,11,12,13,
#   14,15,16,17
# ), nrow = 5, ncol = 4, byrow = TRUE)
# 
# y = array(c(0.00,0.25,0.50,0.75,1.00), c(5,1))
# theta[2,]
# 
# # use plain R fct h, h_dash with expect vectors for theta and y
# source("mlt_utils.r")
# ( y[2] )
# ( h(theta[2,], y[2]) )
# ( h_dash(theta[2,], y[2]) )
# 
# # reproduce result from plain R fct h and h_dash
# len_theta = 4
# 
# beta_dist_h = init_beta_dist_for_h(len_theta)
# beta_dist_h_dash = init_beta_dist_for_h_dash(len_theta)
# 
# # select one col from theta and one value from y and turn it into a tf tensor
# ( theta_select=tf$Variable( theta[2,,drop=FALSE], dtype='float32')  )
# ( y_select=tf$Variable( y[2,drop=FALSE], dtype='float32') )
# eval_h(theta_select,y_select)
# eval_h_dash(theta_select,y_select)
# 
# # let's do it for a whole tensor, like we get it dl applications
# ( y_dl = tf$Variable(y, dtype='float32') )
# ( theta_dl = tf$Variable(theta, dtype='float32') )
# 
# eval_h(theta_dl,y_dl)
# eval_h_dash(theta_dl,y_dl)
# 

###### test of to_theta function
##################################################
# 
# pre_theta_tmp = array(1:-20,dim=c(5,4))
# ( pre_theta_tmp = tf$Variable(pre_theta_tmp, dtype='float32') )
# to_theta(pre_theta_tmp)
# 
# # #Hier weiter machen - juhu!
# #
# #
# # ############
# # # Boston
# # data("BostonHousing2", package = "mlbench")
# # dat=BostonHousing2
# # source("mlt_utils.R")
# # y = matrix(utils_scale(dat$cmedv),ncol=1)
# # y = tf$Variable(y, dtype='float32')
# #
# #
# #
# #
# #
# #
# #
# #
# #
