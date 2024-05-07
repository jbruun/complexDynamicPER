#This is the Ising-like model developed by
#Bordogna, C. M., & Albano, E. V. (2001). 
#Theoretical description of teaching-learning processes: A multidisciplinary approach. 
#Physical Review Letters, 87(11), 118701.

#The model i adapted to accommodate real networks. 
#Thus, the input is a graph object

isingTeachLearn<-function(g){
  require(igraph)
  A<-as_adjacency_matrix(g)
  
  ##Cognitive impact of teacher
  
  
  
}

impactStudent<-function(P_ij,S_ij,sigma_i,sigma_j,sigma_T){
  #I think the minus-sign between P_ij-term and S_ij term is wrong
  CI<-(P_ij*(1-sigma_i*sigma_j)-S_ij*(1+sigma_i*sigma_j))*sign(sigma_i/sigma_T)
  return(CI)
}


impactTeacher<-function(sigma_j,P_jT,sigma_T){
  CI<-P_jT*(1-sigma_j*sigma_T)
  return(CI)
}

impactMaterial<-function(A_j,Q,sigma_J){
  CI<-A_j*Q*(1-sigma_j)
  return(CI)
}

support_ij<-function(S_ij0,sigmat_T,sigma_i){
  S_ij<-S_ij0*(sigma_T+sigma_i)
}

persuation_ij<-function(P_ij0,sigmat_T,sigma_i){
  P_ij<-P_ij0*(sigma_T+sigma_i)
}