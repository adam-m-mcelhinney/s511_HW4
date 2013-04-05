# Draw T uniform samples from U(0,1)

T<-1000
#T<-5
a<-0
b<-1

data<-runif(T,a, b)

# Generate uniform list between (-2,2)
#u<-seq(-2,2,.01)
u<-seq(-2,2,.1)


mgf_mc<-function(data, u_i){
n=length(data)
return ((1/n)*sum(exp(data*u_i)))
                }

# Actual MGF
# Taken from wikipedia
mgf<-function(u_i,a,b){
    return (
        (exp(u_i*b)-exp(u_i*a))/(u_i*(b-a))
        )
        }
mgf(u_i=1,a,b)



actual_mgf<-sapply(u,mgf,a=a,b=b)

out<-vector()
for (i in 1:length(u)){
    u_i=u[i]
    out<-c(out, mgf_mc(data,u_i))
            }

plot(u,out, type='l', col='blue')
points(u,actual_mgf, col='red')


