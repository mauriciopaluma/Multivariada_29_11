x = c(13.6, 13.6, 14.7, 12.1, 12.3, 13.2, 11, 12.4)
y = c(11.4, 12.5, 14.6, 13, 11.7, 10.3, 9.8, 10.4)

d = y-x

media_x = mean(x); media_x
media_y = mean(y); media_y
n = length(x)

sd2 = (1/(n-1))*sum((d-mean(d))^2);sd2
t = sqrt(n) * mean(d)/sqrt(sd2); t

qt(0.975, 7)

#Como |t| = 2.655811 >  2.364624 rejeita-se H_0

#-------------------------------------------------------------------

y1 = c(73, 43, 47, 53, 58, 47, 52, 38, 61, 56 , 56, 34, 55, 65, 75)
y2 = c(31, 19, 22, 26, 36, 30, 29, 36, 34, 33, 19 , 19, 26, 15, 18)
length(y2)

x1 = c(51, 41, 43, 41, 47, 32, 24, 43, 53, 52, 57, 44, 57, 40, 68)
x2 = c(35, 14, 19, 29, 34, 26, 19, 37, 24, 27, 14, 19, 30, 7, 13)
length(x2)

y = matrix(c(y1, y2) ,ncol = 2, byrow = F);y
x = matrix(c(x1, x2) ,ncol = 2, byrow = F);x

n = length(y1);n

d = y-x;d

d_barra = matrix(c(mean(d[,1]),mean(d[,2]))); d_barra

var(d)

t = n*t(d_barra) %*% solve(var(d)) %*% d_barra; t

p=2;p

F1 = qf(0.95,p,(n-p));F1
F2 = qf(0.99,p,(n-p));F2

F0 = ( (n-p) / ((n-1) * p )) * t; F0

Ttab1 = (F1 * ((n-1)*p)) / (n-p);Ttab1 #0.5
Ttab2 = (F2 * ((n-1)*p)) / (n-p);Ttab2 #0.01
