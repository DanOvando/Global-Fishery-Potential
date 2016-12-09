function[negout]=GFRM_fun(f,b,p,MSY,c,r,beta,V,bvec,delta)
%Use this with "GFRM" for value function iteration
%inputs are: f (fishing mortality), b (biomass), and V
%(value function)


profit = p*MSY*f*b - c*(f*r/2)^beta; %Calculate current profits

bnext = b + r*b*(1-b/2) - r/2*b*f; %Calculate the resulting biomass from current fishing


Vnext = interp1(bvec,V,bnext,'pchip');

negout = -(profit + delta*Vnext); %Given current profits (aka biomass), find an F that maximizes next years profits
