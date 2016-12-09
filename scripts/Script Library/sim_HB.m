function [Htime,Btime] = sim_HB(rvec,Kvec,MSYvec,fstart,bstart,T,PolicyFlag)
%This function simulates for T periods and produces time series (plus CIs)
%for raw harvest and raw biomass over time. Kvec, rvec, and MSYvec are Jx1 column
%vectors
%PolicyFlag =
% 1 if close until Bmsy, then Fmsy (equil = 1.0)
% 2 if F/Fmsy = .8 forever (equil = 1.2)
% 3 if F/Fmsy = .66*B/Bmsy (equil = 1.2)

%The simulation below produces J "scaled" harvests and J "scaled" biomasses
J = length(rvec);
b(1:J,1) = bstart;
for t=1:T
    if PolicyFlag==1, f(1:J,t) = 0 + 1*b(:,t)>1;
        elseif PolicyFlag==2, f(1:J,t) = .8;
        elseif PolicyFlag==3, f(1:J,t)=.666*b(:,t);
    end
    h(:,t) = b(:,t).*rvec.*f(:,t)/2; %scaled harvest term
    Htime(:,t) = (2*h(:,t)./rvec).*MSYvec;
    b(:,t+1) = b(:,t)+ rvec.*b(:,t).*(1-b(:,t)/2) - h(:,t);
end

%Next, convert the "scaled" h and b to raw H and B
Bmsy_vec = Kvec/2;
Btime = b(:,1:T).*repmat(Bmsy_vec,1,T);








