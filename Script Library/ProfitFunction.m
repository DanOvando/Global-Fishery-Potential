function [  Profits ] = ProfitFunction( p,MSY,f,b,c,r,beta )
%Calculate Profits

Profits= p.*MSY.*f.*b-c.*(f.*r./2).^beta;

end

