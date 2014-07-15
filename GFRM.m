function[f1,f2,f3,f4]=GFRM(MSY,r,BaseP,BaseC,f0,beta,disc,bvec,TT,CSPrice,CSCost)
%This code takes input parameters as given, and calculates different policy
%functions...one version of the policy function is the dynamic
%optimization.

%f1=max npv
%f2=status quo f0 forever
%f3=catch share
%f4=yield


delta = 1/(1+disc); %Discount parameter
% Vnew = zeros(length(bvec),1);


%This is working by looping through each time step, and at each time step
%looping over all possible biomass values

for s=1:3
    
    Vnew = zeros(length(bvec),1);

    if s==1 %Normal optimization
        p=BaseP;
        c=BaseC;
        Place=1;
    elseif s==2 %Catch share scenario
        p=BaseP.*CSPrice;
        c=BaseC.*CSCost;
        Place=3;
    elseif s==3 %Yield Maximizing Scenario

        p=1;
%         p=BaseP;
        c=0;
        Place=4;
    end
    
    for t=TT:-1:1 %Work backwords in time
        V = Vnew; %Vector of Values in the earlier time step
        for i=1:length(bvec) %Loop over biomass values
            b = bvec(i);
            [AA, BB] = fminbnd('GFRM_fun',0,1.99,[],b,p,MSY,c,r,beta,V,bvec,delta);
            Vnew(i) = -BB; % The optimized profits resulting from F
            eval(strcat('f',num2str(Place),'(i)=AA;'))% The optimized fishing mortality
            
        end
    end
    
end

f2 = f0*ones(size(bvec));
% f3 = ones(size(bvec));
% z1 = (bvec<1); f4(z1)=0;
% z2 = (bvec>=1); f4(z2)=1;

