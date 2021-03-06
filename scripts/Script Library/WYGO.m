%% WillYieldsGoUp
% This code takes estimates of B/Bmsy, F/Fmsy, MSY, and Catch, derived from
% RAM, PRM, Catch-MSY, in order to analyze trajectories and magnitude of
% yield recovery
%Created by: D.Ovando, C. Costello


clear all
close all
pause on

%% Control Parameters
BatchName='No Forage Fish Version';

beta = 1.3; %Elasticity of costs
disc = 0.05; %Discount rate
CatchSharePrice=1.15;
CatchShareCost=.75;
G = 25; %grid for state space
bvec = linspace(0.01,2,G); %Vector of B/Bmsy values
TT=30; %years for Value Function Iteration
MakePlot=1; %make control rule plots
TopCountries= 10; %Number of countries to plot
RunProjection=1; %1: Rerun analysis 0: Use stored results
OnlyOverfished=1; %only include overfished stocks
CapF=1; %Cap F at 1.99
ComparetoSQ=1; %Make % change plots relative to current (0) or status quo(1)
CollapseEU=1; %Collapse EU stocks into one place
CountryList={'Global','USA','EU','Indonesia','Peru'};
SceneNames={'Max NPV','Status Quo','Catch Share','Max Yield'};
SubSample=[0,100]; %set to sample,number of samples

FontType= 'Helvetica';
TextSize=12;
AxesSize=14;
Resolution=850;

set(0,'DefaultAxesFontName', FontType)
set(0,'DefaultAxesFontSize', AxesSize)

set(0,'DefaultTextFontName',FontType)
set(0,'DefaultTextFontSize', TextSize)


%% Clean Up
FigureFolder=strcat('Figures/',BatchName);

ResultFolder=strcat('Results/',BatchName);

BaseFolder='Inputs/';

mkdir(FigureFolder)

[AllData]=readtable(strcat(ResultFolder,'/TotalProjectionOutput.xlsx'),'TreatAsEmpty',{'#N/A','NA'}); %Read in data

% EU=readtable([BaseFolder,'EU Countries.xlsx']);

FAOTotals=readtable([BaseFolder,'FAO Totals.xlsx']);

if CollapseEU==1
    % AllData=join(AllData,EU,'Keys','Country');
    AllData.Country(AllData.EU==1)={'EU'};
end



AllData=AllData(isnan(AllData.b0)==0,:);

AllData=AllData((AllData.b0)~=-999,:);

AllData.f0(AllData.f0==-999)=NaN;

if CapF==1
    AllData.f0(AllData.f0>2)=1.9;
end

AllData.b0(AllData.b0==-999)=NaN;

AllData.b0(isnan(AllData.b0))=NaN;

AllData.mean_r(AllData.mean_r==-999)=NaN;

AllData.msy(AllData.msy==-999)=NaN;

AllData.boa(isnan(AllData.boa))=nanmean(AllData.boa);

if OnlyOverfished==1
    AllData=AllData(AllData.b0<1,:);
end

EU=AllData.EU;

if SubSample(1)==1
    AllData=datasample(AllData,SubSample(2),'Replace',false);
end

SampleSize=size(AllData,1);

b0=nan(1,SampleSize); f0=nan(1,SampleSize); MSY=nan(1,SampleSize);

r=nan(1,SampleSize); p=nan(1,SampleSize); boa=nan(1,SampleSize); c=nan(1,SampleSize);

f1=nan(SampleSize,G); f2=nan(SampleSize,G); f3=nan(SampleSize,G); f4=nan(SampleSize,G);

%% Calculate Control Rules

if RunProjection==1
    
    if MakePlot==1
        figure
        hold on
    end
    for i=1:SampleSize %size(A1,1)Loop over stocks
        
        display(strcat(num2str((i/SampleSize)*100),'% Of Stocks Completed'))
        stock{i} = AllData.stock(i); %Stock ID
        b0(i) =  AllData.b0(i); %Staring B/Bmsy
        f0(i) =  AllData.f0(i); %Starting F/Fmsy
        MSY(i) =  AllData.msy(i); %Estiamte of MSY
        r(i) =  AllData.mean_r(i);%Estimate of growth rate
        p(i) = AllData.Price(i); %Estimate of price per metric ton
        boa(i) = AllData.boa(i); %Estimate of open access biomass
        
        c_num = p(i)*MSY(i)*(2-boa(i))*boa(i); %Numerator of cost function
        c_den = ((2-boa(i))*r(i)/2)^beta; %Denominator of cost function
        c(i) = c_num/c_den; %Marginal Costs
        AllData.MarginalCost(i)=c(i);
        
        if isnan(c(i))
            c(i)=nanmean(c);
        end
        
        Pi_MSY = p(i)*MSY(i) - c(i)*(r(i)/2)^beta; %profit at MSY
        
        %         InitialProfits=ProfitFunction(p(i),MSY(i),f0(i),b0(i),c(i),r(i),beta);
        
        [f1(i,:), f2(i,:) ,f3(i,:), f4(i,:)] = GFRM(MSY(i),r(i),p(i),c(i),f0(i),beta,disc,bvec,TT, CatchSharePrice,CatchShareCost); %for each given value of B, find the logical F to use
        
        if MakePlot==1 && i<=9
            subplot(3,3,i)
            plot(bvec,f1(i,:),'k-')
            xlabel('B/Bmsy')
            ylabel('F/Fmsy')
            title(strcat('Fishery....',(stock{i})))
        end
        if MakePlot==1 && i==10
            hold off
            print(gcf,'-dtiff',['-r',num2str(Resolution)],strcat(FigureFolder,'/Sample Policies.tiff'))
            
            close
        end
        
    end
    
    
    %% Run Trajectories
    
    %Storage space
    BioSeries= NaN(SampleSize,TT+1,4);
    
    FSeries= NaN(SampleSize,TT+1,4);
    
    ProfitSeries= NaN(SampleSize,TT+1,4);
    
    YieldSeries= NaN(SampleSize,TT+1,4);
    
    BioSeries(:,1,:)=repmat(AllData.b0(1:SampleSize),1,1,4);
    
    FSeries(:,1,:)=repmat(AllData.f0(1:SampleSize),1,1,4);
    
    InitialCatch=AllData.b0(1:SampleSize).*AllData.f0(1:SampleSize).*AllData.msy(1:SampleSize);
    
    YieldSeries(:,1,:)=repmat(InitialCatch,1,1,4);
    
    InitialProfits=ProfitFunction(AllData.Price(1:SampleSize),AllData.msy(1:SampleSize),AllData.f0(1:SampleSize),AllData.b0(1:SampleSize),c',AllData.mean_r(1:SampleSize),beta);
    
    ProfitSeries(:,1,:)=repmat(InitialProfits,1,1,4);
    
    for t=1:(TT) % Loop over time
        
        
        CurrentBio= BioSeries(:,t,:);
        
        NextBio=(BioSeries(:,t,:).*(1+repmat(r',1,1,4).*(1-BioSeries(:,t,:)/2)))-(repmat(r',1,1,4).*BioSeries(:,t,:).*FSeries(:,t,:))./2; %Calculate next years B/Bmsy
        
        for f=1:4 %Loop over management scenarios
            
            ptemp=p;
            ctemp=c;
            
            if f==3
                ptemp=p.*CatchSharePrice;
                ctemp=c.*CatchShareCost;
            end
            FPossible=eval(strcat('f',num2str(f)));
            
            FPossible=FPossible';
            
            FPossible(isnan(FPossible))=-999;
            
            FDecision=diag(interp1(bvec',FPossible,NextBio(:,:,f)','pchip')); %Find next years F, based on current control rule and next years biomass
            
            FSeries(:,t+1,f)=FDecision;
            
            ctempmat(:,:,f)=ctemp';
            
            ptempmat(:,:,f)=ptemp';
            
        end
        
        BioSeries(:,t+1,:)=NextBio; %Next Years B/Bmsy
        
        YieldSeries(:,t+1,:)=BioSeries(:,t+1,:).*FSeries(:,t+1,:).*repmat(MSY',1,1,4); %Next Year's Yields
        
        ProfitSeries(:,t+1,:)=ProfitFunction(ptempmat,repmat(MSY',1,1,4),FSeries(:,t+1,:),BioSeries(:,t+1,:),ctempmat,repmat(r',1,1,4),beta); %Next Years Profits
    end
    
end %close if

%% Make Trajectory Plots

if RunProjection==0
    load(strcat(ResultFolder,'/WYGO_Workspace.mat'))
end

% ComparetoSQ=1;


% AllData.EU=EU;
% disc=0;
if CollapseEU==1
    % AllData=join(AllData,EU,'Keys','Country');
    AllData.Country(AllData.EU==1)={'EU'};
end

% CountryList={'Global','USA','EU','Indonesia','Peru'};


DiscMatrix=repmat((1+disc).^-(0:TT),SampleSize,1,4);

DiscProfitSeries=ProfitSeries.*DiscMatrix;

TotalYieldPath=(nansum(YieldSeries,1));

TotalProfitPath=(nansum(ProfitSeries,1));

TotalDiscProfitPath=(nansum(DiscProfitSeries,1));

NPVPath=cumsum(TotalDiscProfitPath,2);

MedianBioPath=(nanmedian(BioSeries,1));

MedianFPath=(nanmedian(FSeries,1));

MeanBioPath=(nanmean(BioSeries,1));

MeanFPath=(nanmean(FSeries,1));


figure

subplot(4,1,1)
plot(squeeze(TotalYieldPath),'LineWidth',3)
ylabel('Yields (MT)')

subplot(4,1,2)
plot(squeeze(NPVPath),'LineWidth',3)
ylabel('NPV ($)')

subplot(4,1,3)
plot(squeeze(MeanBioPath),'LineWidth',3)
ylabel('Mean B/Bmsy')

subplot(4,1,4)
plot(squeeze(MeanFPath),'LineWidth',3)
ylabel('Mean F/Fmsy')
legend(SceneNames,'Location','bestoutside')

print(gcf,'-dtiff',['-r',num2str(Resolution)],strcat(FigureFolder,'/Trajectory Plots.tiff'))

figure
plot(squeeze(MeanBioPath),squeeze(MeanFPath),'LineWidth',3)
xlabel('B/Bmsy')
ylabel('F/Fmsy')
line([1,1],[0,2],'LineStyle','--','Color','black')
line([0,2],[1,1],'LineStyle','--','Color','black')
print(gcf,'-dtiff',['-r',num2str(Resolution)],strcat(FigureFolder,'/Kobe Path.tiff'))

close all

%% Perform regional analysis

Countries=unique(AllData.Country);

AllData.K=(AllData.msy.*4)./AllData.mean_r;

AllData.Finalb=BioSeries(:,end,1);

for f=1:4
    
    eval(strcat('AllData.DeltaRawBio_F',num2str(f),'=repmat(NaN,size(AllData,1),1);'))
    
    eval(strcat('AllData.Deltab_F',num2str(f),'=repmat(NaN,size(AllData,1),1);'))
    
    eval(strcat('AllData.PercBio_F',num2str(f),'=repmat(NaN,size(AllData,1),1);'))
    
    eval(strcat('AllData.DeltaRawF_F',num2str(f),'=repmat(NaN,size(AllData,1),1);'))
    
    eval(strcat('AllData.Deltaf_F',num2str(f),'=repmat(NaN,size(AllData,1),1);'))
    
    eval(strcat('AllData.PercF_F',num2str(f),'=repmat(NaN,size(AllData,1),1);'))
    
    eval(strcat('AllData.DeltaProfits_F',num2str(f),'=repmat(NaN,size(AllData,1),1);'))
    
    eval(strcat('AllData.PercProfits_F',num2str(f),'=repmat(NaN,size(AllData,1),1);'))
    
    eval(strcat('AllData.DeltaYields_F',num2str(f),'=repmat(NaN,size(AllData,1),1);'))
    
    eval(strcat('AllData.PercYields_F',num2str(f),'=repmat(NaN,size(AllData,1),1);'))
    
    if ComparetoSQ==0
        
        DeltaRawBio=(BioSeries(:,end,f).*(AllData.K(1:SampleSize)./2)-BioSeries(:,1,f).*(AllData.K(1:SampleSize)./2)); %raw Change in raw biomass
        
        Deltab=BioSeries(:,end,f)-BioSeries(:,1,f); %raw Change in relative biomass
        
        PercBio=100.*(BioSeries(:,end,f)./BioSeries(:,1,f)); %percent change in biomass
        
        DeltaRawF=(FSeries(:,end,f).*(AllData.mean_r(1:SampleSize)./2)-FSeries(:,1,f).*(AllData.mean_r(1:SampleSize)./2)); %raw change in F
        
        Deltaf=FSeries(:,end,f)-FSeries(:,1,f); %relative change in F
        
        PercF=100.*(FSeries(:,end,f)./FSeries(:,1,f)); % change in F
        
        DeltaProfits=ProfitSeries(:,end,f)-ProfitSeries(:,1,f); %raw change in profits
        
        PercProfits=100.*(ProfitSeries(:,end,f)./ProfitSeries(:,1,f)); %percent change in profits
        
        DeltaYields=YieldSeries(:,end,f)-YieldSeries(:,1,f); %raw change in profits
        
        PercYields=100.*(YieldSeries(:,end,f)./YieldSeries(:,1,f)); %percent change in profits
        
        
        RelativeTo='Current';
        
        
    else
        
        RelativeTo='BAU';
        
        DeltaRawBio=(BioSeries(:,end,f).*(AllData.K(1:SampleSize)./2)-BioSeries(:,end,2).*(AllData.K(1:SampleSize)./2)); %raw Change in raw biomass
        
        Deltab=BioSeries(:,end,f)-BioSeries(:,end,2); %raw Change in relative biomass
        
        PercBio=100.*(BioSeries(:,end,f)./BioSeries(:,end,2)-1); %percent change in biomass
        
        DeltaRawF=(FSeries(:,end,f).*(AllData.mean_r(1:SampleSize)./2)-FSeries(:,end,2).*(AllData.mean_r(1:SampleSize)./2)); %raw change in F
        
        Deltaf=FSeries(:,end,f)-FSeries(:,end,2); %relative change in F
        
        PercF=100.*(FSeries(:,end,f)./FSeries(:,end,2)-1); % change in F
        
        DeltaProfits=ProfitSeries(:,end,f)-ProfitSeries(:,end,2); %raw change in profits
        
        PercProfits=100.*(ProfitSeries(:,end,f)./max(ProfitSeries(:,end,2),.001)-1); %percent change in profits
        
        DeltaYields=YieldSeries(:,end,f)-YieldSeries(:,end,2); %raw change in profits
        
        PercYields=100.*(YieldSeries(:,end,f)./YieldSeries(:,end,2)-1); %percent change in profits
    end
    
    eval(strcat('AllData.DeltaRawBio_F',num2str(f),'(1:SampleSize)=DeltaRawBio;'))
    
    eval(strcat('AllData.Deltab_F',num2str(f),'(1:SampleSize)=Deltab;'))
    
    eval(strcat('AllData.PercBio_F',num2str(f),'(1:SampleSize)=PercBio;'))
    
    eval(strcat('AllData.DeltaRawF_F',num2str(f),'(1:SampleSize)=DeltaRawF;'))
    
    eval(strcat('AllData.Deltaf_F',num2str(f),'(1:SampleSize)=Deltaf;'))
    
    eval(strcat('AllData.PercF_F',num2str(f),'(1:SampleSize)=PercF;'))
    
    eval(strcat('AllData.DeltaProfits_F',num2str(f),'(1:SampleSize)=DeltaProfits;'))
    
    eval(strcat('AllData.PercProfits_F',num2str(f),'(1:SampleSize)=PercProfits;'))
    
    eval(strcat('AllData.DeltaYields_F',num2str(f),'(1:SampleSize)=DeltaYields;'))
    
    eval(strcat('AllData.PercYields_F',num2str(f),'(1:SampleSize)=PercYields;'))
    
end


%% Make Country Snapshot Graphs


OriginalAllData=AllData;

% AllData.catch0=AllData.b0.*AllData.f0.*AllData.msy;

AllData.y0=AllData.catch0./AllData.msy;


% AllData=AllData(strcmp(AllData.Country,'Un. Sov. Soc. Rep.')==0,:);

CountrySums = varfun(@sum, AllData, ...
    'InputVariables', {'msy','catch0','DeltaRawBio_F1','DeltaRawF_F1','DeltaProfits_F1','DeltaYields_F1'},...
    'GroupingVariables',{'Country'});

CountryMeans = varfun(@mean, AllData, ...
    'InputVariables', {'msy','b0','f0','y0','PercBio_F1','PercF_F1','PercProfits_F1','PercYields_F1'},...
    'GroupingVariables',{'Country'});

CountryMedians = varfun(@median, AllData, ...
    'InputVariables', {'msy','b0','f0','y0','PercBio_F1','PercF_F1','PercProfits_F1','PercYields_F1'},...
    'GroupingVariables',{'Country'});

CountrySD = varfun(@std, AllData, ...
    'InputVariables', {'msy','b0','f0','y0','PercBio_F1','PercF_F1','PercProfits_F1','DeltaRawBio_F1','DeltaRawF_F1','DeltaProfits_F1','PercYields_F1','DeltaYields_F1'},...
    'GroupingVariables',{'Country'});


CountryMatrix=join(CountrySums,CountryMeans,'Keys','Country');

CountryMatrix = sortrows(CountryMatrix,'sum_msy','descend');

CountryMatrix=join(CountryMatrix,CountryMedians,'Keys','Country');

CountryMatrix=join(CountryMatrix,CountrySD,'Keys','Country');

writetable(CountryMatrix,strcat(ResultFolder,'/Country Matrix.csv'))

writetable(AllData,strcat(ResultFolder,'/All Data.csv'))

if isempty(CountryList)
    CountryMatrix=CountryMatrix(1:TopCountries,:);
else
    Where=findmatches(CountryList,CountryMatrix.Country,'character');
    CountryMatrix=CountryMatrix(Where,:);
end

%% Make Country Time Series


if isempty(CountryList)
    PlotCountries=unique(CountryMatrix.Country);
else
    PlotCountries=CountryList;
end

clear CountryTraj

CountryTraj.Country=PlotCountries;

TrajColors=jet(length(PlotCountries));

CountryROI=table(PlotCountries','VariableNames',{'Country'});

ROIDist=NaN(2000,length(PlotCountries));

close all

rMatrix=repmat(r',1,TT+1);

for f=1:4
    
    
    figure
    
    for c=1:length(PlotCountries)
        
        if (strcmp(PlotCountries{c},'Global'))
            Where=1:size(AllData,1);
        else
            Where=find(strcmp(AllData.Country,PlotCountries{c}));
        end
        
        Fisheries=AllData.stock(Where);
        
        MSYTraj=repmat(AllData.msy(Where),1,TT+1);
        
        CountryTraj.YieldTotal(:,c,f)=nansum(YieldSeries(Where,:,f));
        
        CountryTraj.YieldTraj(:,c,f)=nanmedian(YieldSeries(Where,:,f));
        
        CountryTraj.YvMSYTraj(:,c,f)=nanmedian(YieldSeries(Where,:,f)./MSYTraj);
        
        CountryTraj.YvMSYSQTraj(:,c,f)=nanmedian(YieldSeries(Where,:,2)./MSYTraj);
        
        CountryTraj.ProfitTotal(:,c,f)=nansum(ProfitSeries(Where,:,f));
        
        CountryTraj.ProfitTraj(:,c,f)=nanmedian(ProfitSeries(Where,:,f));
        
        CountryTraj.BvBmsyTraj(:,c,f)=nanmedian(BioSeries(Where,:,f));
        
        CountryTraj.BvBmsySQTraj(:,c,f)=nanmedian(BioSeries(Where,:,2));
        
        CountryTraj.BvBmsyTotal(:,c,f)=nansum((2.*BioSeries(Where,:,f).*MSYTraj)./rMatrix(Where,:));
        
        
        CountryTraj.FvFmsyTraj(:,c,f)=nanmedian(FSeries(Where,:,f));
        
        CountryTraj.YieldQuant(:,:,c,f)=(quantile(YieldSeries(Where,:,f),[0.25,0.75],1)-repmat(CountryTraj.YieldTraj(:,c,f)',2,1))';
        
        CountryTraj.YvMSYQuant(:,:,c,f)=(quantile(YieldSeries(Where,:,f)./MSYTraj,[0.25,0.75],1)-repmat(CountryTraj.YvMSYTraj(:,c,f)',2,1))';
        
        CountryTraj.BvBmsyQuant(:,:,c,f)=(quantile(BioSeries(Where,:,f),[0.25,0.75],1)-repmat(CountryTraj.BvBmsyTraj(:,c,f)',2,1))';
        
        CountryTraj.ProfitQuant(:,:,c,f)=(quantile(ProfitSeries(Where,:,f),[0.25,0.75],1)-repmat(CountryTraj.ProfitTraj(:,c,f)',2,1))';
        
        CountryTraj.FvFmsyQuant(:,:,c,f)=(quantile(FSeries(Where,:,f),[0.25,0.75],1)-repmat(CountryTraj.FvFmsyTraj(:,c,f)',2,1))';
        
        DiscMatrix=repmat((1+disc).^-(0:TT),length(Where),1);
        
        ROI=cumsum(DiscMatrix.*(ProfitSeries(Where,:,f)-ProfitSeries(Where,:,2)),2);
        
        CountryROI.TotalROI(c,f)=sum(ROI(:,end));
        
        CountryROI.MeanROI(c,f)=mean(ROI(:,end));
        
        CountryROI.MedianROI(c,f)=median(ROI(:,end));
        
        CountryROI.StdROI(c,f)=std(ROI(:,end));
        
        CountryROI.LowerQuantROI(c,f)=quantile(ROI(:,end),0.25);
        
        CountryROI.UpperQuantileROI(c,f)=quantile(ROI(:,end),0.75);
        
        
        [IndividualROI, Order]=(sort(ROI(:,end),'descend'));
        
        ROIDist(1:length(IndividualROI),c)=ROI(:,end);
        
        IndividualROI=cumsum(IndividualROI);
        
        Fisheries=Fisheries(Order);
        
        
        subplot(ceil(length(PlotCountries)/2),2,c)
        plot(IndividualROI,'o','MarkerFaceColor',TrajColors(c,:),'MarkerEdgeColor',TrajColors(c,:),'MarkerSize',10)
        title(PlotCountries{c})
        ylabel('Cumulative ROI')
        xlabel('Fishery')
        set(gca,'XTickLabel',Fisheries)
    end
    
    print(gcf,'-dtiff',[FigureFolder,'/Scen',num2str(f),'ROI Distribution.tiff'])
    close
    
end %Scenario Loop


close
figure
boxplot(ROIDist,'colors',TrajColors,'Labels',PlotCountries)
print(gcf,'-dtiff',[FigureFolder,'/ROI Boxplot.tiff'])
close


close all
PlotVars={'Yield','YvMSY','Profit','BvBmsy','FvFmsy'};

for v=1:length(PlotVars)
    
    set(0,'DefaultAxesFontSize', AxesSize)
    
    set(0,'DefaultTextFontSize', TextSize)
    
    
    TempPlot=eval(strcat('CountryTraj.',PlotVars{v},'Traj(:,:,1)'));
    TempQuant=eval(strcat('CountryTraj.',PlotVars{v},'Quant(:,:,1)'));
    
    TempPlot(isnan(TempPlot))=0;
    
    TempQuant(isnan(TempQuant))=0;
    
    
    if (strcmp(PlotVars{v},'Yield') || strcmp(PlotVars{v},'Profit'))
        TempTotal=eval(strcat('CountryTraj.',PlotVars{v},'Total(:,:,1)'));
        
        
        figure
        plot(1:(TT+1),TempTotal,'LineWidth',2)
        colormap jet
        l=legend(PlotCountries,'Location','BestOutside','FontSize',8);
        set(l, 'Box', 'off')
        xlabel('Years')
        ylabel(['Median ',PlotVars{v}])
        print(gcf,'-dtiff',[FigureFolder,'/Group Total ', PlotVars{v},' Trajectory.tiff'])
        close all
        
    end
    figure
    boundedline(1:(TT+1),TempPlot,abs(TempQuant),'alpha','cmap',TrajColors,'transparency',0.4)
    xlim([0,TT+1])
    l=legend(PlotCountries,'Location','BestOutside','FontSize',8);
    set(l, 'Box', 'off')
    xlabel('Years')
    ylabel(PlotVars{v})
    print(gcf,'-dtiff',[FigureFolder,'/Group ', PlotVars{v},' Trajectory.tiff'])
    close all
    
    figure
    plot(1:(TT+1),TempPlot,'LineWidth',2)
    colormap jet
    l=legend(PlotCountries,'Location','BestOutside','FontSize',8);
    set(l, 'Box', 'off')
    xlabel('Years')
    ylabel(PlotVars{v})
    print(gcf,'-dtiff',[FigureFolder,'/Group Median ', PlotVars{v},' Trajectory.tiff'])
    close all
    
    figure
    set(0,'DefaultAxesFontSize', 8)
    
    set(0,'DefaultTextFontSize', 6)
    for c=1:length(PlotCountries)
        
        TempPlot=squeeze(eval(strcat('CountryTraj.',PlotVars{v},'Traj(:,c,1)')));
        TempQuant=squeeze(eval(strcat('CountryTraj.',PlotVars{v},'Quant(:,:,c,1)')));
        
        TempPlot(isnan(TempPlot))=0;
        
        TempQuant(isnan(TempQuant))=0;
        subplot(ceil(length(PlotCountries)/2),2,c)
        boundedline((1:(TT+1))',TempPlot,abs(TempQuant),'alpha','cmap',TrajColors(c,:),'transparency',0.6)
        
        
        title(PlotCountries{c})
        if c==length(PlotCountries)
            xlabel('Years')
        end
        if c==round(length(PlotCountries)/2)
            ylabel(PlotVars{v})
        end
        
    end
    print(gcf,'-dtiff',[FigureFolder,'/Subplot ', PlotVars{v},' Trajectory.tiff'])
    close all
    
    SceneNames={'Max NPV','Status Quo','Catch Share'};
    
    figure
    set(0,'DefaultAxesFontSize', 10)
    
    set(0,'DefaultTextFontSize', 8)
    for c=1:length(PlotCountries)
        
        TempPlot=squeeze(eval(strcat('CountryTraj.',PlotVars{v},'Traj(:,c,:)')));
        
        TempPlot=100.*(TempPlot./repmat(TempPlot(1,:),size(TempPlot,1),1)-1);
        
        
        TempQuant=squeeze(eval(strcat('CountryTraj.',PlotVars{v},'Quant(:,:,c,:)')));
        TempPlot(isnan(TempPlot))=0;
        
        TempQuant(isnan(TempQuant))=0;
        
        subplot(ceil(length(PlotCountries)/2),2,c)
        
        set(gca, 'ColorOrder', jet(4), 'NextPlot', 'replacechildren');
        plot((1:(TT+1))',TempPlot','LineWidth',3)
        xlim([0,TT+5])
        if c==length(PlotCountries)
            legend(SceneNames,'Location',[.65,.2,.1,.1])
        end
        title(PlotCountries{c})
        if c==length(PlotCountries)
            xlabel('Years')
        end
        if c==round(length(PlotCountries)/2)
            ylabel(['% Change in ',PlotVars{v},' From SQ'])
        end
        
    end
    
    print(gcf,'-dtiff',[FigureFolder,'/Subplot ', PlotVars{v},' Scenario Trajectory.tiff'])
    
    
    set(0,'DefaultAxesFontSize', AxesSize)
    
    set(0,'DefaultTextFontSize', TextSize)
    for c=1:length(PlotCountries)
        
        figure
        hold on
        TempPlot=squeeze(eval(strcat('CountryTraj.',PlotVars{v},'Traj(:,c,:)')));
        TempPlot=TempPlot(:,1:3);
        
        
        TempPlot=100.*(TempPlot./repmat(TempPlot(1,:),size(TempPlot,1),1)-1);
        
        TempPlot(isnan(TempPlot))=0;
        
        TempTable=table(TempPlot(:,1),TempPlot(:,2),TempPlot(:,3),'VariableNames',{'MaxNPV','SQ','CatchShare'});
        
        writetable(TempTable,[ResultFolder,'/',PlotCountries{c},' ',PlotVars{v},' Series.csv'])
        
        
        
        set(gca, 'ColorOrder', jet(4), 'NextPlot', 'replacechildren');
        plot((1:(TT+1))',TempPlot','LineWidth',3)
        xlim([0,TT+5])
        legend(SceneNames,'Location','Best')
        title(PlotCountries{c})
        xlabel('Years')
        
        PlotName=PlotVars{v};
        
        if strcmp(PlotVars{v},'Yield') || strcmp(PlotVars{v},'YvMSY')
            
            PlotName='Filets';
        elseif  strcmp(PlotVars{v},'Profits')
            PlotName='$';
        elseif strcmp(PlotVars{v},'BvBmsy')
            PlotName='Fish';
        end
        
        ylabel(['% Change From Current ',PlotName])
        
        hold off
        print(gcf,'-dtiff',[FigureFolder,'/',PlotCountries{c},' ', PlotVars{v},'  Trajectory.tiff'])
        
        
        figure
        hold on
        TempPlot=squeeze(eval(strcat('CountryTraj.',PlotVars{v},'Traj(:,c,:)')));
        TempPlot=100.*(TempPlot(end,:)./(TempPlot(1,:))-1);
        
        TempPlot(isnan(TempPlot))=0;
        colors=jet(length(PlotVars));
        for i=1:length(SceneNames)
            
            bar(i,TempPlot(i),'facecolor',colors(i,:))
        end
        set(gca,'XTick',1:length(SceneNames),'XTickLabel',SceneNames)
        
        title(PlotCountries{c})
        
        PlotName=PlotVars{v};
        
        if strcmp(PlotVars{v},'Yield') || strcmp(PlotVars{v},'YvMSY')
            
            PlotName='Filets';
        elseif  strcmp(PlotVars{v},'Profits')
            PlotName='$';
        elseif strcmp(PlotVars{v},'BvBmsy')
            PlotName='Fish';
        end
        
        ylabel(['% Change From Current Median ',PlotName])
        
        hold off
        print(gcf,'-dtiff',[FigureFolder,'/',PlotCountries{c},' ', PlotVars{v},'  Barplot.tiff'])
        
        
        if strcmp(PlotVars{v},'Yield') || strcmp(PlotVars{v},'Profit') || strcmp(PlotVars{v},'BvBmsy')
            
            
            figure
            hold on
            TempPlot=squeeze(eval(strcat('CountryTraj.',PlotVars{v},'Total(:,c,:)')));
            TempPlot=100.*(TempPlot(end,:)./(TempPlot(1,:))-1);
            
            TempPlot(isnan(TempPlot))=0;
            
            TempTable=table(TempPlot(:,1),TempPlot(:,2),TempPlot(:,3),'VariableNames',{'MaxNPV','SQ','CatchShare'});
            
            writetable(TempTable,[ResultFolder,'/',PlotCountries{c},' ',PlotVars{v},' Total Series.csv'])
            
            colors=jet(length(PlotVars));
            for i=1:length(SceneNames)
                
                bar(i,TempPlot(i),'facecolor',colors(i,:))
            end
            set(gca,'XTick',1:length(SceneNames),'XTickLabel',SceneNames)
            
            title(PlotCountries{c})
            
            PlotName=PlotVars{v};
            
            if strcmp(PlotVars{v},'Yield') || strcmp(PlotVars{v},'YvMSY')
                
                PlotName='Filets';
            elseif  strcmp(PlotVars{v},'Profits')
                PlotName='$';
            elseif strcmp(PlotVars{v},'BvBmsy')
                PlotName='Fish';
            end
            
            ylabel(['% Change From Current Total ',PlotName])
            
            hold off
            print(gcf,'-dtiff',[FigureFolder,'/',PlotCountries{c},' ', PlotVars{v},'  Total Barplot.tiff'])
        end
        
        
    end
    
    
end

set(0,'DefaultAxesFontSize', AxesSize)

set(0,'DefaultTextFontSize', TextSize)



figure
hold on
colors=jet(length(PlotCountries));
for i=1:length(PlotCountries)
    bar(i,CountryROI.TotalROI(i,1).*((1+disc).^-TT),'facecolor',colors(i,:))
end
ylabel('Annuity From Acting Today ($)')
set(gca,'XTick',1:length(PlotCountries),'XTickLabel',PlotCountries)
hold off
print(gcf,'-dtiff',strcat(FigureFolder,'/Total ROI Annuity.tiff'),['-r',num2str(Resolution)])

figure
hold on
colors=jet(length(PlotCountries));
for i=1:length(PlotCountries)
    bar(i,CountryROI.MedianROI(i,1),'facecolor',colors(i,:))
end
ylabel('Median NPV ROI ($)')
set(gca,'XTick',1:length(PlotCountries),'XTickLabel',PlotCountries)
print(gcf,'-dtiff',strcat(FigureFolder,'/Median ROI.tiff'),['-r',num2str(Resolution)])


figure
boxplot(squeeze(BioSeries(:,end,:)),'labels',{'OptNPV','StatusQuo','Fmsy','CloseDown'})
ylabel('Final B/Bmsy')
print(gcf,'-dtiff',strcat(FigureFolder,'/Final b Boxplot.tiff'),['-r',num2str(Resolution)])


WhyLow=regstats(AllData.Finalb,[AllData.mean_r,AllData.b0,AllData.boa]);

figure
scatter(AllData.Finalb,WhyLow.yhat)
refline(1,0)
xlabel('Known Final b')
ylabel('Predicted Final b')


figure
ax=axes;
s=scatter((CountryMatrix.sum_DeltaRawBio_F1),(CountryMatrix.sum_DeltaYields_F1),(CountryMatrix.sum_msy)/4000,log(CountryMatrix.std_DeltaRawBio_F1+CountryMatrix.std_DeltaProfits_F1),'fill');
text(CountryMatrix.sum_DeltaRawBio_F1,(CountryMatrix.sum_DeltaYields_F1),strcat(CountryMatrix.Country),'FontSize',TextSize,'FontName',FontType)
line([0,0],get(ax,'YLim'),'LineStyle','--','Color','black')
line(get(ax,'XLim'),[0,0],'LineStyle','--','Color','black')
xlabel(['Change from ',RelativeTo,' Biomass (MT)'])
ylabel(['Change from ',RelativeTo,' Yields (MT)'])
cc=colorbar;
title(cc,'Error Magnitude')
print(gcf,'-dtiff',strcat(FigureFolder,'/Raw Yield Tradeoff.tiff'),['-r',num2str(Resolution)])


figure
ax=axes;
scatter((CountryMatrix.median_b0),(CountryMatrix.median_y0),111,CountryMatrix.std_y0,'fill');
text(CountryMatrix.median_b0.*1.1,(CountryMatrix.median_y0),strcat(CountryMatrix.Country),'FontSize',TextSize,'FontName',FontType)
ylim([0,2])
xlim([0,2])
line([1,1],[0,2],'LineStyle','--','Color','black')
line([0,2],[1,1],'LineStyle','--','Color','black')
xlabel(['Median B/Bmsy'])
ylabel(['Median Y/MSY'])
% cc=colorbar('EastOutside');
% title(cc,'STDEV of Y/MSY')
print(gcf,'-dtiff',strcat(FigureFolder,'/MSY Kobe Plot.tiff'),['-r',num2str(Resolution)])


figure
ax=axes;
scatter((CountryTraj.YvMSYSQTraj(end,:,1)),(CountryTraj.BvBmsySQTraj(end,:,1)),111,TrajColors,'fill');
text(CountryTraj.YvMSYSQTraj(end,:,1).*1.1,(CountryTraj.BvBmsySQTraj(end,:,1)),strcat(CountryList),'FontSize',TextSize,'FontName',FontType)
ylim([0,2])
xlim([0,2])
line([1,1],[0,2],'LineStyle','--','Color','black')
line([0,2],[1,1],'LineStyle','--','Color','black')
xlabel(['Future Median B/Bmsy'])
ylabel(['Future Median Y/MSY'])

print(gcf,'-dtiff',strcat(FigureFolder,'/SQ MSY Kobe Plot.tiff'),['-r',num2str(Resolution)])





figure
ax=axes;
scatter((CountryMatrix.sum_DeltaRawBio_F1),(CountryMatrix.sum_DeltaProfits_F1),(CountryMatrix.sum_msy)/4000,log(CountryMatrix.std_DeltaRawBio_F1+CountryMatrix.std_DeltaProfits_F1),'fill')
text(CountryMatrix.sum_DeltaRawBio_F1,(CountryMatrix.sum_DeltaProfits_F1),strcat(CountryMatrix.Country),'FontSize',TextSize,'FontName',FontType)
line([0,0],get(ax,'YLim'),'LineStyle','--','Color','black')
line(get(ax,'XLim'),[0,0],'LineStyle','--','Color','black')
xlabel(['Change from ',RelativeTo,' Biomass (MT)'])
ylabel(['Change from ',RelativeTo,' Profits ($)'])
cc=colorbar;
title(cc,'Error Magnitude')
print(gcf,'-dtiff',strcat(FigureFolder,'/Raw Profit Tradeoff.tiff'),['-r',num2str(Resolution)])



figure
ax=axes;
scatter((CountryMatrix.mean_PercBio_F1),(CountryMatrix.mean_PercProfits_F1),(CountryMatrix.sum_msy)/4000,log(CountryMatrix.sum_msy),'fill');
text((CountryMatrix.mean_PercBio_F1),(CountryMatrix.mean_PercProfits_F1),strcat(CountryMatrix.Country),'FontSize',TextSize,'FontName',FontType)
line(get(ax,'XLim'),[100,100],'LineStyle','--','Color','black')
line([100,100],get(ax,'YLim'),'LineStyle','--','Color','black')
xlabel(['Mean % Increase From ',RelativeTo,' B/Bmsy'])
ylabel(['Mean % Increase From ',RelativeTo,' Profits'])
cc=colorbar;
title(cc,'Log(Total MSY)')
print(gcf,'-dtiff',strcat(FigureFolder,'/Mean Percent Space Tradeoff.tiff'),['-r',num2str(Resolution)])



figure
ax=axes;
scatter((CountryMatrix.median_PercBio_F1),(CountryMatrix.median_PercProfits_F1),(CountryMatrix.sum_msy)/4000,log(CountryMatrix.sum_msy),'fill');
text((CountryMatrix.median_PercBio_F1),(CountryMatrix.median_PercProfits_F1),strcat(CountryMatrix.Country),'FontSize',TextSize,'FontName',FontType)
line(get(ax,'XLim'),[100,100],'LineStyle','--','Color','black')
line([100,100],get(ax,'YLim'),'LineStyle','--','Color','black')
xlabel(['Median % Increase From ',RelativeTo,' B/Bmsy'])
ylabel(['Median % Increase From ',RelativeTo,' Profits'])
cc=colorbar;
title(cc,'Log(Total MSY)')
print(gcf,'-dtiff',strcat(FigureFolder,'/Median Percent Space Tradeoff.tiff'),['-r',num2str(Resolution)])


AllData=OriginalAllData;

clear OriginalAllData

save(strcat(ResultFolder,'/WYGO_Workspace.mat'))


