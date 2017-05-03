function createfigure(X1, Y1, z1)
%CREATEFIGURE(X1, Y1, Z1)
%  X1:  scatter x
%  Y1:  scatter y
%  Z1:  scatter s

%  Auto-generated by MATLAB on 18-Mar-2014 10:14:17

% Create figure
figure1 = figure;

% Create axes
axes1 = axes('Parent',figure1);
hold(axes1,'all');

% Create scatter
scatter(X1,Y1,z1,z1,'MarkerFaceColor','flat','MarkerEdgeColor','none',...
    'LineWidth',2,...
    'Parent',axes1);

% Create colorbar
colorbar('peer',axes1);
