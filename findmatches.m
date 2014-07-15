%% findmatches
% Find matches locates all occurances of vector a in vector b
%%

function pairs=findmatches(a,b,type)
pairs=zeros(1,length(b));

switch type
    case 'character'
        for i=1:length(a)
            where=strcmp(b,a{i});
            pairs(where)=1;
        end
    case 'number'
        for i=1:length(a)
            where=b==a(i);
            pairs(where)=1;
        end
end
pairs=pairs==1;