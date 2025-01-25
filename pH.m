load pHi.dat
load Nak.dat
x=pHi(:,1);
y=pHi(:,2)/max(pHi(:,2));
p=zeros(20501,1);
for i=1:20501%length(x)
    if i<=2501
    p(i,1)=1;
    else
        p(i,1)=y(i-2500);
    
    end 
    
end
    

plot(x/60,p*7.33)
xlabel('Time (min')
ylabel('pH_a')
pH_a=p*7.33;

Time=x/60;
l=[Time,pH_a];
csvwrite('pHiKO.csv',l)
t=table(Time,pH_a);
writetable(t,'pHiKO.xlsx');
