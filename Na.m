load Nak.dat
x=Nak(:,1);
y=Nak(:,2)+1500;
p=zeros(20501,1);
q=zeros(20501,1);
for i=1:20501%length(x)
    if i<=2502
    p(i,1)=12000;
    q(i,1)=x(i);
    else
        p(i,1)=y(i-2185);
        q(i,1)=x(i);
    
    end 
    
end
    

plot(q/60*30/35+.75,p/1000)
xlabel('Time (min')
ylabel('Na_a (mM)')
Na=p/1000;

Time=x/60;
l=[Time,Na];
csvwrite('NaWT.csv',l)
t=table(Time,Na);
writetable(t,'NaWT.xlsx');
