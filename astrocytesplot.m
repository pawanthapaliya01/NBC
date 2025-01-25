clc
close all
%load Cak.dat
%load Clk.dat
%load eetk.dat
load HCO3k.dat
load HCO3s.dat
load ip3k.dat
load hk.dat
load Kk.dat
load Ks.dat
load mk.dat
load Nak.dat
%load naexp.csv
load ko.dat
load ATP.dat

load Nas.dat
load sk.dat
load vk.dat
%load vkk.dat
load vkdas.dat
load wk.dat
load JNCX.dat
load Odap.dat
load Oamp.dat
load OEAAt.dat
load Jdap.dat
load JNbqx.dat
load Jtbo.dat
load JKk.dat
load JNak.dat
load JNAKk.dat
load JKk.dat
load JNBCK.dat
load JNHE.dat
load Hi.dat
load Ho.dat
load pHi.dat
load pHo.dat
load ENBC.dat
load ENHE.dat
%load Nadap.csv
%load naco.csv

figure(1)
subplot(4,6,1)
plot(JNAKk(:,1),JNAKk(:,2))
xlabel('time s')
ylabel('JNa/KATP-ase pA/um^2')

subplot(4,6,2)
plot(JNak(:,1),JNak(:,2))
xlabel('time s')
ylabel('JNa_a pA/um^2')

subplot(4,6,3)
plot(JKk(:,1),JKk(:,2))
xlabel('time s')
ylabel('JK_a pA/um^2')

subplot(4,6,4)
plot(HCO3k(:,1),HCO3k(:,2))
xlabel('time s')
ylabel('HCO3_a uM')

subplot(4,6,5)
plot(HCO3s(:,1),HCO3s(:,2))
xlabel('time s')
ylabel(' HCO3_o uM')

% subplot(4,6,6)
% plot(JNHE(:,1),JNHE(:,2))
% xlabel('time s')
% ylabel('JNHE_a pA/um^2')

subplot(4,6,7)
plot(JNBCK(:,1),JNBCK(:,2))
xlabel('time s')
ylabel('JNBC_a pA/um^2')

subplot(4,6,8)
plot(Kk(:,1),Kk(:,2))
xlabel('time s')
ylabel('K_a uM')

subplot(4,6,9)
plot(Ks(:,1),Ks(:,2))
xlabel('time s')
ylabel('K_o uM')

% subplot(4,6,10)
% plot(mk(:,1),mk(:,2))
% xlabel('time s')
% ylabel('mk')

subplot(4,6,11)
plot(Nak(:,1),Nak(:,2))
hold on
%plot(naexp(:,1),naexp(:,2)*1000)
xlabel('time s')
ylabel('Na_a uM')
hold on
%plot(nahippo(:,1),nahippo(:,2))
%plot(Nadap(:,1),Nadap(:,2))
hold off

subplot(4,6,12)
plot(Nas(:,1),Nas(:,2))
xlabel('time s')
ylabel('Na_o uM')
s=(2*(Kk(:,2)-max(Kk(:,2)))-(3*(min(Nak(:,2)-Nak(:,2)))));
subplot(4,6,13)
%plot(ATPWT(:,1),ATPWT(:,2),'DisplayName','WT')
hold on
plot( ATP(:,1), s(:,1),'DisplayNAme', 'NBC1')
xlabel('time s')
ylabel('ATP uM')

subplot(4,6,14)
%plot(vkk(:,1),vkk(:,2))
hold on
plot(vk(:,1),vk(:,2))
xlabel('time s')
ylabel('v_a mV')

subplot(4,6,15)
plot(pHi(:,1),pHi(:,2))
xlabel('time s')
ylabel('pH_a')

subplot(4,6,16)
plot(pHo(:,1),pHo(:,2))
xlabel('time s')
ylabel('pHo')


subplot(4,6,17)
plot(Hi(:,1),Hi(:,2))
xlabel('time s')
ylabel('H_a uM')

subplot(4,6,18)
plot(Ho(:,1),Ho(:,2))
xlabel('time s')
ylabel('H_o uM')
% 
subplot(4,6,19)
plot(ENBC(:,1),1.18*ENBC(:,2))
xlabel('time s')
ylabel('ENBC mV')
% 
% subplot(4,6,20)
% plot(ENHE(:,1),ENHE(:,2))
% xlabel('time s')
% ylabel('ENHE mV')
% % 
% subplot(4,6,21)
% plot(vkdas(:,1),vkdas(:,2))
% xlabel('time s')
% ylabel('vkdas mV')
% % 
% subplot(4,6,22)
% plot(ko(:,1),ko(:,2))
% xlabel('time s')
% ylabel('Ko uM')




