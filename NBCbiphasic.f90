Program Main

Implicit None


integer, parameter:: dim=36,int9 = selected_int_kind(16)

double precision time, y(dim), fun(dim),var(dim)
double precision  dt
integer (kind=int9) nt,ii,iskip
!integer ii, iskip
EXTERNAL rk4
EXTERNAl Derives

open(unit=10, file='vk.dat')
open(unit=11,file= 'Kk.dat')
open(unit=12,file='Nak.dat')
!open(unit=13,file='HCO3k.dat')
open(unit=14,file='Cak.dat')
open(unit=15,file='Clk.dat')
open(unit=16,file='sk.dat')
Open(unit=17,file='hk.dat')

open(unit=18,file='ip3k.dat')
open(unit=19,file='mk.dat')
open(unit=20,file='eetk.dat')
open(unit=21,file='wk.dat')
open(unit=22,file='Ks.dat')
open(unit=23, file='Nas.dat')
!open(unit=24,file='HCO3s.dat')
open(unit=30, file='pHi.dat')
open(unit=31, file='pHo.dat')
open(unit=200,file='JNCX.dat')
open(unit=210, file='Jdap.dat')
open(unit=220, file='JNbqx.dat')
open(unit=230, file='Jtbo.dat')
open(unit=300, file='Odap.dat')
open(unit=310,file= 'Oamp.dat')
open(unit=320, file='OEAAt.dat')
open(unit=330, file='JNAKk.dat')
open(unit=340, file='JBKk.dat')
open(unit=350, file='jBKp.dat')
open(unit=360, file='JKk.dat')
open(unit=370, file='JNak.dat')
open(unit=380, file='JNBCK.dat')
open(unit=390, file='JKCC1k.dat')
open(unit=400, file='JNCC1k.dat')
open(unit=410, file='JClk.dat')
open(unit=420, file='JTRPVk.dat')
open(unit=430, file='Jserca.dat')
open(unit=440, file='JNHE.dat')
open(unit=450, file='HCO3k.dat')
open(unit=460, file='HCO3s.dat')
open(unit=470, file= 'Ho.dat')
open(unit=480, file= 'Hi.dat')
open(unit=490, file= 'ENBC.dat')
open(unit=500, file= 'ENHE.dat')
open(unit=510, file= 'vkdas.dat')
open(unit=520, file= 'ko.dat')
open(unit=530,file= 'phofit.dat')
open(unit=540, file='Naofit.dat')
open(unit=550, file='ATP.dat')
open(unit=560, file='buffer.dat')

y(1)  =-83.79d0!vk mV s92
y(2)  = 146000.0d0 !Kk uM s84
y(3)  = 12000.0d0 !Nak uM s85
y(4)  = 25000.0d0 ! HCO3k uM s86
y(5)  = 0.082d0! Cak uM 88
y(6)  = 8215.0d0! Clk uM s87
y(7)  = 480.80d0 !sk uM s91
y(8)  = 0.4086d0!hk s94
y(9)  = 0.0483d0!IP3k um 89
y(10) = 0.5635d0!uM
y(11) = 0.4471d0 !eetk um S90
y(12) = 8.349d-5!wk(-) s93
y(13) = 2900.0d0! Ks uM s81
y(14) = 152000.0d0 !Nas uM s82
y(15) = 26000.0d0 !HCO3s uM 83

y(16)=0.0d0
y(17)=0.0d0
y(18)=0.0d0
y(19)=0.0d0

y(20)=0.0d0
y(21)=0.0d0
y(22)=0.0d0
y(23)=0.0d0
y(24)=0.0d0


! y(25)=0.0d0
! y(26)=0.0d0
! y(27)=0.0d0
! y(28)=0.0d0
! y(29)=0.0d0
y(30)=7.33d0 !phi
y(31)= 7.350d0!pho


time = 0.00d0 !sec
nt=24000000000
iskip=10000000


dt=0.00000010d0! sec

DO ii = 1,nt
   time = time + dt
   !write(*,*) time
    Call Derives(y,fun,time,nt,dim,dt,var)
    Call Euler(y,fun,time,nt, y,Derives,dim,dt,var)
    !Call rk4(y,fun,time,nt,y,Derives,dim,dt,var)
    !Call trapez(y,fun,time,nt, y,Derives,dim,dt,var)
    if (time.gt.50.0d0 .and. mod(ii,iskip)==0)then
       
       Write(10,*) time, y(1)
       write(11,*) time, y(2)
       write(12,*) time, y(3)
      ! write(13,*) time, y(4)
       write(14,*) time, y(5)
       write(15,*) time, y(6)
       write(16,*) time, y(7)
       write(17,*) time, y(8)

       write(18,*) time, y(9)
       write(19,*) time, y(10)
       write(20,*) time, y(11)

       write(21,*) time, y(12)
       write(22,*) time, y(13)
       write(23,*) time, y(14)
       !write(24,*) time, y(15)
       write(30,*) time, y(30)
       write(31,*) time, y(31)
       
       write(200,*)time,  var(1)
       write(210,*) time, var(2)
       write(220,*) time, var(3)
       write(230,*) time, var(4)
       write(300,*) time, var(5)
       write(310,*) time, var(6)
       write(320,*) time, var(7)
       write(330,*) time, var(8)
       write(340,*) time, var(9)
       write(350,*) time, var(10)
       write(360,*) time, var(11)
       write(370,*) time, var(12)
       write(380,*) time, var(13)
       write(390,*) time, var(14)
       write(400,*) time, var(15)
       write(410,*) time, var(16)
       write(420,*) time, var(17)
       write(430,*) time, var(18)
       write(440,*) time, var(19)
       write(450,*) time, var(20)
       write(460,*) time, var(21)
       write(470,*) time, var(22)
       write(480,*) time, var(23)
       write(490,*) time, var(24)
       write(500,*) time, var(25)
       write(510,*) time, var(26)
       write(520,*) time, var(27)
       write(530,*), time, var(28)
       write(540,*), time, var(29)
       write(550,*), time, var(30)
       write(560,*) time, var(31)

       
    end if
        
END DO

END Program Main






subroutine Derives(y,fun,time,nt,dim,dt,var)
implicit none
integer, parameter:: int9 = selected_int_kind(16)  
integer(kind=int9) nt
integer ii, dim
double precision time, dt, y(dim),fun(dim),var(dim)
double precision vk,Kp,Cap,Nak,Kk,vi,Clk,HCO3k,Nao,Nas,Ko,Ks,HCO3s,wk,&
     Ik,Cak,hk,sk,mk,eetk,R
double precision phi
double precision Cls
double precision Rs,Rk,VRsa
double precision JKNEToSCk,JNaNEToSCk,diff
double precision EKk,ENak,EClk,ENBCk,EBKk,ETRPVk, zk,zNa,zCl,zNBc,zCa
double precision JNaKk,JNaKmax,KNak,KKs
double precision JBKk,GBKk,JBKp,VRpa
double precision JKk,GKk,JNak,GNak,JNBCk,GNBCk,JKCC1k,GKCC1k,JNKCC1k,GNKCC1k
double precision JClk,GClk,JKIRk,JNaCak,JVOCCk
double precision VKIRk,z1,z2,GKIRk,z5,z3,z4
double precision FKIRk,gammai,GNaCak,cNaCak,vNaCak
double precision GCak,vCa1k,vCa2k
double precision JIP3,Jmax,KI,Kact
double precision JERleak, PL,Jpump,Vmax,kpump
double precision ITRPVk,GTRPVk,JTRPVK,Castrk
double precision rho, rhomin,rhomax,Glumax,Glu,Gluc,Glustm
double precision Bcyt,BKend,Kex,Bex
double precision G,delta,KG
double precision v3,v6,v5,Ca3,Ca4
double precision winf,eetshift,v4,phiw,psiw
double precision HCak,gamcaik,gamcaek,eta,R0passivek
double precision minfk,epshalfk,kappak,v1TRPVk,v2TRPVk
double precision VRERcyt,kon,kinh
double precision rh,kdeg,trpvswitch,tTRPVk
double precision Veet,Cakmin,keet,RCak,kappa,rbuff
double precision gluslope,keswitch
double precision GluSwitch,Glumin,T0Glu,t2Glu,thetaRGlu
double precision thetaLGlu

!NCX
double precision INCXmax,KNCXmN,KNCXmc,etancx,Cao,ksat,&
     JNCX,F,Aa,pi,voli,vola,convg,vstm,kostm,Carest,Caorest,&
     Cakrest,skrest,Naorest,Nakrest,Narest,Korest,Kkrest, Krest,&
     Naofit

!NMDA DAp
double precision ARdap,A2Rdap,Odap,Ddap,Rdap,&
     ENMDANak,ENMDAKk,ENMDACak,JNMDANaK,JNMDAKk,&
     JNMDACak,JNMDA,kondap,koffdap,betadap,alphadap,kddap,krdap,INMDAmax,&
     b1,b2,kc1c2,kc2c1,kc2c3,kc3c2,kc3o,koc3,&
     kc3d,kdc3,fact1,konnmda,koffnmda,betanmda,alphanmda,kdnmda,krnmda,&
     nmda, nmdastm,ENMDA
!ampa nbqx
double precision C1amp,C2amp,Oamp,D1amp,D2amp,C0amp,&
     Rb,Rmu1,Rmu2,Rd,Rr,Ro,Rc,Kc0c11,kc1c01,kc1c21,kc2c11,kc2o1,koc21,&
     kc1d11,kd1c11,kc2d21,kd2c21,Iampmax,fact2,JampaNak,JampaKk,JampaCak,&
     Jampa,EampaNak,EampaKk,EampaCak,Iampamax,Eampa
     
! EAAT TBO
! double precision C1EAAT,C2EAAT,OEAAT,D1EAAT,D2EAAT,C0EAAT,&
!      Rbt,Rmu1t,Rmu2t,Rdt,Rrt,Rot,Rct,Kc0c11t,kc1c01t,kc1c21t,kc2c11t,kc2o1t,koc21t,&
!      kc1d11t,kd1c11t,kc2d21t,kd2c21t,IEAATmax,fact3,JEAATNak,JEAATKk,JEAATglu,&
!      JEAAT,EEAATNak,EEAATKk,EEAATglu
!EAAT
double precision IGluTmax,KGluTmN,KGluTmk,KGluTmg,JGluT
!ph
double precision pHint,pHo,phofit, Hi,Ho,ENHE,JNHE,betai,betao,phosig,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,&
     p11,p21,p31,p41,p51,p61,p71,p81,p91,p101,p12,p22,p32,p42,p52,p62,p72,p82,p92,p102,&
     p13,p23,p33,p43,p53,p63,p73,p83,p93,p103
double precision alphas, betas,EClHNO3,JCLHNO3,ATPNHE,EHCO3,Eh,JHCO3,Jh,vkdas,phext,ATP,EHCO3cl,JHCO3cl

vk    = y(1)    ! Membrane potential of the astrocyt(Ac) mV s92
Kk    = y(2)    ! K+ concentration in the astrocyte uM s84
Nak   = y(3)    ! Na+ concentration in the astrocytes(uM) s85
!write(*,*) Nak
ATP=2.0d0*abs(Kk-152000.0d0)-3.0d0*abs(5422.0d0-Nak) !
var(30)=ATP
HCO3k =y(4)    ! HCO3-concetration in the astrocytes(uM): s86
Cak   = y(5)    ! The astrocytic cytosolic Ca2+ concentation(uM) s88
Clk   = y(6)    ! Cl- concentration in the astrocyts via electroneutrality(uM) s87

sk    = y(7)    ! The ca2+ concentation in the astrocytic endoplasmic reciculum(ER)
hk    = y(8)    ! The inactivation variable hk of the astrocytic IP3R channel(-) s94
Ik    = y(9)    ! The astrocytic inositol triphophate(IP3) concentration(uM):s89
mk    = y(10)   ! The open probability of the transient receptor potential(-)

eetk  = y(11)   ! The astrocytic eposyeicosatrienoic acid(EET) concentration(uM) s90
wk=y(12)
Ks    = y(13)   ! K+ concentration in the synaptic cleft(sc)(uM):s81
Nas   = y(14)   ! Na+ concentration in the SC(uM):s82
HCO3s =y(15)   ! HCO3-concentration in the SC(UM):s83 1.80d0*1.530d0* 3.4 for beta stimulus
!write(*,*) HCO3s


ARdap = y(16)
A2Rdap = y(17)
Odap = y(18)
var(5)=Odap
Ddap = y(19)

Rdap = 1.0d0-ARdap-A2Rdap-Odap-Ddap! R

C1amp = y(20)
C2amp = y(21)
Oamp = y(22)
var(6)=Oamp
D1amp = y(23)
D2amp=y(24)
C0amp = 1.0d0-C1amp-C2amp-Oamp-D1amp-D2amp


! C1EAAT = y(25)
! C2EAAT = y(26)
! OEAAT = y(27)
! var(7)=OEAAT
! D1EAAT = y(28)
! D2EAAT=y(29)
! C0EAAT = 1.0d0-C1EAAT-C2EAAT-OEAAT-D1EAAT-D2EAAT


!PH
phint=y(30)!3.10d0+log(HCO3k/(24.0d0*48.2d0))!y(30)
!write(*,*) phint

!write(*,*) Hi
!var(20)=phint
pho=y(31)!3.10d0+log(HCO3s/(24.0d0*48.2d0))!y(31)
!var(21)=pho
!if (time .gt. 100.0d0 .and. time .lt. 160.0d0) then








 !p1 =   3.813d-19 ! %(3.165e-19, 4.461e-19)
  
 !p2 =  -1.376d-15! % (-1.595e-15, -1.157e-15)
       
 !p3 =   2.015d-12! % (1.715e-12, 2.315e-12)
       
       
 !p4 =  -1.529d-09 ! %(-1.743e-09, -1.316e-09)
       
 !p5 =   6.351d-07 !% (5.512e-07, 7.19e-07)
       
 !p6 =  -0.0001385d0! % (-0.0001565, -0.0001206)
       
 !p7 =     0.01361d0 !% (0.01171, 0.01551)
       
!p8 =       6.925d0 !% (6.85, 7)

p1=-4.7273d-26
p2=2.47264d-22
p3=-5.4275d-19
p4=6.4515d-16
p5=-4.4486d-13
p6=1.7487d-10
p7= -3.4571d-08
p8=2.0687e-06
p9= 1.0069e-04
p10=0.9999


if (time .le. 120.0d0 ) then
   phofit=7.350d0!34943d0
elseif (time .gt. 120.0d0 .and. time .le.1127.2999952d0 ) then
!time =419.0d0
!phofit =7.350d0* (p1*time**9.0d0 + p2*time**8.0d0 + p3*time**7.0d0 + p4*time**6.0d0 + p5*time**5.0d0 + &
   !p6*time**4.0d0 + p7*time**3.0d0+p8*time**2.0d0 +p9*time+ p10)
   
   phofit=(p1*(time-120.0d0)**9.0d0+p2*(time-120.0d0)**8.0d0+p3*(time-120.0d0)**7.0d0+&
        p4*(time-120.0d0)**6.0d0+p5*(time-120.0d0)**5.0d0+p6*(time-120.0d0)**4.0d0+&
        p7*(time-120.0d0)**3.0d0+p8*(time-120.0d0)**2.0d0+p9*(time-120.0d0)+p10)*7.350d0!*7.34943d0
else
   phofit=7.34716767d0

end if

!if (time .le. 80.0d0 ) then
!   phofit=7.350d0!34943d0
!elseif (time .gt. 80.0d0 ) then
!   phofit=7.220d0
!end if

!write(*,*) phofit
!phofit=7.350d0
var(28)=phofit
!pho=y(31)
!Mathematical modeling of acid-base physiology
!Rossana Occhipintia,* and Walter F. Borona,b
HCO3s=10.0d0**(pho-6.1d0)*0.03d0*1000.0d0*39.0d0!48.6d0!*5000.0d0!26000.0d0 Co2=s*pCo2=0.03mM/mmHg *40 mmHG pK=6.1
!write(*,*) HCO3s
var(21)=HCO3s
HCO3k=HCO3s*(10.0d0)**(phint-pho)
!write(*,*) HCO3k
var(20)=HCO3k
!if (time .gt. 40.0d0) then
   
   !write(*,*) HCO3k
!end if
!else
   !pho=7.4d0!20.530d0!0.01d0
!end if

 !if (time .gt. 100.0d0 .and. time .lt.160.0d0) then




Ho=24.0d0*39.0d0/HCO3s!48.6(10.0d0)**(-pho)*1.0d6!HCO3s/(10.0d0**(pho-phint)+1.0d0)*10.0d0**(-3.0d0)!
var(22)=Ho
!write(*,*) Ho
Hi=24.0d0*39.0d0/HCO3k!48.6!Ho*10.0d0**(pho-phint)!Ho*HCO3s/HCO3k(10.0d0)**(-phint)*1.0d6!Ho*10.0d0**(pho-phint)!

var(23)=Hi
 !else
    !Ho= 0.630d0
!end if
!write(*,*) Hi
!betai=0.0d0!HCO3k/phint !107.79d0* (phint)**2.0d0 - 1522.2d0* (pHint) + 5396.9d0!HCO3k/phint !2000

!write(*,*) betai!if(time .lt.100.0d0) then
   !betao=7.0d0!2738.700d0
!if(time .ge. 100.0d0 .and. time .le. 160.0d0) then
betao=10000.0d0+2.30*(HCO3k)/phint!(Hi-Ho)/(pho-phint)*(HCO3s*10.0d0**(6.10d0-phint))/(1.0d0+10.0d0**(6.10d0-pho))!25000.0d0+2.30d0*(HCO3k+Hi)/phint !1000.0d0
var(31)=betao
!write(*,*) betao
!else
!betao=7800.0d0!10.5d0!2738.70d0
!end if
!external
!Kp  = 3040.0d0   ! Perivascular potassium concentration uM external
!for test
vkdas=26.70d0*log((ks+Nas*0.01d0)/(kk+0.01d0*Nak))
var(26)=vk
Kp=1000.0d0
Cap = 1846.40d0  ! Perivascular Calcium concentration uM external
Naorest=149200.0d0 ! Extracellular Na+ concentration in the unit of uM
Nakrest=18730.0d0  !Astrocyte Na+ concentration in hte unit of uM
Narest=167930.0d0 !sum of extra and astrocyte Na+ concentration at rest
Nao =152000.0d0!164000.0d0-NaK!152000.0d0!!Narest-Nak! 149200.0d0 ! Na+ concentration in the extracellular space uM
Korest=1934.0d0 ! Extracellular potassium concentration at rest in units of uM
Kkrest=92680.0d0! Astrocyte potassium concentration at rest in units of uM
Krest=94614.0d0 ! sum of extra and astrocyte K+ concentration at rest
Ko  =2900.0d0!-Kk!3834.0d0!krest-Kk!2934.0d0! K+ concentration in the extracellular space uM
!if (time .lt. 80.0d0) then
!   kostm =0.0d0
!   phext=7.350d0
!elseif (time .ge.50.0d0 ) then
   !kostm=(-14.0d0*1.0/(1.0+exp((-52.0d0-(time-80.0))/15.0d0))*exp(-(time-300.0d0)/99.0d0))
!   kostm=time*5.0d0*1000.0d0*3.0d0/1.056d0*exp(-0.086d0*(time))!!/(1.0d0+exp((5.0d0-(time-100.0d0))/10.0d0))*exp((time-100.0d0)/100.0d0) !time*16.0d0
!   phext=7.35d0!*(exp(-0.00009d0*(time-120.0d0)))
!else
!   kostm=0.0d0!!time*exp(0.026d0*130.0d0)*exp(-0.026d0*(time-130.0d0))!Ko*(1.0d0+0.000036d0)**130.0d0*(1.0d0-0.00003660d0*time)**(time)!time*16.0d0*1000.0d0*8/1.057313818186789d0*exp(-0.08603407566155757*time)!time*16.0d0/(1.0d0+exp((5.0d0-(time-120.0d0))/10.0d0))*exp((time-120.0d0)/100.0d0)
!   phext=7.350d0

!end if

!*******

p11= -1.7213d-22
p21= 1.3143d-18
p31= -4.2030d-15
 p41=   7.3038d-12
p51=-7.4564d-09
p61=4.48029d-06
p71=-0.00147681d0
p81=    0.2147d0
p91=    -4.8618d0
p101=   2.5716d+03


!if (time .le. 160.0d0  .or. time .gt.1180.0d0) then
   !kostm=2875.2861d0
   !kostm=2541.6d0
  
!elseif(time .gt. 160.0d0) then
!time=100.0d0
   
!kostm =(p11*(time-113.2d0)**9.0d0+p21*(time-113.2d0)**8.0d0+p31*(time-113.2d0)**7.0d0+&
!     p41*(time-113.2d0)**6.0d0+p51*(time-113.2d0)**5.0d0+p61*(time-113.2d0)**4.0d0+&
!     p71*(time-113.2d0)**3.0d0+p81*(time-113.2d0)**2.0d0+p91*(time-113.2d0)+p101)


!end if




!if (time .le.80.0d0) then
!   kostm=2500.0d0
!elseif (time.gt.  80.0d0 .and. time .le.200.0d0) then
!     kostm=-(exp(-0.005d0*(time-80.0d0))-2.0d0)*2500.0d0
!elseif (time .gt.200.0d0 .and. time .le.800.0d0) then
!   kostm=(exp(-0.003d0*(time-200.0d0))/1.33d0+0.7d0)*2500.0d0
!else
!   kostm=-(exp(-0.00199d0*(time-800.0d0))/5.1d0-1.02d0)*2500.0d0
!end if

!write(*,*) kostm
!if (time .le.80.0d0) then
!   kostm=2500.0d0
!elseif (time.gt.  80.0d0) then
 !  kostm=3500.0d0!-(exp(-0.005d0*(time-80.0d0))-2.0d0)*2500.0d0
!end if

!kostm=2900.0d0

if (time .le.80.0d0) then
   kostm=2500.0d0
elseif (time.gt.  80.0d0 .and. time .le.320.0d0) then
   kostm=-(exp(-0.002675d0*(time-80.0d0))-2.0d0)*2500.0d0
elseif (time .gt.320.0d0 .and. time .le.615.0d0) then
   kostm=(exp(-0.0052d0*(time-320))/1.350+0.73d0)*2500.0d0
else
   kostm=-(exp(-0.00169d0*(time-615.0d0))/7.6d0-1.02d0)*2500.0d0
end if



Ko=kostm
var(27)=Ko

p12=-1.1906d-26
 p22=5.045d-23

 p32= -8.122d-20
 p42= 5.6637d-17
  
p52=-7.006d-15

p62=-1.3156d-11
p72= 7.521d-09
p82= -1.326d-06
p92= 3.51d-06
 p102=1.0019d0

if (time .le.80.0d0 .or. time .gt.800.0d0) then
   Naofit=157299.0d0
elseif (time .ge. 80.d0) then   
Naofit=(p12*(time-80.0d0)**9.0d0+p22*(time-80.0d0)**8.0d0+p32*(time-80.0d0)**7.0d0+&
     p42*(time-80.0d0)**6.0d0+p52*(time-80.0d0)**5.0d0+p62*(time-80.0d0)**4.0d0+&
     p72*(time-80.0d0)**3.0d0+p82*(time-80.0d0)**2.0d0+p92*(time-80.0d0)+p102)*157000.0d0
end if

!if (time .le.80.0d0) then
!   Naofit=157000.0d0
!elseif (time .ge. 80.d0) then
!   Naofit=155000.0d0
!end if
var(29)=Naofit
!Nao=152.0d0
Nao=Naofit
vi  = -34.68d0   ! Membrane potential of SMC mV section 6 s133
R  = 22.92d-6   ! Vessel radius m s213
Caorest=2000.0d0 ! Extracellular Calcium conentrati0n at resting membrane potential in untis of uM
Cakrest=0.1447d0 !Astrocyte Caclcium concentration at resting membrane potential in units of uM
skrest=480.8d0   !ER Calcium concentration at resting membrane potential in units of uM
Carest=2480.1447d0 ! Sum of calcium concentrations at resting membrane potential i.e Caorest+Cakrest+skrest
cao=2000.0d0!Carest-cak-sk!2000.0d0 ! extracellular calcium concentration
R0passivek = 20.0d-6 ! Vessel radius when passive and no stress is applied m

!Flux conversion from pA/um^2 or A/m^2 to uM/s
F=96485.0d0 ! c/mol
pi=3.14159265358979d0
vola=50.0d-18  !m^3
Voli=vola
Aa = 4.0d0*pi*(3.0d0*Voli/(4.0d0*pi))**(2.0d0/3.0d0)!m^2
!write(*,*) Aa
voli=vola
convg= Aa/(F*voli)*1.0d3  ! 1.0d3 convert pA/um^2 to uM/sec
!write(*,*) convg

! Ko stimulus by 10 mM for 100 ms
!if(time.lt.100.0d0) then
  ! kostm=0.0d0
!if (time .ge.100.0d0 .and. time .le.100.1d0) then
 !  kostm=11000.0d0
!else
 !  kostm=0.0d0!(1100.0d0-2934.0d0)*exp(-0.10d0*(time-100.0d0))!-exp(-(time-100.4))
!end if
!kostm=0.0d0
!ko=ko+kostm
!Synaptic Cleft:

! Cl_concentration in the SC via electroneutrality(uM):s97
Cls  = Nas + Ks - HCO3s

EHCO3cl=-26.7d0/2.0d0*log(Cls*HCO3s/(Clk*HCO3k))
!write(*,*) EHCO3cl
JHCO3cl=0.0d0!9.0d0*(vk-EHCO3cl)

!write(*,*) JHCO3cl
!write(*,*) Cls
!Volume ratio of SC to AC
Rs   = 2.79d-8
Rk   = 6.0d-8
!VRsa = Rs/Rk
!for test
VRsa=3.0d0!3
!Input of K+ to the SC (assuming that the SC is a small part of-
!the ECS and everything that happens to the ECS also happens to the SC)
diff       = 50.0d0      ! S^-1
JKNEtoSCk  = diff*(Ko-Ks)
JNaNEtoSCk = diff*(Nao-Nas)
phi    = 26.69950d0 ! RgT/F mV
gammai =1970.0d0 !7.350197546d0 !1970.0d0 ! Change in membrane potential by scaling factor mVuM^-1
!Ionic valence for K+,Na+,cl-,Ca2+
zK      = 1.0d0
zNa     = 1.0d0
zCl     = -1.0d0
zCa     = 2.0d0
zNBC    = -1.0d0 ! Effective valence for NBC cotransporter complex

JNakmax =2.3667d4 ! Maximum flux through the Na+/K+ ATP-ase pump uMs^-1
JNakmax=JNakmax
!for test
!JNAkmax=2.3667d3
KNak    = 10000.0d0 ! Na+/K+ATP-ase pump constant uM
!for test
!KNak=1000.0d0
KKs     = 1500.0d0  ! Na+K+ ATP-ase pump constant uM
!for test
!KKs=15000.0d0
VRpa    = 0.0010d0  ! Volume ratio between PVS and astrocyte
! for test
!VRpa=0.04d0
! Conductances- now in uMmV^-1s^-1 so consistent with other fluxes
! original conductances in mhom^-2 are comented
!converted by dividing vy R_K=6d-8 and F=9.65d4
GBKk    = 10.250d0
GKk     = 6907.770d0
GNak    = 226.940d0
GNBCk   = 130.740d0
GKCC1k  = 1.7280d0
GNKCC1k = 9.5680d0
Gclk    = 151.930d0
GTRPVk  = 3.17d-4
GNacak  = 3.160d-3
!Model estimati0n for membrane voltage KIR
z1      = 4.50d-3 !
z2      = 112.0d0
z3      = 4.20d-4
!Model estimation for the KIR channel conductance
z4      = 12.60d0
z5      = -7.4d-2
FKIRk   = 750.0d0   ! Scaling factor of K+ efflux through the KIR channel uMmV^-1s^-1
cNacak  = 0.50d0    ! Half-point for activation of NA+/ca2+ exchange uM
vNaCak  = -30.0d0   ! Reversal potential for the NA+Ca2+ exchanger
GCak    = 1.290d-3  ! VOCC whole cell conductance
vCa1k   = 100.0d0   ! VOCC reversal potential 100mV
vCa2k   = -24.0d0   ! Half point of the VOCC acitivation sigmoidal mV
RCak    = 8.50d0    ! Maximum slope of the VOCC atctivation sigmoidal mV
Jmax    = 2880.0d0  ! Maximum rate of Ca2+ through the IP3 mediated channel
KI      = 0.030d0   ! Disassociation constant for IP3 binding to an IP3R
Kact    = 0.170d0   ! Disassociation 

!on IP3R
PL      = 0.08040d0 ! ER leak channel steady state balance constant uMs^-1
Vmax    = 20.0d0    ! Maximum rate of Ca2+ uptake pump on the ER uMs^-1
kpump   = 0.240d0   ! Ca2+ uptake pump dissociation constant uM

Castrk    = 40.0d0  ! Astrocyte membrane capacitance
rhomin    = 0.10d0  ! Minimum ratio of bound to unbound IP3 receptors
rhomax    = 0.70d0  ! Maximum ratio of bound to unbound IP3 receptors
GluSwitch = 1.0d0
Glumax    =1846.0d0! Maximum glutamate concentration(one vesicle) uM
Glumin    = 0.0d0   ! MInimum glutamate concentration(one vesicle) uM
t0Glu     = 100.0d0!200.0d0 ! Glutamate relsease start time
t2Glu     =100.1d0! !400.0d0 ! Glutamate release end time
!slope scaling factor
thetaRGlu  = 1.0d0
thetaLGlu  = 1.0d0
Bkend      = 40.0d0    ! Ratio of endogenous buffer concentration to dissociation constant
Kex        = 0.260d0    ! Dissociation constant of excogenous buffer uM
Bex        = 11.350d0  ! Concentration of excogenous buffer uM
delta      = 1.2350d-2 ! Ratio of the activities of the unbound and bound receptors
KG         = 8.820d0   ! G-protein dissociation constant
v4         = 8.0d0     ! Measure of the spread of winfy mV
v5         = 15.0d0    ! Determines the range of the shift of winfy as Ca2+ varies mv
v6         = -55.0d0   ! Shifts the range of winfy mV
Ca3        = 0.40d0    ! BK open porbability Ca2+ constant uM
Ca4        = 0.350d0   ! BK open probability Ca2+ constant uM
eetshift   = 2.0d0     ! Describe the EEt dependent voltage shift mVuM^-1
psiw       = 2.6640d0  ! Characteristic time for the opening of the BK channel
gamcaek    = 200.0d0   ! Ca2+ concentration constant uM
gamCaik    = 0.010d0   ! Ca2+ concentration constant uM
epshalfk   = 0.10d0    ! strain required for half activatin of the TRPV4 channel
Kappa      = 0.10d0    ! TRPVS channel constant
v1TRPVk    = 120.0d0   ! TRPV4 channel voltage gating constant
v2TRPVk    = 13.0d0    ! TRPV4 channel voltage gating constant
rbuff      = 0.050d0   ! Rate of Ca2+ buffering astrocyte enfoot compared to the astrocytes body
VRERcyt    = 0.1850d0  ! Volume ration between ER ans astrocytic cytosol
kon        = 2.0d0     ! Rate of Ca2+ binding to the inhibitory site on the IP3R uMs^-1
kinh       = 0.1d0     ! Disassciation constant of IP3R uM
rh         = 4.80d0    ! Maximum rate of IP3 production in astrocytes due to glutamate receptors uMs^-1
kdeg       = 1.250d0   ! Rate constant for IP3 degradation in astrocyte s^-1
trpvswitch = 1.0d0     !
tTRPVk     = 0.90d0    ! Characteristic time constant for mk
Veet       = 72.0d0    ! EET production rate
Cakmin     = 0.01d0!0.10d0    ! Minimum Ca2+ concentration for EET production uM
keet       = 7.20d0    ! EET degradation rate s^-1
Keswitch   = 5.50d3    ! Threshold past which glutamte is released uM
gluslope   = 0.10d3    ! Slope of glutamate sigmoidal  uM
FKIRk      = 750.0d0   ! Scaling factor of K+ efflux through the KIR channel
! glutamate concentration in the SC(uM): s95 
! glutamate concentration in the SC(uM): s95 
!if (time .ge. 400.0d0 .and. time .le. 400.2d0) then
   

 !Ko =200000.0d0!!GluSwitch*(Glumax-Glumin)*0.50d0*tanh((time-t0glu)/thetaLGlu)-&
      ! 0.50d0* tanh((time-t2glu)/thetaRGlu)+Glumin
  !   rho = rhomin + (rhomax - rhomin)/Glumax * Glu
!else
   !Ko=0.0d0
!   rho=0.0d0
!end if







!Glutamate signaling
if(time .lt. 80.0d0) then
   Glustm=0.0d0
   
elseif (time .ge. 80.0d0 .and. time .le. 80.1d0) then
   
  Glustm =1000.0d0
else
   Glustm=1000.0d0*exp(-0.012250d0*(time-t2glu))!tanh((time-t0glu)/thetaLGlu)-& 
        !tanh((time-t2glu)/thetaRGlu)+Glumin ! 0.1250d0, 0.07250d0 0.1825 exp(-0.086d0*(time-t2glu))
  !   rho = rhomin + (rhomax - rhomin)/Glumax * Glu!0.0d0!
end if
!   rho=0.0d0
Glustm=0.0d0!1000.0d0

Gluc=1200.0d0
Glu =Glustm!Glumax/2.0d0 * (1.0d0+ tanh((Ko-Keswitch)/Gluslope))+Glustm



rho = rhomin + (rhomax - rhomin)/Glumax * Glu!*0.3d-3


EKk    = phi/zK * log(Ks/Kk)     ! Nernst potentials (in mV)  !106
!write(*,*) EKK
ENak   = phi/zNa *log(Nas/Nak)  ! 107
!write(*,*) ENak
EClk   = phi/zCl * log(Cls/Clk)  ! s108
!write(*,*) EClk
ENBCk  =phi *log((Nak * HCO3k**2.0d0)/(Nas * HCO3s**2.0d0))! s109
!write(*,*) ENBCk

var(24)=ENBCk
EBKk  = phi/zK * log(Kp/Kk)! s110
!!write(*,*) EBKK
ETRPVk = phi/zCa * log(Cao/Cak) ! s126
EHCO3=-26.70d0*log(HCO3s/HCO3k)
EH=26.70d0*log(Ho/Hi)
!write(*,*) ETRPVk
!p1=!15.2d0!23.7209d0!0.27d0
!p2=!-1.92170d0!0.0d0!5.80d0!5.0d0!5.8d0
!p3=!0.27350d0!5.0d0!4.0d0
!p4=!1.30d0!*60.0d0!4.50880d0!20.0d0
!p5=!0.0d0!4.4d0
!p1 = -14.84d0  !%(-16.91, -12.77)
!p2 = -52.06d0 ! %(-64.9, -39.23)
!p3 = 15.88d0  !%(11.03, 20.74)
!p4 = 99.6d0 ! %(74.61, 124.6)
!p5 = -4.112d0! % (-4.701, -3.523)
!JNaKmax=1.50d0*JNaKmax
!if (time .ge. 40.0d0 .and. time .le. 160.0d0) then
!JNakmax=0.50d0*JNakmax*p1/(1.0d0+exp((p2-(time-40.0d0))/p3))*exp(-(time-40.0d0)/p4)
!end if
alphas=0.080d0
betas=0.01d0
!if (time .lt. 40.0d0) then.or. time .gt. 160.0d0)then
 !  JNaKmax=JNaKmax
!elseif
 !  JNakmax=0.5d0 * JNaKmax
!end if
JNaKk  =JNaKmax * Nak**1.5d0/(Nak**1.50d0 + KNak**1.50d0) * Ks/(Ks + KKs) !* 1.50d0  ! Flux through the Sodium Potassium pump s104

JNaKk=JNaKK*1.7d0! 1.380d0 !2.20d0 !1.7



   !JNaKk=-0.5d0*JNaKk*(p1*1.0/(1.0+exp((p2-(time-80.0))/p3))*exp(-(time-300.0d0)/p4)+p5)/18.0d0


   !*************
   !JNaKk=0.50d0 * JNaKk*2.33d0/(1.0d0+exp(exp((100.0d0-(time-64.0d0))/170.0d0)))*exp(exp(-(time-600.0d0)/550.0d0)) !working function

   !*******************************************


!current
JNaKk=((-1.0d0/(0.0001d0+exp(-0.000917d0*(-(time-80.0d0)*1.02d0)))*1.0d0/(1.120d0+exp(-0.019d0*((time-80.0d0)*1.06d0-80.0d0)))+1.13d0))*JNaKk!&
     !JNaKmax * Nak**1.5d0/(Nak**1.50d0 + KNak**1.50d0) * Ks/(Ks + KKs)


!end if









   
                                                                                                                                                                                                                                        

!write(*,*) (1.0d0/(1.0d0+exp((p1+p2*(time-40.0d0)/p3)))*exp(-(time-40.0d0)/p4)+p5)

var(8)=JNAKk
JBKk   =0.0d0!GBKk * wk * (vk - EBKk) ! K+ flux through the BK channel(uMs^-1) s105
var(9)=JBKK

JBKp    =0.0d0!  JBKk/VRpa! K+ influx into the PVS (uM/s)
!if (time .gt.100.0d0 .and. time .le. 100.20d0) then
 var(10)=JBKp  
JKk    =0.16d0*GKk * (vk - EKk)! K+ flux through the K+ channel(uMs^-1)s99
var(11)=JKk
!else
   !JKk = 0.0d0!
!end if
!write(*,*) JKk
JNak    =1.180d0*GNak * (vk - ENak)                                          ! Na+ flux through the Na+ channel(uMs^-1) s100
var(12)=JNak
JNBCk  =GNBCk * (vk -1.18d0* ENBCk)*3.0d0!*10.0d0!*20.0d0 !5.5! Na+ and HCO3- flux through the NBC channel (uMs^-1) s101
var(13)=JNBCk
!If (time .ge. 40.0d0) then
 !write(*,*) JNBCK
!end if
!write(*,*) JNBCk
JKCC1k  =GKCC1k * phi * log((Ks * Cls)/(Kk * Clk))*30.0d0! Cl- and K+ flux through the KCC1 channel(uMs^-1) s102
Var(14)=JKCC1k

JNKCC1k =GNKCC1k * phi * log((Nas * Ks * Cls**2.0d0)/(Nak * Kk * Clk**2.0d0)) !*0.5  ! Na+, K+ and cl- flux through the NKCC1 channel(uMs^-1) s103
var(15)=JNKCC1k

JClk    =GClk * (vk - EClk)                                            ! Cl-flux through the cl- channel(uMs^-1) s98
var(16)=Jclk

!% New fluxes
vKIRk  =0.0d0! z1 * Ks - z2
GKIRk  = 0.0d0!exp(z5 * vk + z3 * Ks - z4)
JKIRk  = 0.0d0! FKIRk * GKIRk / gammai * (vk - vKIRk)
JNaCak = 0.0d0!GNaCak * Cak / (Cak + cNaCak) * (vk - vNaCak)
JVOCCk = 0.0d0! GCak * (vk - vCa1k) / (1 + exp(-(vk - vCa2k) / RCak))

!Calcium Equations
JIP3    = 0.0d0!Jmax * ( Ik/(Ik + KI) *  Cak/(Cak + Kact) * hk)**3.0d0 * (1.0d0 - Cak/sk) ! Flux of Ca2+ thorugh the uptake pump(uMs^-1)
JERleak =0.0d0! PL * (1.0d0 - Cak/sk)                                              ! Flux of Ca2+ through the leak channel(uMs^-1) s120
Jpump  =0.0d0! Vmax * Cak**2.0d0/(Cak**2.0d0 + kpump**2.0d0)                                  ! The flux of Ca2+ through thhe uptake pump(uMs^-1) s119
var(18)= Jpump
JTRPVk  =0.0d0! GTRPVk/2.0d0 * mk * (vk - ETRPVk)                                      ! Calcium current through the TRPV4 channel(uMs^-1) 125
!JTRPVk  = ITRPVk*(zCa * Castrk * gammai)                                      ! TRPV4 flux [uM/s]
var(17)=JTRPVK
Bcyt    = 1.0d0/(1.0d0 + BKend + Kex * Bex/(Kex + Cak)**2.0d0) ! Fast Ca2+ buffering is described with in the steady stat approximatin(-)s117

G  = (rho + delta)/(KG + rho + delta)       ! The ratio of active to total G-protein due to metabotropic glutamate receptor s116
v3 = v6 - v5/2.0d0 * tanh((Cak - Ca3)/Ca4) ! Voltage associated with half open probability(mV) s114

!Parent Calcium equations
winf =  (1.0d0 + tanh((vk + eetshift * eetk - v3)/v4))  ! The equilibrium state of teh BK channel(-) s113

phiw =psiw * cosh((vk - v3)/(2.0d0 * v4))! time constant associated with the open of the BK channel(s^-1) s112

HCak  = Cak/gamcaik + Cap/gamcaek                                ! TRPV Channel open probability equations s130
eta   = (R - R0passivek)/(R0passivek)                             ! The strain on the perivascualr endfoot of the astrocte(-) s129
minfk =(1.0d0/(1.0d0 + exp(-(eta - epshalfk)/ kappa))) *&       ! The equilibrium state of the TRpV4 chanel(-) s127
           ((1.0d0/(1.0d0 + HCak)) * (HCak + tanh((vk - v1TRPVk)/ &
           v2TRPVk)))


! Na+/Ca2+(NCX) exchanger equations Osman

INCXmax=0.001d0! pA/um^2

KNCXmN=87500.0d0 !uM
KNCXmc=1380.0d0 !uM
etancx=0.350d0
ksat=0.40d0
JNCX=INCXmax*Nao**3.0d0/(KNCXmN**3.0d0+Nao**3.0d0)* Cao/(KNCXmC+Cao)*&
        ((Nak**3.0d0/Nao**3.0d0*exp(etancx*vk/26.6d0))-&
        (Cak/Cao*exp((etancx-1.0d0)*vk/26.6d0)))/(1.0d0+ksat*exp((etancx-1.0d0)*vk/26.60d0)) ! Sodium Calcium exchanger flux in unit of pA/um^2
JNCX=JNCX * 0.5d4
!
JNCX=0.0d0
var(1)=JNCX
!write(*,*) JNCX


! NMDA dap sensitive equations

! Dap sensitive Model parameters for 3.0uM Glutamate parameters


Kondap =0.290068618027045d2*0.00045d0!uM/sec
koffdap =1.536853108823587d2 !1/sec
betadap = 6.999325772825553d2 !1/sec
alphadap = 0.322429186738045d2 !1/sec
kddap = 0.933948651371395d2 !1/sec
krdap = 0.046334056559303d2 !1/sec


kc1c2 = 2.0d0*glu*kondap
kc2c1 = koffdap
kc2c3 = glu*kondap
kc3c2 = 2.0d0*koffdap
kc3o  = betadap
koc3  = alphadap
kc3d = kddap
kdc3  = krdap

!NMDA due to glutamate mediated signaling

! ! ! Reversal potential mV
ENMDANak=ENak!26.7d0 *log(Nao/Nak)
!write(*,*) ENMDANAk
ENMDAKk=EKk!26.7d0*log(Ko/Kk)
! write(*,*) ENMDAKk
ENMDACak=26.7d0/2.0d0*log(Cao/Cak)
!write(*,*) ENMDACak

b1=0.0334d0 !mV^-1
b2=0.007d0 !mV^-1
INMDAmax=150.0d0/(Aa*1.0d12) !120pA/um^2

!INMDAmax=1.7d0*INMDAmax
!write(*,*) INMDAmax
fact1=0.0d0!INMDAmax/1.50d0*Odap*b1/(1.0d0+exp(b2*vk))!*0.03d0!0.18d0!0.35d0

JNMDANak=fact1*(vk-ENMDANak)
JNMDAKk=fact1*(vk-ENMDAKk)
JNMDACak=1.0/1.0d4*fact1*(vk-ENMDACak)
JNMDA=JNMDANak-JNMDAKk+JNMDACak
!if (JNMDA .lt.1.0d-20) then
   !JNMDA =0.0d0
   !JNMDANak=0.0d0
   !JNMDAKK=0.0d0
   !JNMDACak=0.0d0
!end if
!write(*,*) JNMDA
!ENMDA=26.70d0/2.0d0*log(Nao/Nak*Ko/Kk*(Cao/Cak)**(1.0d0/7.0d0))

!JNMDA=fact1*(vk-ENMDA)
!write(*,*) ENMDA
var(2)=JNMDA
!write(*,*) JNMDA

!NBQX sensitive model parameters at 10.0uM Glutamate concentrations


Rb=0.008958226674662d4 *0.0011d0 ! 0.30d0/1000.0d0 uM^-1s^-1
Rmu1=0.000510161496685d4 !s^-1
Rmu2=0.270573342681692d4 !s^-1
Rd=0.105384667339115d4   !s^-1
Rr=0.000133536542600d4   !s^-1
Ro=2.900068049786384d4   !s^-1
Rc=0.014640686198563d4   !s^-1

kc0c11=Rb*glu
kc1c01=Rmu1
kc1c21=kc0c11
kc2c11=Rmu2
kc2o1=Ro
koc21=Rc
kc1d11=Rd
kd1c11=Rr
kc2d21=kc1d11
kd2c21=kd1c11

EampaNak=ENMDANak
EampaKk=ENMDAKk
Eampa=26.70d0/2.0d0*log(Nas/Nak*Ks/Kk)

Iampamax=500.0d0/(Aa*1.0d12) !350pA/um^2
fact2=0.0d0!Iampamax/1.50d0*Oamp*b1/(1.0d0+exp(b2*vk))!*0.03d0!!0.170d0!*0.05d0
JampaNak=fact2*(vk-EampaNak)
JampaKk=fact2*(vk-EampaKk)


!write(*,*) Eampa
Jampa=JampaNak-JampaKK
!Jampa=fact2*(vk-ENMDA)

!If(Jampa .lt. 1.0d-20) then
   !Jampa = 0.0d0
   !JampaNak=0.0d0
   !JampaKk=0.0d0
   !JampaCak=0.0d0
!end if
var(3)= Jampa

! ! TBO sensitive model parameters at 1000.0um Glutamate concentration
! ! same markov kinetic modelfor ampa  were tested to fit tbo current
! ! Reversal potential mV
! EEAATNak=26.7d0 *log(Nao**3/Nak**3)

! EEAATKk=ENMDAKk!26.7d0*log(Ko/Kk)
! !write(*,*) EEAATKk
! EEAATglu=0.0d0!26.7d0*log(gluc/Glu)


! IEAATmax=25.0d0/(Aa*1.0d12) !pA/um^2
! fact3=0.0d0! IEAATmax*OEAAT*b1/(1.0d0+exp(b2*vk))


! JEAATNak=fact3*(vk-EEAATNak)
! JEAATKk=fact3*(vk-EEAATKk)
! JEAATglu=fact3*(vk-EEAATglu)
! JEAAT=JEAATNak+JEAATKk+JEAATglu
! var(4)=JEAAT
! Rbt=0.215987916304682d2 
! Rmu1t=0.040597319592650d2
! Rmu2t=0.391544747928199d2
! Rdt=0.504343944480852d2
! Rrt=0.017042896941856d2
! Rot=7.008605865353812d2
! Rct=0.171749388264924d2


! kc0c11t=Rbt*glu
! kc1c01t=Rmu1t
! kc1c21t=kc0c11t
! kc2c11t=Rmu2t
! kc2o1t=Rot
! koc21t=Rct
! kc1d11t=Rdtsubplot(4,2,2)

! kd1c11t=Rrt
! kc2d21t=kc1d11t
! kd2c21t=kd1c11t 


IGluTmax=0.1d0!0.08625d0!0.75d0!0.15d0!0.75d0 0.22 ! pA/uM^2
KGluTmN=15000.0d0 !uM
KGluTmK=5000.0d0 !uM
KGluTmg=34.0d0 !uM
JGluT=IGluTmax* Kk/(Kk+KGluTmK)*&
         Nao**3.0d0/(Nao**3.0d0+KGluTmN**3.0d0) * &
         glu/(glu+KGluTmg)!* 7.21d3* Ho**2.140d0/(0.264**2.14d0+Ho**2.14)
!
JGluT=0.0d0

var(4)=JGluT

ENHE=26.70d0*(log((Nak*Ho)/(Nas*Hi)))!7.3 ! membrane potential can be increased if constant is changed from 1.70d0 to 3.8d0
!write(*,*) ENHE
!if (phint .lt. 6.830d0) then 
var(25)=ENHE
JNHE=GNBCK*(vk-ENHE)!*0.11d0!*0.04d0!*(1.0d0-2.0d0*exp(-2.0d0*(phint-7.330d0)))**2.0d0!*(1.d0-(pho-phint)!*exp(-0.0120d0*time))!/(1.0d0+exp(0.005d0*(phint-7.20d0))) !1.5
!else
   !JNHE=0.10d0*GNBCK*(vk-ENHE)* (1.0d0-(pho-phint)*exp(-0.0120d0))
!end if
  
!write(*,*) JNHE
var(19)=JNHE
!If (time .ge. 40.0d0) then
 !  write(*,*) JNHE
!end if
EClHNO3=0.0d0!26.70d0*log(Ho/Hi*10.0d0)

!write(*,*) EClHNO3
!!ClHNO3=0.0d0!30.0d0*(vk-ECLHNO3)!0.0d0!3.0d0*(vk-EClHNO3)
!jh=120.0d0*(vk-26.70d0*log(Ho/Hi))
!Write(*,*) JClHNO3
!output
JHCO3=0.0d0!GNBCK*0.60d0*(vk-EHCO3)
!write(*,*) JHCO3
Jh=0.0d0!0.320d0*GNBCK*(vk-Eh)
!write(*,*) Jh
fun(1)  = gammai*(-JBKk-JKk-JClk-JNak-JNaKk+JNBCK-JNHE)!-JBKk-JClk-2.0d0*JTRPVk-(-2.0d0*JGluT+JNCX-JNMDA-Jampa)*convg)

fun(2)  = -JKk+2.0d0*JNaKk+JNKCC1k+JKCC1k!-JBKk-JKIRk-(JGluT+JNMDAKk+JampaKk)*convg!JNMDA*convg!
fun(3)  = -JNak-3.0d0*JNaKk+JNBCk-JNHE+JNKCC1k!-JNBCk-3.0d0*JNaCak-JNHE-(3.0d0*JNCX-JNMDANak-JampaNak-3.0d0*JGluT)*convg
fun(4)  =0.0d0!2.0d0*JNBCk-JNHE+JClHNO3
fun(5)  =0.0d0! Bcyt * (JIP3-Jpump+JERleak+JNaCak-JVOCCk+JTRPVk/rbuff +(JNCX+JNMDACak)*convg)
fun(6)  =fun(3) + fun(2) -(2.0d0*JNBCK +JNHE)!+ 2.0d0*fun(5)-JHCO3cl
fun(7)  =0.0d0! -Bcyt * (JIP3-Jpump+JERleak)/VRERcyt
fun(8)  =0.0d0! kon * (Kinh-(Cak + Kinh)* hk)
fun(9)  =0.0d0! rh*G-kdeg * Ik
fun(10) =0.0d0! trpvswitch*((minfk - mk)/tTRPVk)
fun(11) =0.0d0! Veet*max(Cak - Cakmin, 0.0d0)-keet * eetk
fun(12) =0.0d0! phiw * (winf - wk)
fun(13) = 1.0d0/VRsa * (JKk-2.0d0*JNaKk-JNKCC1k-JKCC1k)+JKNEtoSCk!-JNKCC1k-JKCC1k!+JKIRk+(-JNMDAKk-JampaKk+JGlut)*convg)+JKNEtoSCk !(JNMDAKk+JampaKk+JEAATKk)*convg)
fun(14) = 1.0d0/VRsa*(JNak + 3.0d0*JNaKk-JNBCk+JNHE-JNKCC1k)+JNaNEtoScK!+JNBCk+JNHE+(3.0d0*JNCX+JNMDANak+JampaNak-3.0d0*JGluT)*convg)+JNaNEtoSCk  !JNMDANak+JampaNak+JEAATNak)*convg)
fun(15) =0.0d0! (-2.0d0*JNBCk+JNHE+JClHNO3)/VRsa

fun(16)=0.0d0! kc1c2*Rdap+kc3c2*A2Rdap-(kc2c1+kc2c3)*ARdap!    %C2 AR
fun(17) =0.0d0! kc2c3*ARdap+koc3*Odap-(kc3c2+kc3o)*A2Rdap!      %C3 A2R
fun(18)= 0.0d0!= kc3o*A2Rdap-koc3*Odap!                        %O
fun(19) =0.0d0! kc3d*A2Rdap-kdc3*Ddap!

fun(20)=0.0d0!kc0c11*C0amp-(kc1c01+kc0c11+kc1d11)*C1amp+kc2c11*C2amp+kd1c11*D1amp!C1
fun(21)=0.0d0!kc1c21*C1amp+koc21*Oamp+kd2c21*D2amp-(kc2c11+kc2o1+kc2d21)*C2amp !C2
fun(22)=0.0d0!kc2o1*C2amp-koc21*Oamp ! O
fun(23)=0.0d0!kc1d11*C1amp-kd1c11*D1amp ! D1
fun(24)=0.0d0!kc2d21*C2amp-kd2c21*D2amp !D2




! fun(25)=kc0c11t*C0EAAT-(kc1c01t+kc0c11t+kc1d11t)*C1EAAT+kc2c11t*C2EAAT+kd1c11t*D1EAAT!C1
! fun(26)=kc1c21t*C1EAAT+koc21t*OEAAT+kd2c21t*D2EAAT-(kc2c11t+kc2o1t+kc2d21t)*C2EAAT !C2
! fun(27)=kc2o1t*C2EAAT-koc21t*OEAAT ! O
! fun(28)=kc1d11t*C1EAAT-kd1c11t*D1EAAT ! D1
! fun(29)=kc2d21t*C2EAAT-kd2c21t*D2EAAT !D2

fun(30)=(JNHE+2.0d0*JNBCk+Jh-JHCO3)/(betao)!+0.45d0*(phint-7.33d0)!-JGluT*convg/betai
fun(31)=1.0d0/VRSa*((-JNHE-2.0d0*JNBCK-Jh+JHCO3)/betao)+0.5d0*(phofit-pho)!*ATPNHE/HCO3s!*0.1d0*JNak/HCO3s!-0.0142d0*(pHo-6.20d0)!1000 !.7
end subroutine Derives


SUBROUTINE Euler( y,fun,time,nt, yout,derivs,dim,dt,var)
  
implicit none
integer, parameter:: int9 = selected_int_kind(16) 
integer(kind=int9)  nt
integer dim
double precision y(dim),fun(dim),time,yout(dim),yt(dim),var(dim)
double precision k1(dim)!,k2(dim),k3(dim),k4(dim)
double precision it,dyt(dim)
double precision h,dt
EXTERNAL derivs
 h=dt!B. NBCe1 (astrocytes)
 k1(:)=h*fun(:)   ! 	! k1 = h*f(tn,yn)

 yout(:)=y(:)+k1(:) !!  Accumulate increments with proper weights.
END



! SUBROUTINE trapez( y,fun,time,nt, yout,derivs,dim,dt,var)
  
! implicit none
! integer, parameter:: int9 = selected_int_kind(16) 
! integer(kind=int9)  nt
! integer dim
! double precision y(dim),fun(dim),time,yout(dim),yt(dim),var(dim)
! double precision k1(dim),k2(dim),k3(dim),k4(dim)
! double precision it,dyt(dim)
! double precision h,dt
! EXTERNAL derivs
!  h=dt
!  k1(:)=h*fun(:)   ! 	! k1 = h*f(tn,yn)
!  if (time .eq. dt) then
!     yt(:)=y(:)+k1(:) !Compute y(n) + k1 and store it in yt
!  else
!    yt(:)=y(:)
!  end if
!  it=time+h
!  Call Derivs(yt,dyt,it,nt,dim,dt,var)
!  k2=h*dyt(:)
!  yout(:)=y(:)+0.50d0*(k1(:)+k2(:)) !!  Accumulate increments with proper weights.
! END


! SUBROUTINE rk4( y,fun,time,nt, yout,derivs,dim,dt,var)
  
! implicit none
! integer, parameter:: int9 = selected_int_kind(16) 
! integer(kind=int9)  nt
! integer dim
! double precision y(dim),fun(dim),time,yout(dim),yt(dim),var(dim)
! double precision k1(dim),k2(dim),k3(dim),k4(dim)
! double precision it,dyt(dim)
! double precision h,dt
! EXTERNAL derivs
!  h=dt
!  k1(:)=h*fun(:)   ! 	! k1 = h*f(tn,yn)
!  yt(:)=y(:)+k1(:)/2.0d0! Compute y(n) + k1/2 and store it in yt
!  it=time+h/2.0d0
!  Call Derivs(yt,dyt,it,nt,dim,dt,var)
!  k2(:)=h*dyt(:)  !k2 = h *  f(tn+h/2,yn+k1/2)
!  yt(:)=y(:)+k2(:)/2.0d0 ! Compute y(n) + k2/2 and store it in yt
!  call derivs(yt,dyt,it,nt,dim,dt,var)
!  k3(:)=h*dyt(:) !k3 = h *  f(tn+h/2,yn+k2/2)
!  yt(:)=y(:)+k3(:) !! Compute y(n) + k3 and store it in yt
!  it=time+h
!  Call derivs(yt,dyt,it,nt,dim,dt,var)
!  k4(:)=h*dyt(:) !! k4 = h *  f(tn+h,yn+k3)
!  yout(:)=y(:)+(k1(:)+2.0d0*k2(:)+2.0d0*k3(:)+k4(:))/6.0d0 !!  Accumulate increments with proper weights.
! END


! SUBROUTINE rk4(y,fun,t,nt,yout,derivs,dim,dt,var)

! implicit none

  
! INTEGER i,nt,dim
! double precision h,dt,t,fun(dim),y(dim),yout(dim), k1(dim), k2(dim), k3(dim), k4(dim), yt(dim),dyt(dim),var(dim)
! EXTERNAL derivs
! h=dt
! do i = 1, dim
! k1(i) = h * fun(i) 				! k1 = h*f(tn,yn)
! yt(i)=y(i)+ k1(i)/2.0d0 			! Compute y(n) + k1/2 and store it in yt
! end do
! call derivs(yt,dyt,t+h/2.0d0,nt,dim,dt,var)    			! Get f(tn+h/2,yn+k1/2) and store in dyt.
! do i = 1, dim
! k2(i) = h * dyt(i)				! k2 = h *  f(tn+h/2,yn+k1/2)
! yt(i)=y(i)+k2(i)/2.0d0 			! Compute y(n) + k2/2 and store it in yt
! end do
! call derivs(yt,dyt,t+h/2.0d0,nt,dim,dt,var)     			! Get f(tn+h/2,yn+k2/2) and store in dyt.
! do i = 1, dim
! k3(i) = h * dyt(i)	 			! k3 = h *  f(tn+h/2,yn+k2/2)
! yt(i)=y(i)+k3(i) 				! Compute y(n) + k3 and store it in yt
! end do
! call derivs(yt,dyt,t+h,nt,dim,dt,var)   			! Get f(tn+h,yn+k3) and store in dyt.
! do i = 1, dim
! k4(i) = h * dyt(i)				! k4 = h *  f(tn+h,yn+k3)
! yout(i)=y(i)+(k1(i)+2.0d0*k2(i)+2.0d0*k3(i)+k4(i))/6.0d0  !  Accumulate increments with proper weights.
! end do
! END
