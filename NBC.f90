Program Main

Implicit None


integer, parameter:: dim=31,int9 = selected_int_kind(16)

double precision time, y(dim), fun(dim),var(dim)
double precision  dt
integer (kind=int9) nt,ii,iskip
!integer ii, iskip
EXTERNAL rk4
EXTERNAl Derives

open(unit=20, file='vk.dat')
open(unit=21,file= 'Kk.dat')
open(unit=22,file='Nak.dat')

open(unit=23,file='Clk.dat')

open(unit=24,file='Ks.dat')
open(unit=25, file='Nas.dat')

open(unit=26, file='pHi.dat')
open(unit=27, file='pHo.dat')


open(unit=290, file='JNAKk.dat')
open(unit=300, file='JKk.dat')
open(unit=310, file='JNak.dat')
open(unit=320, file='JNBCK.dat')
open(unit=330, file='JKCC1k.dat')
open(unit=340, file='JNCC1k.dat')
open(unit=350, file='JClk.dat')
open(unit=370, file='JNHE.dat')

open(unit=220, file='HCO3k.dat')
open(unit=210, file='HCO3s.dat')
open(unit=230, file= 'Ho.dat')
open(unit=240, file= 'Hi.dat')
open(unit=280, file= 'ENBC.dat')
open(unit=360, file= 'ENHE.dat')
open(unit=250, file= 'vkdas.dat')
open(unit=260, file= 'ko.dat')
open(unit=200,file= 'phofit.dat')
open(unit=270, file='Naofit.dat')


y(1)  =-83.79d0!vk mV s92
y(2)  = 146000.0d0 !Kk uM s84
y(3)  = 12000.0d0 !Nak uM s85

y(4)  = 8215.0d0! Clk uM s87

y(5) = 2900.0d0! Ks uM s81
y(6) = 152000.0d0 !Nas uM s82


y(7)=7.33d0 !phi
y(8)= 7.350d0!pho


time = 0.00d0 !sec
nt=21000000000_8
iskip=1000000_8


dt=0.00000010d0! sec

DO ii = 1,nt
   time = time + dt
   !write(*,*) time
    Call Derives(y,fun,time,nt,dim,dt,var)
    Call Euler(y,fun,time,nt, y,Derives,dim,dt,var)
   
    if (time.gt.50.0d0 .and. mod(ii,iskip)==0)then
       
       Write(20,*) time, y(1)
       write(21,*) time, y(2)
       write(22,*) time, y(3)
       write(23,*) time, y(4)
       write(24,*) time, y(5)
       write(25,*) time, y(6)
       write(26,*) time, y(7)
       write(27,*) time, y(8)

       
       
       write(200,*)time,  var(1)
       write(210,*) time, var(2)
       write(220,*) time, var(3)
       write(230,*) time, var(4)
       write(240,*) time, var(5)
       write(250,*) time, var(6)
       write(260,*) time, var(7)
       write(270,*) time, var(8)
       write(280,*) time, var(9)
       write(290,*) time, var(10)
       write(300,*) time, var(11)
       write(310,*) time, var(12)
       write(320,*) time, var(13)
       write(330,*) time, var(14)
       write(340,*) time, var(15)
       write(350,*) time, var(16)
       write(360,*) time, var(17)
       write(370,*) time, var(18)
       
       

       
    end if
        
END DO

END Program Main






subroutine Derives(y,fun,time,nt,dim,dt,var)
implicit none
integer, parameter:: int9 = selected_int_kind(16)  
integer(kind=int9) nt
integer ii, dim
double precision time, dt, y(dim),fun(dim),var(dim)
double precision vk,Kp,Nak,Kk,vi,Clk,HCO3k,Nao,Nas,Ko,Ks,HCO3s,R
double precision phi
double precision Cls
double precision Rs,Rk,VRsa
double precision JKNEToSCk,JNaNEToSCk,diff
double precision EKk,ENak,EClk,ENBCk
double precision JNaKk,JNaKmax,KNak,KKs
double precision JBKk,GBKk,JBKp,VRpa
double precision JKk,GKk,JNak,GNak,JNBCk,GNBCk,JKCC1k,GKCC1k,JNKCC1k,GNKCC1k
double precision JClk,GClk

double precision F,Aa,pi,voli,vola,convg,vstm,kostm,,skrest,Naorest,&
Nakrest,Narest,Korest,Kkrest, Krest,&
     Naofit
!ph
double precision pHint,pHo,phofit, Hi,Ho,ENHE,JNHE,betai,betao,&
phosig,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,&
     p11,p21,p31,p41,p51,p61,p71,p81,p91,p101,p12,p22,p32,p42,&
     p52,p62,p72,p82,p92,p102,&
     p13,p23,p33,p43,p53,p63,p73,p83,p93,p103
double precision alphas, betas,EClHNO3,JCLHNO3,ATPNHE,&
EHCO3,Eh,JHCO3,Jh,vkdas,phext,ATP,EHCO3cl,JHCO3cl


vk    = y(1)    ! Membrane potential of the astrocyt(Ac) mV s92
Kk    = y(2)    ! K+ concentration in the astrocyte uM s84
Nak   = y(3)    ! Na+ concentration in the astrocytes(uM) s85
Clk   = y(4)    ! Cl- concentration in the astrocyts via electroneutrality(uM) s87
Ks    = y(5)   ! K+ concentration in the synaptic cleft(sc)(uM):s81
Nas   = y(6)   ! Na+ concentration in the SC(uM):s82
phint=y(7)
pho=y(8)

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

!pHo bath

if (time .le. 80.0d0 ) then
   phofit=7.34943d0
elseif (time .gt. 80.0d0 .and. time .le.1127.2999952d0 ) then

   phofit=(p1*(time-80.0d0)**9.0d0+p2*(time-80.0d0)**8.0d0+p3*(time-80.0d0)**7.0d0+&
        p4*(time-80.0d0)**6.0d0+p5*(time-80.0d0)**5.0d0+p6*(time-80.0d0)**4.0d0+&
        p7*(time-80.0d0)**3.0d0+p8*(time-80.0d0)**2.0d0+p9*(time-80.0d0)+p10)*7.34943d0
else
   phofit=7.34716767d0

end if

var(1)=phofit

HCO3s=10.0d0**(pho-6.1d0)*0.03d0*1000.0d0*39.0d0!48.6d0!*5000.0d0!26000.0d0 Co2=s*pCo2=0.03mM/mmHg *40 mmHG pK=6.1

!write(*,*) HCO3s
var(2)=HCO3s
HCO3k=HCO3s*(10.0d0)**(phint-pho)
!write(*,*) HCO3k
var(3)=HCO3k

Ho=24.0d0*39.0d0/HCO3s!
var(4)=Ho
!write(*,*) Ho
Hi=24.0d0*39.0d0/HCO3k

var(5)=Hi

betao=10000+2.30*HCO3s
betai=10000+2.30*HCO3k
var(6)=vk

Kp=1000.0d0
Naorest=149200.0d0 ! Extracellular Na+ concentration in the unit of uM
Nakrest=18730.0d0  !Astrocyte Na+ concentration in hte unit of uM
Narest=167930.0d0 !sum of extra and astrocyte Na+ concentration at rest
Nao =152000.0d0!164000.0d0-NaK!152000.0d0!!Narest-Nak! 149200.0d0 ! Na+ concentration in the extracellular space uM
Korest=1934.0d0 ! Extracellular potassium concentration at rest in units of uM
Kkrest=92680.0d0! Astrocyte potassium concentration at rest in units of uM
Krest=94614.0d0 ! sum of extra and astrocyte K+ concentration at rest
Ko  =2900.0d0!-Kk!3834.0d0!krest-Kk!2934.0d0! K+ concentration in the extracellular space uM

! Kobath
if (time .le.80.0d0) then
   kostm=2500.0d0
elseif (time.gt.  80.0d0 .and. time .le.320.0d0) then
   kostm=-(exp(-0.002675d0*(time-80.0d0))-2.0d0)*2500.0d0
elseif (time .gt.320.0d0 .and. time .le.615.0d0) then
   kostm=(exp(-0.0052d0*(time-320))/1.35d0+0.73d0)*2500.0d0
else
   kostm=-(exp(-0.00169d0*(time-615.0d0))/7.6d0-1.02d0)*2500.0d0
end if

Ko=kostm
var(7)=Ko

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

! Naobath
if (time .le.80.0d0 .or. time .gt.800.0d0) then
   Naofit=157000.0d0
elseif (time .ge. 80.d0) then   
Naofit=(p12*(time-80.0d0)**9.0d0+p22*(time-80.0d0)**8.0d0+p32*(time-80.0d0)**7.0d0+&
     p42*(time-80.0d0)**6.0d0+p52*(time-80.0d0)**5.0d0+p62*(time-80.0d0)**4.0d0+&
     p72*(time-80.0d0)**3.0d0+p82*(time-80.0d0)**2.0d0+p92*(time-80.0d0)+p10)*157000.0d0
end if

var(8)=Naofit
!Nao=152.0d0

Nao=Naofit
vi  = -34.68d0   ! Membrane potential of SMC mV section 6 s133
R  = 22.92d-6   ! Vessel radius m s213

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

EHCO3cl=-26.7d0/2.0d0*log(Cls*HCO3s/(Clk*HCO3k))
!write(*,*) EHCO3cl
JHCO3cl=0.0d0!9.0d0*(vk-EHCO3cl)


Rs   = 2.79d-8
Rk   = 6.0d-8

VRsa=3.0d0!3

diff       = 50.0d0      ! S^-1
JKNEtoSCk  = diff*(Ko-Ks)
JNaNEtoSCk = diff*(Nao-Nas)
phi    = 26.69950d0 ! RgT/F mV
gammai =1970.0d0 !Change in membrane potential by scaling factor mVuM^-1

!Ionic valence for K+,Na+,cl-,Ca2+
zK      = 1.0d0
zNa     = 1.0d0
zCl     = -1.0d0
zCa     = 2.0d0
zNBC    = -1.0d0 ! Effective valence for NBC cotransporter complex

JNakmax =2.3667d4 ! Maximum flux through the Na+/K+ ATP-ase pump uMs^-1
JNakmax=JNakmax

KNak    = 10000.0d0 ! Na+/K+ATP-ase pump constant uM

KKs     = 1500.0d0  ! Na+K+ ATP-ase pump constant uM




GKk     = 6907.770d0
GNak    = 226.940d0
GNBCk   = 130.740d0
GKCC1k  = 1.7280d0
GNKCC1k = 9.5680d0
Gclk    = 151.930d0


EKk    = phi/zK * log(Ks/Kk)     ! Nernst potentials (in mV)  !106
!write(*,*) EKK
ENak   = phi/zNa *log(Nas/Nak)  ! 107
!write(*,*) ENak
EClk   = phi/zCl * log(Cls/Clk)  ! s108
!write(*,*) EClk
ENBCk  =-phi *log((Nas * HCO3s**2.0d0)/(Nak * HCO3k**2.0d0))! s109
!write(*,*) ENBCk

var(9)=ENBCk
EBKk  = phi/zK * log(Kp/Kk)! s110
!!write(*,*) EBKK
ETRPVk = phi/zCa * log(Cao/Cak) ! s126
EHCO3=-26.70d0*log(HCO3s/HCO3k)
EH=26.70d0*log(Ho/Hi)

alphas=0.080d0
betas=0.01d0

!end if
JNaKk  =JNaKmax * Nak**1.5d0/(Nak**1.50d0 + KNak**1.50d0) * Ks/(Ks + KKs) !* 1.50d0  ! Flux through the Sodium Potassium pump s104

JNaKk=JNaKK*2.2d0! 1.380d0 !2.20d0 !1.7

 
!current
JNaKk=((-1.0d0/(0.0001d0+exp(-0.000917d0*(-(time-80.0d0)*1.02d0)))*1.0d0/(1.0d0+exp(-0.019d0*((time-80.0d0)*1.06d0-80.0d0)))+1.13d0))*JNaKk!&
     
                                                                                                                                                                                                                                  
var(10)=JNAKk
 
JKk    =0.180d0*GKk * (vk - EKk)! K+ flux through the K+ channel(uMs^-1)s99
var(11)=JKk

JNak    =1.30d0*GNak * (vk - ENak)                                          ! Na+ flux through the Na+ channel(uMs^-1) s100
var(12)=JNak
JNBCk  =GNBCk * (vk - ENBCk)!*10.0d0!*20.0d0 !5.5! Na+ and HCO3- flux through the NBC channel (uMs^-1) s101
var(13)=JNBCk

JKCC1k  =GKCC1k * phi * log((Ks * Cls)/(Kk * Clk))*30.0d0! Cl- and K+ flux through the KCC1 channel(uMs^-1) s102
Var(14)=JKCC1k

JNKCC1k =GNKCC1k * phi * log((Nas * Ks * Cls**2.0d0)/(Nak * Kk * Clk**2.0d0)) !*0.5  ! Na+, K+ and cl- flux through the NKCC1 channel(uMs^-1) s103
var(15)=JNKCC1k

JClk    =GClk * (vk - EClk)                                            ! Cl-flux through the cl- channel(uMs^-1) s98
var(16)=Jclk


ENHE=26.70d0*(log((Nak*Ho)/(Nas*Hi)))!
 
var(17)=ENHE
JNHE=GNBCK*(vk-ENHE)*2.0d0 !uMS^-1
var(18)=JNHE

!write(*,*) Jh

! Rate equations
fun(1)  = gammai*(-JBKk-JKk-JClk-JNak-JNaKk+JNBCK-JNHE)

fun(2)  = -JKk+2.0d0*JNaKk+JNKCC1k+JKCC1k
fun(3)  = -JNak-3.0d0*JNaKk+JNBCk-JNHE+JNKCC1k

fun(4)  =fun(3) + fun(2) -(2.0d0*JNBCK +JNHE)

fun(5) = 1.0d0/VRsa * (JKk-2.0d0*JNaKk-JNKCC1k-JKCC1k)+JKNEtoSCk!
fun(6) = 1.0d0/VRsa*(JNak + 3.0d0*JNaKk-JNBCk+JNHE-JNKCC1k)+JNaNEtoScK


fun(7)=(JNHE+2.0d0*JNBCk+Jh-JHCO3)/(betai)
fun(8)=1.0d0/VRSa*((-JNHE-2.0d0*JNBCK-Jh+JHCO3)/betao)+0.5d0*(phofit-pho)!
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



