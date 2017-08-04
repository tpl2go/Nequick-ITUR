C =========================================================================
C            ***   NeQuick electron density model package  ***
C                  prepared for ITU-R recommendation 371-8
C =========================================================================
C
C NeQuick was developed at Abdus Salam ICTP Trieste and at the University
C   of Graz.
C
C It is based on the Di Giovanni - Radicella (DGR) model
C   (Di Giovanni, G., S. R. Radicella, An analytical model of the electron
C      density profile in the ionosphere, Adv. Space Res. 10, 27-30, 1990)
C   which was modified to the requirements of the COST 238 Action PRIME
C   to give vertical electron content from ground to 1000 km consistent
C   with the COST 238 regional electron content model
C   (Radicella, S.M., M.-L. Zhang, The improved DGR analytical model of
C      electron density height profile and total electron content in the
C      ionosphere. A. Geofisica, 38, 35-41, 1995).
C
C The next generation COST model, COSTprof, which was adopted by the
C   COST Action 251 uses a modified DGR in the height range from 100 km
C   to the peak of the F2 layer and a O+ - H+ diffusive equilibrium
C   formulation for the topside F layer. The modification of the DGR
C   formulation ensures a true electron density maximum at the F2 peak
C   under all conditions.
C
C The topside of NeQuick is a simplified approximation to a diffusive
C   equilibrium, the main improvement over the DGR and COST 238 models
C   being a limited increase with height of the electron density scale
C   height used.
C
C NeQuick is a "profiler" which makes use of three profile anchor points:
C   E layer peak (at a fixed height of 120 km), F1 peak, F2 peak.
C   To model the anchor points it uses the "ionosonde parameters"
C   foE, foF1, foF2 (critical frequencies) and M3000(F2) (transfer
C   parameter). For foE we use a model by John Titheridge; foF1 is taken
C   to be proportional to foE during daytime (foF1=1.4*foE) and 0 during
C   nighttime. For foF2 and M3000(F2) we use the ITU-R (CCIR) maps in
C   the mode used by the International Reference Ionosphere (IRI).
C
C The bottom side of the electron density profile consists of the
C   superposition of three Epstein layers which peak at the anchor points.
C   The Epstein layers have different thickness parameters for their
C   bottom and top sides (5 "semi-Epstein" layers). The topside of the
C   electron density profile consists of the topside of an Epstein
C   layer with a height dependent thickness parameter.
C
C The sub-models contained in NeQuick use monthly average values of solar
C   activity in two forms: average sunspot number R12 and average 10.7 cm
C   solar radio flux F107. The latter is considered to be the primary
C   input parameter. A fixed relation between R12 and F107 is used:
C            F107= 63.7+8.9D-4*R12**2+0.728*R12  ÿ(ITU-R recommendation)
C       or   R12 = sqrt(167273.0+(flx-63.7)*1123.6)-408.99
C The inclination of the geomagnetic induction vector (Dip) is also used
C   by NeQuick sub-models. To be consistent with the ITU-R (CCIR) maps
C   the limited spherical harmonics expansion for 1977 was used to
C   calculate a grid point map of dip latitude (input file diplats.asc).
C   NeQuick calculates dip latitude by third order interpolation in
C   geographic latitude and longitude. Dip is calculated from dip latitude.
C
C Authors of the modified Di Giovanni - Radicella (DGR) model code:
C   Man-Lian Zhang and S.M. Radicella, Abdus Salam ICTP Trieste.
C
C Source of the code for the "CCIR maps" (subroutine cciri and
C   function gamma1) and for the   ccirXX.asc   data files:
C   International Reference Ionosphere (IRI)
C   (  real FUNCTION xNeIRI(height,lati,longi,dhour,month,iday,R)
C      SUBROUTINE F2OUT(XMODIP,XLATI,XLONGI,FF0,XM0,UT,
C     &                              FOF2,XM3000)
C      REAL FUNCTION GAMMA1(SMODIP,SLAT,SLONG,HOUR,IHARM,NQ,
C    in:
C    IRIF12.FOR, Version 12.2.2, September 1994  )
C
C Author of modifications of DGR code,
C   of adaptations of "CCIR map code"
C   and responsible author for this package:
C       Dr. Reinhart Leitinger
C       Institut fuer Geophysik, Astrophysik und Meteorologie (IGAM)
C       Universitaet Graz
C       Halbaerthgasse 1
C       A-8010 Graz, Austria
C       e-mail  reinhart.leitinger@uni-graz.ac.at
C
C   with contributions by Gerald Hochegger (IGAM), Bruno Nava and
C       Pierdavide Coisson (Abdus Salam ICTP)
C
C =========================================================================
C
C Release date of code: 31 May 2002
C
C =========================================================================
C
C This package is written in standard FORTRAN 77. No common blocks are
C      used. Implicit double precision for all real variables and
C      functions (real*8). It consists of 421 FORTRAN statement lines.
C Driver programs delivered:
C      source programs:
C      eldens_ITUR.for       (simple driver to test NeQuick and vert)
C      slQu.for              (electron density along arbitrarily chosen
C                             straight line "rays")
C      executables for PC-DOS:
C      eldens_ITUR.exe
C      slQu.exe
C
C =========================================================================
C
C program modules (description: see NeQuick_ITU_R_software.doc)
C      real*8 function NeQuick(h,alat,along,mth,flx,UT)
C      entry eldens(h,alat,along)
C      entry vert(h)
C      real*8 function NeMdGR(A,hm,BB,h)
C      real*8 function topq(h,No,hmax,Ho)
C      subroutine prepmdgr(mth,R12,foF2,foF1,foE,M3000,Dip,hm,BB,A)
C      subroutine ef1(alat,mth,flx,chi,foE,foF1)
C      subroutine cciri(xMODIP,mth,UT,R12,alat,along,foF2,M3000)
C      real*8 function gamma1(xMODIP,alat,along,hour,iharm,nq,
C     ,                          k1,m,mm,m3,sfe)
C      real*8 function peakh(foE,foF2,M3000)
C      subroutine sdec(mth,UT,sdelta,cdelta)
C      subroutine geomagin(filenam,pdip)
C      real*8 function philam(pdip,alat,along)
C      real*8 function fexp(a)
C      real*8 function djoin(f1,f2,alpha,x)
C      real*8 function finter3(z,x)
C
C =========================================================================
C
C Data files needed:
C   (1) 12 CCIR map files       ccir11.asc   (for January) to
C                               ccir22.asc   (for December)
C   (2) dip latitude grid file  diplats.asc
C
C =========================================================================
C
C Input parameters:
C   NeQuick:
C     h:     height (km)
C     alat:  gg. latitude  (degrees N)
C     along: gg. longitude (degrees E)
C     mth:   month (1 .. 12)
C     flx:   10.7 cm solar radio flux (flux units)
C     UT:    Universal Time (hours)
C   eldens (entry point):
C     h:     height (km)
C     alat:  gg. latitude  (degrees N)
C     along: gg. longitude (degrees E)
C   vert (entry point)
C     h:     height (km)
C Usage:
C   use NeQuick(h,alat,along,mth,flx,UT) as a function each time a
C     change of the input parameters mth or flx or UT is wanted;
C   use eldens(h,alat,along) along arbitrarily chosen "rays";
C   use vert(h) for height profiles of electron density.
C
C =========================================================================
C
C Output (function value) is electron density in units of m^-3
C   (electrons per cubic meter)
C
C =========================================================================
C
C
C
C =========================================================================
C
C Changes:
C
C 02 Sep 2002
C
C     B. Nava: introduced flux saturation at 193 flux unit (R12=150)
C     as indicated in Rec. ITU-R P. 1239 (Point 3: "Prediction of
C     f0F2 and M(3000)F2"
C
C

      real*8 function NeQuick(h,alat,along,mth,flx,UT)
      implicit real*8 (a-h,o-z)
      character*11 filenam
      real*8 Nmax,NeMdgr,M3000
      dimension hm(3),A(3),BB(6)
      dimension pdip(0:38,-1:37)

      save

      parameter (pi=3.141592653589793D0)
      parameter (DR=1.74532925199433D-2,RD=5.729577951308232D1)
      data UT0,mth0,flx0,jdip/-100.0D0,-1,0D0,0/
      if (flx .gt. 193.0D0) flx=193.0D0
      if (UT.lt. 0.0D0) UT=UT+24.0D0
      if (UT.ge.24.0D0) UT=UT-24.0D0
      along=dmod(along+360.0D0,360.0D0)
      if (jdip.eq.0) then
         filenam='diplats.asc'
         call geomagin(filenam,pdip)
         jdip=1
      endif
      if (UT.ne.UT0.or.mth.ne.mth0) then
         call sdec(mth,UT,sdelta,cdelta)
         UT0=UT
         mth0=mth
      endif
      if (flx.ne.flx0) then
         R12=sqrt(167273.0D0+(flx-63.7D0)*1123.6D0)-408.99D0
         flx0=flx
      endif

      entry eldens(h,alat,along)
      xlt=UT0+along/15.0D0
      if (xlt.lt. 0.0D0)  xlt=xlt+24.0D0
      if (xlt.ge.24.0D0)  xlt=xlt-24.0D0
      dipl=philam(pdip,alat,along)
      Dip=atan2(2.0*sin(dipl*DR),cos(dipl*DR))
      xMODIP=atan2(Dip,sqrt(abs(cos(alat*DR))))*RD
      Dip=Dip*RD
      cchi=sin(alat*DR)*sdelta+cos(alat*DR)*cdelta*
     * cos(pi*(12.0D0-xlt)/12.0D0)
      chi=atan2(sqrt(1.0D0-cchi*cchi),cchi)*RD
      call cciri(xMODIP,mth0,UT0,R12,alat,along,foF2,M3000)
      call ef1(alat,mth0,flx0,chi,foE,foF1)
      call prepmdgr(mth0,R12,foF2,foF1,foE,M3000,Dip,hm,BB,A)
      Nmax=NeMdgr(A,hm,BB,hm(1))
      entry vert(h)
      if (h.gt.hm(1)) then
         NeQuick=topq(h,Nmax,hm(1),BB(6))
         return
      endif
      NeQuick=NeMdgr(A,hm,BB,h)
      return
      end

      real*8 function NeMdGR(A,hm,BB,h)
      implicit real*8 (a-h,o-z)
      dimension A(3),hm(3),BB(6),B(3)

      save

      parameter (f1=10.0D0,f2=2.0D0)
      parameter (h0=100.0D0)
      parameter (Hd=10.0D0)
      data aN0 /-1.0D0/

      B(1)=BB(5)
      B(2)=BB(3)
      B(3)=BB(1)
      if (h.gt.hm(3)) B(3)=BB(2)
      if (h.gt.hm(2)) B(2)=BB(4)
      if (h.lt.h0) then
         if (aN0.le.0.0D0) then
            sum =0.0D0
            dsum=0.0D0
            do jj=1,3
               arg0=(h0-hm(jj))
               arg=arg0/B(jj)
               if (jj.gt.1) then
                  d=abs(h0-hm(1))
                  arg=arg*exp(f1/(1.0D0+f2*d))
               endif
               if (abs(arg).gt.25.0D0) then
                  s0=0.0D0
                  ds=0.0D0
               else
                  ee=exp(arg)
                  s0=A(jj)*ee/(1.0D0+ee)**2
                  ds=(1.0D0-ee)/(1.0D0+ee)/B(jj)
               endif
               sum=sum+s0
               dsum=dsum+s0*ds
            enddo
            bf=1.0D0-dsum/sum*Hd
            aN0=sum*1.0D11
         endif
         z=(h-h0)/Hd
         NeMdGR=aN0*fexp(1.D0-bf*z-fexp(-z))
         return
      else
         sum=0.0D0
         do jj=1,3
            arg0=(h-hm(jj))
            arg=arg0/B(jj)
            if (jj.gt.1) then
               d=abs(h-hm(1))
               arg=arg*exp(f1/(1.0D0+f2*d))
            endif
            if (abs(arg).gt.25.0D0) then
               s0=0.0D0
            else
               ee=exp(arg)
               s0=A(jj)*ee/(1.0D0+ee)**2
            endif
            sum=sum+s0
         enddo
         NeMdGR=sum*1.0D11
         return
      endif
      end

      real*8 function topq(h,No,hmax,Ho)
      implicit real*8 (a-h,o-z)
      real*8 No
      parameter (g=0.125D0,rfac=100.0D0)
         dh=h-hmax
         g1=g*dh
         z=dh/(Ho*(1.0D0+rfac*g1/(rfac*Ho+g1)))
         ee=fexp(z)
         if (ee.gt.1.0D11) then
            ep=4.0D0/ee
         else
            ep=4.0D0*ee/(1.0D0+ee)**2
         endif
         topq=No*ep
      return
      end

      subroutine prepmdgr(mth,R12,foF2,foF1,foE,M3000,Dip,hm,BB,A)
      implicit real*8 (a-h,o-z)
      real*8 NmE,NmF1,NmF2,M3000
      dimension A(3),hm(3),BB(6)
      data hmE,B2bot,B1,B1top /120.0D0,40.0D0,40.0D0,40.0D0/

      FNe(X)=0.124*X*X
      FEpst(X,Y,Z,W)=X*fexp((W-Y)/Z)/(1.+fexp((W-Y)/Z))**2

      NmF2=FNe(foF2)
      NmF1=FNe(foF1)
      if(foF1.le.0.0D0.and.foE.gt.2.0D0) NmF1=FNe(foE+0.5D0)
      NmE=FNe(foE)
      hmF1=djoin(108.8D0+14.0D0*NmF1+0.71D0*Dip,
     , 108.8D0+14.0D0*NmF1-0.71D0*Dip,12.0D0,Dip)
      hmF2=peakh(foE,foF2,M3000)
      hm(1)=hmF2
      hm(2)=hmF1
      hm(3)=hmE
      dNdHmx=-3.467D0+0.857D0*log(foF2*foF2)+2.02D0*log(M3000)
      dNdHmx=exp(dNdHmx)*0.01D0
      B2bot=0.385*NmF2/dNdHmx
      A(1)=4.0D0*NmF2
      A(2)=4.0D0*(NmF1-FEpst(A(1),hmF2,B2bot,hmF1))
      A(2)=djoin(A(2),0.05D0,60.0D0,A(2)-0.005D0)
      if(NmF1.le.0.001D0) then
         ax=0.0D0
      else
         ax=A(2)/(0.1D0*NmF1)
      endif
      ax=djoin(ax,1.5D0,20.0D0,ax-1.5D0)
      B1=(hmF2-hmF1)/log(ax)
      B1top=djoin(B2bot+50.0D0,B1,20.0D0,B1-B2bot-50.0D0)
      B1bot=0.7*B1top
      A(3)=4.0D0*(NmE-FEpst(A(2),hmF1,B1bot,hmE)-
     - FEpst(A(1),hmF2,B2bot,hmE))
      A(3)=djoin(A(3),0.005D0,60.0D0,A(3)-0.005D0)
      Betop=0.5D0*B1top
      Bebot=5.0D0
      if (Betop.lt.7.0D0) Betop=7.0D0
      if (mth.gt.3.and.mth.lt.10) then
         b2k=6.705D0-0.014D0*R12-0.008D0*hmF2
      else
         b2k=-7.77D0+0.097D0*(hmF2/B2bot)**2+0.153D0*NmF2
      endif
      b2k=djoin(b2k,2.0D0,1.0D0,b2k-2.0D0)
      b2k=djoin(8.0D0,b2k,1.0D0,b2k-8.0D0)
      B2top=b2k*B2bot
      x=(B2top-150.0D0)/100.0D0
      v=(0.041163D0*x-0.183981D0)*x+1.424472D0
      B2top=B2top/v
      BB(1)=Bebot
      BB(2)=Betop
      BB(3)=B1bot
      BB(4)=B1top
      BB(5)=B2bot
      BB(6)=B2top
      return
      end

      subroutine ef1(alat,mth,flx,chi,foE,foF1)
      implicit real*8 (a-h,o-z)
      parameter (DR=1.74532925199433D-2)
      parameter (chi0=86.23292796211615D0)

      goto(10,10,20,20,30,30,30,30,20,20,10,10) mth
   10 seas=-1.0D0
      goto 40
   20 seas=0.0
      goto 40
   30 seas=1.0D0
   40 ee=fexp(0.3D0*alat)
      seas=seas*(ee-1.0D0)/(ee+1.0D0)
      chin=djoin(90.0D0-0.24D0*fexp(20.0D0-0.20D0*chi),chi,12.0D0,
     , chi-chi0)
      sfac=(1.112D0-0.019D0*seas)*sqrt(sqrt(flx))
      fa=sfac*fexp(log(cos(chin*DR))*0.3D0)
      foE=sqrt(fa*fa+0.49D0)
      foF1=1.4D0*foE
      foF1=djoin(foF1,0.0D0,12.0D0,chi0-chi)
      return
      end

      subroutine cciri(xMODIP,mth,UT,R12,alat,along,foF2,M3000)
      implicit real*8 (a-h,o-z)
      real*8 M3000
      dimension FF0(988),xm0(441),F2(13,76,2),FM3(9,49,2)
      character*10 filena
      integer QM(7),QF(9)
      save
      data QF/11,11,8,4,1,0,0,0,0/,QM/6,7,5,2,1,0,0/
      data montha,monthb,Rga/13,14,-10.0D0/

      if (mth.ne.montha) then
         write(filena,'(4Hccir,I2.2,4H.asc)') mth+10
         open(77,file=filena,status='OLD',form='FORMATTED')
         read(77,'(4E16.8)') F2,FM3
         close(77)
         montha=mth
      endif
      if (R12.ne.Rga.or.mth.ne.monthb) then
         RR2=R12/100.0D0
         RR1=1.0D0-RR2
         do i=1,76
         do j=1,13
            k=j+13*(i-1)
            FF0(k)=F2(j,i,1)*RR1+F2(j,i,2)*RR2
         enddo
         enddo
         do i=1,49
         do j=1,9
            k=j+9*(i-1)
            xm0(k)=FM3(j,i,1)*RR1+FM3(j,i,2)*RR2
         enddo
         enddo
         Rga=R12
         monthb=mth
      endif
      foF2= gamma1(xMODIP,alat,along,UT,6,QF,9,76,13,988,FF0)
      M3000=gamma1(xMODIP,alat,along,UT,4,QM,7,49, 9,441,xm0)
      return
      end

      real*8 function gamma1(xMODIP,alat,along,hour,iharm,nq,
     ,                          k1,m,mm,m3,sfe)
      implicit real*8 (a-h,o-z)
      real*8 c(12),s(12),coef(100),sum
      dimension nq(k1),xsinx(13),sfe(m3)
      logical numok
      parameter (DR=1.74532925199433D-2)

      hou=(15.0D0*hour-180.0D0)*DR
      s(1)=sin(hou)
      c(1)=cos(hou)
      do i=2,iharm
         c(i)=c(1)*c(i-1)-s(1)*s(i-1)
         s(i)=c(1)*s(i-1)+s(1)*c(i-1)
      enddo
      do i=1,m
         mi=(i-1)*mm
         coef(i)=sfe(mi+1)
         do j=1,iharm
            coef(i)=coef(i)+sfe(mi+2*j)*s(j)+sfe(mi+2*j+1)*c(j)
         enddo
      enddo
      sum=coef(1)
      ss=sin(xMODIP*DR)
      s3=ss
      xsinx(1)=1.0D0
      index=nq(1)
      do j=1,index
         numok=abs(ss).ge.1.0D-30
         if (numok) then
            sum=sum+coef(1+j)*ss
            xsinx(j+1)=ss
            ss=ss*s3
         else
            xsinx(j+1)=0.0D0
         endif
      enddo
      if (numok) then
         xsinx(nq(1)+2)=ss
      else
         xsinx(nq(1)+2)=0.0D0
      endif
      np=nq(1)+1
      ss=cos(alat*DR)
      s3=ss
      do j=2,k1
         s0=along*(j-1)*DR
         s1=cos(s0)
         s2=sin(s0)
         index=nq(j)+1
         do L=1,index
            np=np+1
            sum=sum+coef(np)*xsinx(L)*ss*s1
            np=np+1
            sum=sum+coef(np)*xsinx(L)*ss*s2
         enddo
         ss=ss*s3
      enddo
      gamma1=sum
      return
      end

      real*8 function peakh(foE,foF2,M3000)
      implicit real*8 (a-h,o-z)
      real*8 MF,M3000
      sqM=M3000*M3000
      MF=M3000*sqrt((0.0196D0*sqM+1.)/(1.2967D0*sqM-1.0D0))
      If(foE.ge.1.0D-30) then
         ratio=foF2/foE
         ratio=djoin(ratio,1.75D0,20.0D0,ratio-1.75D0)
         dM=0.253D0/(ratio-1.215D0)-0.012D0
      else
         dM=-0.012D0
      endif
      peakh=1490.0D0*MF/(M3000+dM)-176.0D0
      return
      end

      subroutine sdec(mth,UT,sdelta,cdelta)
      implicit real*8 (a-h,o-z)
      parameter (DR=1.74532925199433D-2)

      doy=mth*30.5D0-15.0D0
      t =doy + (18.0D0-UT)/24.0D0
      amrad=(0.9856D0*t - 3.289D0)*DR
      aLrad = amrad + (1.916D0*sin(amrad)+0.020D0*sin(2.0D0*amrad)+
     + 282.634D0)*DR
      sdelta=0.39782D0*sin(aLrad)
      cdelta=sqrt(1.0D0-sdelta*sdelta)
      return
      end

      subroutine geomagin(filenam,pdip)
      implicit real*8 (a-h,o-z)
      character*11 filenam
      dimension pdip(0:38,-1:37)
      parameter (latp=36,lngp=36,lathp=18,lnghp=18)

      open(77,file=filenam,status='OLD',form='FORMATTED')
      do i=-lnghp,lnghp
         read(77,*) (pdip(i+lnghp+1,j+lathp),j=-lathp,lathp)
      enddo
      close(77)
      do i=0,latp
         pdip(0,i)=pdip(2,mod((i+lathp),latp))
      enddo
      do i=0,latp
         pdip(lngp+2,i)=pdip(lngp-1,mod((i+lathp),latp))
      enddo
      do i=0,lngp+2
         pdip(i,-1)=pdip(i,latp-1)
      enddo
      do i=0,lngp+2
         pdip(i,latp+1)=pdip(i,1)
      enddo
      return
      end

      real*8 function philam(pdip,alat,along)
      implicit real*8 (a-h,o-z)
      dimension pdip(0:38,-1:37)
      dimension z(4),z1(4)
      parameter (lngp=36,dlatp=5.0D0,dlngp=10.0D0)

      dlng1=(along+180.0D0)/dlngp
      dlng1=dlng1-dint(dlng1)
      j1=idint((along+180.0D0)/dlngp)-2
      if (j1.lt.0) j1=j1+lngp
      if (j1.gt.lngp-3) j1=j1-lngp
      a=(alat+90.0D0)/dlatp+1.0D0
      i=idint(a-1.0D-6)-2
      a=a-dfloat(i+2)
      do k = 1,4
         do j=1,4
         z1(j)=pdip(i+j,j1+k)
         enddo
         z(k)=finter3(z1,a)
      enddo
      philam=finter3(z,dlng1)
      return
      end

      real*8 function fexp(a)
      real*8 a
      if(a.gt.80.0D0) then
         fexp=5.5406D34
         return
      endif
      if(a.lt.-80.0) then
         fexp=1.8049D-35
         return
      endif
      fexp=exp(a)
      return
      end

      real*8 function djoin(f1,f2,alpha,x)
      real*8 f1,f2,alpha,x,ee,fexp
      ee=fexp(alpha*x)
      djoin=(f1*ee+f2)/(ee+1.0D0)
      return
      end

      real*8 function finter3(z,x)
      implicit real*8 (a-h,o-z)
      dimension z(4),a(0:3)

      dx=x*2.0D0-1.0D0
      if (abs(dx+1.0D0).lt.1.0D-10) then
         finter3=z(2)
         return
      else
         g1=(z(3)+z(2))
         g2=(z(3)-z(2))
         g3=(z(4)+z(1))
         g4=(z(4)-z(1))/3.0D0
         a(0)=(9.0D0*g1-g3)
         a(1)=(9.0D0*g2-g4)
         a(2)=(g3-g1)
         a(3)=(g4-g2)
         zi=0.0
         do j=3,0,-1
            zi=zi*dx+a(j)
         enddo
      endif
      finter3=zi/16.0D0
      return
      end
