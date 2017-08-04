C ========================================================================
C               ***   slQu, version without common blocks  ***
C ========================================================================
C
C Main program to calculate electron content from NeQuick-ITU-R
C for arbitrarily chosen rays which do not cut the surface of
C the Earth between the given endpoints.
C
C    Output file: slQu.dat
C
C    This package (main program plus auxiliary modules listed below)
C    is written in standard FORTRAN 77. No common blocks are
C    used. Implicit double precision for all real variables and
C    functions (real*8). It consists of 444 FORTRAN statement lines.
C    (comment lines and empty lines not counted)
C
C    Release date of parent programs slprof3:      2001/07/30
C                                    rayaux:       2000/02/18
C    Release date of this package:                 2002/05/31
C
C ========================================================================
C usage: link object code with object code of NeQUITUR
C ========================================================================
C
C ray conventions:
C    coordinate s (in km) along the ray, origin in ray perigee which
C       has a radius pp (in km).
C       Latitude and longitude of ray perigee: php, alamp
C    input of a ray: subroutine RAYS uses the endpoint coordinates
C       (height in km, latitude in deg. N, longitude in deg. E)
C        to define a ray (1 for lower, 2 for higher endpoint)
C
C ========================================================================
C additional data file (optional): R12.dat
C    each line contains the year (format I5) and the 12 R12 data for this year
C    (format 12 F6.1), e.g.
C 1998   43.7  48.9  53.4  56.5  59.4  62.5  65.4  67.8  69.5  70.5  73.0  77.9
C 1999   82.6  84.6  83.8  85.5  90.5  93.1  94.3  97.5 102.3 107.8 111.0 111.1
C 2000  112.9 116.8 119.9 120.8 119.0 118.7 119.8 118.6 116.3 114.5 112.7 112.0
C 2001  108.7 104.0 104.8 107.6 108.6 109.8 111.7 113.6 114.1 114.0 115.5 114.9
C (the values for October and November 2001 are provisional, the value for
C  December 2001 is the provisional R11)
C ========================================================================

C ========================================================================
C
C This source code contains the following modules:

C (1) numerical integration along a slant ray
C
C      real*8 function gint (f,g1,g2,eps,pp,Re,s1,c1,ssig,csig,along1)
C numerical integration (specialized, no external function)

C (2) numerical integration along a vertical ray
C
C      real*8 function gintv (f,g1,g2,eps,alat,along)
C numerical integration (specialized, no external function)

C (3) separation in functional units
C      subroutine rays(r1,h1,ph1,alng1,r2,h2,ph2,alng2,zeta,
C     & pp,Re,s1,c1,s2,c2,ssig,csig,along1)
C set and check ray endpoints, calculate geometric parameters for ray

C      subroutine dat_t_sa(iyr,mth,nday,ut,R12,flx)
C set date and solar activity

C (4) package with auxiliary subroutines and functions
C      subroutine gcirc(alat1,alat2,along1,along2,s1,c1,s2,c2,ssig,
C     & csig,psi)
C  calculates great circle path properties

C      subroutine naut(r1,r2,ph1,ph2,alng1,alng2,akappa,
C     & pp,php,alamp,zeta,cchi)
C  calculates position of ray perigee, zenith angle of ray at
C  lower endpoint, and slant to vertical projection factor cos(chi)

C      subroutine geogra(s,pp,Re,s1,c1,ssig,csig,along1,
C     &   h,alat,along)
C  calculates height, latitude and longitude along the given "ray"
C  for given distance from the perigee of the ray, s

c      real*8 function eld(s,pp,Re,s1,c1,ssig,csig,along1)
C  gives electron density as a function of the coordinate s

C  gint, rays, geogra, eld: input parameters pp,Re,s1,c1,ssig,csig,along1
C    necessary in  "common block free" formulation; they provide the
C    ray properties
C      pp:     radius of perigee
C      Re:     Earth radius
C      s1:     sine   of latitude of lower end point
C      c1:     cosine of latitude of lower end point
C      ssig:   sine   of azimuth of ray at lower end point
C      csig:   cosine of azimuth of ray at lower end point
C      along1: longitude of lower end point

C ========================================================================
C
C This package is written in standard FORTRAN 77. No common blocks are
C      used. Implicit double precision for all real variables and
C      functions (real*8). It consists of 454 FORTRAN statement lines.
C
C ========================================================================
C
C    author:
C    Dr. Reinhart Leitinger
C    Institut fuer Geophysik, Astrophysik und Meteorologie (IGAM)
C    Universitaet Graz
C    Universitaetsplatz 5
C    A-8010 Graz, Austria
C    e-mail  reinhart.leitinger@uni-graz.at
C ========================================================================

      program slQu
      implicit real*8 (a-h,o-z)
      real*8 NeQuick
      character*80 filen1
      character*1 yn
      parameter (Re=6371.2D0)
      parameter (RD=5.729577951308232D1)

      write(6,*)
      write(6,*)'          ****************************************'
      write(6,*)'          *        Test of NeQuick_ITU-R         *'
      write(6,*)'          *  slant profile and electron content  *'
      write(6,*)'          ****************************************'
      write(6,*)
      write(6,*)
     & 'Electron density is calculated along straight line rays'
      write(6,*)
     & '   from a lower endpoint (1) to a higher one (2).'
      write(6,*)

      filen1='slQu.dat'
      open(16,file=filen1)

      call rays(r1,h1,ph1,alng1,r2,h2,ph2,alng2,zeta,
     & pp,Re,sa,ca,sb,cb,ssig,csig,along1)
      s1=sqrt(r1*r1-pp*pp)
      s2=sqrt(r2*r2-pp*pp)
      write(16,'(A/2F7.2,F9.2)')
     &    'Ray endpoint 1: lat. (deg. N), long. (deg. E), height (km)',
     &     ph1,alng1,h1
      write(16,'(A/2F7.2,F9.2)')
     &    'Ray endpoint 2: lat. (deg. N), long. (deg. E), height (km)',
     &     ph2,alng2,h2
      if (pp.ge.0.1) write(16,'(2A/2F7.2)')
     & 'zenith angle (deg.) and azimuth (N over E to S, deg.)',
     &  ' of ray at endpoint 1 ',zeta,atan2(ssig,csig)*RD

      call dat_t_sa(iyr,mth,nday,ut,R12,flx)

      write(16,'(A,2(F6.1,1H,),I3,1H,,F5.1)')
     + 'S10.7, R12, month, UT: ',flx,R12,mth,ut
      write(16,'(/A/2A)')
     &    'Electron contents along ray.',
     &    '  (h1-h2) means from point in ',
     &    'height h1 to point in height h2 (heights in km)'

      write(6,*)'List electron density profile along ray (y/n)?'
      read(5,'(A)')yn
      h0=0.0D0
      if (h1.gt.h0) h0=h1
      r0=Re+h0
      s0=sqrt(r0*r0-pp*pp)
C  use of NeQuick necessary to condition the model to   mth,flx,UT
      aNe=NeQuick(h2,ph1,alng1,mth,flx,UT)
      if (yn.eq.'Y'.or.yn.eq.'y') then
         if (pp.ge.0.1) write(16,*) 's: coordinate along ray'
         write(16,*)
     &    'r: radius (distance from center of Earth)'
         write(16,*)
         if (pp.lt.0.1) then
         write(16,*)'     r    height  lat   long   el.density'
         write(16,*)'    km      km   deg N  deg E    m^-3'
         else
         write(16,*)'     s       r    height  lat   long   el.density'
         write(16,*)'    km      km      km   deg N  deg E    m^-3'
         endif
         dh=10.0D0
         if (h1.ge.500.0D0) dh= 50.0D0
         if (h1.ge.2000.0D0)dh=250.0D0
         h=h1-dh
   10    h=h+dh
         r=h+Re
         s=sqrt(r*r-pp*pp)
         if (pp.lt.0.1) then
           aNe=vert(h)
           if (aNe.lt.1000.0D0) aNe=0.0D0
           write(16,'(2F8.1,2F7.2,E13.6)') r,h,alat,along,aNe
         else
C  (call of geogra needed only to provide  alat,along  for output)
           call geogra (s,  pp,Re,sa,ca,ssig,csig,along1,  h,alat,along)
           aNe=eld(s, pp,Re,sa,ca,ssig,csig,along1)
           if (aNe.lt.1000.0D0) aNe=0.0D0
           write(16,'(3F8.1,2F7.2,E13.6)') s-s1,r,h,alat,along,aNe
         endif
         if (nint(h).eq.500)  dh=50.0D0
         if (nint(h).eq.2000) dh=250.0D0
         if (h+dh.le.h2) goto 10
         if (h+0.01D0.lt.h2) then
           r=h2+Re
           s=sqrt(r*r-pp*pp)
           if (pp.lt.0.1) then
             aNe=vert(h2)
             if (aNe.lt.1000.0D0) aNe=0.0D0
             write(16,'(2F8.1,2F7.2,E13.6)') r,h,alat,along,aNe
           else
C  (call of geogra needed only to provide  alat,along  for output)
             call geogra (s, pp,Re,sa,ca,ssig,csig,along1, h,alat,along)
             aNe=eld(s, pp,Re,sa,ca,ssig,csig,along1)
             if (aNe.lt.1000.0D0) aNe=0.0D0
             write(16,'(3F8.1,2F7.2,E13.6)') s-s1,r,h2,alat,along,aNe
           endif
         endif
      endif

      if (pp.lt.0.1) then
C  integration along vertical profile
        alat=ph1
        along=alng1
        if (h2.le.1000.0D0) then
          tec1=gintv(h0,h2,1.0D-3)
          write(16,'(A,I4,1H-,I4,1H))')
     &     'Electron content (',nint(h0),nint(h2)
          write(16,'(16X,F12.2)')tec1/1.0D12
        else
          h1a=1000.0D0
          if (h2.le.2000.0D0) then
            if (h1.ge.1000.0D0) then
              tec1=gintv(h1,h2,1.0D-3)
              write(16,'(A,I4,1H-,I4,1H) )')
     &         'Electron content (',nint(h1),nint(h2)
              write(16,'(16X,F12.2,A)')tec1/1.0D12,'   x10^15 m^-2'
            else
              tec1=gintv(h0, h1a,1.0D-3)
              tec2=gintv(h1a,h2, 1.0D-2)
              tec4=tec1+tec2
              write(16,'(A,2(I4,1H-,I4,3H),(),I4,1H-,I4,1H))')
     &          'Electron contents (',nint(h0),nint(h1a),
     &                                nint(h1a),nint(h2),
     &                                nint(h0), nint(h2)
              write(16,'(16X,3F12.2,A)')
     &          tec1/1.0D12,tec2/1.0D12,tec4/1.0D12,'   x10^15 m^-2'
            endif
          else
            if (h1.ge.2000.0D0) then
              tec1=gintv(h1,h2,1.0D-3)
              write(16,'(A,I4,1H-,I5,1H) )')
     &         'Electron content (',nint(h1),nint(h2)
              write(16,'(16X,F12.2,A)')tec1/1.0D12,'   x10^15 m^-2'
            else
              h1b=2000.0D0
              if (h1.ge.1000.0D0) then
                tec1=gintv(h1,h1b,1.0D-3)
                tec2=gintv(h1b,h2,1.0D-3)
                tec4=tec1+tec2
                write(16,'(A,I4,1H-,I4,3H),(,I4,1H-,I5,3H),(,
     &             I4,1H-,I5,1H))')
     &            'Electron contents (',nint(h1),nint(h1b),
     &                                  nint(h1b),nint(h2),
     &                                  nint(h1), nint(h2)
                write(16,'(16X,3F12.2,A)')
     &            tec1/1.0D12,tec2/1.0D12,tec4/1.0D12,'   x10^15 m^-2'
              else
                tec1=gintv(h0, h1a,1.0D-3)
                tec2=gintv(h1a,h1b,1.0D-2)
                tec3=gintv(h1b,h2, 1.0D-2)
                tec4=tec1+tec2+tec3
                write(16,'(A,2(I4,1H-,I4,3H),(),I4,1H-,I5,3H),(,
     &                         I4,1H-,I5,1H))')
     &            'Electron contents (',nint(h0),nint(h1a),
     &                                  nint(h1a),nint(h1b),
     &                                  nint(h1b),nint(h2),
     &                                  nint(h0), nint(h2)
                write(16,'(16X,4F12.2,A)')
     &            tec1/1.0D12,tec2/1.0D12,tec3/1.0D12,tec4/1.0D12,
     &            '   x10^15 m^-2'
              endif
            endif
          endif
        endif
      else
C  integration along slant profile
        if (h2.le.1000.0D0) then
           tec1=gint(s0,s2,1.0D-3,  pp,Re,sa,ca,ssig,csig,along1)
           write(16,'(A,I4,1H-,I4,1H))')
     &      'Electron content (',nint(h0),nint(h2)
           write(16,'(16X,F12.2)')tec1/1.0D12
        else
          h1a=1000.0D0
          r1a=h1a+Re
          s1a=sqrt(r1a*r1a-pp*pp)
          if (h2.le.2000.0D0) then
            if (h1.ge.1000.0D0) then
              tec1=gint(s1,s2,1.0D-3,  pp,Re,sa,ca,ssig,csig,along1)
              write(16,'(A,I4,1H-,I4,1H) )')
     &         'Electron content (',nint(h1),nint(h2)
              write(16,'(16X,F12.2,A)')tec1/1.0D12,'   x10^15 m^-2'
            else
              s2=sqrt(r2*r2-pp*pp)
              tec1=gint(s0, s1a,1.0D-3,  pp,Re,sa,ca,ssig,csig,along1)
              tec2=gint(s1a,s2, 1.0D-2,  pp,Re,sa,ca,ssig,csig,along1)
              tec4=tec1+tec2
              write(16,'(A,2(I4,1H-,I4,3H),(),I4,1H-,I4,1H))')
     &          'Electron contents (',nint(h0),nint(h1a),
     &                                nint(h1a),nint(h2),
     &                                nint(h0), nint(h2)
              write(16,'(16X,3F12.2,A)')
     &          tec1/1.0D12,tec2/1.0D12,tec4/1.0D12,'   x10^15 m^-2'
            endif
          else
            if (h1.ge.2000.0D0) then
              tec1=gint(s1,s2,1.0D-3,  pp,Re,sa,ca,ssig,csig,along1)
              write(16,'(A,I4,1H-,I5,1H) )')
     &         'Electron content (',nint(h1),nint(h2)
              write(16,'(16X,F12.2,A)')tec1/1.0D12,'   x10^15 m^-2'
            else
              h1b=2000.0D0
              r1b=h1b+Re
              s1b=sqrt(r1b*r1b-pp*pp)
              if (h1.ge.1000.0D0) then
                tec1=gint(s1,s1b,1.0D-3,  pp,Re,sa,ca,ssig,csig,along1)
                tec2=gint(s1b,s2,1.0D-3,  pp,Re,sa,ca,ssig,csig,along1)
                tec4=tec1+tec2
                write(16,'(A,I4,1H-,I4,3H),(,I4,1H-,I5,3H),(,
     &             I4,1H-,I5,1H))')
     &            'Electron contents (',nint(h1),nint(h1b),
     &                                  nint(h1b),nint(h2),
     &                                  nint(h1), nint(h2)
                write(16,'(16X,3F12.2,A)')
     &            tec1/1.0D12,tec2/1.0D12,tec4/1.0D12,'   x10^15 m^-2'
              else
                tec1=gint(s0, s1a,1.0D-3,  pp,Re,sa,ca,ssig,csig,along1)
                tec2=gint(s1a,s1b,1.0D-2,  pp,Re,sa,ca,ssig,csig,along1)
                tec3=gint(s1b,s2, 1.0D-2,  pp,Re,sa,ca,ssig,csig,along1)
                tec4=tec1+tec2+tec3
                write(16,'(A,2(I4,1H-,I4,3H),(),I4,1H-,I5,3H),(,
     &                         I4,1H-,I5,1H))')
     &            'Electron contents (',nint(h0),nint(h1a),
     &                                  nint(h1a),nint(h1b),
     &                                  nint(h1b),nint(h2),
     &                                  nint(h0), nint(h2)
                write(16,'(16X,4F12.2,A)')
     &            tec1/1.0D12,tec2/1.0D12,tec3/1.0D12,tec4/1.0D12,
     &            '   x10^15 m^-2'
              endif
            endif
          endif
        endif
      endif

      close(16)
      write(6,*)'Output in ',filen1
      end

      real*8 function gint (g1,g2,eps,pp,Re,s1,c1,ssig,csig,along1)
C  special; integrates ELD
      implicit real*8 (a-h,o-z)
         n = 8
    1    h  =  (g2-g1) / dfloat(n)
         hh = 0.5D0*h
         g  = h*0.5773502691896D0
         y  = g1 + (h-g)*0.5D0
         gint2  =  eld(y,pp,Re,s1,c1,ssig,csig,along1)+
     +             eld(y+g,pp,Re,s1,c1,ssig,csig,along1)
         do m = 1,n-1
            gint2 = gint2 + eld(y+h-g,pp,Re,s1,c1,ssig,csig,along1)+
     +                      eld(y+h,pp,Re,s1,c1,ssig,csig,along1)
            y = y + h
         enddo
         gint2 = gint2*hh
         if (n.eq.8.or.abs(gint1-gint2).gt.eps*abs(gint1)) then
            n = n*2
            gint1 = gint2
            if (n.lt.1024) goto 1
         endif
         gint = gint2+(gint2-gint1)/15.0D0
      return
      end

      real*8 function gintv (g1,g2,eps)
C  special; integrates VERT
      implicit real*8 (a-h,o-z)
         n = 8
    1    h  =  (g2-g1) / dfloat(n)
         hh = 0.5D0*h
         g  = h*0.5773502691896D0
         y  = g1 + (h-g)*0.5D0
         gint2  =  vert(y)+
     +             vert(y+g)
         do m = 1,n-1
            gint2 = gint2 + vert(y+h-g)+
     +                      vert(y+h)
            y = y + h
         enddo
         gint2 = gint2*hh
         if (n.eq.8.or.abs(gint1-gint2).gt.eps*abs(gint1)) then
            n = n*2
            gint1 = gint2
            if (n.lt.1024) goto 1
         endif
         gintv = gint2+(gint2-gint1)/15.0D0
      return
      end

      subroutine rays(r1,h1,ph1,alng1,r2,h2,ph2,alng2,zeta,
     & pp,Re,s1,c1,s2,c2,ssig,csig,along1)
      implicit real*8 (a-h,o-z)
      akappa=Re/(Re+400.0D0)
   10 write(6,*)'INPUT: ',
     &'Ray endpoint 1: latitude (deg N), longitude (deg E), height (km)'
      read(5,*)ph1,alng1,h1
      write(6,*)'INPUT: ',
     &'Ray endpoint 2: latitude (deg N), longitude (deg E), height (km)'
      read(5,*)ph2,alng2,h2
C
C put "vertical" for "near vertical"
      if (abs(ph2-ph1).lt.1.0D-5.and.abs(alng2-alng1).lt.1.0D-5) then
         ph2=ph2
         alng2=alng1
      endif
      r1=Re+h1
      r2=Re+h2
C
C provides coordinates (pp,php,alamp) of ray perigee,
C zenith angle (zeta) for the ray endpoint 1,
C slant to vertical projection factor (cchi) for the
C conditions given by akappa (<1)
      call naut(r1,r2,ph1,ph2,alng1,alng2,akappa,
     &  pp,php,alamp,zeta,cchi)
      if (abs(zeta).gt.90.0.and.pp.lt.Re) then
         write(6,*) ' ray cuts surface of Earth'
         write(6,*) '      or endpoint 2 lower than endpoint 1.'
         write(6,*) ' Repeat input'
         goto 10
      endif
C parameters of ray properties for use in other modules (formerly in
C    a common block)
C    (origin for the s coordinate is the ray perigee with
C       lat-long coordinates php and alamp and the radius pp)
C    s1,c1:     sine and cosine of latitude point 1 (= ray perigee)
C    s2,c2:     sine and cosine of latitude point 2
C    ssig,csig: sine and cosine of ray azimuth
C               (point 2 seen from point 1)
      if (pp.ge.0.1D0)
     & call gcirc(php,ph2,alamp,alng2,s1,c1,s2,c2,ssig,csig,psi)
C    along1: longitude of point 1 (= ray perigee)
      along1=alamp
      return
      end

      subroutine dat_t_sa(iyr,mth,nday,ut,R12,flx)
C set date and solar activity
      implicit real*8 (a-h,o-z)
      dimension R12y(12)
      character*1 yn,fs
      nday=15
    1  write(6,*)'INPUT: year, month, UT:'
      read(5,*)iyr,mth,UT
      if (iyr.gt.100.and.(iyr.lt.1931.or.iyr.gt.2049)
     &    .or.iyr.lt.0) then
         write(6,*)
     &    'error in year (valid: 1931-2049 or 0-49 for 2000-2049'
         write(6,*)'   or 50-99 for 1950-1999)'
         write(6,*)'Repeat'
         goto 1
      endif
      if (mth.lt.1.or.mth.gt.12.or.UT.lt.0.or.UT.gt.24) then
         write(6,*)
     &    'input of month or UT not valid (valid: 1-12 and 0-24)'
         write(6,*)'Repeat'
         goto 1
      endif
      if (iyr.lt.50) iyr=iyr+2000
      if (iyr.lt.1900) iyr=iyr+1900
      if (iyr.ge.1931.and.iyr.le.2001) then
         write(6,*)'R12/F10.7 for this year and month (y/n)'
         read(5,'(A)')yn
       else
         yn='N'
       endif
      if (yn.eq.'y'.or.yn.eq.'Y') then
         open(15,file='R12.dat',status='OLD')
   11    read(15,*)j,R12y
         if (j.lt.iyr) goto 11
         close(15)
         R12=R12y(mth)
         flx=63.7D0+(0.728D0+8.9D-4*R12)*R12
      else
         write(6,*)
     &    'INPUT: solar activity type:',
     &    ' sunspot number (S) or 10.7 cm radio flux (F)?'
         read(5,'(A)')fs
         if (fs.eq.'F'.or.fs.eq.'f') then
            write(6,*)'INPUT: radio flux (>=63 units)'
            read(5,*)flx
            R12=sqrt(167273.0D0+(flx-63.7D0)*1123.6D0)-408.99D0
         else
            write(6,*)'INPUT: sunspot number (R12)'
            read(5,*)R12
            flx=63.7D0+(0.728D0+8.9D-4*R12)*R12
         endif
      endif
      if (flx.gt.193.0D0)
     &  write(6,'(2A/A/)')
     &  ' *** Input solar flux F exceeds 193 units. ',
     &  ' Following Recommendation ',
     &  '     ITU-R P. 1239, NeQuick limits effective F to 193 units.'
      return
      end

C  ***** special formulation without common blocks *****
C Package with auxiliary subroutines and functions needed to calculate
C "rays" given by their end points and to prepare electron density for
C integration along slant rays.
C The subroutines and functions were programmed (in this form) in 1998,
C NAUT was modified (additional input and output) in Nov. 1999)
C GEOGRA was modified (elimination of common blocks) in May 2002)
C ELD was modified (inclusion of GEOGRA) im May 2002
C
C Conventions:
C    spherical Earth
C    straight line "rays"
C    coordinate along rays, s, counted from ray perigee (point of
C       ray closest to the centre of the Earth), ray perigees below
C       the surface of the Earth are allowed
C
C    Release date (rayaux with common blocks): 2000/01/18
C    Release date (this version):              2002/05/31
C
C    author:
C    Dr. Reinhart Leitinger
C    Institut fuer Geophysik, Astrophysik und Meteorologie
C    (formerly Institut fuer Meteorologie und Geophysik)
C    Universitaet Graz
C    Unviersitaetsplatz 5
C    A-8010 Graz, Austria
C    e-mail  reinhart.leitinger@kfunigraz.ac.at

      subroutine gcirc(alat1,alat2,along1,along2,s1,c1,s2,c2,ssig,
     & csig,psi)
C
C  calculates great circle path properties
C  input parameters are the endpoints coordinates
C    (alat1,along1), (alat2,along2) (degrees)
C  output parameters are sine and cosine of the endpoint latitudes
C    s1,c1 and s2,c2, resp.,
C    sine and cosine of the azimuth of endpoint 1 seen from endpoint 1
C    ssig, csig (N over E to S)
C    and the great circle distance psi (deg.)
C
      implicit real*8 (a-h,o-z)
      parameter (DR=1.74532925199433D-2)
      rlat1=alat1*DR
      rlat2=alat2*DR
      dlong=(along2-along1)*DR
      s1=sin(rlat1)
      s2=sin(rlat2)
      c1=cos(rlat1)
      c2=cos(rlat2)
      sd=sin(dlong)
      cd=cos(dlong)
      if (abs(abs(alat1)-90.0D0).lt.1.0D-10) then
         psi=abs(alat2-alat1)
         ssig=0.0D0
         if (alat1.gt.0.0D0) then
            csig=-1.0D0
         else
            csig=1.0D0
         endif
      else
         cpsi=s1*s2+c1*c2*cd
         spsi=sqrt(1.0D0-cpsi*cpsi)
         ssig=c2*sd/spsi
         csig=(s2-s1*cpsi)/c1/spsi
         psi=atan2(spsi,cpsi)/DR
      endif
      return
      end

      subroutine naut(r1,r2,ph1,ph2,alng1,alng2,akappa,
     & pp,php,alamp,zeta,cchi)
C
C *** new version:
C  additional input parameter akappa, add. output parameter cchi ***
C
C  calculates position of ray perigee, zenith angle of ray at
C  lower endpoint (endpoint 1), and slant to vertical projection
C  factor cos(chi)
C
C  input parameters are the endpoint coordinates
C    (r1,ph1,alng1) (r2,ph2,alng2)
C    (distance from centre in km, latitude, longitude in degrees)
C    and the ratio akappa=r1/(r1+hi), hi being the "mean ionospheric
C    height" if endpoint 1 is at the surface of the Earth
C  output parameters are the coordinates of the ray perigee
C    (point on the ray [straight line through the endpoints] closest
C     to the centre of the Earth)
C    (pp,php,alamp),
C    the zenith angle of endpoint 2 seen from entpoint 1, zeta (deg.)
C    and the projection factor cchi=cos(chi), chi being the zenith
C    angle in the point with height hi above endpoint 1.
C
      implicit real*8 (a-h,o-z)
      parameter (DR=1.74532925199433D-2,RD=5.729577951308232D1)
      parameter ( pi=3.141592653589793D0)
      if (abs(ph1-ph2).lt.1.0D-5.and.abs(alng1-alng2).lt.1.0D-5) then
C vertical profile
         pp=0.0D0
         php=ph1
         alamp=alng1
         zeta=0.0D0
         cchi=1.0D0
      else
C slant profile
         sph1=sin(ph1*DR)
         cph1=cos(ph1*DR)
         sph2=sin(ph2*DR)
         cph2=cos(ph2*DR)
         cdl12=cos((alng2-alng1)*DR)
         sdl12=sin((alng2-alng1)*DR)
         cdel=sph1*sph2+cph1*cph2*cdl12
         sdel=sqrt(1.0D0-cdel*cdel)
         zeta=atan2(sdel,cdel-r1/r2)
         ssigp=sdl12*cph2/sdel
         csigp=(sph2-cdel*sph1)/sdel/cph1
         delp=-zeta+pi/2.0D0
         sdelp=sin(delp)
         cdelp=cos(delp)
         sphp=sph1*cdelp-cph1*sdelp*csigp
         cphp=sqrt(1.0D0-sphp*sphp)
         php=atan2(sphp,cphp)*RD
         slamp=-ssigp*sdelp/cphp
         clamp=(cdelp-sph1*sphp)/cph1/cphp
         alamp=atan2(slamp,clamp)*RD+alng1
         szeta=sin(zeta)
         pp=r1*szeta
         zeta=zeta*RD
         schi=akappa*szeta
         cchi=sqrt(1.0D0-schi*schi)
      endif
      return
      end

      subroutine geogra(s, pp,Re,s1,c1,ssig,csig,along1, h,alat,along)
C
C  calculates height, latitude and longitude along the given "ray"
C  for given distance from the perigee of the ray
C  input parameter: distance from the perigee of the ray, s (km)
C  output parameters:
C    height h (km), latitude alat (deg. N), longitude along (deg. E)
C
C  the properties of the ray pp,s1,c1,ssig,csig,along1
C      and the Earth radius Re are input parameters
C
      implicit real*8 (a-h,o-z)
      parameter (RD=5.729577951308232D1)
      tdel=s/pp
      cdel=1.0D0/sqrt(1.0D0+tdel*tdel)
      sdel=tdel*cdel
      arg=s1*cdel+c1*sdel*csig
      alat=atan2(arg,sqrt(1.0D0-arg*arg))*RD
      clong=atan2(sdel*ssig*c1,cdel-s1*arg)*RD
      along=clong+along1
      h=sqrt(s*s+pp*pp)-Re
      return
      end

      real*8 function eld(s,pp,Re,s1,c1,ssig,csig,along1)
C
C  gives electron density as a function of the coordinate s
C    (distance from the perigee of the ray)
C
      implicit real*8 (a-h,o-z)
      parameter (RD=5.729577951308232D1)
      tdel=s/pp
      cdel=1.0D0/sqrt(1.0D0+tdel*tdel)
      sdel=tdel*cdel
      arg=s1*cdel+c1*sdel*csig
      alat=atan2(arg,sqrt(1.0D0-arg*arg))*RD
      clong=atan2(sdel*ssig*c1,cdel-s1*arg)*RD
      along=clong+along1
      h=sqrt(s*s+pp*pp)-Re
      eld=eldens(h,alat,along)
      return
      end
