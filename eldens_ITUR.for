      program elditur
      implicit real*8 (a-h,o-z)
      character*10 filen1
      character*1 fs
      real*8 NeQuick
      filen1='eldens.dat'
      open(16,file=filen1)
      write(6,*)
      write(6,*)'          ****************************************'
      write(6,*)'          *        Test of NeQuick_ITU-R         *'
      write(6,*)'          *   single values and height profile   *'
      write(6,*)'          ****************************************'
      write(6,*)
      write(6,*)'     Three tests are made:'
      write(6,*)'         1) single values of electron density for'
      write(6,*)'            given Universal Time'
      write(6,*)'         2) single values of electron density for'
      write(6,*)'            given Local Time'
      write(6,*)'         3) height profile of electron density for'
      write(6,*)'            the Universal Time used in test 1.'
      write(6,*)'     The locations for tests 1 and 2 have to be put in'
      write(6,*)'        as geographic coordinates and height.'
      write(6,*)'        Repeated location input is expected until a'
      write(6,*)'        value >90 or <-90 is put in for the latitude'
      write(6,*)'     Test 3 provides a single height profile over a'
      write(6,*)'        location given by its geographic coordinates.'
      write(6,*)'     The tests are sequential.'
      write(6,*)
      write(6,*)'INPUT: month and UT (hours)'
      read(5,*)mth,UT
      write(6,*)
     & 'INPUT: solar activity type:',
     & ' sunspot number (S) or 10.7 cm radio flux (F)?'
      read(5,'(A)')fs
      if (fs.eq.'F'.or.fs.eq.'f') then
         write(6,*)'INPUT: radio flux (>=63 units)'
         read(5,*)flx
         R12=sqrt(167273.0D0+(flx-63.7)*1123.6D0)-408.99D0
      else
         write(6,*)'INPUT: sunspot number (R12)'
         read(5,*)R12
         flx=63.7D0+(0.728D0+8.9D-4*R12)*R12
      endif
      if (flx.gt.193.0D0)
     &  write(6,'(2A/A/)')
     &  ' *** Input solar flux F exceeds 193 units. ',
     &  ' Following Recommendation ',
     &  '     ITU-R P. 1239, NeQuick limits effective F to 193 units.'
      write(16,'(A,2(F6.1,1H,),I3,1H,,F5.1)')
     + 'S10.7, R12, month, UT: ',flx,R12,mth,ut

      alat=45.0D0
      along=15.0D0
      h=300.0D0
      aNe=NeQuick(h,alat,along,mth,flx,ut)
      write(6,*)
      write(6,*)'NeQuick test 1: electron densities for constant UT'
      write(6,*)
     & 'coordinates loop: end of input: lat > 90 or lat < 90 degrees'
  100 write(6,*)
     & 'INPUT:',
     &' gg. latitude (deg. N), gg. longitude (deg. E), height (km)'
      read(5,*)alat,along,h
      if (abs(alat).gt.90.0D0) goto 110
      aNe=eldens(h,alat,along)
      write(6,'(A,E12.5,A)')
     & ' NeQuick electron density =',aNE,' m^-3'
      goto 100
  110 write(6,*)
      write(6,*)'NeQuick test 2: electron densities for constant LT'
      write(6,*)'INPUT: Local time (LT in hours)'
      read(5,*) xlt
      write(6,*)
      write(6,*)
     & 'coordinates loop: end of input: lat > 90 or lat < 90 degrees'
  200 write(6,*)
     & 'INPUT:',
     &' gg. latitude (deg. N), gg. longitude (deg. E), height (km)'
      read(5,*)alat,along,h
      if (abs(alat).gt.90.0D0) goto 210
      ut1=xlt-along/15.0d0
      aNe=NeQuick(h,alat,along,mth,flx,ut1)
      write(6,'(A,E12.5,A)')
     & ' NeQuick electron density =',aNE,'  m^-3'
      goto 200
  210 write(6,*)
      write(6,*)'NeQuick test 3: height profile of electron density'
      write(6,*)
     & 'INPUT: gg. latitude (deg. N), gg. longitude (deg. E)'
      read(5,*)alat,along
      write(6,*)
     & 'INPUT:',
     & ' lower height limit, upper height limit, heigt step (all in km)'
      read(5,*)ih1,ih2,idh
      write(16,'(A,F7.2,A,F8.2,A)')
     & '   electron density profile for',alat,'N',along,'E'
      write(16,*)'height electron density'
      write(16,*)' km     m^-3'
      do ih=ih1,ih2,idh
      h=dfloat(ih)
      write(16,'(I5,E12.5)')ih,vert(h)
      enddo

      close(16)
      write(6,*)'Profile output in ',filen1
      end
