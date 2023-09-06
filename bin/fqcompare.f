      program fqcompare

********************************************************************
c     Read formfactors and calcate they difference 
*******************************************************************
      
      implicit none
            
      open(ki1,file=file1,status='old',err=550)	
      open(k12,file=file2,status='old',err=550)
      
      read(ki1,*) nstat, nr, rstep, rfirst, qmax
      read(ki2,*) nstat, nr, rstep, rfirst, qmax
      
      
    do i=1, nstat
         read(ki2,*) ener(i),jtot(i),par(i),nec(i),nbin(i)
         if (.not.incl(i)) cycle
         ijtot=nint(jtot(i))
         if (ener(i).gt.ecutv(ijtot)) then
            incl(i)=.false.

         else
            ninc=ninc+1
            ind(i)=ninc
         endif
     enddo
     
100   read(ki2,*,end=200) n1, n2, iq 
      m1=ind(n1)
      m2=ind(n2)
      write(*,'(2x,"<",i3,"|",i2,"|",i3,">")') m1,iq,m2
     
      do ir=1,nr
         x=rfirst+(ir-1)*rstep
         read(ki1,*) r11,r12
         read(ki2,*) r21,r22 

      enddo
 
      goto 100 ! read new formfactor
      
200   write(*,*) 'Cabo Programa - Rogerinho'
      
  
