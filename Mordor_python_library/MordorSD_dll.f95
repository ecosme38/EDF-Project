!Programation     2016
!-----------------------------------------------------------------
!DLL de MORDOR_SD à partir de la librarie MORDOR_SD_OM.for
!-----------------------------------------------------------------
      subroutine mordor_sd(jjqpt,pmt,cond,out,init)

      !DEC$ ATTRIBUTES DLLEXPORT :: mordor_sd
      !DEC$ ATTRIBUTES ALIAS:"mordor_sd_" :: mordor_sd
      !DEC$ ATTRIBUTES C,REFERENCE :: mordor_sd            ! Pour "R"
!!DEC$ ATTRIBUTES STDCALL,REFERENCE :: mordorssd   ! Pour "Excel"

      implicit none
      include 'dim_mordor_SD.f95'
! USE DIM_MORDOR

!Declarations arguments (dble pour compataibilite R)
	  double precision, intent(in) :: jjqpt(tmax,4)
	  double precision, intent(inout) :: pmt(npmt)
	  double precision, intent(inout) :: cond(ncond)
	  double precision, intent(inout) :: out(tmax,nres)
	  
      integer npdt,init

!Declarations variables locales
      integer flag(nflag),hour,i,j
      real pmtMOR(npmt),don(tmax,ndon),res(tmax,nres),stock(tmax)
      real Rpot,Kpot,cond_s(ncond)
      double precision radiation
      integer mod_deriv,signe
      real pdt,lqs,qext,csbv,nsbv,qres,fr_nat,fr_der
                 
!Preparation du vecteur des donnees
      pmtMOR = real(pmt)
      npdt = cond(1)
      
!Parametres modèle dérivation
      mod_deriv = pmtMOR(2)
      if (mod_deriv.ge.1) then
          pdt  = pmtMOR(8)
          lqs  = pmtMOR(91)**(pdt/24.)   ! parametre de lissage exponentiel        
          qext = pmtMOR(48)
          csbv = pmtMOR(19)
          nsbv = pmtMOR(89)
          qres = pmtMOR(90)
      endif

!Q, PS et Tmoy
      don(1:npdt,1) = real(jjqpt(1:npdt,2)) ! Q 
      don(1:npdt,2) = real(jjqpt(1:npdt,3)) ! PS
      don(1:npdt,3) = real(jjqpt(1:npdt,4)) ! Tmoy
      
!Calcul rayonnement       
      do i=1,npdt
        don(i,8) = int(jjqpt(i,1)) ! Jour Julien
        hour  =  nint((real(jjqpt(i,1))-don(i,8))*24) ! Heure
        if (don(i,8).eq.366.) don(i,8)=365.
        Rpot=sngl(radiation(int(don(i,8)),dble(pmtMOR(12))))
        if(int(pmtMOR(8)).eq.24)then
          don(i,9)=Rpot
        else
          Kpot=0
          do j=1,int(pmtMOR(8))
            Kpot=Kpot+max(0.,3.1416*sin((2.*(real(hour-j+1)-6.5))/24.*3.1416))
          enddo
          don(i,9)=Rpot*Kpot/pmtMOR(8) ! Vecteur rayonnement 
        endif
      enddo

!Preparation du vecteur des parametres
!Calcul du rayonnement moyen
      pmtMOR(47) = 0
      do i=1,365
        pmtMOR(47)= pmtMOR(47) + sngl(radiation(i,dble(pmtMOR(12))))
      enddo
      pmtMOR(47)=pmtMOR(47)/365.
!Calcul du facteur echelle dh
      pmtMOR(9)=pmtMOR(7)/(pmtMOR(8)*3.6)         

!Initialisation ptjp1
      if(int(pmtMOR(8)).eq.24)then;pmtMOR(46)=0.5;else;pmtMOR(46)=0;endif !ptjp1 fixé en fonction du pdt

!Initialisation des stocks (si init egal a 1)      
      flag = 0.
      cond_s = real(cond)  
      if (init.eq.1) call init_mordor(pmtMOR,don,cond_s,res,flag)                  
      
!Run MORDOR SD     
      call mordor_SD_OM(pmtMOR,don,cond_s,res,flag)
      
!Passage en m3/s
      out(1:npdt,:) = dble(res(1:npdt,:))
      out(1:npdt,1) = out(1:npdt,1)*pmtMOR(9)
      
!Prise en compte csbv si modDERIV=0
      if(mod_deriv.lt.1) out(1:npdt,1) = pmtMOR(19)*out(1:npdt,1)     
        
!Calcul derivation 
       if (mod_deriv.eq.1) then 
            signe = sign(1.,csbv-nsbv) ! 0 +1 ou -1
            fr_nat = nsbv              ! % naturel (fraction)
            fr_der = abs(csbv-nsbv)    ! % dérivé (fraction)
            out(1:npdt,1) = fr_nat*out(1:npdt,1)&
     &      + signe*min(max(fr_der*out(1:npdt,1)-qres,0.),qext)
       endif
      
!Calcul debordement avec stockage et restitution
      if (mod_deriv.eq.2) then 
        signe  = sign(1.,csbv-nsbv) ! 0 +1 ou -1
        fr_nat = nsbv              ! % naturel (fraction)
        fr_der = abs(csbv-nsbv)    ! % dérivé (fraction)
        stock(1:npdt) = max(fr_der*out(1:npdt,1)-qres-qext,0.)
        out(1:npdt,1) = fr_nat*out(1:npdt,1)&
     & + signe*min(max(fr_der*out(1:npdt,1)-qres,0.),qext)
        do i = 2,npdt
           stock(i) = lqs*stock(i-1) + (1-lqs)*stock(i)
        enddo
        out(1:npdt,1) = out(1:npdt,1) + stock(1:npdt)
      endif    
      !print *, "from fortran MordorSD_dll, out npdt :", npdt
      !print *, out(1:npdt,1)
      pmt = dble(pmtMOR)
      cond = dble(cond_s)
      return
      end
      
!c =================================================================
 subroutine litres(nomfic,pmtMOR)
 !DEC$ ATTRIBUTES DLLEXPORT :: litres
  !DEC$ ATTRIBUTES ALIAS:"litres_" :: litres
 !DEC$ ATTRIBUTES C,REFERENCE :: litres            ! Pour "R"
!c  !DEC$ ATTRIBUTES STDCALL,REFERENCE :: litres   ! Pour "Excel" 
!c -----------------------------------------------------------------
!c Lecture du paramétrage du modèle hydrologique MORDOR SD
!c Version 2011.10.A
!Jan 2017 : Initialisation du parametres ptjp1 en fonction du pdt
!c -----------------------------------------------------------------

!c Declarations
      double precision pmtMOR(*)
      character nomfic*256
 integer i,j
 logical err
 real pmt(100),tmp(10)
 character ligne*256,modul*12,opt*10,rep*3
 logical :: old_deriv = .FALSE.

!c Initialisation des paramètres
      pmt(1:100) = 0.
      pmt(22)    = 100.     ! etp1 oudin
      pmt(23)    = -5.      ! etp2 oudin
      pmt(89)    = 1        ! nsbv
        
!c Ouverture de la fiche de calage .RES
 err=.true.
 open(10,file=nomfic,form='formatted',status='old',err=99)
 
 read(10,fmt=5,err=99) ligne
      read(10,fmt=5,err=99) ligne
 read(10,fmt=5,err=99) ligne
 read(10,fmt=5,err=99) ligne
 read(10,fmt=5,err=99) ligne
5 format(a) 

!c Balayage des options de modélisation
 do i=1,6
  read(10,fmt=16,err=99) modul,opt
16  format(1x,a12,3x,a10)
  rep=opt(1:3)
  
  select case(modul)
  case('Module neige')
   if (rep.eq.'oui') pmt(1)=1.
  case('Module glace')
   if (rep.eq.'oui') pmt(5)=1.
  case('Module bvi  ')
   if (rep.eq.'oui') tmp(1)=0.
  case('Module lac  ')
   if (rep.eq.'oui') pmt(3)=1.
  case('Module Deriv')
      old_deriv = .TRUE.
   if (rep.eq.'oui') pmt(2)=1.
  case('Module deriv')
   select case(opt)
    case('derivation')
     pmt(2)=1.
    case('debordemen')
     pmt(2)=2.
   end select    
  case('Module etp  ')
   select case(opt)
    case('etp MORDOR')
     pmt(4)=0.
    case('etp Oudin ')
     pmt(4)=1.
    case('etp extern')
     pmt(4)=2.
   end select    
  end select
 enddo
 
!c Recherche du bloc des paramètres
 do while (index(ligne,'PARAMETRES').eq.0)
  read(10,fmt=5,err=99) ligne
 enddo 

!c Lecture des parametres sbv, pdt, zsta et z50 
 read(10,fmt=5,err=99) ligne
 read(ligne,20) pmt(7),pmt(8),pmt(14),pmt(13)
20 format(1x,8x,f7.1,8x,f5.0,2x,2(8x,f7.0))
 pmt(9)=pmt(7)/(pmt(8)*3.6)

!c Lecture des parametres xlat, flac, fglace, lon 
 read(10,fmt=5,err=99) ligne
 read(ligne,25) pmt(12),pmt(11),pmt(10),pmt(15)
25 format(1x,4(8x,f7.2))

!c Lecture des paramètres pluie-debit 
      read(10,fmt=5,err=99) ligne
      read(10,fmt=5,err=99) ligne
      read(10,fmt=5,err=99) ligne  
 read(ligne,25) pmt(17),pmt(18),pmt(19),pmt(24)                 ! cp,cetp,csbv,kmin
      read(10,fmt=5,err=99) ligne
   if(pmt(4).ge.1) read(ligne,25) pmt(21),pmt(20)                 ! gpz,gtz   
   if(pmt(4).lt.1) read(ligne,25) pmt(21),pmt(20),pmt(22),pmt(23) ! gpz,gtz,etp1,etp2
      read(10,fmt=5,err=99) ligne
 read(ligne,26) pmt(25),pmt(26),pmt(27),pmt(29)                 ! umax,lmax,zmax,kr
26 format(1x,3(8x,f7.0),(8x,f7.2))
      read(10,fmt=5,err=99) ligne
 read(ligne,25) pmt(28),pmt(30),pmt(31)                         ! evl,lkn,evn
      if (old_deriv) read(ligne,28) pmt(28),pmt(30),pmt(31),pmt(48)  ! evl,lkn,evn,qext
28 format(1x,3(8x,f7.2),(8x,f7.1))
      read(10,fmt=5,err=99) ligne
   read(ligne,25) pmt(32),pmt(33)                                 ! cel,dif    

!c Lecture des paramètres derivation 
 if (.NOT.old_deriv.AND.pmt(2).ge.1) then
        read(10,fmt=5,err=99) ligne
     read(ligne,25) pmt(89),pmt(48),pmt(90),pmt(91)               ! nsbv,qext,qres,lqs  
      endif
      
!c Lecture des paramètres neige 
 if (pmt(1).eq.1) then
   read(10,fmt=5,err=99) ligne
   read(ligne,25) pmt(36),pmt(37),pmt(39)       ! kf,kfp,eft
   read(10,fmt=5,err=99) ligne
    read(ligne,25) pmt(38),pmt(40),pmt(41)       ! efp,lts,gm
 endif

!c Lecture des paramètres glace 
 if(pmt(5).ge.1.) then
  read(10,fmt=5,err=99) ligne
  read(ligne,27) pmt(42),pmt(43),pmt(44),pmt(45)               !kg,kgp,efg,jg   
      endif
27 format(1x,3(8x,f7.2),8x,f7.0)

!c Recherche du bloc Bandes d'altitudes
 do while (index(ligne,'DECOUPAGE').eq.0)
  read(10,fmt=5,err=99) ligne
 enddo 
 read(10,fmt=5,err=99) ligne
      read(ligne,30) (tmp(j),j=1,10)
      pmt(16)=maxval(tmp)
      read(10,fmt=5,err=99) ligne
      read(ligne,30) (pmt(49+4*(i-1)),i=1,pmt(16)) !Z50 stocks   
      read(10,fmt=5,err=99) ligne
      read(ligne,30) (pmt(50+4*(i-1)),i=1,pmt(16)) !Fsurf
      if (pmt(5).eq.1) then
        read(10,fmt=5,err=99) ligne
        read(ligne,30) (pmt(51+4*(i-1)),i=1,pmt(16)) !Fglacei
        read(10,fmt=5,err=99) ligne
        if(ligne(2:2).ne."-") then
                read(ligne,30) (pmt(52+4*(i-1)),i=1,pmt(16)) !Fglacef
                pmt(5) = 2. 
        endif              
      endif      
              
30 format(9x,10(f6.0))

!c Fermeture du fichier .RES
 err=.false.
 close(10)

!Initialisation du parametres ptjp1 en fonction du pdt 
 if(int(pmt(8)).eq.24)then;pmt(46)=0.5;else;pmt(46)=0;endif
 
!Sortie 
 pmtMOR(1:100) = dble(pmt(1:100))   
 err=.false.

!c Fin
99 return

 end
