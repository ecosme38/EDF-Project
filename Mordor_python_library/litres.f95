!c	=================================================================
	subroutine litres(nomfic,pmtMOR)
	!DEC$ ATTRIBUTES DLLEXPORT :: litres
 	!DEC$ ATTRIBUTES ALIAS:"litres_" :: litres
	!DEC$ ATTRIBUTES C,REFERENCE :: litres            ! Pour "R"
!c 	!DEC$ ATTRIBUTES STDCALL,REFERENCE :: litres   ! Pour "Excel"	
!c	-----------------------------------------------------------------
!c	Lecture du paramétrage du modèle hydrologique MORDOR SD
!c	Version 2011.10.A
!c     Jan 2017 : Initialisation du parametres ptjp1 en fonction du pdt
!c	-----------------------------------------------------------------

!c	Declarations
      double precision pmtMOR(*)
      character nomfic*256
	integer i,j
	logical err
	real pmt(100),tmp(10)
	character ligne*256,modul*12,opt*10,rep*3
	logical :: old_deriv = .FALSE.

!c	Initialisation des paramètres
      pmt(1:100) = 0.
      pmt(22)    = 100.     ! etp1 oudin
      pmt(23)    = -5.      ! etp2 oudin
      pmt(89)    = 1        ! nsbv
      		
!c	Ouverture de la fiche de calage .RES
	err=.true.
	open(10,file=nomfic,form='formatted',status='old',err=99)
	
	read(10,fmt=5,err=99) ligne
      read(10,fmt=5,err=99) ligne
	read(10,fmt=5,err=99) ligne
	read(10,fmt=5,err=99) ligne
	read(10,fmt=5,err=99) ligne
5	format(a)	

!c	Balayage des options de modélisation
	do i=1,6
		read(10,fmt=16,err=99) modul,opt
16		format(1x,a12,3x,a10)
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
	
!c	Recherche du bloc des paramètres
	do while (index(ligne,'PARAMETRES').eq.0)
		read(10,fmt=5,err=99) ligne
	enddo	

!c	Lecture des parametres sbv, pdt, zsta et z50 
	read(10,fmt=5,err=99) ligne
	read(ligne,20) pmt(7),pmt(8),pmt(14),pmt(13)
20	format(1x,8x,f7.1,8x,f5.0,2x,2(8x,f7.0))
	pmt(9)=pmt(7)/(pmt(8)*3.6)

!c	Lecture des parametres xlat, flac, fglace, lon 
	read(10,fmt=5,err=99) ligne
	read(ligne,25) pmt(12),pmt(11),pmt(10),pmt(15)
25	format(1x,4(8x,f7.2))

!c	Lecture des paramètres pluie-debit 
      read(10,fmt=5,err=99) ligne
      read(10,fmt=5,err=99) ligne
      read(10,fmt=5,err=99) ligne  
	read(ligne,25) pmt(17),pmt(18),pmt(19),pmt(24)                 ! cp,cetp,csbv,kmin
      read(10,fmt=5,err=99) ligne
  	if(pmt(4).ge.1) read(ligne,25) pmt(21),pmt(20)                 ! gpz,gtz   
  	if(pmt(4).lt.1) read(ligne,25) pmt(21),pmt(20),pmt(22),pmt(23) ! gpz,gtz,etp1,etp2
      read(10,fmt=5,err=99) ligne
	read(ligne,26) pmt(25),pmt(26),pmt(27),pmt(29)                 ! umax,lmax,zmax,kr
26	format(1x,3(8x,f7.0),(8x,f7.2))
      read(10,fmt=5,err=99) ligne
	read(ligne,25) pmt(28),pmt(30),pmt(31)                         ! evl,lkn,evn
      if (old_deriv) read(ligne,28) pmt(28),pmt(30),pmt(31),pmt(48)  ! evl,lkn,evn,qext
28	format(1x,3(8x,f7.2),(8x,f7.1))
      read(10,fmt=5,err=99) ligne
  	read(ligne,25) pmt(32),pmt(33)                                 ! cel,dif    

!c	Lecture des paramètres derivation 
	if (.NOT.old_deriv.AND.pmt(2).ge.1) then
        read(10,fmt=5,err=99) ligne
  	  read(ligne,25) pmt(89),pmt(48),pmt(90),pmt(91)               ! nsbv,qext,qres,lqs  
      endif
      
!c	Lecture des paramètres neige 
	if (pmt(1).eq.1) then
	  read(10,fmt=5,err=99) ligne
	  read(ligne,25) pmt(36),pmt(37),pmt(39)						 ! kf,kfp,eft
	  read(10,fmt=5,err=99) ligne
 	  read(ligne,25) pmt(38),pmt(40),pmt(41)						 ! efp,lts,gm
	endif

!c	Lecture des paramètres glace 
	if(pmt(5).ge.1.) then
		read(10,fmt=5,err=99) ligne
		read(ligne,27) pmt(42),pmt(43),pmt(44),pmt(45)               !kg,kgp,efg,jg   
      endif
27	format(1x,3(8x,f7.2),8x,f7.0)

!c	Recherche du bloc Bandes d'altitudes
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
              
30	format(9x,10(f6.0))

!c	Fermeture du fichier .RES
	err=.false.
	close(10)

!c     Initialisation du parametres ptjp1 en fonction du pdt	
	if(int(pmt(8)).eq.24)then;pmt(46)=0.5;else;pmt(46)=0;endif
	
!c     Sortie	
	pmtMOR(1:100) = dble(pmt(1:100))			
	err=.false.

!c	Fin
99	return

	end
