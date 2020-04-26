!-----------------------------------------------------------------
!MODELE MORDOR 
!-----------------------------------------------------------------
 subroutine mordor_SD_OM(pmt,don,cond,res,flag)
!-----------------------------------------------------------------
!MARCO-POLO V2 - Outils Métiers Eau
!Sub de référence
!-----------------------------------------------------------------
!Programation        1990-2003
!Adaptation OM        aout 2004
!Integration variante NN  juin 2005
!NN non routée par la FT  janv 2007
!Sortie du modèle neige        sept 2012
!Sortie module ETP             dec  2012
!Sortie du module FT           sept 2013
!-----------------------------------------------------------------
!Modif FGa + MLL               fev  2014
!Refus proportionel de U 
!Remplissage et vidange non linaire de L
!Neutralisation P par ETP
!Introduction Kc variable 
!Routage de rbase 
!-----------------------------------------------------------------
!Modif FGa + MLL               janv 2015
!Réécriture refus Zmax         
!Version differentielle de rbase
!FT predice
!-----------------------------------------------------------------
!Modif MLL                     avri 2015
!Coef cultural fonction de Rpotentiel
!-----------------------------------------------------------------
!Modif FGa + MLL               oct 2015
!Modification pour integration dans Schadex
!Optimisation du multi-run 
!-----------------------------------------------------------------
!Modif FGa + MLL               mai 2016           
!Re-ecriture pmt                
!Integration surface glaciaire variable 
!Re-ecriture FT
!-----------------------------------------------------------------
!Modif FGa + MLL + JG          sept 2016           
!Modif calcul FRL               
!Modif calcul FNEIGE
!-----------------------------------------------------------------

      implicit none
include 'dim_mordor_SD.f95'
!        USE DIM_MORDOR
       
!-----------------------------------------------------------------
!DECLARATIONS VARIABLES LOCALES
!-----------------------------------------------------------------
 
      integer flag(nflag)
      integer i,j,mod_out
      integer mod_neige,mod_lac,mod_etp,mod_glace
      integer npdt,nsn
      real pmt(npmt),don(tmax,ndon),cond(ncond),res(tmax,*)
      real u(0:tmax,nstock),l(0:tmax,nstock),z(0:tmax,nstock)
      real ep(tmax,nstock),er(tmax,nstock)
      real emax(tmax,nstock)
      real pn(tmax,nstock)
      real rvers(tmax,nstock)
      real vs(tmax,nstock),al(tmax,nstock)
      real tmn(tmax,nstock)
      real tft(0:tmax,nstock)
      real psn(tmax,nstock),rui(tmax,nstock),ruc(tmax,nstock)
      real sn(0:tmax,nstock),sns(0:tmax,nstock),snl(0:tmax,nstock)
      real tst(0:tmax,nstock),lft(tmax,nstock),fliq(tmax,nstock)
      real pls(tmax,nstock),ngs(tmax,nstock)
      real lgl(tmax,nstock),agl(tmax,nstock)
      real ft(tmax,nstock),fb(tmax,nstock)

      real z_sn(nstock)
      real s_sn(nstock),s_gl(tmax,nstock),s_ng(tmax,nstock)
      real tmp(nstock),tmp2(nstock)
      real s_gi(nstock),s_gf(nstock)
      real evu(nstock),vl(nstock)
      real evz(nstock),ev0(nstock),an(tmax,nstock)
      real dtu1(nstock),dtr1(nstock),dtz(nstock)
      
      real n(0:tmax)
      real rt(-ftrm:tmax),rtr(tmax)
      real ep_t(tmax),er_t(tmax),an_t(tmax),rbase(-ftrm:tmax),emax_t(tmax)
      real rbaser(tmax)
      
      real cp,dh,flac,Umax,Lmax,zmax,evn,evl,kr,cetp,lkn,erl
      real gpz,gtz,zt,z50,z50p,pdt
      real fnts0,fnts1,fnts0i,fnts0f,sumgl
      integer use_fnts0
      
      real kc(365)
!-----------------------------------------------------------------
!LECTURE DES PARAMETRES
!-----------------------------------------------------------------

      npdt = cond(1)

!Choix de modélisation
      mod_neige =int(pmt(1))
      mod_lac   =int(pmt(3))
      mod_etp   =int(pmt(4))
      mod_glace =int(pmt(5))

!Options SCHADEX
 mod_out = nint(pmt(6))
      
!Lecture des paramètres
      pdt  =pmt(8)
 dh   =pmt(9)
  cp   =pmt(17)
 cetp =pmt(18)
  flac =pmt(11)
  
!reservoirs U,L,Z,N
 Umax =pmt(25)
 Lmax =pmt(26)
 Zmax =pmt(27) 
 lkn   =(10**pmt(30))*pdt/24
 evn  =pmt(31)

!Parametres du remplissage et de la vidange de L 
 evl  =pmt(28) ! exposant de la vidange de L
 kr   =pmt(29) ! repartition entre rvers et an

!Température
      gtz  =pmt(20) ! gradient
      z50  =pmt(13) ! altitude z50 (m)
      zt   =pmt(14) ! altitude du poste Tair(m)

!Precip
      gpz =pmt(21)

!Superficie glaciaire   
      fnts0=pmt(10)
      nsn=int(pmt(16))
      
!Parametre fixe
      erl = 2
 
!-----------------------------------------------------------------
!INITIALISATION
!-----------------------------------------------------------------

!Variables globales
      flag=0
      n(0)=cond(2)
      rtr=0.
      rt=0.
      
!Variables étagées
do j=1,nsn 
        u(0,j) = cond((j-1)*7+3)
  l(0,j) = cond((j-1)*7+4)
  z(0,j) = cond((j-1)*7+5)
  sn(0,j) = cond((j-1)*7+6)
  sns(0,j) = cond((j-1)*7+7)
  snl(0,j) = cond((j-1)*7+8)
  tst(0,j) = cond((j-1)*7+9)
 enddo


      
!c    ----------------------------------------------------------------- 
!c    CARACTERISTIQUE DES STOCKS 
!c    ----------------------------------------------------------------- 

!Initialisation
      z50p     = 0.
      fnts0i   = 0.
      fnts0f   = 0.
      use_fnts0= 0
      s_gi     = 0.
      s_gf     = 0.
      fnts1    = 0.

!Affectation
      do i=1,nsn
        z_sn(i)=pmt(4*i+45)
        s_sn(i)=pmt(4*i+46)
        s_gi(i)=pmt(4*i+47)
        z50p=z50p+z_sn(i)*s_sn(i)
        fnts0i=fnts0i+s_gi(i)*s_sn(i)
        if((s_gi(i).eq.NODATA).or.(s_gf(i).eq.NODATA)) use_fnts0=1
        if(mod_glace.eq.2) then ! surface glaciaire var
            s_gf(i)=pmt(4*i+48)
            fnts0f=fnts0f+s_gf(i)*s_sn(i)
        endif
      enddo
      
!répartition de la glace (fnts0) sur les stocks 

!Cas fglace renseigné dans fichier DECOUP
      if(use_fnts0.eq.0)then 
        pmt(10)=fnts0i
        fnts0=fnts0i
      endif
      if((fnts0.eq.NODATA).or.(fnts0.lt.0).or.(fnts0.gt.1)) fnts0=0
      
!Cas fglace non renseigné dans fichier DECOUP
      if(use_fnts0.eq.1.and.mod_glace.gt.0)then
        fnts1=s_sn(nsn)
        j=nsn
        do while(fnts1.lt.fnts0)
          j=j-1
          fnts1=fnts1+s_sn(j)
        enddo
        s_gi((j+1):nsn)=1
        s_gi(j)=(fnts0-fnts1+s_sn(j))/s_sn(j)
        s_gi(1:(j-1))=0
        s_gf(1:nsn) = s_gi(1:nsn)
        do i=1,nsn
          pmt(4*i+47)=s_gi(i)
          pmt(4*i+48)=s_gf(i)
        enddo
      endif

!z50 et altitude de la mesure de température
      if(zt.eq.NODATA)then
        zt=z50
        pmt(14)=zt
      endif
        
!c    ----------------------------------------------------------------- 
!c    CALCUL COEFFICIENT CULTURAL
!c    ----------------------------------------------------------------- 
    
      call CoefCultural(pmt,kc(1:365))
    
!c    ----------------------------------------------------------------- 
!c    FONCTION DE PRODUCTION
!c    ----------------------------------------------------------------- 
      
!Boucle sur stocks
      do j=1,nsn
      
!  Répartition P et T sur les bandes
        psn(1:npdt,j)=cp*don(1:npdt,2)*(1+gpz/1000*(z_sn(j)-z50p)) 
        tmn(1:npdt,j)=don(1:npdt,3)+gtz/100*(z_sn(j)-zt)
        
!  Module Neige
        if(mod_neige.eq.0)then
          rui(1:npdt,j) =psn(1:npdt,j)
          pls(1:npdt,j) =psn(1:npdt,j)
          ngs(1:npdt,j) =0.
          lft(1:npdt,j) =0.
          lgl(1:npdt,j) =0.
          agl(1:npdt,j) =0.
          s_gl(1:npdt,j)=0. 
          
        else
          call ModuleNeige(pmt, &
     &    psn(1,j),tmn(1,j),don(1,9),don(1,8),&
     &    sn(0,j),sns(0,j),snl(0,j),lft(1,j),pls(1,j),ngs(1,j),&
     &    fliq(1,j),tst(0,j),agl(1,j),&
     &    rui(1,j),ruc(1,j),ft(1,j),fb(1,j),npdt)      

!  Module glace   
          if(s_gi(j).gt.0)then
            call ModuleGlace(pmt,npmt,sn(1,j),tmn(1,j),don(1,9),lgl(1,j),npdt)
            if(mod_glace.eq.2) then     ! surf glace variable
                sumgl=sum(lgl(2:npdt,j))
                if(sumgl.gt.0)then
                    s_gl(1,j)=s_gi(j)
                    do i=2,npdt
                       s_gl(i,j)=s_gl(i-1,j)+lgl(i,j)/sumgl*(s_gf(j)-s_gi(j))
                    enddo
                else  
                    s_gl(1:npdt,j)=s_gi(j)
                endif
             else
                s_gl(1:npdt,j)=s_gi(j)
             endif        
          else
            lgl(1:npdt,j)=0
            s_gl(1:npdt,j)=0
          endif  ! Fin du modèle glace
    
    endif  ! Fin du modèle neige
    
      
!  Module ETP :
        select case (mod_etp)
        case(0)
          call ETP_V0(pmt,tmn(1:npdt,j),ep(1:npdt,j),npdt)
        case(1)
          call ETP_V1(pmt,tmn(1:npdt,j),ep(1:npdt,j),don(1:npdt,8),npdt)
        case(2)
          ep(1:npdt,j) = don(1:npdt,6)
        end select

!  BOUCLE EN TEMPS
        do i=1,npdt

!    Calcul evapotranspiration maximale emax
          emax(i,j)=cetp*kc(NINT(don(i,8)))*ep(i,j)

!    Neutralisation pluie par Emax
          pn(i,j) = rui(i,j)
     if (pn(i,j).ge.emax(i,j)) then
           ev0(j)=emax(i,j)
       pn(i,j)=pn(i,j)-emax(i,j)
          else 
            ev0(j)=pn(i,j)
            pn(i,j) = 0. 
          endif

!    Refus de la pluie/lame de fonte/lame glaciaire
          dtr1(j) = max(0.,(pn(i,j)+lft(i,j)+lgl(i,j)*s_gl(i,j))&
     &-(Umax-u(i-1,j))&
     &*(1-exp(-(pn(i,j)+lft(i,j)+lgl(i,j)*s_gl(i,j))/Umax)))
   
          dtu1(j) = (pn(i,j)+lft(i,j)+lgl(i,j)*s_gl(i,j)) - dtr1(j)

!    Reservoir de surface U      
          u(i,j)=u(i-1,j)+dtu1(j)
          vs(i,j)=dtr1(j)
    evu(j)=(emax(i,j)-ev0(j))*u(i,j)/Umax
    if (evu(j).gt.u(i,j)) evu(j)=u(i,j)
    u(i,j)=u(i,j)-evu(j)  

!    Remplissage de L
          al(i,j)=vs(i,j)*(1-(l(i-1,j)/Lmax)**erl)
          if (al(i,j)+l(i-1,j).gt.Lmax) al(i,j)=Lmax-l(i-1,j)
          l(i,j)=l(i-1,j)+al(i,j)
            
!    Vidange de L
          vl(j) = 1/evl*((l(i,j)**evl)/(Lmax**(evl-1)))
          l(i,j)=l(i,j)-vl(j)
    
!    Remplissage de Z
    dtz(j)=vl(j)*(1.-z(i-1,j)/zmax)
    if ((dtz(j).gt.zmax-z(i-1,j)).and.(dtz(j).gt.0.)) then
      dtz(j)=zmax-z(i-1,j)
    endif
    z(i,j)=z(i-1,j)+dtz(j)

!    Part d'eau non captée paz Z (alimentation N et rvers)      
          vl(j) = vl(j)-dtz(j)

!    Calcul du ruissellement retardé
          rvers(i,j)=kr*vl(j)

!    Remplissage de N
          an(i,j)=(1-kr)*vl(j)

!    Evaporation dans Z et mise à jour
          evz(j)=(emax(i,j)-ev0(j)-evu(j))*z(i,j)/Zmax
          if (evz(j).gt.z(i,j)) evz(j)=z(i,j)
          er(i,j)=ev0(j)+evu(j)+evz(j)
          z(i,j)=z(i,j)-evz(j)
          
        enddo ! Fin de la boucle en temps
        
      enddo ! fin de la boucle sur les stocks
      
!Cumul des apports nappe
      an_t(1:npdt)=matmul(an(1:npdt,1:nsn),s_sn(1:nsn))

!Boucle en temps Nappe globale
      do i=1,npdt

!  Mise à jour de la nappe
        n(i)=n(i-1)+an_t(i)

!  Vidange de N et mise à jour
        if(evn.eq.1)then
        rbase(i)=lkn*n(i)
        else
  rbase(i)=n(i)-(lkn*(evn-1) + n(i)**(1-evn))**(1/(1-evn))
  endif
  if (rbase(i).gt.n(i)) rbase(i)=n(i)
  n(i)=n(i)-rbase(i)

      enddo

!Cumul des contributions

      rt(1:npdt)=matmul(vs(1:npdt,1:nsn),s_sn(1:nsn))&
     &           -matmul(al(1:npdt,1:nsn),s_sn(1:nsn))&
     &           +matmul(rvers(1:npdt,1:nsn),s_sn(1:nsn))&
     &           +rbase(1:npdt)

      er_t(1:npdt)=matmul(er(1:npdt,1:nsn),s_sn(1:nsn))     ! évapotranspiration réelle
      ep_t(1:npdt)=matmul(ep(1:npdt,1:nsn),s_sn(1:nsn))     ! évapotranspiration potentielle
      emax_t(1:npdt)=matmul(emax(1:npdt,1:nsn),s_sn(1:nsn)) ! évapotranspiration maximale
        
!c    ----------------------------------------------------------------- 
!c    FONCTION DE TRANSFERT
!c    ----------------------------------------------------------------- 

!FT type onde diffusante sans dependance au débit
      call ModuleRoutage(pmt,pdt,rt,rtr,npdt)      
              
!c    ----------------------------------------------------------------- 
!c    MODULE LAC (optionnel)
!c    ----------------------------------------------------------------- 

      select case (mod_lac) 
      
      ! Sans modèle LAC
      case(0) 
        res(1:npdt,1)=rtr(1:npdt)
      
      ! Modèle LAC V1 (effet pluvio seul)
      case(1) 
        res(1:npdt,1) = &
     &   rtr(1:npdt)*(1-flac)+cp*don(1:npdt,2)*flac
         er_t(1:npdt) = er_t(1:npdt)*(1-flac) 
      
      ! Modèle LAC V2 (effet pluvio+evapotranspiration) 
      case(2) 
!  call MoyenneGlissante(ep,elac,npdt,3)
        res(1:npdt,1) = rtr(1:npdt)*(1-flac)&
     &   +(cp*don(1:npdt,2)-ep_t(1:npdt))*flac
        er_t(1:npdt) = er_t(1:npdt)*(1-flac) + ep_t(1:npdt)*flac
 
 end select      

!c    ----------------------------------------------------------------- 
!c    Stockage des séries chrono resultats
!c    -----------------------------------------------------------------  
!c    Piloté par mod_out (pmtMOR(6)):
!c    - mod_out=0 : Mode "CAL" (sorties minimales)
!c    - mod_out=1 : Mode "SIM" (sorties réduite - SCHADEX)
!c    - mod_out=2 : Mode "SIM+" (exhaustives - PAMELAH)
!     - mod_out=3 : Mode "ASSIM" (recup data pour initialisation)

        if (mod_out.eq.0) then
!  Stockage dans le colonne 2 fneige exclusivement pour les sorties minimales
!  Modification pour la parallelisation dans genetic            
          call Modulefneige(sn(1:npdt,1:nsn),s_sn,nsn,npdt,res(1:npdt,2))            ! fneige
          res(1:npdt,3) = matmul(sn(1:npdt,1:nsn),s_sn(1:nsn))         ! s

            do j=1,nsn
                 res(1:npdt,3+j) = sn(1:npdt,j)
           enddo
      endif
       
  if (mod_out.ge.1) then
   res(1:npdt,6) = matmul(sn(1:npdt,1:nsn),s_sn(1:nsn))         ! s
   res(1:npdt,8) = matmul(u(1:npdt,1:nsn),s_sn(1:nsn))          ! u
   res(1:npdt,9) = matmul(l(1:npdt,1:nsn),s_sn(1:nsn))          ! l
   res(1:npdt,10)= matmul(z(1:npdt,1:nsn),s_sn(1:nsn))          ! z
   res(1:npdt,11)= n(1:npdt)                                    ! n
  endif

  if (mod_out.eq.1) then
!  Stockage dans le colonne 2 fneige exclusivement pour les sorties minimales
!  Modification pour la parallelisation dans genetic            
    call Modulefneige(sn(1:npdt,1:nsn),s_sn,nsn,npdt,res(1:npdt,2))            ! fneige
  endif
  
  if (mod_out.eq.2) then  
!      Sorties globales
          res(1:npdt,2) = rbase(1:npdt)                                ! rbase
   res(1:npdt,3) = matmul(rvers(1:npdt,1:nsn),s_sn(1:nsn))      ! rvers
   res(1:npdt,4) = matmul(vs(1:npdt,1:nsn),s_sn(1:nsn)) &
      & -matmul(al(1:npdt,1:nsn),s_sn(1:nsn))        ! rsurf
   res(1:npdt,5) = emax_t(1:npdt)                     ! emax
            call Modulefneige(sn(1:npdt,1:nsn),s_sn,nsn,npdt,res(1:npdt,7))            ! fneige      
   res(1:npdt,12)= cetp*ep_t(1:npdt)                            ! etp(*cetp)
   res(1:npdt,13)= er_t(1:npdt)                                 ! etr
   res(1:npdt,14)= matmul(tst(1:npdt,1:nsn),s_sn(1:nsn))        ! tst
   res(1:npdt,15)= matmul(tft(1:npdt,1:nsn),s_sn(1:nsn))        ! tft
            ! Routage de rbase pour le calcul de frl
            call ModuleRoutage(pmt,pdt,rbase,rbaser,npdt) 
   res(1:npdt,16)= rbaser(1:npdt)/rtr(1:npdt)                 ! frl
   res(1:npdt,17)= matmul(lft(1:npdt,1:nsn),s_sn(1:nsn))        ! lft                

   do j=1,nsn  
       s_ng(1:npdt,j)=s_gl(1:npdt,j)*s_sn(j)                    ! fraction glace par rapport à surface bv
   enddo
   
            do i = 1,npdt
              tmp(1:nsn) = s_ng(i,1:nsn)
              tmp2(1:nsn)= lgl(i,1:nsn)
              res(i,18)  = sum(tmp(1:nsn)*tmp2(1:nsn))                 ! lglace        
   enddo
   res(1:npdt,19) = matmul(agl(1:npdt,1:nsn),s_sn(1:nsn))     ! accu
   res(1:npdt,20) = matmul(pls(1:npdt,1:nsn),s_sn(1:nsn))     ! pluie(*cp)
   res(1:npdt,21) = matmul(ngs(1:npdt,1:nsn),s_sn(1:nsn))     ! neige(*cp)
   res(1:npdt,22) = matmul(rui(1:npdt,1:nsn),s_sn(1:nsn))&
      & +matmul(ruc(1:npdt,1:nsn),s_sn(1:nsn))      ! Pluie sur neige  
   res(1:npdt,23) = res(1:npdt,1)*pmt(9)                       ! Qbvi seul  

!      Sorties par bande
   do j=1,nsn
     res(1:npdt,30+j) = psn(1:npdt,j)
     res(1:npdt,40+j) = tmn(1:npdt,j)
     res(1:npdt,50+j) = rvers(1:npdt,j)
     res(1:npdt,60+j) = vs(1:npdt,j)-al(1:npdt,j)
     res(1:npdt,70+j) = sn(1:npdt,j)
     res(1:npdt,80+j) = u(1:npdt,j)
     res(1:npdt,90+j) = l(1:npdt,j)
     res(1:npdt,100+j)= z(1:npdt,j)
     res(1:npdt,110+j)= cetp*ep(1:npdt,j)
     res(1:npdt,120+j)= emax(1:npdt,j)
     res(1:npdt,130+j)= er(1:npdt,j)
     res(1:npdt,140+j)= lft(1:npdt,j)
     res(1:npdt,150+j)= pls(1:npdt,j)
     res(1:npdt,160+j)= ngs(1:npdt,j)
     res(1:npdt,170+j)= agl(1:npdt,j)
     res(1:npdt,180+j)= lgl(1:npdt,j)
     res(1:npdt,190+j)= s_gl(1:npdt,j)
     res(1:npdt,200+j)= sns(1:npdt,j)
     res(1:npdt,210+j)= snl(1:npdt,j)
     res(1:npdt,220+j)= tst(1:npdt,j)
   enddo
 
 endif


! mod_out = 3 "ASSIM"
! tableau res avec 74 colonnes

if (mod_out.eq.3) then

    ! res(1:npdt,1) deja fait plus haut                             ! 1 debit
    call Modulefneige(sn(1:npdt,1:nsn),s_sn,nsn,npdt,res(1:npdt,2)) ! 2 fneige
    res(1:npdt,3) = matmul(sn(1:npdt,1:nsn),s_sn(1:nsn))            ! 3 s

    res(1:npdt,4)= n(1:npdt)                                        ! 4 nappe

    ! Sorties par bande
    do j=1,nsn
      res(1:npdt,4 +j) = u(1:npdt,j)                                ! [5;14]  u
      res(1:npdt,14+j) = l(1:npdt,j)                                ! [15;24] l
      res(1:npdt,24+j) = z(1:npdt,j)                                ! [25;34] z
      res(1:npdt,34+j) = sn(1:npdt,j)                               ! [35;44] sn
      res(1:npdt,44+j) = sns(1:npdt,j)                              ! [45;54] teneur eau solide
      res(1:npdt,54+j) = snl(1:npdt,j)                              ! [55;64] teneur eau liquide
      res(1:npdt,64+j) = tst(1:npdt,j)                              ! [65;74] temperature de stock
    enddo

endif


  
return
 
end
      

!c-----------------------------------------------------------------
!MODELE NEIGE MORDOR SD
!c-----------------------------------------------------------------
      subroutine ModuleNeige(pmt,pr,tmn,ry,jj,&
     &sn,sns,snl,lft,pls,ngs,fliq,tst,ag,rui,run,ft,fb,npdt)
!c-----------------------------------------------------------------
!Programmation FGo+FGa+MLL
!c-----------------------------------------------------------------   
!    Sorties du modèle neige
!    rui = pluie sur sol
!    ruc = pluie sur neige
!    ft = fonte degré-jour
!    fb = fonte basale
!    lft = ruc + ft + fb
!    le routage peut donc être fait avec rui+lft ou rui+ruc+ft+fb
!c-----------------------------------------------------------------
!Sep 2016
!Modification du calcul de la surface enneigee (sca) : hors sub ModuleNeige
!c-----------------------------------------------------------------
 
      implicit none
      include 'dim_mordor_SD.f95'
!Declatations 
      integer npdt,jg,i
      real pmt(*)
      real pr(npdt),tmn(npdt),ry(npdt),jj(npdt)
      real sn(0:npdt),lft(npdt),pls(npdt),ngs(npdt),fliq(npdt)
      real tst(0:npdt),tf(0:npdt),ag(npdt),rui(npdt),run(npdt)
      real ft(npdt),fb(npdt)
      real sns(0:npdt),snl(0:npdt),dsnl(npdt),snlmax(npdt)
      real vida(npdt),fonte(npdt),tp(npdt)
      real t50,delt,kfp,scf,gm,wct,div
      real efp,eft,kf,ptjp1,raym,pdt,lts
!c-----------------------------------------------------------------    

!c-----------------------------------------------------------------   
!Initialisations
!c-----------------------------------------------------------------   

!Paramètres fixes :
      div=100000 
      wct=0.1       ! Contenu en eau maximum
      t50=1.        ! Séparation de phase
      delt=4.       ! Séparation de phase
      scf=1.        ! Snow Corection Factor

!Récupération des paramètres 
      pdt=pmt(8)
      gm=pmt(41)*pdt/24
      efp=pmt(38)
      eft=pmt(39)
      kfp=pmt(37)*pdt/24
      kf=pmt(36)*pdt/24
      lts=pmt(40)**(pdt/24)
      jg=pmt(45)
      ptjp1=pmt(46)
      raym=pmt(47)
      
!Etats initiaux : sn(0)/tst(0)/tf(0) sont initialisés avant l'appel        
      sns(0)=sn(0)            ! teneur en eau solide
      snl(0)=0                ! teneur en eau liquide
      
!phasage des précipitations et des températures (en journalier)
      do i=1,npdt-1
        tp(i)= ((1.-ptjp1)*tmn(i))+(ptjp1*tmn(i+1))+ efp
      enddo
      tp(npdt)=tp(npdt-1)

!c----------------------------------------------------------------- 
!Modèle neige
!c----------------------------------------------------------------- 

!Boucle temps
      do i=1,npdt
      
!  Phase des précipitations = modèle à 2 seuils avec transition S-schape
        fliq(i)=(1-(1/(1+exp((10/delt)*(tp(i)-t50)))))
            
!  Calcul Pluie & Neige 
        pls(i)=pr(i)*fliq(i)
        ngs(i)=pr(i)*(1-fliq(i))*scf ! scf appliqué à ngs

!  Lissage exponentiel de la température de stock      
        tst(i)=(lts*tst(i-1)+(1-lts)*(tmn(i)+efp))&
     & *(0.5*(1-tanh((lts*tst(i-1)+(1-lts)*(tmn(i)+efp))*div)))&
     & *(1-exp(-div*(sn(i-1)+ngs(i))**2))

!  Lissage exponentiel de la température de fonte et calcul de la fonte
        tf(i)=tmn(i)
        fonte(i)=(kf+kfp*ry(i)/raym)*(tf(i)+eft+tst(i))
        if(fonte(i).lt.0) fonte(i)=0
        
        sns(i)=sns(i-1)+ngs(i)-fonte(i)-gm     ! Bilan de la partie solide
        
        if(sns(i).le.0.) then
          sns(i)=0
          snl(i)=0
          lft(i)=sn(i-1)+ngs(i)
          rui(i)=pls(i)   ! Pluie à router
          vida(i)=0
          snlmax(i)=0
          fonte(i)=0
          goto 100
        endif
  
        snlmax(i)=wct/(1-wct)*sns(i) ! Limite physique du contenu en eau
        
        if(snl(i-1).gt.snlmax(i))then ! Vidange du snl si au dessus
          vida(i)=snl(i-1)-snlmax(i)
        else
          vida(i)=0
        endif
        
        dsnl(i)=(fonte(i)+pls(i))*(1-(snl(i-1)-vida(i))/snlmax(i))
        lft(i)=(fonte(i)+pls(i))*(snl(i-1)-vida(i))/snlmax(i)+gm+vida(i)
       
        if((snl(i-1)-vida(i)+dsnl(i)).gt.snlmax(i))then
          snl(i)=snlmax(i)
          lft(i)=lft(i)+snl(i-1)-vida(i)+dsnl(i)-snlmax(i)
        else
          snl(i)=snl(i-1)-vida(i)+dsnl(i)
        endif
        rui(i)=0 ! La pluie est prise dans la fonte

100     sn(i)=sns(i)+snl(i)

!Calcul surface enneigee
!  if(sn(i).gt.0.) then ;sca(i)=1.;else;sca(i)=0;endif


!  « stock de froid » fin de l'intervalle
        tst(i)=tst(i)*(1-exp(-div*sn(i)**2))       ! si sn(i)=0 => 0
        tst(i)=tst(i)+(tmn(i)-tst(i))&
     &*exp(-div*sn(i-1)**2)*(1-exp(-div*sn(i)**2)) ! si sn(i-1)=0 => 1 et si sn(i)> 0 => 1 (s'il neige sur i)

        tst(i)=tst(i)*(0.5*(1-tanh(tst(i)*div)))   ! si tst>0 =>tst=0 

!  transformation de la neige en glace, le jour jg si mod_glace activé
        if ((int(jj(i)).eq.jg).and.(pmt(5).ge.1.)) then
          ag(i)=sn(i)
          sn(i)=0
          sns(i)=0
          snl(i)=0
          tst(i)=0
        else
          ag(i)=0
        endif
      
!  Clé de répartition
        if((lft(i).le.0).or.(pls(i)+fonte(i)+gm.eq.0))then
          fb(i)=0
          ft(i)=0
          run(i)=0
        else
          fb(i)=lft(i)*gm/(pls(i)+fonte(i)+gm)
          ft(i)=lft(i)*fonte(i)/(pls(i)+fonte(i)+gm)
          run(i)=lft(i)*pls(i)/(pls(i)+fonte(i)+gm)
        endif
        
      enddo
      
      return

      end

!c-----------------------------------------------------------------
!MODELE SURFACE ENNEIGEE
!c-----------------------------------------------------------------
      subroutine Modulefneige(sn,s_sn,nsn,npdt,fneige)
!c-----------------------------------------------------------------
!Programmation FGa+MLL
!c-----------------------------------------------------------------
!c   include 'dim_mordor_SD.f95'
  
      integer nsn,npdt,i,j
      real sn(npdt,nsn),FSC(npdt,nsn),s_sn(nsn),Sref
      real fneige(npdt)
    
      Sref = 100.
      FSC(1:npdt,1:nsn) = 0.
          
          do j=1,npdt      
                do i=1,nsn-1
                    if(sn(j,i+1).gt.0.) FSC(j,i) = min((sn(j,i)/sn(j,i+1))**(0.5),1.)
                enddo
                FSC(j,nsn) = min((sn(j,nsn)/Sref)**(0.5),1.)    
          enddo
      
      fneige(1:npdt) = matmul(FSC(1:npdt,1:nsn),s_sn(1:nsn))
      
      end

!c----------------------------------------------------------------- 
!MODELE GLACE : degré-jour simple avec Hock (rayonnement)     
!c-----------------------------------------------------------------  
      subroutine ModuleGlace(pmt,npmt,sn,tf,ry,lg,npdt)
!c-----------------------------------------------------------------
!Programmation FGo
!c-----------------------------------------------------------------   
      implicit none
!Declarations 
      integer npdt,i,npmt
      real sn(npdt),tf(npdt),ry(npdt),lg(npdt)
      real pmt(npmt)
      real kgp,kg,efg,raym,pdt
!c-----------------------------------------------------------------   

!Initialisations  
      pdt       = pmt(8)
      efg       = pmt(44)
      kgp       = pmt(43)*pdt/24
      kg        = pmt(42)*pdt/24
      raym      = pmt(47)

!Boucle temps
      do i=1,npdt
        if((sn(i).le.0).and.((tf(i)+efg).gt.0.))then
          lg(i)=(kg+kgp*ry(i)/raym)*(tf(i)+efg)
        else
          lg(i)=0
        endif
      enddo

      return
      end
      
!c-----------------------------------------------------------------
!RAYONNEMENT POTENTIEL 
!c-----------------------------------------------------------------      
      DOUBLE PRECISION FUNCTION RADIATION ( DOY, LAT )   
!Computes mean daily extraterrestrial radiation in W.m-2
!Latitude input in decimal degrees (ex: 43.86)
!Solar constant in W.m-2   
      
      IMPLICIT NONE
   
      DOUBLE PRECISION LAT,PHI,DELTA,OMEGAS,DR,I0,PI,ARG
      INTEGER DOY 
      DATA I0/1367.D0/
      DATA PI/3.14159D0/
   
      PHI = PI*LAT/180.D0
      DELTA = 0.409D0 * DSIN(2.D0*PI*DFLOAT(DOY)/365.D0-1.39D0)
      ARG = -1.D0*DTAN(PHI)*DTAN(DELTA)
      IF(ARG.GT.1.D0)ARG=1.D0
      IF(ARG.LT.-1.D0)ARG=-1.D0
      OMEGAS = DACOS(ARG)
      DR = 1.D0+0.033D0*DCOS(2.D0*PI*DFLOAT(DOY)/365.D0)
   
      RADIATION = (I0/PI) * DR *&
     &            ( OMEGAS*DSIN(PHI)*DSIN(DELTA) +&
     &              DCOS(PHI)*DCOS(DELTA)*SIN(OMEGAS) )
   
      END

!-----------------------------------------------------------------
!ModuleRoutage : calcul de la fonction de transfert et du routage
!-----------------------------------------------------------------
!Routage                                                 juin 2016
!Modèle FT: formulation type PREDICE (onde diffusante) sans  
!dependance au debits
!-----------------------------------------------------------------
      subroutine ModuleRoutage(pmt,pdt,rt,rtr,npdt)
!-----------------------------------------------------------------
!DECLARATIONS VARIABLES LOCALES
!-----------------------------------------------------------------
      include 'dim_mordor_SD.f95'
      integer npdt,ftrl,i,j,k
      real pmt(npmt),pdt
 real rt(-ftrm:npdt),rtr(npdt)
 real lon,cel,dif
 real ft(int(ftrm/pdt)),rti(4)
!-----------------------------------------------------------------

!Initailisation des variables et des parametres     
      rti  = 0.
      cel  = pmt(32)
      dif  = pmt(33)
      lon  = pmt(15)
      pdt  = pmt(8)
      ftrl = 0
      ft   = 0.
            
!Calcul de la function de transfert
 call ft_OndeDiffusante(cel,dif,lon,pdt,ft,ftrl)

!Routage
 do i=1,4
  rt(-i)=rti(i)
 enddo
 do i=5,ftrl
  rt(-i)=rt(-4)
 enddo
 
      do i = 1,npdt       
          rtr(i) = 0.
          do j = 1,ftrl
            k = max(i-j+1,1)
            rtr(i) = rtr(i) + ft(j)*rt(k)
          enddo             
 enddo
 
 end
!-----------------------------------------------------------------

!-----------------------------------------------------------------
 subroutine ft_OndeDiffusante(cel,dif,lon,pdt,ft,ftrl)
!-----------------------------------------------------------------
!Programation                              sept. 2013 JG
!Modification / simplification                       juin 2016 FGa
!-----------------------------------------------------------------

      include 'dim_mordor_SD.f95'

 integer ftrl,i,j,m
 real cel,dif,lon,pdt,ft(*)
 real onde(ftrm),onde_cum(ftrm),norm,seuil
!-----------------------------------------------------------------
 
!Initialisation      
      onde     = 0.
      onde_cum = 0.
      norm     = 0.
    
!Calcul de la FT sur ftrm heures
      call hayami(cel,dif,lon,ftrm,onde)

!Calcul de la FT cumulée      
      onde_cum(1) = onde(1)
      do j=2,ftrm
         onde_cum(j) = onde_cum(j-1) + onde(j)
      enddo
      
!CAS A : pas de temps horaire
      if (pdt.eq.1) then 
        norm = onde_cum(ftrm)
        ft(1:ftrm) = onde(1:ftrm)
        m = ftrm
      endif 
              
!CAS B : aggregation au pas de temps cible si pdt superieur à l'heure
      if (pdt.gt.1) then
        m  = 0
        do i=1,ftrm,int(pdt)
            m = m + 1
            ft(m) = onde_cum(i+int(pdt)-1) - onde_cum(i)
            norm = norm + ft(m)
        enddo
!  Verification de la coherence de la ft
        if (norm.eq.0.) then 
            ft(1) = 1.
            norm = 1.
            m = 1
        endif              
      endif
      
!Normalisation de la fonction de transfert      
      ft(1:m) = ft(1:m)/norm

!Elimination de pourcentage inferiueres à 0.01 pour pdt égal à 24
      norm=0.
 seuil=1./(ftrm/(pdt/10))
      do j=1,m
         if (ft(j).le.seuil) ft(j) = 0.
         norm = norm + ft(j)
      enddo
      
!Normalisation de la fonction de transfert après filtrage     
      ft(1:m) = ft(1:m)/norm

!Calcul de nombre de pas de temps de définition de ft (ftrl)            
 ftrl = m
      do while(ft(ftrl).eq.0)
   ftrl = ftrl - 1
 enddo  
              
      return
 end

!-----------------------------------------------------------------
      Subroutine hayami(cel,dif,lon,ftrm,onde)
!-----------------------------------------------------------------
!Auteur: Yves Gregoris (1996)
!Modification : FGa    (2016)                                       
!                                                     
!c    Objet:  Solveur de l'equation de l'onde diffusive
!c    -----   Calcul d'une valeur val en fonction de la longueur
!       de propagation lon, de la celerite de l'onde cel, de la
!       diffusion dif et du temps de propagation t (1:ftrm)
!-----------------------------------------------------------------
      integer ftrm,i
      real lon,cel,dif,onde(ftrm)
      real pi,aux1,aux2

      pi = 4.*atan(1.)
      aux1 = lon/(2*sqrt(pi*dif))
      
      do i = 1,ftrm 
         aux2 = (lon-cel*real(i))**2 / (4.*dif*real(i))
         onde(i) = aux1 * real(i)**(-1.5) * exp(-aux2)
      enddo

!Controle sur la forme de l'onde        
      if (sum(onde(1:ftrm)).eq.0.) then
        onde(1) = 1.
      endif     

      return
      end

!-----------------------------------------------------------------
      subroutine ETP_V0(pmt,tm,ep,n)
!-----------------------------------------------------------------
!Modèle ETP_V0: formulation historique MORDOR
!-----------------------------------------------------------------
!MLL          nov  2012
!-----------------------------------------------------------------     
      implicit none  
!Declarations   
      integer n
 real tm(n),ep(n)
 real pmt(*)
 real fe1,fe3,pdt
   
!Paramètres
      pdt = pmt(8)
      fe1 = pmt(22)*pdt/24
      fe3 = pmt(23)
      
!Boucle en temps
 where(tm(1:n).gt.fe3) 
   ep(1:n) =fe1*((tm(1:n)-fe3)**2)
 elsewhere
   ep(1:n) = 0.
 endwhere
 
 return
 
      end


!-----------------------------------------------------------------
      subroutine ETP_V1(pmt,tm,ep,JD,n)
!-----------------------------------------------------------------
!Modèle ETP_V1: formulation OUDIN (2004)
!-----------------------------------------------------------------
!MLL          nov  2012 
!-----------------------------------------------------------------
!This subroutine calculates daily potential evapotranspiration (PE)
!using daily temperature and daily extra-atmospheric global radiation 
!(that depends only on Julian day)
!The PE formula is is that described in:
!Oudin, L., Hervieu, F., Michel, C., Perrin, C., Andréassian, V., 
!Anctil, F. and Loumagne, C., 2005. Which potential evapotranspiration 
!input for a rainfall-runoff model? Part 2 - Towards a simple and 
!efficient PE model for rainfall-runoff modelling. Journal of Hydrology 
!303(1-4), 290-306.
!For the calculation of extra-atmospheric global radiation, see Appendix C of
!the article by Morton, F.I., 1983. Operational estimates of areal 
!evapotranspiration and their significance to the science and practice 
!of hydrology. Journal of Hydrology 66 (1/4), 1-76.
!Inputs:
!LAT: Latitude in degree
!DT: Temperature in degree C
!JD: Julian day
!K1: Scale factor
!K2: Temperature treshold
!Output:
!ep: Daily potential evapotranspiration in mm     
!-----------------------------------------------------------------
      implicit none
!Declarations
      integer n,i
 real tm(n),JD(n),ep(n)
 real pmt(*),pdt
      real LAT,FI,COSFI,TETA,COSTETA,COSGZ,COSGZ2
      real COSOM,SINOM,OM,COSPZ,ETA,GE,COSOM2,K1,K2,RD
      
      DATA RD/57.3/

!Données et Paramètres
      LAT = pmt(12)  
      pdt = pmt(8)

!Paramètres fixés (Oudin, 2004) avec correction fct du pas de temps       
      K1 = pmt(22)*24/pdt
      K2 = pmt(23)
             
!Calculation of extra-atmospheric global radiation (Appendix C in Morton
!(1983), Eq. C-6 to C-11, p.60-61) - Converts latitude in radians
      FI=LAT/RD
      COSFI=COS(FI)
       
      do i=1,n

!TETA: Declination of the sun in radians
      TETA=0.4093*SIN(JD(i)/58.1-1.405)
      COSTETA=COS(TETA)
      COSGZ=MAX(0.001,COS(FI-TETA))
      COSGZ2=COSGZ*COSGZ

      COSOM=1.-COSGZ/COSFI/COSTETA
 if (COSOM .LT. -1.) then 
  COSOM=-1.
  COSOM2 = 1.
  SINOM=0.
      else
   if (COSOM .GT. 1.) then
      COSOM=1.
   COSOM2 = 1.
   SINOM=0.
  else
   COSOM2 = COSOM * COSOM
   if (COSOM2 .GE. 1.)then
       SINOM=0.
   else
    SINOM=SQRT(1.-COSOM2)
   endif
  endif
 endif

      OM=ACOS(COSOM)
!PZ: Average angular zenith distance of the sun
      COSPZ=COSGZ+COSFI*COSTETA*(SINOM/OM-1.)
      IF(COSPZ.LT.0.001)COSPZ=0.001
!ETA: Radius vector of the sun
      ETA=1.+COS(JD(i)/58.1)/30.
!GE: extra-atmospheric global radiation 
      GE=446.*OM*COSPZ*ETA

!Daily PE by Oudin et al. (2006) formula:
      ep(i) = MAX(0., GE*(tm(i)-K2) /(K1*28.5))
      
      enddo
  
      return

      end

      
!-----------------------------------------------------------------
      subroutine CoefCultural(pmt,kc)
!-----------------------------------------------------------------
!Modèle CoefCultural: calcul du Kc
!-----------------------------------------------------------------
!MLL          janv  2014
!MLL          avril 2015: utilisation du Rpot pour saisonnaliser Kc
!-----------------------------------------------------------------  
      implicit none
!Declarations
      integer i
 real pmt(*)
 real kmin,kcmoy
      real kc(365),Rpot(365)
      double precision RADIATION

!Paramètres
      kmin = pmt(24)

!Calcul cycle rayonnement potentiel
      do i = 1,365
         Rpot(i) = SNGL(RADIATION(i,DBLE(pmt(12)))) 
 enddo
!Coefficient cultural: formulation avec parametre de forme (kmin) 
      kc(1:365)=kmin+(1-kmin)*((Rpot(1:365)-MINVAL(Rpot(1:365))) &
     &/(MAXVAL(Rpot(1:365))-MINVAL(Rpot(1:365))))
      kcmoy=SUM(kc(1:365))/365
      kc=kc/kcmoy
            
      return

      end     
                  
