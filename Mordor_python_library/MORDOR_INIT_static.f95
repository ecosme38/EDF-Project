subroutine init_mordor(pmt,don,cond,res,flag)
!Initialisation du modele MORDOR
!-----------------------------------------------------------------
!Programation	FGa+MLL				2016
!-----------------------------------------------------------------
implicit none
include 'dim_mordor_SD.f95'
!Declarations      
integer flag(nflag),k,i,j,npdt
real pmt(npmt),don(tmax,ndon),res(tmax,nres),cond(ncond)
!-----------------------------------------------------------------      
!Initialisation des conditions 
npdt = cond(1)
cond(2:72)=1.
	
!Initialisation des sorties
res(1:npdt,1:nres) = 0.	

!Run MORDOR
pmt(6) = 2.	! sorties completes pour initialisation
    call mordor_SD_OM(pmt,don,cond,res,flag)

!Calcul des stocks initiaux moyens
k=0
do i=1,npdt
   if(don(i,8).eq.don(1,8)) then 
      k=k+1
      cond(2) = cond(2) + res(i,11)                       !n
       do j=1,int(pmt(16)) ! nombre de stocks
	      cond((j-1)*7+3) = cond((j-1)*7+3) + res(i,80+j)   !u
    	      cond((j-1)*7+4) = cond((j-1)*7+4) + res(i,90+j)   !l
    	      cond((j-1)*7+5) = cond((j-1)*7+5) + res(i,100+j)  !z
	      cond((j-1)*7+6) = cond((j-1)*7+6) + res(i,70+j)   !sn
	      cond((j-1)*7+7) = cond((j-1)*7+7) + res(i,200+j)  !sns
	      cond((j-1)*7+8) = cond((j-1)*7+8) + res(i,210+j)  !snl
	      cond((j-1)*7+9) = cond((j-1)*7+9) + res(i,220+j)  !tst
       enddo
    endif
enddo
cond(2:72)=cond(2:72)/float(k)
      
return
      
end
