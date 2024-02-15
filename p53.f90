!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   wav :    Periodic waves in an a nonlinear parabolic PDE
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)
      DOUBLE PRECISION  b1, alpha, r, k1, beta, alpha1, alpha2, k4, k7,x, y, z
     ! DOUBLE PRECISION R

    b1=PAR(1) 
	alpha=PAR(2)
	r=PAR(3)
	k1=PAR(4)
	beta=PAR(5)
	alpha1=PAR(6)
	alpha2=PAR(7)
	k4=PAR(8)
	k7=PAR(9)
	
	
	x=U(1)
    y=U(2)
	z=U(3)

	
	 
	F(1)= b1*z+alpha*x**2/(k1+x**2)-r*x*y/(x+0.0261)-0.01*x
	F(2)= 0.00235+beta*x**4/(4+x**4)-0.03*y
	F(3)=0.0001+ alpha1*z*(50-z)/(k4+(50-z))-alpha2*z/((k7+0.1)*(50+z))
      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T



    
      !b1, alpha, r, k1, beta, alpha1, alpha2, k4
	   
	   PAR(1) = 0.002         !b1  
       PAR(2) = 0.1           !alpha  
       PAR(3) = 0.015         !r
       PAR(4) = 2.5           !k1
	   PAR(5) = 0.2           !beta
	   PAR(6) = 1             !alpha1
	   PAR(7) = 50            !alpha2
	   PAR(8) = 0.05          !k4
	   PAR(9) = 0.1           !k7

  
		 U(1)=9.6706069404374
	     U(2)=6.741952419210312
		 U(3)=50.08337972566026

	 
		! U(1)=39.856723150002246
	    ! U(2)=6.744989432757255
		! U(3)=199.91649528056632

 
        ! U(1)=0.000000908575
	    ! U(2)= 0.0783333
		! U(3)=0.0000249938

 
        ! U(1)=0.373549
	    ! U(2)= 0.110628
		! U(3)=0.0000249938

        ! U(1)= 0.927415
	    ! U(2)=  1.11885
		! U(3)=0.0000249938

      END SUBROUTINE STPNT

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
