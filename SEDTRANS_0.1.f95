!-------------------------------------------------------------------------------------------------------
!---                                         Modules                                                 ---
!-------------------------------------------------------------------------------------------------------

MODULE definitions
	!Define variables and parameters
!$$$$$$     character(len=10):: name
	integer :: T,n,m,Lx,Ly,i,j,T_current
	real*8 :: deltaT,deltaX,deltaY,pi,fill
	real*8,dimension(:,:),allocatable :: y_omni,Q_omni
	real*8,dimension(:),allocatable :: x,y,Q
    parameter(pi=3.1415927)
ENDMODULE definitions

!-------------------------------------------------------------------------------------------------------
!---                                       Main program                                              ---
!-------------------------------------------------------------------------------------------------------

PROGRAM sedtrans
	winapp
    library 'c:\TFG\simple.dll','c:\TFG\salflibc.dll'
    use definitions
	implicit none
    include <windows.ins>
	integer :: update_plot,sure_quit
    external update_plot,sure_quit

	!Provisional size of beach
    Lx=150
    Ly=100

	!Define accuracy of net
!$$$$$$     print *, 'Number of ''x'' divisions?'
!$$$$$$     read *, n
    n=120
	allocate(x(n+1))
	allocate(y(n+1))
    allocate(Q(n+1))
!$$$$$$     print *, 'Number of time steps?'
!$$$$$$     read *, T
	T=120
	allocate(y_omni(T+1,n+1))
	allocate(Q_omni(T+1,n+1))

	!Calculate deltas
	deltaX=Lx/n
!$$$$$$     deltaY=Ly/m
	deltaT=365*24*3600/T	!Length of experiment is 1 yr. (measured in seconds)

	!Set initial conditions
	do i=1,n+1
		x(i)=i-1
		y(i)=sin(x(i)/5)*10+500
		y_omni(1,i)=y(i)
	enddo
	!Calculate 'Q' and 'y' through time (TEMPORARY: rough approximations made!!)
	do j=1,T
    	do i=2,n+1
			Q_omni(j+1,i)=0.15*(1.2**(5/2))*sin(2*((pi/12)-atan((y_omni(j,i)-y_omni(j,i-1))/deltaX)))
		enddo
        Q_omni(j+1,1)=Q_omni(j+1,2)
        do i=2,n+1
        	y_omni(j+1,i-1)=y_omni(j,i-1)-(deltaT/(deltaX*10))*(Q_omni(j+1,i)-Q_omni(j+1,i-1))
        enddo
        y_omni(j+1,n+1)=y_omni(j+1,n)
	enddo

    deallocate(Q,Q_omni)

	!Paint and plot
    T_current=0
	fill=T_current/real(T)

	i=winio@('%ww%ca[Shoreline Plot]%bg[window]%pv&')
	i=winio@('%pl[colour=red,y_min=0,y_max=1500,x_array]&',400,250,n,x,y)
    i=winio@('%ff%nl%cn%2.1ob[status,thin_panelled]&')
    i=winio@('%`bg[white]%30br[no_border]%cb&',fill,RGB@(0,128,128))
    i=winio@('%`bg[white]%co[no_data_border,right_justify]%dd%il%^rd%cb&',1,0,T,T_current,update_plot)
   ! i=winio@('%ff%nl%cn%^tt[Evaluar]&',update_plot)
    i=winio@('%cc',sure_quit)

!-------------------------------------------------------------------------------------------------------
!---                                       Subroutines                                               ---
!-------------------------------------------------------------------------------------------------------

	CONTAINS
        
	SUBROUTINE resize_array
!$$$$$$         INTEGER,DIMENSION(:),ALLOCATABLE :: tmp_arr
!$$$$$$         
!$$$$$$         ALLOCATE(tmp_arr(2*SIZE(array)))
!$$$$$$         tmp_arr(1:SIZE(array))=array
!$$$$$$         DEALLOCATE(array)
!$$$$$$         ALLOCATE(array(size(tmp_arr)))
!$$$$$$         array=tmp_arr
    
	ENDSUBROUTINE resize_array
    
ENDPROGRAM sedtrans

!-------------------------------------------------------------------------------------------------------
!---                                    External functions                                           ---
!-------------------------------------------------------------------------------------------------------

FUNCTION update_plot
    use definitions
    implicit none
    include <windows.ins>
    integer :: update_plot
   
    fill=T_current/real(T)
    CALL window_update@(fill)

    do i=1,n+1
        y(i)=y_omni(T_current+1,i)
    enddo
    CALL SIMPLEPLOT_REDRAW@

	update_plot=1
ENDFUNCTION update_plot

FUNCTION sure_quit
	implicit none
    integer :: a,sure_quit
    !The parameter 'a' returns 1 if 'Si' is pressed, 2 if 'No' is pressed and zero if the window is closed.
    a=winio@('%1SI!¿Estás seguro de querer salir?%nl%nl%CN%6bt[Sí]          %6`bt[No]%ca[Atención]')
    sure_quit=abs(a-1)
ENDFUNCTION sure_quit