!-------------------------------------------------------------------------------------------------------
!---                                         Modules                                                 ---
!-------------------------------------------------------------------------------------------------------

MODULE definitions
	!Define variables and parameters
!$$$$$$     character(len=10):: name
	character*7 shore_cases(2)
    data shore_cases /'Case 1','Case 2'/
	integer :: T,n,m,Lx,Ly,i,j,T_current,selected_case
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
	integer :: setup_plot,update_plot,sure_quit
    external shore_case_1,shore_case_2,setup_plot,update_plot,sure_quit

	!Provisional size of beach
    Lx=1500
    Ly=100

	!Define accuracy of net
!$$$$$$     print *, 'Number of ''x'' divisions?'
!$$$$$$     read *, n
    n=75
	allocate(x(n+1))
	allocate(y(n+1))
    allocate(Q(n+1))
!$$$$$$     print *, 'Number of time steps?'
!$$$$$$     read *, T
	T=10000
	allocate(y_omni(T+1,n+1))
	allocate(Q_omni(T+1,n+1))

	!Calculate deltas
	deltaX=Lx/n
!$$$$$$     deltaY=Ly/m
	deltaT=365*24*3600/T	!Length of experiment is 1 yr. (measured in seconds)

	!Set initial conditions
	do i=1,n+1
		x(i)=i-1
		y(i)=sin(x(i)/pi)*10+200
		y_omni(1,i)=y(i)
	enddo
	CALL shore_case_1
    selected_case=1

	!Paint and plot (create the window)
    T_current=0
	fill=T_current/real(T)
	i=winio@('%ww%ca[Shoreline Plot]%bg[window]%pv&')
    i=winio@('%pl[colour=red,y_min=0,y_max=700,x_array]&',400,250,n,x,y)
    i=winio@('%ff%nl%cn%3.1ob[status,thin_panelled]&')
    i=winio@('%`bg[white]%30br[percentage]%cb&',fill,RGB@(0,128,128))
    i=winio@('%`bg[white]%co[no_data_border,right_justify]%dd%il%^rd%cb&',1,0,T,T_current,update_plot)
    !i=winio@('%ff%nl%cn%^tt[Evaluar]&',update_plot)
    i=winio@('%`bg[white]%^`ls%cb&',shore_cases,size(shore_cases),selected_case,setup_plot)
    i=winio@('%cc',sure_quit)


!-------------------------------------------------------------------------------------------------------
!---                                       Subroutines                                               ---
!-------------------------------------------------------------------------------------------------------

	CONTAINS
!---------------------------------------------------------------------
!---                          Utilities                            ---
!---------------------------------------------------------------------
    
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

!---------------------------------------------------------------------
!---                         Shore Cases                           ---
!---------------------------------------------------------------------
    
!Case 1: Groins at x=o and x=Lx
SUBROUTINE shore_case_1
    use definitions
    implicit none
    do j=1,T
        Q_omni(j+1,1)=0
        do i=2,n
            Q_omni(j+1,i)=0.20*(1.2**(5/2))*sin(2*((pi/12)-atan((y_omni(j,i+1)-y_omni(j,i-1))/(2*deltaX))))
        enddo
        Q_omni(j+1,n+1)=0
        y_omni(j+1,1)=y_omni(j,1)-(deltaT/(deltaX*10))*(Q_omni(j+1,2)-Q_omni(j+1,1))
        do i=2,n
            y_omni(j+1,i)=y_omni(j,i)-(deltaT/(2*deltaX*10))*(Q_omni(j+1,i+1)-Q_omni(j+1,i-1))
        enddo
        y_omni(j+1,n+1)=y_omni(j,n+1)-(deltaT/(deltaX*10))*(Q_omni(j+1,n+1)-Q_omni(j+1,n))
    enddo
ENDSUBROUTINE shore_case_1

!Case 2: Infinite beach
SUBROUTINE shore_case_2
    use definitions
    implicit none
    do j=1,T
        Q_omni(j+1,1)=0.20*(1.2**(5/2))*sin(2*((pi/12)-atan((y_omni(j,2)-y_omni(j,1))/(deltaX))))
        do i=2,n
            Q_omni(j+1,i)=0.20*(1.2**(5/2))*sin(2*((pi/12)-atan((y_omni(j,i+1)-y_omni(j,i-1))/(2*deltaX))))
        enddo
        Q_omni(j+1,n+1)=0.20*(1.2**(5/2))*sin(2*((pi/12)-atan((y_omni(j,n)-y_omni(j,n-1))/(deltaX))))
        y_omni(j+1,1)=y_omni(j,1)-(deltaT/(deltaX*10))*(Q_omni(j+1,2)-Q_omni(j+1,1))
        do i=2,n
            y_omni(j+1,i)=y_omni(j,i)-(deltaT/(2*deltaX*10))*(Q_omni(j+1,i+1)-Q_omni(j+1,i-1))
        enddo
        y_omni(j+1,n+1)=y_omni(j,n+1)-(deltaT/(deltaX*10))*(Q_omni(j+1,n+1)-Q_omni(j+1,n))
    enddo
ENDSUBROUTINE shore_case_2

!-------------------------------------------------------------------------------------------------------
!---                                    External functions                                           ---
!-------------------------------------------------------------------------------------------------------

FUNCTION setup_plot
    use definitions
    implicit none
    include <windows.ins>
    integer :: setup_plot,update_plot
    external update_plot,shore_case_1,shore_case_2

	T_current=0
	if (selected_case == 1) then
    	CALL shore_case_1
    elseif (selected_case == 2) then
    	CALL shore_case_2
    endif
    i=update_plot()
    
	setup_plot=1
ENDFUNCTION setup_plot

FUNCTION update_plot
    use definitions
    implicit none
    include <windows.ins>
    integer :: update_plot
    
    !Update the filling bar
    fill=T_current/real(T)
    CALL window_update@(fill)
    
    !Update the plot
    do i=1,n+1
        y(i)=y_omni(T_current+1,i)
    enddo
    CALL simpleplot_redraw@
        
	update_plot=1
ENDFUNCTION update_plot

FUNCTION sure_quit
	implicit none
    integer :: a,sure_quit
    !The parameter 'a' returns 1 if 'Si' is pressed, 2 if 'No' is pressed and zero if the window is closed.
    a=winio@('%1SI!¿Estás seguro de querer salir?%nl%nl%cn%`6bt[Sí]          %6bt[No]%ca[Atención]')
    sure_quit=abs(a-1)
ENDFUNCTION sure_quit