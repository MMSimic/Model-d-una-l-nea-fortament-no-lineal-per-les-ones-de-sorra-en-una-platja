program sedtrans
	winapp
	implicit none
    include <windows.ins>
    
	!Define variables and parameters
!$$$$$$     character(len=10):: name
	integer :: T,n,m,Lx,Ly,i,j,T_current,secs
    double precision fill
	real*8 :: T_real,deltaT,deltaX,deltaY,pi
	real*8,dimension(:,:),allocatable :: y_omni,Q_omni,beta,alpha
	real*8,dimension(:),allocatable :: Cq,Hb,x,y,Q
    parameter(pi=3.1416)

	!Provisional size of beach
    Lx=50
    Ly=100

	!Define accuracy of net
	print *, 'Number of ''x'' divisions?'
	read *, n
	allocate(x(n+1))
	allocate(y(n+1))
    allocate(Q(n+1))
	print *, 'Number of time steps?'
	read *, T
    T_real=T
	allocate(y_omni(T+1,n+1))
	allocate(Q_omni(T+1,n+1))

	!Calculate deltas
	deltaX=Lx/n
!$$$$$$     deltaY=Ly/m
	deltaT=365*24*3600/T	!Length of experiment is 1 yr. (measured in seconds)

	!Set initial conditions
	do i=1,n+1
		x(i)=i-1
		y(i)=sin(x(i))/10+3
		y_omni(1,i)=y(i)
	enddo
	!Calculate 'Q' and 'y' through time (TEMPORARY: rough approximations made!!)
	do j=1,T
    	do i=2,n+1
			Q_omni(j+1,i)=0.15*(1.2**(5/2))*sin(2*((pi/12)-(y_omni(j,i)-y_omni(j,i-1))/deltaX))
		enddo
        Q_omni(j+1,1)=Q_omni(j+1,2)
        do i=2,n+1
        	y_omni(j+1,i-1)=y_omni(j,i-1)-(deltaT/(deltaX*10))*(Q_omni(j+1,i)-Q_omni(j+1,i-1))
        enddo
        y_omni(j+1,n+1)=y_omni(j+1,n)
	enddo

	!Paint and plot
    T_current=5
    fill=T_current/T_real
	i=winio@('%ww%ca[Shoreline Plot]%bg[window]%pv&')
	i=winio@('%pl[colour=red,y_min=0,x_array]&',400,250,n,x,y)
    !i=winio@('%ff%nl%cn%20sl&',T_current,0.0D0,T_real)
    i=winio@('%ff%nl%cn%2.1ob[status,thin_panelled]&')
    i=winio@('%30br%cb&',fill,RGB@(0,128,128))
    i=winio@('%co[no_data_border,right_justify]%dd%il%6rd%cb&',1,0,T,T_current)
    i=winio@('%ff%nl%cn%tt[Close]')
!$$$$$$     call axgrid('*cartesian',1,-1)

!$$$$$$     secs=2
!$$$$$$     do while(secs>=1)
!$$$$$$         if (secs>=1000) then
!$$$$$$             secs=2
!$$$$$$         else
!$$$$$$             secs=secs+1
!$$$$$$         endif
        CALL update_plot    
!$$$$$$     enddo

	CONTAINS
    
    SUBROUTINE update_plot
		do i=1,n+1
			y(i)=y_omni(T_current+1,i)
		enddo
        CALL window_update@(T_current,fill,y)
    ENDSUBROUTINE update_plot
    
	SUBROUTINE resize_array
!$$$$$$         INTEGER,DIMENSION(:),ALLOCATABLE :: tmp_arr
!$$$$$$         
!$$$$$$         ALLOCATE(tmp_arr(2*SIZE(array)))
!$$$$$$         tmp_arr(1:SIZE(array))=array
!$$$$$$         DEALLOCATE(array)
!$$$$$$         ALLOCATE(array(size(tmp_arr)))
!$$$$$$         array=tmp_arr
    
	ENDSUBROUTINE resize_array
end program sedtrans