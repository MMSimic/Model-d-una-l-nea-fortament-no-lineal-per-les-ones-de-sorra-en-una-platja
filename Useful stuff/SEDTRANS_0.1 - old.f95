program sedtrans
	winapp
    library 'c:\TFG\simple.dll'
	implicit none
    include <windows.ins>
    external update_plot
	!Define variables and parameters
!$$$$$$     character(len=10):: name
	integer :: T,n,m,Lx,Ly,i,j,T_current
    double precision,dimension(:),allocatable :: fill
	real*8 :: T_real,deltaT,deltaX,deltaY,pi
	real*8,dimension(:,:),allocatable :: y_omni,Q_omni,beta,alpha
	real*8,dimension(:),allocatable :: Cq,Hb,x,y,Q
    parameter(pi=3.1416)

!$$$$$$     common T,T_current,fill

	!Provisional size of beach
    Lx=50
    Ly=100

	!Define accuracy of net
!$$$$$$     print *, 'Number of ''x'' divisions?'
!$$$$$$     read *, n
    n=12
	allocate(x(n+1))
	allocate(y(n+1))
    allocate(Q(n+1))
!$$$$$$     print *, 'Number of time steps?'
!$$$$$$     read *, T
	T=12
    T_real=T
	allocate(y_omni(T+1,n+1))
	allocate(Q_omni(T+1,n+1))
    allocate(fill(T+1))

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
    T_current=3
    do i=1,T+1
	    fill(i)=i/T_real
    enddo
	i=winio@('%ww%ca[Shoreline Plot]%bg[window]%pv&')
	i=winio@('%pl[colour=red,y_min=0,x_array]&',400,250,n,x,y)
    !i=winio@('%ff%nl%cn%20sl&',T_current,0.0D0,T_real)
    i=winio@('%ff%nl%cn%2.1ob[status,thin_panelled]&')
    i=winio@('%`bg[white]%30br[no_border]%cb&',fill(T_current),RGB@(0,128,128))
    i=winio@('%`bg[white]%co[no_data_border,right_justify]%dd%il%6rd%cb&',1,0,T,T_current)
    i=winio@('%ff%nl%cn%tt[Close]')
!$$$$$$     call axgrid('*cartesian',1,-1)

!$$$$$$     secs=2
!$$$$$$     do while(secs>=1)
!$$$$$$         if (secs>=1000) then
!$$$$$$             secs=2
!$$$$$$         else
!$$$$$$             secs=secs+1
!$$$$$$         endif   
!$$$$$$     enddo

	CALL ask

	CONTAINS
    
	SUBROUTINE ask
    	print*,'time-step? (prev: ',T_current
        read*,T_current
        do i=1,n+1
            y(i)=y_omni(T_current+1,i)
        enddo
        CALL plot
    ENDSUBROUTINE ask
    
    SUBROUTINE plot
    	print *, fill(T_current)
        	i=winio@('%ww%ca[Shoreline Plot]%bg[window]%pv&')
			i=winio@('%pl[colour=red,y_min=0,x_array]&',400,250,n,x,y)
   		 	!i=winio@('%ff%nl%cn%20sl&',T_current,0.0D0,T_real)
   		 	i=winio@('%ff%nl%cn%2.1ob[status,thin_panelled]&')
   			i=winio@('%`bg[white]%30br[no_border]%cb&',fill(T_current),RGB@(0,128,128))
  			i=winio@('%`bg[white]%co[no_data_border,right_justify]%dd%il%6rd%cb&',1,0,T,T_current)
  			i=winio@('%ff%nl%cn%tt[Close]')
    	CALL window_update@(fill)
        CALL ask
    ENDSUBROUTINE plot
    
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

function update_plot()

	integer :: T_current
    real*8 :: T_real
    double precision fill
!$$$$$$     common T,T_current,fill

!$$$$$$     fill=T_current/T_real
!$$$$$$     print *, fill
!$$$$$$         do i=1,n+1
!$$$$$$             y(i)=y_omni(T_current+1,i)
!$$$$$$         enddo
!		y=y_omni(T_current+1,:)
        CALL window_update@(fill)
        update_plot=1
endfunction update_plot