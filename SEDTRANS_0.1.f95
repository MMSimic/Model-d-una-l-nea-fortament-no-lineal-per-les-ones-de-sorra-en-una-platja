program sedtrans
	winapp
    implicit none
    !character(len=10):: name
    integer   ::  T,n,m,Lx,Ly,i
	real :: deltaT,deltaX,deltaY
    real,dimension(:,:),allocatable :: Q,beta,alpha
    real*8,dimension(:),allocatable :: Cq,Hb,x,y

    print *, 'Number of ''x'' divisions?'
    read *, n
    allocate(x(n+1))
    allocate(y(n+1))
!$$$$$$     print *, 'Number of time divisions?'
!$$$$$$     read *, T
!$$$$$$     allocate()

!$$$$$$     real*8,dimension(12):: x,y
!$$$$$$     nx=12;m=12
!$$$$$$     allocate(x(n))
!$$$$$$     allocate(y(m))

    !Set initial conditions
    do i=1,n+1
      x(i)=i-1
      y(i)=sin(x(i))/10+3
    enddo
!$$$$$$     deallocate(x,y) 

	i=winio@('%ww%ca[Sediment Transport Plot]%bg[grey]%pv&')
	i=winio@('%pl[colour=red,y_min=0,x_array]&',400,250,n,x,y)
    i=winio@('%ff%nl%cn%tt[Close]')
!$$$$$$     !print *,'time?'
!$$$$$$     !read*,t
!$$$$$$     t=365
!$$$$$$     n=365
!$$$$$$     deltaT=t/n
!$$$$$$     Lx=50; Ly=100

!$$$$$$     a=winio@('%1SI!Tío, eres muy feo.%nl%nl%CN%6bt[Lo sé]&')
!$$$$$$     a=winio@('%ca[Suspense!]')

   
!$$$$$$    print *,'What is your name?'
!$$$$$$    read *,name
!$$$$$$    print *, 'Hi ',TRIM(name),'! Enter number of pounds and  pence'
!$$$$$$    read *, pounds,pence
!$$$$$$    total =100 * pounds + pence
!$$$$$$    print *,'the total money in pence is ',total
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
end program sedtrans