program sedtrans
	winapp
    implicit none
    !character(len=10):: name
    integer   ::  t,nx,ny,Lx,Ly,a,N,i
	real::deltaT,deltaX,deltaY
    real,dimension(:)::Q
   ! real*8, allocatable,dimension(:):: x,y
    real*8,dimension(12):: x,y
    nx=12;ny=12
!$$$$$$     allocate(x(nx))
!$$$$$$     allocate(y(ny))

    
    do i=1,nx
      x(i)=i-1
    enddo
    do i=1,ny
      y(i)=x(i)**2
    enddo
!$$$$$$     deallocate(x,y) 

	i=winio@('%ww%ca[SIMPLEPLOT-Quadratic]%bg[grey]%pv&')
	i=winio@('%pl[colour=black,x_array]&',400,250,nx,x,y)
    i=winio@('%ff%nl%cn%tt[ok]')
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
end program sedtrans