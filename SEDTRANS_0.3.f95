!-------------------------------------------------------------------------------------------------------
!---                                         Modules                                                 ---
!-------------------------------------------------------------------------------------------------------

MODULE definitions
    !Define variables and parameters
!$$$$$$     character(len=10):: name
    character*7 shore_cases(2)
    data shore_cases /'Case 1','Case 2'/
    integer :: nT,n,ny,Lx,Ly,i,j,k,T_current,selected_case,show_init
    real*8 :: deltaT,deltaX,deltaY,pi,fill,slider_current,theta,K_par,ro_s,ro_w,p,brindex,mu,T,H0
    real*8,dimension(:,:),allocatable :: ys_omni,Q,Hb
    real*8,dimension(:,:,:),allocatable :: D,L,H
    real*8,dimension(:),allocatable :: x,ys,y
    parameter(pi=3.1415927)
    parameter(g=9.80665)
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
    integer :: setup_plot,update_plot,update_chart,strtonum,numtostr,sure_quit,plot_window,page1,page2
    external shore_case_1,shore_case_2,setup_plot,update_plot,update_chart,strtonum,numtostr,sure_quit

    !Provisional size of beach
    Lx=1500
    Ly=1500

    !Define accuracy of net
!$$$$$$     print *, 'Number of ''x'' divisions?'
!$$$$$$     read *, n
    n=75
    ny=75
    allocate(x(n+1))
    allocate(ys(n+1))
    allocate(y(ny+1))
!$$$$$$     print *, 'Number of time steps?'
!$$$$$$     read *, T
    nT=10000
    allocate(D(n+1,ny+1,nT+1))
    allocate(ys_omni(nT+1,n+1))
    allocate(Q(nT+1,n+1))

    !Calculate deltas
    deltaX=Lx/n
    deltaY=Ly/ny
    deltaT=365*24*3600/nT	!Length of experiment is 1 yr. (measured in seconds)

    !Set initial conditions
    do i=1,n+1
        x(i)=(i-1)*deltaX
        ys(i)=sin((i-1)*(pi/(37.1/8)))*10+200
!$$$$$$         y(i)=200
        ys_omni(1,i)=ys(i)
    enddo
    theta=15.0
    T=14.0
    H0=3
    K_par=0.7
    ro_s=2.65
    ro_w=1.0
    p=0.4
    brindex=0.5
    mu=0.2
    CALL shore_case_1
    selected_case=1
    T_current=0
    fill=T_current/real(nT)

    !Create the window
    !Sheet pages setup
    i=winio@('%sh&',page1)
        i=winio@('%ca[Parameters]&')
        i=winio@('%1.1ob[named_l][Accuracy]&')
            i=winio@('%1.5ob[no_border]&')
                i=winio@('nT:  %cb&')
                i=winio@('Lx:  %cb&')
                i=winio@('Ly:  %cb&')
                i=winio@('nx:  %cb&')
                i=winio@('ny:  %cb&')
        i=winio@('%cb&')
        i=winio@('%ff%nl%1.1ob[named_l][Data]&')
            i=winio@('%2.1ob[no_border]&')
                i=winio@('%`bg[inactivecaption]%1.2ob&')
                    i=winio@('%1.5ob[no_border]&')
i=winio@('%eq[K]    %co[check_on_focus_loss,right_justify]%dy%`bg[white]%^5rf%cb&',23L,31L,0.25D0,K_par,update_chart)
i=winio@('%eq[{rho}{sub S}]%co[check_on_focus_loss,right_justify]%dy%`bg[white]%^5rf%cb&',36L,33L,0.5D0,ro_s,update_chart)
i=winio@('%eq[{rho}{sub W}]%co[check_on_focus_loss,right_justify]%dy%`bg[white]%^5rf%cb&',36L,33L,0.5D0,ro_w,update_chart)
i=winio@('%eq[p]    %co[check_on_focus_loss,right_justify]%dy%`bg[white]%^5rf%cb&',23L,33L,0.5D0,p,update_chart)
i=winio@('%eq[{gamma}{sub b}]%co[check_on_focus_loss,right_justify]%dy%`bg[white]%^5rf%cb%cb&',36L,33L,0.5D0,brindex,update_chart)
                    i=winio@('%eq[{mu}]    %co[check_on_focus_loss,right_justify]%dy%`bg[white]%`5rf%cb%cb&',22L,29L,0.5D0,mu)
                i=winio@('%`bg[inactivecaption]%1.1ob&')
i=winio@('%eq[{theta}]      %co[check_on_focus_loss,right_justify]%dy%`bg[white]%5rf%cb&',17L,25L,0.25D0,theta)
                i=winio@('%ff%nl%`bg[inactivecaption]%1.1ob&')
i=winio@('%eq[T]      %co[check_on_focus_loss,right_justify]%dy%`bg[white]%5rf%cb&',17L,25L,0.25D0,T)
                i=winio@('%ff%nl%`bg[inactivecaption]%1.1ob&')
i=winio@('%eq[H{sub 0}]%co[check_on_focus_loss,right_justify]%dy%`bg[white]%5rf%cb%cb&',35L,32L,0.25D0,H0)
        i=winio@('%cb')
    i=winio@('%sh&',page2)
        i=winio@('%ca[Structures]&')
        i=winio@('%ff%nl%cn%`bg[window]%1.1ob[thin_panelled]ASDF%cb')
    !Plot sub-window
    i=winio@('%ww[no_caption,no_frame]%`lw&',plot_window)
        i=winio@('%ap&',65L,0L)
        i=winio@('%`bg[window]%1.1ob[thin_panelled]%`rb[Show initial line.]%cb%ff&',show_init)
        i=winio@('%pl[colour=red,y_min=0,y_max=700,x_array]&',600,400,n,x,ys)
        i=winio@('%ff%nl%cn%`bg[window]%3.1ob[thin_panelled]&')
            i=winio@('%dy%`bg[white]%30br[percentage]%cb&',0.5D0,fill,RGB@(0,128,128))
            i=winio@('    %dy%`bg[white]%1.1ob[scored]%co[no_data_border,right_justify]&',0.25D0)
                i=winio@('%dd%il%^rd%cb%cb&',nT/200,0,nT,T_current,update_plot)
            i=winio@('%`bg[white]%^`ls%cb',shore_cases,size(shore_cases),selected_case,setup_plot)
    !Main frame window
    i=winio@('%ww[no_maxbox]%ca[Shoreline Plot]%bg[grey]&')
        i=winio@('%2.1ob[no_border]%`bg[white]%1.1ob[scored]%ch%cb%cb&',plot_window)
            i=winio@('%1.2ob[no_border]%2ps%cb&',page1,page2)
            i=winio@('%nl%cn%fn[Times New Roman]%ts%`bg[inactiveborder]%^bt[Calculate]%sf%cb%cb&',1.4D0,setup_plot)
        i=winio@('%cc',sure_quit)
        
        !i=winio@('%pl[colour=red,x_array]',600,400,ny,y,-D(1,:))
       
    !i=winio@('%ff%nl%cn%^tt[Evaluar]&',update_plot)

ENDPROGRAM sedtrans

!-------------------------------------------------------------------------------------------------------
!---                                       Subroutines                                               ---
!-------------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------
!---                          Utilities                            ---
!---------------------------------------------------------------------
    
SUBROUTINE resize_array
!$$$$$$     INTEGER,DIMENSION(:),ALLOCATABLE :: tmp_arr
!$$$$$$     
!$$$$$$     ALLOCATE(tmp_arr(2*SIZE(array)))
!$$$$$$     tmp_arr(1:SIZE(array))=array
!$$$$$$     DEALLOCATE(array)
!$$$$$$     ALLOCATE(array(size(tmp_arr)))
!$$$$$$     array=tmp_arr
ENDSUBROUTINE resize_array

!---------------------------------------------------------------------
!---                         Shore Cases                           ---
!---------------------------------------------------------------------
    
!Case 1: Groins at x=o and x=Lx
SUBROUTINE shore_case_1
    use definitions
    implicit none
    include <windows.ins>
    real*8 :: progress
    integer :: ctrl
    progress=0.0
    i=winio@('%ww[no_caption,topmost]&')
    i=winio@('Processing... %2nl&')
    i=winio@('%`bg[white]%20br[percentage]&',progress,RGB@(0,128,128))
    i=winio@('%lw',ctrl)
    do k=1,nT
        Q(k+1,1)=0
        do i=2,n
            Q(k+1,i)=mu*(1.2**(5/2))*sin(2*((theta*pi/180)-atan((ys_omni(k,i+1)-ys_omni(k,i-1))/(2*deltaX))))
        enddo
        Q(k+1,n+1)=0
        ys_omni(k+1,1)=ys_omni(k,1)-(deltaT/(deltaX*10))*(Q(k+1,2)-Q(k+1,1))
        do i=2,n
            ys_omni(k+1,i)=ys_omni(k,i)-(deltaT/(2*deltaX*10))*(Q(k+1,i+1)-Q(k+1,i-1))
        enddo
        ys_omni(k+1,n+1)=ys_omni(k,n+1)-(deltaT/(deltaX*10))*(Q(k+1,n+1)-Q(k+1,n))
        progress=progress+0.20/(nT+1)
        CALL window_update@(progress) 
    enddo
    do k=1,nT+1
        do j=1,ny+1
            y(j)=(j-1)*deltaY
            do i=1,n+1
                if ( y(j)-ys_omni(k,i)>0 ) then
                    D(i,j,k)=0.5*((y(j)-ys_omni(k,i))**(0.6667))
                else
                    D(i,j,k)=0
                endif
            enddo
        enddo
        progress=progress+0.80/(nT+1)
        CALL window_update@(progress) 
    enddo
    ctrl=1
    CALL window_update@(ctrl) 
ENDSUBROUTINE shore_case_1

!Case 2: Periodic beach
SUBROUTINE shore_case_2
    use definitions
    implicit none
    include <windows.ins>
    real*8 :: progress
    integer :: ctrl
    progress=0.0D0
    i=winio@('%ww[no_caption,topmost]&')
    i=winio@('Processing... %2nl&')
    i=winio@('%`bg[white]%20br[percentage]&',progress,RGB@(0,128,128))
    i=winio@('%lw',ctrl)
    do k=1,nT
        Q(k+1,1)=mu*(1.2**(5/2))*sin(2*((theta*pi/180)-atan((ys_omni(k,2)-ys_omni(k,1))/(deltaX))))
        do i=2,n
            Q(k+1,i)=mu*(1.2**(5/2))*sin(2*((theta*pi/180)-atan((ys_omni(k,i+1)-ys_omni(k,i-1))/(2*deltaX))))
        enddo
        Q(k+1,n+1)=mu*(1.2**(5/2))*sin(2*((theta*pi/180)-atan((ys_omni(k,n+1)-ys_omni(k,n))/(deltaX))))
        ys_omni(k+1,1)=ys_omni(k,1)-(deltaT/(2*deltaX*10))*(Q(k+1,2)-Q(k+1,n))
        do i=2,n
            ys_omni(k+1,i)=ys_omni(k,i)-(deltaT/(2*deltaX*10))*(Q(k+1,i+1)-Q(k+1,i-1))
        enddo
        ys_omni(k+1,n+1)=ys_omni(k,n+1)-(deltaT/(2*deltaX*10))*(Q(k+1,2)-Q(k+1,n))
        progress=progress+0.20/(nT+1)
        CALL window_update@(progress) 
    enddo
    do k=1,nT+1
        do j=1,ny+1
            y(j)=(j-1)*deltaY
            do i=1,n+1
                if ( y(j)-ys(i)>0 ) then
                    D(i,j,k)=0.5*((y(j)-ys_omni(k,i))**(0.6667))
                else
                    D(i,j,k)=0
                endif
            enddo
        enddo
        progress=progress+0.80/(nT+1)
        CALL window_update@(progress) 
    enddo
    ctrl=1
    CALL window_update@(ctrl) 
ENDSUBROUTINE shore_case_2

!-------------------------------------------------------------------------------------------------------
!---                                    External functions                                           ---
!-------------------------------------------------------------------------------------------------------

! Convert a string to a numeric value
FUNCTION strtonum(str)
    integer strtonum
    character str
    read (str,'(I10)') strtonum
ENDFUNCTION strtonum

! Convert a value to a formatted string
FUNCTION numtostr(num)
    integer num
    character(len=10) numtostr
    write (numtostr,'(I10)') num
ENDFUNCTION numtostr

FUNCTION update_chart
    use definitions
    implicit none
    include <windows.ins>
    integer :: update_chart
    
    mu=(K_par*ro_w*sqrt(g/brindex))/(16*(ro_s-ro_w)*(1-p))

    update_chart=1
ENDFUNCTION update_chart
    
FUNCTION setup_plot
    use definitions
    implicit none
    include <windows.ins>
    integer :: setup_plot,update_plot
    external update_plot,shore_case_1,shore_case_2

    ys_omni(1,:)=ys_omni(T_current+1,:)
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
    fill=T_current/real(nT)
    CALL window_update@(fill) 
    CALL window_update@(T_current)   

    !Update the plot
    ys=ys_omni(T_current+1,:)
    CALL simpleplot_redraw@

    update_plot=1
ENDFUNCTION update_plot

FUNCTION sure_quit
    implicit none
    integer :: a,sure_quit
    !The parameter 'a' returns 1 if 'Si' is pressed, 2 if 'No' is pressed and zero if the window is closed.
    a=winio@('%1SI!Are you sure you want to quit?%nl%nl%cn%`6bt[Yes]          %6bt[No]%ca[Warning]')
    sure_quit=abs(a-1)
ENDFUNCTION sure_quit