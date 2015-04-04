!-------------------------------------------------------------------------------------------------------
!---                                         Modules                                                 ---
!-------------------------------------------------------------------------------------------------------

MODULE definitions
    !Define variables and parameters
!$$$$$$     character(len=10):: name
    character*7 shore_cases(2)
    data shore_cases /'Case 1','Case 2'/
    integer :: T,n,m,Lx,Ly,i,j,T_current,selected_case,show_init
    real*8 :: deltaT,deltaX,deltaY,pi,fill,slider_current,theta,K,ro_s,ro_w,p,brindex,mu
    real*8,dimension(:,:),allocatable :: y_omni,Q_omni
    real*8,dimension(:),allocatable :: x,y,Q
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
    integer :: update_tick,setup_plot,update_plot,strtonum,numtostr,sure_quit,plot_window,page1,page2
    external shore_case_1,shore_case_2,update_tick,setup_plot,update_plot,strtonum,numtostr,sure_quit

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
        y(i)=sin(x(i)*(pi/(37.1/8)))*10+200
        y_omni(1,i)=y(i)
    enddo
    theta=15
    K=0.7
    ro_s=2.65
    ro_w=1
    p=0.4
    brindex=0.5
    mu=0.2
    CALL shore_case_1
    selected_case=1

    !Create the window
    !Initial parameters
    T_current=0
    fill=T_current/real(T)
    !Sheet pages setup
    i=winio@('%sh&',page1)
        i=winio@('%ca[Parameters]&')
        i=winio@('%1.1ob[named_l][Accuracy]&')
            i=winio@('%1.3ob[no_border]&')
                i=winio@('T:  %cb&')
                i=winio@('nx:  %cb&')
                i=winio@('ny:  %cb&')
        i=winio@('%cb&')
        i=winio@('%ff%nl%1.1ob[named_l][Data]&')
            i=winio@('%2.1ob[no_border]&')
                i=winio@('%`bg[inactivecaption]%1.2ob&')
                    i=winio@('%1.5ob[no_border]&')
i=winio@('%eq[K]    %co[check_on_focus_loss,right_justify]%dy%`bg[white]%^5rf%cb&',23L,31L,0.25D0,K,setup_plot)
i=winio@('%eq[{rho}{sub S}]%co[check_on_focus_loss,right_justify]%dy%`bg[white]%^5rf%cb&',36L,33L,0.5D0,ro_s,setup_plot)
i=winio@('%eq[{rho}{sub W}]%co[check_on_focus_loss,right_justify]%dy%`bg[white]%^5rf%cb&',36L,33L,0.5D0,ro_w,setup_plot)
i=winio@('%eq[p]    %co[check_on_focus_loss,right_justify]%dy%`bg[white]%^5rf%cb&',23L,33L,0.5D0,p,setup_plot)
i=winio@('%eq[{gamma}{sub b}]%co[check_on_focus_loss,right_justify]%dy%`bg[white]%^5rf%cb%cb&',36L,33L,0.5D0,brindex,setup_plot)
                    i=winio@('%eq[{mu}]    %co[check_on_focus_loss,right_justify]%dy%`bg[white]%`5rf%cb%cb&',22L,29L,0.5D0,mu)
                i=winio@('%`bg[inactivecaption]%1.1ob&')
i=winio@('%eq[{theta}]  %co[check_on_focus_loss,right_justify]%dy%`bg[white]%^5rf%cb&',17L,25L,0.25D0,theta,setup_plot)
                i=winio@('%ff%nl%`bg[inactivecaption]%1.1ob&')
                    i=winio@('%eq[H{sub 0}]  %cb%cb&',37L,32L)
        i=winio@('%cb')
    i=winio@('%sh&',page2)
        i=winio@('%ca[Structures]&')
        i=winio@('%ff%nl%cn%`bg[window]%1.1ob[thin_panelled]ASDF%cb')
    !Plot sub-window
    i=winio@('%ww[no_caption,no_frame]%`lw&',plot_window)
        i=winio@('%ap&',65L,0L)
        i=winio@('%`bg[window]%1.1ob[thin_panelled]%`rb[Show initial line.]%cb%ff&',show_init)
        i=winio@('%pl[colour=red,y_min=0,y_max=700,x_array]&',600,400,n,x,y)
        i=winio@('%ff%nl%cn%`bg[window]%3.1ob[thin_panelled]&')
            i=winio@('%`bg[white]%30br[percentage]%cb&',fill,RGB@(0,128,128))
            i=winio@('%`bg[white]%1.1ob[scored]%co[no_data_border,right_justify]&')
                i=winio@('%dd%il%^rd%cb%cb&',T/200,0,T,T_current,update_plot)
            i=winio@('%`bg[white]%^`ls%cb',shore_cases,size(shore_cases),selected_case,setup_plot)
    !Main frame window
    i=winio@('%ww[no_maxbox]%ca[Shoreline Plot]%bg[grey]&')
        i=winio@('%2.1ob[no_border]%`bg[white]%1.1ob[scored]%ch%cb%cb&',plot_window)
            i=winio@('%1.2ob[no_border]%2ps%cb&',page1,page2)
            i=winio@('%nl%cn%fn[Times New Roman]%ts%`bg[inactiveborder]%bt[Reset]%sf%cb%cb&',1.4D0)
        i=winio@('%cc',sure_quit)
       
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
    do j=1,T
        Q_omni(j+1,1)=0
        do i=2,n
            Q_omni(j+1,i)=mu*(1.2**(5/2))*sin(2*((theta*pi/180)-atan((y_omni(j,i+1)-y_omni(j,i-1))/(2*deltaX))))
        enddo
        Q_omni(j+1,n+1)=0
        y_omni(j+1,1)=y_omni(j,1)-(deltaT/(deltaX*10))*(Q_omni(j+1,2)-Q_omni(j+1,1))
        do i=2,n
            y_omni(j+1,i)=y_omni(j,i)-(deltaT/(2*deltaX*10))*(Q_omni(j+1,i+1)-Q_omni(j+1,i-1))
        enddo
        y_omni(j+1,n+1)=y_omni(j,n+1)-(deltaT/(deltaX*10))*(Q_omni(j+1,n+1)-Q_omni(j+1,n))
    enddo
ENDSUBROUTINE shore_case_1

!Case 2: Periodic beach
SUBROUTINE shore_case_2
    use definitions
    implicit none
    do j=1,T
        Q_omni(j+1,1)=mu*(1.2**(5/2))*sin(2*((theta*pi/180)-atan((y_omni(j,2)-y_omni(j,1))/(deltaX))))
        do i=2,n
            Q_omni(j+1,i)=mu*(1.2**(5/2))*sin(2*((theta*pi/180)-atan((y_omni(j,i+1)-y_omni(j,i-1))/(2*deltaX))))
        enddo
        Q_omni(j+1,n+1)=mu*(1.2**(5/2))*sin(2*((theta*pi/180)-atan((y_omni(j,n+1)-y_omni(j,n))/(deltaX))))
        y_omni(j+1,1)=y_omni(j,1)-(deltaT/(2*deltaX*10))*(Q_omni(j+1,2)-Q_omni(j+1,n))
        do i=2,n
            y_omni(j+1,i)=y_omni(j,i)-(deltaT/(2*deltaX*10))*(Q_omni(j+1,i+1)-Q_omni(j+1,i-1))
        enddo
        y_omni(j+1,n+1)=y_omni(j,n+1)-(deltaT/(2*deltaX*10))*(Q_omni(j+1,2)-Q_omni(j+1,n))
    enddo
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

FUNCTION setup_plot
    use definitions
    implicit none
    include <windows.ins>
    integer :: setup_plot,update_plot
    external update_plot,shore_case_1,shore_case_2

    y_omni(1,:)=y_omni(T_current+1,:)
    T_current=0
    mu=(K*ro_w*sqrt(g/brindex))/(16*(ro_s-ro_w)*(1-p))
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
    CALL window_update@(T_current)   

    !Update the plot
    y=y_omni(T_current+1,:)
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