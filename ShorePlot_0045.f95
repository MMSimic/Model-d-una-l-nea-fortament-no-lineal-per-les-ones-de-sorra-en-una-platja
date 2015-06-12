!-------------------------------------------------------------------------------------------------------
!---                                         Modules                                                 ---
!-------------------------------------------------------------------------------------------------------

MODULE definitions
    !Define variables and parameters
    character*80,dimension(25) :: groins
    real*8,dimension(24,3) :: groin_vals
    integer :: nT,n,ny,Lx,Ly,T_current,show_init,lock_plot,lock_data,groin_sel(24),lock_AG,lock_RG,sel_gr
    real*8 :: deltaT,deltaX,deltaY,pi,fill,slider_current,K_par,ro_s,ro_w,p,brindex,mu,T,theta0,H0,A,d50,Hb_local,theta_b_local,d
    real*8 :: x_gr,h_gr,w_gr
    real*8,dimension(:,:),allocatable :: ys_omni,Q,Hb,theta_b,H_vis
    real*8,dimension(:,:,:),allocatable :: H,theta
    real*8,dimension(:),allocatable :: x,ys,y,H_local,ygroin
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
    integer :: i,j,k,m,setup_plot,update_plot,update_chart,sure_quit,plot_window,page1,page2,lock_unlock,propagate_snell,Gt
    external setup_plot,update_plot,update_chart,sure_quit,lock_unlock,propagate,resize_array,reset_shore,add_groin,remove_groin
    external select_groin

    !Provisional size of beach
    Lx=10000
    Ly=10000

    !Define accuracy of net
!$$$$$$     print *, 'Number of ''x'' divisions?'
!$$$$$$     read *, n
    n=50
    ny=50
    allocate(x(n+1))
    allocate(ys(n+1))
    allocate(ygroin(n+1))
    allocate(H_vis(n+1,ny+1))
    allocate(y(ny+1))
    allocate(H_local(ny+1))
!$$$$$$     print *, 'Number of time steps?'
!$$$$$$     read *, T
    nT=3000
    allocate(H(n+1,ny+1,nT+1))
    allocate(theta(n+1,ny+1,nT+1))
    allocate(Hb(n+1,nT+1))
    allocate(theta_b(n+1,nT+1))
    allocate(ys_omni(nT+1,n+1))
    allocate(Q(nT+1,n+1))

    !Calculate deltas
    deltaX=Lx/n
    deltaY=Ly/ny
    deltaT=365*24*3600/nT	!Length of experiment is 1 yr. (measured in seconds)
    
    do j=1,ny+1
        y(j)=(j-1)*deltaY
    enddo
    !Set initial conditions
    d50=0.2
    A=0.09
    T=14.0
    theta0=15.0
    H0=3.0
    K_par=0.7
    ro_s=2.65
    ro_w=1.0
    p=0.4
    brindex=0.5
    mu=(K_par*ro_w*sqrt(g/brindex))/(16*(ro_s-ro_w)*(1-p))
    ygroin(:)=0
    x_gr=0.0
    h_gr=0.0
    w_gr=0.0
    d=200.0
    do i=1,n+1
        x(i)=(i-1)*deltaX
!$$$$$$         ys(i)=sin((i-1)*(pi/(37.1/8)))*10+200
        ys(i)=d
        ys_omni(1,i)=ys(i)
        m=propagate_snell(ys(i))
            Hb(i,1)=Hb_local
            theta_b(i,1)=theta_b_local
            H(i,:,1)=H_local(:)
    enddo
    Gt=mu*((sum(Hb(:,1))/size(Hb(:,1)))**2.5)/(A*(Ly**0.6667))
!$$$$$$     print*,(deltaX**2)/(2*G)
!$$$$$$     print*,deltaT
    groins(:)=''
    groin_sel(:)=0
    sel_gr=0
    groins(1)='|Position|Height|Width'
    groin_vals(:,:)=0
    lock_plot=0
    lock_data=1
    lock_AG=1
    lock_RG=0
    T_current=0
    fill=T_current/real(nT)


    !Create the window
    !Sheet pages setup
    i=winio@('%sh&',page1)
        i=winio@('%ca[Parameters]&')
        i=winio@('%1.1ob[named_l][Accuracy]&')
            i=winio@('%1.3ob[no_border]&')
                i=winio@('nT:  %cb&')
                i=winio@('nx:  %cb&')
                i=winio@('ny:  %cb&')
        i=winio@('%cb&')
        i=winio@('%ff%nl%1.1ob[named_l][Data]&')
            i=winio@('%2.1ob[no_border]&')
                i=winio@('%`bg[inactivecaption]%1.2ob&')
                    i=winio@('%1.5ob[no_border]&')
                        i=winio@('%eq[K]    %co[check_on_focus_loss,right_justify]&',23L,31L)
                            i=winio@('%dy%`bg[white]%~^5rf%cb&',0.25D0,K_par,lock_data,update_chart)
                        i=winio@('%eq[{rho}{sub S}]%co[check_on_focus_loss,right_justify]&',36L,33L)
                            i=winio@('%dy%`bg[white]%~^5rf%cb&',0.5D0,ro_s,lock_data,update_chart)
                        i=winio@('%eq[{rho}{sub W}]%co[check_on_focus_loss,right_justify]&',36L,33L)
                            i=winio@('%dy%`bg[white]%~^5rf%cb&',0.5D0,ro_w,lock_data,update_chart)
                        i=winio@('%eq[p]    %co[check_on_focus_loss,right_justify]&',23L,33L)
                            i=winio@('%dy%`bg[white]%~^5rf%cb&',0.5D0,p,lock_data,update_chart)
                        i=winio@('%eq[{gamma}{sub b}]%co[check_on_focus_loss,right_justify]&',36L,33L)
                            i=winio@('%dy%`bg[white]%~^5rf%cb%cb&',0.5D0,brindex,lock_data,update_chart)
                    i=winio@('%eq[{mu}]    %co[check_on_focus_loss,right_justify]&',22L,29L)
                        i=winio@('%dy%`bg[white]%`~5rf%cb%cb&',0.5D0,mu,lock_data)
                i=winio@('%`bg[inactivecaption]%1.2ob&')
                        i=winio@('%eq[d{sub 50}]%co[check_on_focus_loss,right_justify]&',35L,33L)
                            i=winio@('%dy%`bg[white]%~^5rf%cb&',0.25D0,d50,lock_data,update_chart)
                        i=winio@('%eq[A]      %co[check_on_focus_loss,right_justify]&',17L,25L)
                            i=winio@('%dy%`bg[white]%`~5rf%cb&',0.25D0,A,lock_data)
                i=winio@('%ff%nl%`bg[inactivecaption]%1.1ob&')
                        i=winio@('%eq[T]      %co[check_on_focus_loss,right_justify]&',17L,25L)
                            i=winio@('%dy%`bg[white]%~5rf%cb&',0.25D0,T,lock_data)
                i=winio@('%ff%nl%`bg[inactivecaption]%1.1ob&')
                    i=winio@('%1.2ob[no_border]&')
                        i=winio@('%eq[{theta}{sub 0}]  %co[check_on_focus_loss,right_justify]&',30L,33L)
                            i=winio@('%dy%`bg[white]%~5rf%cb&',0.25D0,theta0,lock_data)
                        i=winio@('%nl%eq[H{sub 0}]%co[check_on_focus_loss,right_justify]&',35L,32L)
                            i=winio@('%dy%`bg[white]%~5rf%cb%cb%cb&',0.25D0,H0,lock_data)
        i=winio@('%cb&')
        i=winio@('%ff%nl%cn%`bg[scrollbar]%1.1ob&')
            i=winio@('Initial shore at:  %co[check_on_focus_loss,right_justify]%`bg[white]%~5rf (m)&',d,lock_data)
            i=winio@('%ff%nl%cn%~^bb[Reset Shoreline]&',lock_data,reset_shore)
        i=winio@('%cb')
    i=winio@('%sh&',page2)
        i=winio@('%ca[Structures]&')
        i=winio@('%cn%1.2ob[no_border]&')
            i=winio@('%ff%nl           %`bg[scrollbar]%1.1ob%ulInsert groin%`ul%ff%nl%2.3ob[no_border]&')
                i=winio@('Position:%cb  %co[check_on_focus_loss,right_justify]%`bg[white]%~5rf (m)%cb&',x_gr,lock_data)
                i=winio@('Height:%cb  %co[check_on_focus_loss,right_justify]%`bg[white]%~5rf (m)%cb&',h_gr,lock_data)
                i=winio@('Width:%cb  %co[check_on_focus_loss,right_justify]%`bg[white]%~5rf (m)%cb&',w_gr,lock_data)
                i=winio@('%ff%nl%cn%~^bb[Insert]%cb&',lock_AG,add_groin)
            i=winio@('%cb&')
        i=winio@('%nl    %^lv[single_selection,show_selection_always]%cb&',185,200,groins,size(groins)-1,groin_sel,1,select_groin)
            i=winio@('%ff%nl%cn%~^bb[Remove]',lock_RG,remove_groin)
    !H plot sub-window
    !i=winio@('%ww[topmost,no_caption,no_frame]%ca[Wave Height]&')
        !i=winio@('%1.1ob%1.2ob[no_border]%pl[x_axis=Y,y_axis=H,colour=blue,x_array]%cb%cn%bt[Close]%cb%cb',500,400,ny+1,y,H(1,:,1))
    !ys plot sub-window
    i=winio@('%ww[no_caption,no_frame]%`lw&',plot_window)
        i=winio@('%ap&',65L,0L)
        i=winio@('%`bg[window]%1.1ob[thin_panelled]%`rb[Show initial line.]%cb%ff&',show_init)
        i=winio@('%pl[n_graphs=2,colour=red,colour=black,y_min=0,y_max=700,x_array]&',600,450,n+1,x,ys,ygroin)
        i=winio@('%ff%nl%cn%`bg[window]%3.1ob[thin_panelled]&')
            i=winio@('%dy%`bg[white]%30br[percentage]%cb&',0.7D0,fill,RGB@(0,128,128))
            i=winio@('    %dy%`bg[white]%1.1ob[scored]%co[no_data_border,right_justify]&',0.4D0)
                i=winio@('%dd%il%~^rd%cb%cb&',nT/200,0,nT,T_current,lock_plot,update_plot)
            i=winio@(' %dy%`bg[white]%~^bt[Edit Data]%cb',0.25D0,lock_plot,lock_unlock)
    !Main frame window
    i=winio@('%mi[MainIcon]%ww[no_maxbox]%ca[Shoreline Plot]%bg[grey]&')
        i=winio@('%2.1ob[no_border]%`bg[white]%1.1ob[scored]%ch%cb%cb&',plot_window)
            i=winio@('%1.2ob[no_border]%2ps%cb&',page1,page2)
            i=winio@('%nl%cn%fn[Times New Roman]%ts%`bg[inactiveborder]%~^bt[Calculate]%sf%cb%cb&',1.4D0,lock_data,setup_plot)
        i=winio@('%cc',sure_quit)
        
!$$$$$$         i=winio@('%pl[colour=red,x_array]',600,400,ny+1,y,H(1,:,1))

!$$$$$$     do i=1,n+1
!$$$$$$         H_vis(i,:)=H(i,:,1)
!$$$$$$     enddo
!$$$$$$     CALL VSNEW
!$$$$$$ !$$$$$$     CALL VSRG(H_vis,n+1,ny+1)
!$$$$$$     CALL sfeqx(0,deltaX)
!$$$$$$     CALL sfeqy(0,deltaY)
!$$$$$$     CALL rgsurf(H_vis,n+1,ny+1)
!$$$$$$     CALL VSOUT
!$$$$$$     CALL title7('T','C','Surface')
!$$$$$$     CALL ENDPLT
       
    !i=winio@('%ff%nl%cn%^tt[Evaluar]&',update_plot)

ENDPROGRAM sedtrans

!-------------------------------------------------------------------------------------------------------
!---                                       Subroutines                                               ---
!-------------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------
!---                       Calculate Shore                         ---
!---------------------------------------------------------------------

SUBROUTINE test
    use definitions
    implicit none
    include <windows.ins>
    integer :: m,propagate_q2dmorpho
    external propagate_q2dmorpho

    m=propagate_q2dmorpho()

ENDSUBROUTINE test

SUBROUTINE shore_calc
    use definitions
    implicit none
    include <windows.ins>
    real*8 :: progress
    integer :: i,j,k,m,ctrl,propagate_snell
    external propagate_snell
    progress=0.0
    i=winio@('%ww[no_caption,topmost]&')
    i=winio@('Processing... %2nl&')
    i=winio@('%`bg[white]%20br[percentage]&',progress,RGB@(0,128,128))
    i=winio@('%lw',ctrl)
    
!$$$$$$     if then
!$$$$$$       m=propagate_q2dmorpho
!$$$$$$     endif

    do i=1,n+1
        !Initial time:
        m=propagate_snell(ys_omni(1,i)) !Call function and store the updated local parameters in global vars.
            Hb(i,1)=Hb_local
            theta_b(i,1)=theta_b_local
            H(i,:,1)=H_local(:)
    enddo
    do k=1,nT
     !!Calculate Q (Sediment flux)
      !Initial point (i=1)
      if (ygroin(1)-ys_omni(k,1)>0) then !Groin
        Q(k+1,1)=0 
      else !Periodic
        Q(k+1,1)=mu*(Hb(1,k)**(5/2))*sin(2*((theta_b(1,k)*pi/180)-atan((ys_omni(k,2)-ys_omni(k,n))/(2*deltaX)))) 
      endif
      !Intermediate points
      do i=2,n
        if (ygroin(i)-ys_omni(k,i)>0) then !Groin
          Q(k+1,i)=0 
        else !Periodic
          if (theta0<0 .and. ygroin(i+1)-ys_omni(k,i+1)>0) then !Before a groin and waves coming from East (don't use its 'ys' value!)
            Q(k+1,i)=mu*(Hb(i,k)**(5/2))*sin(2*((theta_b(i,k)*pi/180)-atan((ys_omni(k,i)-ys_omni(k,i-1))/(deltaX)))) 
          elseif (theta0>=0 .and. ygroin(i-1)-ys_omni(k,i-1)>0) then !After a groin and waves coming from West (don't use its 'ys' value!)
            Q(k+1,i)=mu*(Hb(i,k)**(5/2))*sin(2*((theta_b(i,k)*pi/180)-atan((ys_omni(k,i+1)-ys_omni(k,i))/(deltaX)))) 
          else
            if (theta0<0 .and. ygroin(i)>0) then !Groin surpassed, only consider excess sand. East waves.
              Q(k+1,i)=mu*(Hb(i,k)**(5/2))*sin(2*((theta_b(i,k)*pi/180)-atan((ys_omni(k,i)-ys_omni(k,i-1))/(deltaX))))
            elseif (theta0>=0 .and. ygroin(i)>0) then !Groin surpassed, only consider excess sand. West waves.
              Q(k+1,i)=mu*(Hb(i,k)**(5/2))*sin(2*((theta_b(i,k)*pi/180)-atan((ys_omni(k,i+1)-ys_omni(k,i))/(deltaX))))
            else !No groin
              Q(k+1,i)=mu*(Hb(i,k)**(5/2))*sin(2*((theta_b(i,k)*pi/180)-atan((ys_omni(k,i+1)-ys_omni(k,i-1))/(2*deltaX))))
            endif
          endif
        endif
      enddo
      !Final point (i=n+1)
      if (ygroin(n+1)-ys_omni(k,n+1)>0) then !Groin
        Q(k+1,n+1)=0 
      else !Periodic
        Q(k+1,n+1)=mu*(Hb(n+1,k)**(5/2))*sin(2*((theta_b(n+1,k)*pi/180)-atan((ys_omni(k,2)-ys_omni(k,n))/(2*deltaX))))
      endif
      
     !!Calculate ys (Shore-line)
      i=1 !Initial point
      if (ygroin(i)-ys_omni(k,i)>0) then !Groin
        if (theta0>=0) then
            ys_omni(k+1,1)=ys_omni(k,1)-(deltaT/(deltaX*10))*(Q(k+1,1)-Q(k+1,n))
        else
            ys_omni(k+1,1)=ys_omni(k,1)-(deltaT/(deltaX*10))*(Q(k+1,2)-Q(k+1,1))
        endif
        m=propagate_snell(ys_omni(k+1,1)) !This function calculates Hb and theta_b for the given 'i' position.
            Hb(1,k+1)=Hb_local
            theta_b(1,k+1)=theta_b_local
            H(1,:,k+1)=H_local(:)
      else
        if (ygroin(1)>0) then !Sand has reached the tip of the groin.
          if (theta0>=0) then
              ys_omni(k+1,1)=ys_omni(k,1)-(deltaT/(deltaX*10))*(Q(k+1,1)-Q(k+1,n))
          else
              ys_omni(k+1,1)=ys_omni(k,1)-(deltaT/(deltaX*10))*(Q(k+1,2)-Q(k+1,1))
          endif
          m=propagate_snell(ys_omni(k+1,i))
              Hb(i,k+1)=Hb_local
              theta_b(i,k+1)=theta_b_local
              H(i,:,k+1)=H_local(:)
        else !No groin (periodic)
          ys_omni(k+1,1)=ys_omni(k,1)-(deltaT/(2*deltaX*10))*(Q(k+1,2)-Q(k+1,n))
          m=propagate_snell(ys_omni(k+1,1))
              Hb(1,k+1)=Hb_local
              theta_b(1,k+1)=theta_b_local
              H(1,:,k+1)=H_local(:)
        endif
      endif
      !Intermediate points
      do i=2,n
        if (ygroin(i)-ys_omni(k,i)>0) then !Groin
          if (theta0>=0) then
              ys_omni(k+1,i)=ys_omni(k,i)-(deltaT/(deltaX*10))*(Q(k+1,i)-Q(k+1,i-1))
          else
              ys_omni(k+1,i)=ys_omni(k,i)-(deltaT/(deltaX*10))*(Q(k+1,i+1)-Q(k+1,i))
          endif
          m=propagate_snell(ys_omni(k+1,i))
              Hb(i,k+1)=Hb_local
              theta_b(i,k+1)=theta_b_local
              H(i,:,k+1)=H_local(:)
        else
          if (ygroin(i)>0) then !Sand has reached the tip of the groin.
            if (theta0>=0) then
                ys_omni(k+1,i)=ys_omni(k,i)-(deltaT/(deltaX*10))*(Q(k+1,i)-Q(k+1,i-1))
!$$$$$$                 ys_omni(k+1,i)=ygroin(i)
            else
                ys_omni(k+1,i)=ys_omni(k,i)-(deltaT/(deltaX*10))*(Q(k+1,i+1)-Q(k+1,i))
!$$$$$$                 ys_omni(k+1,i)=ygroin(i)
            endif
            m=propagate_snell(ys_omni(k+1,i))
                Hb(i,k+1)=Hb_local
                theta_b(i,k+1)=theta_b_local
                H(i,:,k+1)=H_local(:)
          else !No groin
            ys_omni(k+1,i)=ys_omni(k,i)-(deltaT/(2*deltaX*10))*(Q(k+1,i+1)-Q(k+1,i-1))
            m=propagate_snell(ys_omni(k+1,i))
                Hb(i,k+1)=Hb_local
                theta_b(i,k+1)=theta_b_local
                H(i,:,k+1)=H_local(:)
          endif
        endif
      enddo
      i=n+1 !Final point
      if (ygroin(i)-ys_omni(k,i)>0) then !Groin
        if (theta0>=0) then
            ys_omni(k+1,n+1)=ys_omni(k,n+1)-(deltaT/(deltaX*10))*(Q(k+1,n+1)-Q(k+1,n))
        else
            ys_omni(k+1,n+1)=ys_omni(k,n+1)-(deltaT/(deltaX*10))*(Q(k+1,2)-Q(k+1,n+1))
        endif 
        m=propagate_snell(ys_omni(k+1,n+1))
            Hb(n+1,k+1)=Hb_local
            theta_b(n+1,k+1)=theta_b_local
            H(n+1,:,k+1)=H_local(:)
      else
        if (ygroin(n+1)>0) then !Sand has reached the tip of the groin.
          if (theta0>=0) then
              ys_omni(k+1,n+1)=ys_omni(k,n+1)-(deltaT/(deltaX*10))*(Q(k+1,n+1)-Q(k+1,n))
          else
              ys_omni(k+1,n+1)=ys_omni(k,n+1)-(deltaT/(deltaX*10))*(Q(k+1,2)-Q(k+1,n+1))
          endif
          m=propagate_snell(ys_omni(k+1,n+1))
              Hb(n+1,k+1)=Hb_local
              theta_b(n+1,k+1)=theta_b_local
              H(n+1,:,k+1)=H_local(:)
        else !No groin (periodic)
          ys_omni(k+1,n+1)=ys_omni(k+1,1)
          m=propagate_snell(ys_omni(k+1,n+1))
              Hb(n+1,k+1)=Hb_local
              theta_b(n+1,k+1)=theta_b_local
              H(n+1,:,k+1)=H_local(:)
        endif
      endif
        
        progress=progress+1.0/(nT+1) !Display progress.
        CALL window_update@(progress) 
    enddo

    ctrl=1 !This is so the progress-bar window closes.
    CALL window_update@(ctrl)
ENDSUBROUTINE shore_calc

!-------------------------------------------------------------------------------------------------------
!---                                    External functions                                           ---
!-------------------------------------------------------------------------------------------------------

! Resize an array    
FUNCTION resize_array(array,oldsize,newsize)
!$$$$$$     integer :: oldsize,newsize
!$$$$$$     real*8,dimension(oldsize),allocatable :: array
!$$$$$$     real*8,dimension(:),allocatable :: tmp_arr
!$$$$$$     
!$$$$$$     allocate(tmp_arr(newsize))
!$$$$$$     tmp_arr(1:size(array))=array
!$$$$$$     deallocate(array)
!$$$$$$     allocate(array(size(tmp_arr)))
!$$$$$$     array=tmp_arr
ENDFUNCTION resize_array

! Convert a string to a numeric value
FUNCTION strtonum(str)
    integer strtonum
    character str
    read (str,'(I10)') strtonum
ENDFUNCTION strtonum

! Convert a value to a formatted string
FUNCTION numtostr(num)
    real*8 num
    character(len=10) numtostr
    write (numtostr,'(F10.2)') num
ENDFUNCTION numtostr

!Q2D-morpho model - Eikonal eq. and energy conservation (consider propagation and changes in H and theta along arbitrary bathymetry)
FUNCTION propagate_q2dmorpho
    use definitions
    implicit none
    include <windows.ins>
    integer :: propagate_q2dmorpho,i,j,ctrl
    real*8 :: lim_iter,progress,k0,theta_2dm
    real*8,dimension(n+1,ny+1) :: Dpth,kf,kx,ky
    progress=0.0
    i=winio@('%ww[no_caption,topmost]&')
    i=winio@('Processing... %2nl&')
    i=winio@('%`bg[white]%20br[percentage]&',progress,RGB@(0,128,128))
    i=winio@('%lw',ctrl)

  do i=1,n+1
    !Calculate D(depth) along the x and y axis:
    do j=1,ny+1
        if ( y(j)-ys(i)>0 ) then
            Dpth(i,j)=A*((y(j)-ys(i))**(0.6667))
        else
            Dpth(i,j)=0
        endif
    enddo
    
    !First step: find K (apply Newton-Raphson iterative method)
    kf(:,:)=(2*pi/((g*(T**2))/(2*pi))) !D.W. Approximation
    !print*,kf(1,1)
    j=ny+1
    do while (Dpth(i,j-1)>0 .and. j>1)
        j=j-1
        lim_iter=1.0
        do while (lim_iter>0.000001)
            k0=kf(i,j)-((g*kf(i,j)*tanh(kf(i,j)*Dpth(i,j))-((2*pi)/T))/(g*tanh(kf(i,j)*Dpth(i,j))+g*kf(i,j)*&
                (1-(tanh(kf(i,j)*Dpth(i,j)))**2)*Dpth(i,j)))
            lim_iter=abs(k0-kf(i,j))
            kf(i,j)=k0
            !print*,kf(i,j)
        enddo
        !print*,kf(i,j)
    enddo
  enddo
    
  !Second step: find theta
  !We need 2 boundary conditions (1 at each axis):
  kx(1,ny+1)=kf(1,ny+1)*sin(theta0*pi/180) ! (1st)
  !Snell to get K's at x=0? (2nd)
  do i=2,n+1
    kx(i,ny+1)=kf(i,ny+1)*sin(theta0*pi/180)
    j=ny+1
    do while (Dpth(i,j-1)>0)
      j=j-1
      !print*,kf(i-1,j)
      if (i>=ny+2-j) then
      kx(i,j)=kx(i,j+1)+(deltaY/deltaX)*(sqrt(kf(i,j+1)**2-kx(i,j+1)**2)-sqrt(kf(i-1,j+1)**2-kx(i-1,j+1)**2))
      endif
    enddo
  enddo
    
    !Third step: Compute H


    progress=progress+1.0/(n+1) !Display progress.
    CALL window_update@(progress) 

  
!$$$$$$     !Iterate while checking for breaking condition (approaching the shoreline).
!$$$$$$     do while (j>1.and.D_local(j)>0.and.H_local(j)/D_local(j)<=brindex)
!$$$$$$ 
!$$$$$$     enddo
!$$$$$$     !Breaking conditions achieved:
!$$$$$$     Hb_local=H_local(j)
!$$$$$$     theta_b_local=0
!$$$$$$     !From now on, H decreases until meeting the shoreline:
!$$$$$$     do while (j>1)
!$$$$$$         j=j-1
!$$$$$$         H_local(j)=brindex*D_local(j)
!$$$$$$     enddo

    ctrl=1 !This is so the progress-bar window closes.
    CALL window_update@(ctrl)
    propagate_q2dmorpho=1
ENDFUNCTION propagate_q2dmorpho

!Snell's Law (consider propagation and changes in H and theta along parallel bathymetry)
FUNCTION propagate_snell(y_value)
    use definitions
    implicit none
    include <windows.ins>
    integer :: propagate_snell,j,iters
    real*8 :: y_value,L0,Ks,Kr
    real*8,dimension(ny+1) :: D_local,L1,L2,L3

    L0=(g*(T**2))/(2*pi)
    L1(:)=0.75*L0
    
    !Calculate D(depth) along the y axis:
    do j=1,ny+1
        if ( y(j)-y_value>0 ) then
            D_local(j)=A*((y(j)-y_value)**(0.6667))
        else
            D_local(j)=0
        endif
    enddo
    !Estimate L(wave length) along the y axis:
    do iters=1,10
        do j=1,ny+1
            L2(j)=L0*tanh((2*pi*D_local(j))/L1(j))
            L3(j)=(2*L2(j)+L1(j))/3
            L1(j)=L3(j)
        enddo
    enddo
    !Calculate H(wave height) along the y axis:
    !First iteration to allow initial comparison in subsequent loop.
    j=ny+1
    if (D_local(j)/L1(j)>=0.5) then !deep waters
        Ks=L0/(2*T)
    elseif (D_local(j)/L1(j)<=0.05) then !shallow waters
        Ks=sqrt(g*D_local(j))
    else !intermediate waters
        Ks=sqrt((L0/(2*T))/((L1(j)/(2*T))*(1+(4*pi*D_local(j)/L1(j))/(sinh(4*pi*D_local(j)/L1(j))))))
    endif
    Kr=sqrt(cos(theta0*pi/180)/cos(asin((L1(j)/L0)*sin(theta0*pi/180))))
    H_local(j)=H0*Ks*Kr
    !Iterate while checking for breaking condition (approaching the shoreline).
    do while (j>1.and.D_local(j)>0.and.H_local(j)/D_local(j)<=brindex)
        j=j-1
        if (D_local(j)/L1(j)>=0.5) then !deep waters
            Ks=L0/(2*T)
        elseif (D_local(j)/L1(j)<=0.05) then !shallow waters
            Ks=sqrt(g*D_local(j))
        else !intermediate waters
            Ks=sqrt((L0/(2*T))/((L1(j)/(2*T))*(1+(4*pi*D_local(j)/L1(j))/(sinh(4*pi*D_local(j)/L1(j))))))
        endif
        Kr=sqrt(cos(theta0*pi/180)/cos(asin((L1(j)/L0)*sin(theta0*pi/180))))
        H_local(j)=H0*Ks*Kr
    enddo
    !Breaking conditions achieved:
    Hb_local=H_local(j)
    theta_b_local=(180/pi)*asin((L1(j)/L0)*sin(theta0*pi/180))
    !From now on, H decreases until meeting the shoreline:
    do while (j>1)
        j=j-1
        H_local(j)=brindex*D_local(j)
    enddo
    
    propagate_snell=1
ENDFUNCTION propagate_snell

FUNCTION update_chart
    use definitions
    implicit none
    include <windows.ins>
    integer :: update_chart
    mu=(K_par*ro_w*sqrt(g/brindex))/(16*(ro_s-ro_w)*(1-p))

    if (d50<=0.4) then
        A=0.41*(d50**(0.94))
    elseif (d50>0.4 .and. d50<=10) then
        A=0.23*(d50**(0.32))
    elseif (d50>10 .and. d50<=40) then
        A=0.23*(d50**(0.28))
    else
        A=0.46*(d50**(0.11))
    endif

    update_chart=1
ENDFUNCTION update_chart

FUNCTION setup_plot
    use definitions
    implicit none
    include <windows.ins>
    integer :: i,setup_plot,update_plot,lock_unlock
    external update_plot,lock_unlock
    
    i=lock_unlock()

    ys_omni(1,:)=ys_omni(T_current+1,:) !Reset timer
    T_current=0
    CALL shore_calc
!$$$$$$     CALL test

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

FUNCTION reset_shore
    use definitions
    implicit none
    include <windows.ins>
    integer :: reset_shore

    ys=d
    ys_omni(1,:)=d
    T_current=0
    CALL simpleplot_redraw@

    reset_shore=1
ENDFUNCTION reset_shore

FUNCTION add_groin
    use definitions
    implicit none
    include <windows.ins>
    integer :: add_groin,taken,i,j
    character*10 numtostr
    external numtostr

    if (h_gr<=0.1) then !Too low, not a groin.
        j=winio@('%1SI!Height value is too low.%nl%nl%cn%`6bt[Ok]%ca[Warning]')
    !The shore is longshore-wise periodic. As such, a groin at the first point must also appear at the final point.
    elseif (nint(x_gr/deltaX)+1==(n+1).or.nint(x_gr/deltaX)+1==1) then !Enclosed between two groins.
      if (ygroin(1)==0) then !Check if there's a groin there already.
        ygroin(1)=h_gr
        ygroin(n+1)=h_gr
        do i=1,nint((w_gr/2)/deltaX)+1 !From 1 to W/2
            ygroin(i)=h_gr
        enddo
        do i=nint((n+1)-((w_gr/2)/deltaX)),n+1 !From (n+1-W/2) to (n+1)
            ygroin(i)=h_gr
        enddo
        !List managing:
        j=1
        do while (groins(j)/='')
          j=j+1
        enddo
        groins(j)='|'//numtostr(x_gr)//'|'//numtostr(h_gr)//'|'//numtostr(w_gr)
        groin_vals(j-1,1)=x_gr
        groin_vals(j-1,2)=h_gr
        groin_vals(j-1,3)=w_gr
      else !There was a groin there already.
        j=winio@('%1SI!There is another groin there.%nl%nl%cn%`6bt[Ok]%ca[Warning]')
      endif
    elseif (nint((x_gr+(w_gr/2))/deltaX)+1>=(n+1).or.nint((x_gr-(w_gr/2))/deltaX)+1<=1) then !Too wide.
        j=winio@('%1SI!The groin specified exceeds beach dimensions.%nl%nl%cn%`6bt[Ok]%ca[Warning]')
    else !Normal groin.
        taken=0
        do i=nint((x_gr-(w_gr/2))/deltaX)+1,nint((x_gr+(w_gr/2))/deltaX)+1 !Check if there's a groin there already.
            if (ygroin(i)/=0) then
              taken=1
            endif
        enddo
        if (taken==0) then !No groins there.
          do i=nint((x_gr-(w_gr/2))/deltaX)+1,nint((x_gr+(w_gr/2))/deltaX)+1
              ygroin(i)=h_gr
          enddo
          !List managing:
          j=1
          do while (groins(j)/='')
            j=j+1
          enddo
          groins(j)='|'//numtostr(x_gr)//'|'//numtostr(h_gr)//'|'//numtostr(w_gr)
          groin_vals(j-1,1)=x_gr
          groin_vals(j-1,2)=h_gr
          groin_vals(j-1,3)=w_gr
        else !There was a groin there already.
          j=winio@('%1SI!There is another groin there.%nl%nl%cn%`6bt[Ok]%ca[Warning]')
        endif
    endif
    CALL simpleplot_redraw@     

    x_gr=0.0
    h_gr=0.0
    w_gr=0.0

    if (groins(25) /= '')then
        j=winio@('%1SI!Maximum number of groins reached!%nlRemove existing groins before adding new ones.&')
        j=winio@('%nl%nl%cn%`6bt[Ok]%ca[Warning]')
        lock_AG=0
    else
        lock_AG=1
    endif
    CALL window_update@(lock_AG)
    
    add_groin=1
ENDFUNCTION add_groin

FUNCTION select_groin
    use definitions
    implicit none
    include <windows.ins>
    integer :: select_groin

!$$$$$$     sel_gr=0
!$$$$$$     select case(clearwin_string@('CALLBACK_REASON'))
!$$$$$$       case('SET_SELECTION')
      sel_gr=clearwin_info@('ROW_NUMBER')
!$$$$$$     end select

    if (sel_gr/=0 .and. lock_data==1) then
        lock_RG=1 !Unlock the 'remove groin' button
    elseif (lock_data==1) then
        lock_RG=0 !Lock it
    endif
    
    CALL window_update@(lock_RG)

    select_groin=1
ENDFUNCTION select_groin

FUNCTION remove_groin
    use definitions
    implicit none
    include <windows.ins>
    integer :: i,j,remove_groin,select_groin,enclosed
    real*8 :: xg,hg,wg
    external select_groin

    xg=groin_vals(sel_gr,1)
    hg=groin_vals(sel_gr,2)
    wg=groin_vals(sel_gr,3)

    enclosed=0
    if (nint(xg/deltaX)+1==(n+1).or.nint(xg/deltaX)+1==1) then !Enclosed between two groins.
      enclosed=1
    endif
    
    groin_vals(sel_gr,1)=0.0
    groin_vals(sel_gr,2)=0.0
    groin_vals(sel_gr,3)=0.0

    groins(sel_gr+1)=''
    i=1
    do while (groins(sel_gr+1+i)/='')
      groins(sel_gr+i)=groins(sel_gr+i+1)
      groin_vals(sel_gr-1+i,1)=groin_vals(sel_gr+i,1)
      groin_vals(sel_gr-1+i,2)=groin_vals(sel_gr+i,2)
      groin_vals(sel_gr-1+i,3)=groin_vals(sel_gr+i,3)
      i=i+1
    enddo
    groins(sel_gr+i)=groins(sel_gr+i+1)
    groin_vals(sel_gr-1+i,1)=groin_vals(sel_gr+i,1)
    groin_vals(sel_gr-1+i,2)=groin_vals(sel_gr+i,2)
    groin_vals(sel_gr-1+i,3)=groin_vals(sel_gr+i,3)
    
    i=select_groin()
    CALL window_update@(groins)

    do j=nint((xg-(wg/2))/deltaX)+1,nint((xg+(wg/2))/deltaX)+1
        ygroin(j)=0.0
    enddo
    if (enclosed==1) then
        ygroin(1)=0.0
        ygroin(n+1)=0.0
        do i=1,nint((wg/2)/deltaX)+1 !From 1 to W/2
            ygroin(i)=0.0
        enddo
        do i=nint((n+1)-((wg/2)/deltaX)),n+1 !From (n+1-W/2) to (n+1)
            ygroin(i)=0.0
        enddo
    endif
    CALL simpleplot_redraw@

    remove_groin=1
ENDFUNCTION remove_groin

FUNCTION lock_unlock
    use definitions
    implicit none
    include <windows.ins>
    integer :: lock_unlock

    if (lock_plot==1) then
        lock_plot=0
        lock_data=1
        lock_AG=1
        lock_RG=1
    else
        lock_plot=1
        lock_data=0
        lock_AG=0
        lock_RG=0
    endif

    CALL window_update@(lock_plot) 
    CALL window_update@(lock_data) 
    CALL window_update@(lock_AG) 
    CALL window_update@(lock_RG) 

    lock_unlock=1
ENDFUNCTION lock_unlock

FUNCTION sure_quit
    implicit none
    integer :: i,sure_quit
    !The parameter 'i' returns 1 if 'Yes' is pressed, 2 if 'No' is pressed and zero if the window is closed.
    i=winio@('%1SI!Are you sure you want to quit?%nl%nl%cn%`6bt[Yes]          %6bt[No]%ca[Warning]')
    sure_quit=abs(i-1)
ENDFUNCTION sure_quit

!-------------------------------------------------------------------------------------------------------
!---                                        Resources                                                ---
!-------------------------------------------------------------------------------------------------------

RESOURCES
	MainIcon ICON c:\TFG\Media\icono2.ico