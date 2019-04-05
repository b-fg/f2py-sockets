! The main program which runs our driver test case potentials
!
! Copyright (C) 2013, Joshua More and Michele Ceriotti

! Permission is hereby granted, free of charge, to any person obtaining
! a copy of this software and associated documentation files (the
! "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish,
! distribute, sublicense, and/or sell copies of the Software, and to
! permit persons to whom the Software is furnished to do so, subject to
! the following conditions:
!
! The above copyright notice and this permission notice shall be included
! in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
! IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
! CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
! TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
! SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!
!
! Currently the potentials implemented are the lennard-jones
! potential, the silvera-goldman para-hydrogen potential and
! the ideal gas (i.e. no interaction at all)
!
! driver.x -h localhost -p 31415

program driver
     use f90sockets, only : open_socket, writebuffer, readbuffer
     use, intrinsic :: iso_c_binding

    implicit none

    ! socket variables
    integer, parameter :: msglen=12   ! length of the headers of the driver/wrapper communication protocol
    integer socket, inet, port        ! socket id & address of the server
    character(len=1024) :: host

    ! command line parsing
    character(len=1024) :: cmdbuffer
    integer ccmd, vstyle
    integer verbose
    integer commas(2), par_count      ! stores the index of commas in the parameter string
    double precision vpars(2)         ! array to store the parameters of the potential

    ! socket communication buffers
    character(len=12) :: header
    logical :: isinit=.false., hasdata=.false.
    integer cbuf, rid
    character(len=2048) :: initbuffer      ! it's unlikely a string this large will ever be passed...
    double precision, allocatable :: msgbuffer(:)

    ! parameters of the system (cell, atom positions, ...)
    double precision sigma, eps, rc, rn, ks ! potential parameters
    double precision stiffness ! lennard-jones polymer
    integer n_monomer ! lennard-jones polymer
    integer nat
    double precision pot, dpot, dist
    double precision, allocatable :: atoms(:,:), forces(:,:), datoms(:,:)
    double precision cell_h(3,3), cell_ih(3,3), virial(3,3), mtxbuf(9), dip(3), charges(3), dummy(3,3,3), vecdiff(3)

    double precision volume
    double precision, parameter :: fddx = 1.0d-5

    ! neighbour list arrays
    integer, dimension(:), allocatable :: n_list, index_list
    double precision init_volume, init_rc ! needed to correctly adjust the cut-off radius for variable cell dynamics
    double precision, allocatable :: last_atoms(:,:) ! holds the positions when the neighbour list is created
    double precision displacement ! tracks how far each atom has moved since the last call of nearest_neighbours

    ! dmw
    double precision efield(3)
    integer i, j, d

    double precision, allocatable :: msgb(:)
!    integer :: dims = 2
!    integer :: sh(2) = [4,2]
    double precision :: a(4,2)
     integer :: dims
     integer, allocatable :: sh(:)

    ! parse the command line parameters
    ! intialize defaults
    ccmd = 0
    inet = 1
    host = "localhost"//achar(0)
    port = 31415
    verbose = 0
    par_count = 0
    rc = 0.0d0
    init_rc = 0.0d0
    volume = 0.0d0
    init_volume = 0.0d0

    ! read command arguments
    do i = 1, command_argument_count()
         call get_command_argument(i, cmdbuffer)
         if (cmdbuffer == "-u") then ! flag for unix socket
            inet = 0
            ccmd = 0
         elseif (cmdbuffer == "-h") then ! read the hostname
            ccmd = 1
         elseif (cmdbuffer == "-p") then ! reads the port number
            ccmd = 2
         elseif (cmdbuffer == "-m") then ! reads the style of the potential function
            ccmd = 3
         elseif (cmdbuffer == "-o") then ! reads the parameters
            ccmd = 4
         elseif (cmdbuffer == "-v") then ! flag for verbose standard output
            verbose = 1
         elseif (cmdbuffer == "-vv") then ! flag for verbose standard output
            verbose = 2
         else
            if (ccmd == 0) then
               write(*,*) " unrecognized command line argument", ccmd
               call helpmessage
               stop "ended"
            endif
            if (ccmd == 1) then
               host = trim(cmdbuffer)//achar(0)
            elseif (ccmd == 2) then
               read(cmdbuffer,*) port
            elseif (ccmd == 3) then
               if (trim(cmdbuffer) == "lj") then
                  vstyle = 1
               elseif (trim(cmdbuffer) == "sg") then
                  vstyle = 2
               elseif (trim(cmdbuffer) == "harm") then
                  vstyle = 3
               elseif (trim(cmdbuffer) == "morse") then
                  vstyle = 4
               elseif (trim(cmdbuffer) == "zundel") then
                  vstyle = 5
               elseif (trim(cmdbuffer) == "qtip4pf") then
                  vstyle = 6
               elseif (trim(cmdbuffer) == "linear") then
                  vstyle = 7
               elseif (trim(cmdbuffer) == "pswater") then
                  vstyle = 8
               elseif (trim(cmdbuffer) == "lepsm1") then
                  vstyle = 9
               elseif (trim(cmdbuffer) == "lepsm2") then
                  vstyle = 10
               elseif (trim(cmdbuffer) == "qtip4pf-efield") then
                  vstyle = 11
               elseif (trim(cmdbuffer) == "eckart") then
                  vstyle = 20
               elseif (trim(cmdbuffer) == "ch4hcbe") then
                  vstyle = 21
               elseif (trim(cmdbuffer) == "ljpolymer") then
                  vstyle = 22
               elseif (trim(cmdbuffer) == "gas") then
                  vstyle = 0  ! ideal gas
               else
                  write(*,*) " unrecognized potential type ", trim(cmdbuffer)
                  write(*,*) " use -m [gas|lj|sg|harm|morse|zundel|qtip4pf|lepsm1|lepsm2|qtip4pf-efield|eckart|ch4hcbe|ljpolymer] "
                  stop "ended"
               endif
            elseif (ccmd == 4) then
               par_count = 1
               commas(1) = 0
               do while (index(cmdbuffer(commas(par_count)+1:), ',') > 0)
                  commas(par_count + 1) = index(cmdbuffer(commas(par_count)+1:), ',') + commas(par_count)
                  read(cmdbuffer(commas(par_count)+1:commas(par_count + 1)-1),*) vpars(par_count)
                  par_count = par_count + 1
               enddo
               read(cmdbuffer(commas(par_count)+1:),*) vpars(par_count)
            endif
            ccmd = 0
         endif
      enddo

    ! open port
    if (verbose > 0) then
     write(*,*) " driver - connecting to host ", trim(host)
     if (inet > 0) then
        write(*,*) " on port ", port, " using an internet socket."
     else
        write(*,*) " using an unix socket."
     endif
    endif
    call open_socket(socket, inet, port, host)

    ! main loop
    nat = -1
    do while (.true.) ! loops forever (or until the wrapper ends!)

     ! reads from the socket one message header
     call readbuffer(socket, header, msglen)
     if (verbose > 0) write(*,*) " message from server: ", trim(header)

     if (trim(header) == "status") then
        ! the wrapper is inquiring on what we are doing
        if (.not. isinit) then
           call writebuffer(socket,"needinit    ",msglen)  ! signals that we need initialization data
           if (verbose > 1) write(*,*) "    !write!=> ", "needinit    "
        elseif (hasdata) then
           call writebuffer(socket,"havedata    ",msglen)  ! signals that we are done computing and can return forces
           if (verbose > 1) write(*,*) "    !write!=> ", "havedata    "
        else
           call writebuffer(socket,"ready       ",msglen)  ! we are idling and eager to compute something
           if (verbose > 1) write(*,*) "    !write!=> ", "ready       "
        endif

     elseif (trim(header) == "init") then     ! the driver is kindly providing a string for initialization
        call readbuffer(socket, rid)
        if (verbose > 1) write(*,*) "    !read!=> rid: ", rid
        call readbuffer(socket, cbuf)
        if (verbose > 1) write(*,*) "    !read!=> init_lenght: ", cbuf
        call readbuffer(socket, initbuffer, cbuf)
        if (verbose > 1) write(*,*) "    !read!=> init_string: ", cbuf
        if (verbose > 0) write(*,*) " initializing system from wrapper, using ", trim(initbuffer)
        isinit=.true. ! we actually do nothing with this string, thanks anyway. could be used to pass some information (e.g. the input parameters, or the index of the replica, from the driver

     elseif (trim(header) == "posdata") then  ! the driver is sending the positions of the atoms. here is where we do the calculation!
        ! parses the flow of data from the socket
        call readbuffer(socket, mtxbuf, 9)  ! cell matrix
        if (verbose > 1) write(*,*) "    !read!=> cell: ", mtxbuf
        cell_h = reshape(mtxbuf, (/3,3/))
        call readbuffer(socket, mtxbuf, 9)  ! inverse of the cell matrix (so we don't have to invert it every time here)
        if (verbose > 1) write(*,*) "    !read!=> cell-1: ", mtxbuf
        cell_ih = reshape(mtxbuf, (/3,3/))
        hasdata = .true. ! signal that we have data ready to be passed back to the wrapper

!     elseif (trim(header) == "getforce") then  ! the driver calculation is finished, it's time to send the results back to the wrapper
!        ! data must be re-formatted (and units converted) in the units and shapes used in the wrapper
!         sh = [4,2]
!         allocate(a,reshape([1,2,3,4,5,6,7,8], sh))
!!         a = reshape([1,2,3,4,5,6,7,8], [4,2])
!         write(*,*) a
!         write(*,*) size(a)
!
!         msgbuffer = reshape(a, [8])
!         write(*,*) msgbuffer
!
!        call writebuffer(socket,"forceready  ",msglen)
!        write(*,*) "    !write!=> ", "forceready  "
!        call writebuffer(socket,msgbuffer,size(a)) ! writing the forces
!        write(*,*) "    !write!=> forces:", msgbuffer
!        hasdata = .false.

     elseif (trim(header) == "getdata") then  ! the driver calculation is finished, it's time to send the results back to the wrapper
        ! data must be re-formatted (and units converted) in the units and shapes used in the wrapper

         a = reshape([1,2,3,4,5,6,7,8], shape(a))
         write(*,*) a(:,1)
         write(*,*) size(a), shape(a)
        dims = size(shape(a))
         sh = shape(a)

        call writebuffer(socket,"dataready   ",msglen)
        write(*,*) "    !write!=> ", "dataready   "
        call writebuffer(socket,dims)  ! writing the number of dimensions
        write(*,*) "    !write!=> dims:", dims
        do d=1,dims
            call writebuffer(socket,sh(d))  ! writing the number of dimensions
         end do
        write(*,*) "    !write!=> sh:", shape(a)

        msgbuffer = reshape(a, [size(a)])
        write(*,*) msgbuffer
        call writebuffer(socket,msgbuffer,size(a)) ! writing the forces
        write(*,*) "    !write!=> forces:", msgbuffer
        hasdata = .false.

     else
        write(*,*) " unexpected header ", header
        stop "ended"
     endif
    enddo
    if (nat > 0) deallocate(atoms, forces, msgbuffer)

contains
    subroutine helpmessage
     ! help banner
     write(*,*) " syntax: driver.x [-u] -h hostname -p port -m [gas|lj|sg|harm|morse|zundel|qtip4pf|pswater|lepsm1|lepsm2|qtip4p-efield|eckart|ch4hcbe] "
     write(*,*) "         -o 'comma_separated_parameters' [-v] "
     write(*,*) ""
     write(*,*) " for lj potential use -o sigma,epsilon,cutoff "
     write(*,*) " for sg potential use -o cutoff "
     write(*,*) " for 1d harmonic oscillator use -o k "
     write(*,*) " for 1d morse oscillator use -o r0,d,a"
     write(*,*) " for qtip4pf-efield use -o ex,ey,ez with ei in v/nm"
     write(*,*) " for ljpolymer use -o n_monomer,sigma,epsilon,cutoff "
     write(*,*) " for the ideal gas, qtip4pf, zundel, ch4hcbe or nasa no options needed! "
    end subroutine helpmessage

end program
