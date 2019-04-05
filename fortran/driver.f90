! The main program which runs our driver test case potentials
!
! Copyright (C) 2019, Bernat Font Garcia

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
! ./driver.x -h localhost -p 31415

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
    logical :: hflag=.false., pflag=.false.

    ! socket communication buffers
    character(len=12) :: header
    logical :: isinit=.false., hasdata=.false.
    double precision, allocatable :: msgbuffer(:)

    ! data to send and receive
    double precision :: a(4,2)
    integer :: i, d, dims
    integer, allocatable :: sh(:)

    ! intialize defaults
    inet = 1
    host = "localhost"//achar(0)
    port = 31415
    
    ! read command arguments
    if (mod(command_argument_count(), 2) /= 0) then
        call helpmessage
        stop "ended"
    end if
    
    do i = 1, command_argument_count()
        call get_command_argument(i, cmdbuffer)
        if (cmdbuffer == "-h") then ! read the hostname
            hflag = .true.
        elseif (cmdbuffer == "-p") then ! reads the port number
            pflag = .true.
        elseif (hflag .and. mod(i, 2) == 0) then
            host = trim(cmdbuffer)//achar(0)
            hflag = .false.
        elseif (pflag .and. mod(i, 2) == 0) then
            read(cmdbuffer,*) port
            pflag = .false.
        else
            write(*,*) " unrecognized command line argument", i
            call helpmessage
            stop "ended"
        endif
     enddo

    ! open port
    write(*,*) " driver - connecting to host ", trim(host), " on port ", port, " using an internet socket."
    call open_socket(socket, inet, port, host)

    ! main loop
    do while (.true.) ! loops forever (or until the wrapper ends!)
        ! reads from the socket one message header
        call readbuffer(socket, header, msglen)
        write(*,*) " message from server: ", trim(header)

        if (trim(header) == "STATUS") then ! the wrapper is inquiring on what we are doing
            if (.not. isinit) then
                call writebuffer(socket, "NEEDINIT    ", msglen)  ! signals that we need initialization dat
                write(*,*) "    !write!=> ", "needinit    "
            elseif (hasdata) then
                call writebuffer(socket, "HAVEDATA    ", msglen)  ! signals that we are done computing and can data
                write(*,*) "    !write!=> ", "havedata    "
            else
                 call writebuffer(socket, "READY       ", msglen)  ! we are idling and eager to compute something
                 write(*,*) "    !write!=> ", "ready       "
            endif

        elseif (trim(header) == "INIT") then     ! the driver is kindly sending a string for initialization
            write(*,*) " initializing system from wrapper"
            isinit=.true. ! we actually do nothing with this string, thanks anyway. could be used to pass some information (e.g. the input parameters, or the index of the replica, from the driver

        elseif (trim(header) == "SENDDATA") then  ! the driver is sending the positions of the atoms. here is where we do the calculation!
            ! parses the flow of data from the socket
            ! call readbuffer(socket, mtxbuf, 9)  ! cell matrix
            ! write(*,*) "    !read!=> cell: ", mtxbuf
            ! cell_h = reshape(mtxbuf, (/3,3/))
            ! call readbuffer(socket, mtxbuf, 9)  ! inverse of the cell matrix (so we don't have to invert it every time here)
            ! write(*,*) "    !read!=> cell-1: ", mtxbuf
            ! cell_ih = reshape(mtxbuf, (/3,3/))
            ! hasdata = .true. ! signal that we have data ready to be passed back to the wrapper

        elseif (trim(header) == "GETDATA") then  ! Serevr signaling driver to get data
            a = reshape([1,2,3,4,5,6,7,8], shape(a))
!            write(*,*) a
!            write(*,*) size(a), shape(a)
            dims = size(shape(a))
            sh = shape(a)
            
            call writebuffer(socket, "DATAREADY   ", msglen)
            write(*,*) "    !write!=> ", "dataready   "
            call writebuffer(socket, dims)  ! writing the number of dimensions
            write(*,*) "    !write!=> dims:", dims
            do d=1,dims
                call writebuffer(socket, sh(d))  ! writing the number of dimensions
             end do
            write(*,*) "    !write!=> sh:", shape(a)

            msgbuffer = reshape(a, [size(a)])   ! flatten data
            call writebuffer(socket, msgbuffer, size(a)) ! writing data
            write(*,*) "    !write!=> A:", msgbuffer
            hasdata = .false.

        else
            write(*,*) " unexpected header ", header
            stop "ended"
        endif
    enddo

contains

    subroutine helpmessage
     ! help banner
     write(*,*) " syntax: driver.x -h hostname -p port "
    end subroutine helpmessage

end program
