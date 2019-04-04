! The main program which runs our driver test case potentials
!
! Copyright (C) 2013, Joshua More and Michele Ceriotti
!
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
! Currently the potentials implemented are the Lennard-Jones
! potential, the Silvera-Goldman para-hydrogen potential and
! the ideal gas (i.e. no interaction at all)
!
! driver.x -h localhost -p 31415

PROGRAM DRIVER
     USE F90SOCKETS, ONLY : open_socket, writebuffer, readbuffer
     use, intrinsic :: iso_c_binding

    IMPLICIT NONE

    ! SOCKET VARIABLES
    INTEGER, PARAMETER :: MSGLEN=12   ! length of the headers of the driver/wrapper communication protocol
    INTEGER socket, inet, port        ! socket ID & address of the server
    CHARACTER(LEN=1024) :: host

    ! COMMAND LINE PARSING
    CHARACTER(LEN=1024) :: cmdbuffer
    INTEGER ccmd, vstyle
    INTEGER verbose
    INTEGER commas(2), par_count      ! stores the index of commas in the parameter string
    DOUBLE PRECISION vpars(2)         ! array to store the parameters of the potential

    ! SOCKET COMMUNICATION BUFFERS
    CHARACTER(LEN=12) :: header
    LOGICAL :: isinit=.false., hasdata=.false.
    INTEGER cbuf, rid
    CHARACTER(LEN=2048) :: initbuffer      ! it's unlikely a string this large will ever be passed...
    DOUBLE PRECISION, ALLOCATABLE :: msgbuffer(:)

    ! PARAMETERS OF THE SYSTEM (CELL, ATOM POSITIONS, ...)
    DOUBLE PRECISION sigma, eps, rc, rn, ks ! potential parameters
    DOUBLE PRECISION stiffness ! lennard-jones polymer
    INTEGER n_monomer ! lennard-jones polymer
    INTEGER nat
    DOUBLE PRECISION pot, dpot, dist
    DOUBLE PRECISION, ALLOCATABLE :: atoms(:,:), forces(:,:), datoms(:,:)
    DOUBLE PRECISION cell_h(3,3), cell_ih(3,3), virial(3,3), mtxbuf(9), dip(3), charges(3), dummy(3,3,3), vecdiff(3)

    DOUBLE PRECISION volume
    DOUBLE PRECISION, PARAMETER :: fddx = 1.0d-5

    ! NEIGHBOUR LIST ARRAYS
    INTEGER, DIMENSION(:), ALLOCATABLE :: n_list, index_list
    DOUBLE PRECISION init_volume, init_rc ! needed to correctly adjust the cut-off radius for variable cell dynamics
    DOUBLE PRECISION, ALLOCATABLE :: last_atoms(:,:) ! Holds the positions when the neighbour list is created
    DOUBLE PRECISION displacement ! Tracks how far each atom has moved since the last call of nearest_neighbours

    ! DMW
    DOUBLE PRECISION efield(3)
    INTEGER i, j

    DOUBLE PRECISION, ALLOCATABLE :: A(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: msgb(:)



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

    ! READ COMMAND ARGUMENTS
    DO i = 1, COMMAND_ARGUMENT_COUNT()
         CALL GET_COMMAND_ARGUMENT(i, cmdbuffer)
         IF (cmdbuffer == "-u") THEN ! flag for unix socket
            inet = 0
            ccmd = 0
         ELSEIF (cmdbuffer == "-h") THEN ! read the hostname
            ccmd = 1
         ELSEIF (cmdbuffer == "-p") THEN ! reads the port number
            ccmd = 2
         ELSEIF (cmdbuffer == "-m") THEN ! reads the style of the potential function
            ccmd = 3
         ELSEIF (cmdbuffer == "-o") THEN ! reads the parameters
            ccmd = 4
         ELSEIF (cmdbuffer == "-v") THEN ! flag for verbose standard output
            verbose = 1
         ELSEIF (cmdbuffer == "-vv") THEN ! flag for verbose standard output
            verbose = 2
         ELSE
            IF (ccmd == 0) THEN
               WRITE(*,*) " Unrecognized command line argument", ccmd
               CALL helpmessage
               STOP "ENDED"
            ENDIF
            IF (ccmd == 1) THEN
               host = trim(cmdbuffer)//achar(0)
            ELSEIF (ccmd == 2) THEN
               READ(cmdbuffer,*) port
            ELSEIF (ccmd == 3) THEN
               IF (trim(cmdbuffer) == "lj") THEN
                  vstyle = 1
               ELSEIF (trim(cmdbuffer) == "sg") THEN
                  vstyle = 2
               ELSEIF (trim(cmdbuffer) == "harm") THEN
                  vstyle = 3
               ELSEIF (trim(cmdbuffer) == "morse") THEN
                  vstyle = 4
               ELSEIF (trim(cmdbuffer) == "zundel") THEN
                  vstyle = 5
               ELSEIF (trim(cmdbuffer) == "qtip4pf") THEN
                  vstyle = 6
               ELSEIF (trim(cmdbuffer) == "linear") THEN
                  vstyle = 7
               ELSEIF (trim(cmdbuffer) == "pswater") THEN
                  vstyle = 8
               ELSEIF (trim(cmdbuffer) == "lepsm1") THEN
                  vstyle = 9
               ELSEIF (trim(cmdbuffer) == "lepsm2") THEN
                  vstyle = 10
               ELSEIF (trim(cmdbuffer) == "qtip4pf-efield") THEN
                  vstyle = 11
               ELSEIF (trim(cmdbuffer) == "eckart") THEN
                  vstyle = 20
               ELSEIF (trim(cmdbuffer) == "ch4hcbe") THEN
                  vstyle = 21
               ELSEIF (trim(cmdbuffer) == "ljpolymer") THEN
                  vstyle = 22
               ELSEIF (trim(cmdbuffer) == "gas") THEN
                  vstyle = 0  ! ideal gas
               ELSE
                  WRITE(*,*) " Unrecognized potential type ", trim(cmdbuffer)
                  WRITE(*,*) " Use -m [gas|lj|sg|harm|morse|zundel|qtip4pf|lepsm1|lepsm2|qtip4pf-efield|eckart|ch4hcbe|ljpolymer] "
                  STOP "ENDED"
               ENDIF
            ELSEIF (ccmd == 4) THEN
               par_count = 1
               commas(1) = 0
               DO WHILE (index(cmdbuffer(commas(par_count)+1:), ',') > 0)
                  commas(par_count + 1) = index(cmdbuffer(commas(par_count)+1:), ',') + commas(par_count)
                  READ(cmdbuffer(commas(par_count)+1:commas(par_count + 1)-1),*) vpars(par_count)
                  par_count = par_count + 1
               ENDDO
               READ(cmdbuffer(commas(par_count)+1:),*) vpars(par_count)
            ENDIF
            ccmd = 0
         ENDIF
      ENDDO

    ! OPEN PORT
    IF (verbose > 0) THEN
     WRITE(*,*) " DRIVER - Connecting to host ", trim(host)
     IF (inet > 0) THEN
        WRITE(*,*) " on port ", port, " using an internet socket."
     ELSE
        WRITE(*,*) " using an UNIX socket."
     ENDIF
    ENDIF
    CALL open_socket(socket, inet, port, host)

    ! MAIN LOOP
    nat = -1
    DO WHILE (.true.) ! Loops forever (or until the wrapper ends!)

     ! Reads from the socket one message header
     CALL readbuffer(socket, header, MSGLEN)
     IF (verbose > 0) WRITE(*,*) " Message from server: ", trim(header)

     IF (trim(header) == "STATUS") THEN
        ! The wrapper is inquiring on what we are doing
        IF (.not. isinit) THEN
           CALL writebuffer(socket,"NEEDINIT    ",MSGLEN)  ! Signals that we need initialization data
           IF (verbose > 1) WRITE(*,*) "    !write!=> ", "NEEDINIT    "
        ELSEIF (hasdata) THEN
           CALL writebuffer(socket,"HAVEDATA    ",MSGLEN)  ! Signals that we are done computing and can return forces
           IF (verbose > 1) WRITE(*,*) "    !write!=> ", "HAVEDATA    "
        ELSE
           CALL writebuffer(socket,"READY       ",MSGLEN)  ! We are idling and eager to compute something
           IF (verbose > 1) WRITE(*,*) "    !write!=> ", "READY       "
        ENDIF

     ELSEIF (trim(header) == "INIT") THEN     ! The driver is kindly providing a string for initialization
        CALL readbuffer(socket, rid)
        IF (verbose > 1) WRITE(*,*) "    !read!=> RID: ", rid
        CALL readbuffer(socket, cbuf)
        IF (verbose > 1) WRITE(*,*) "    !read!=> init_lenght: ", cbuf
        CALL readbuffer(socket, initbuffer, cbuf)
        IF (verbose > 1) WRITE(*,*) "    !read!=> init_string: ", cbuf
        IF (verbose > 0) WRITE(*,*) " Initializing system from wrapper, using ", trim(initbuffer)
        isinit=.true. ! We actually do nothing with this string, thanks anyway. Could be used to pass some information (e.g. the input parameters, or the index of the replica, from the driver

     ELSEIF (trim(header) == "POSDATA") THEN  ! The driver is sending the positions of the atoms. Here is where we do the calculation!
        ! Parses the flow of data from the socket
        CALL readbuffer(socket, mtxbuf, 9)  ! Cell matrix
        IF (verbose > 1) WRITE(*,*) "    !read!=> cell: ", mtxbuf
        cell_h = RESHAPE(mtxbuf, (/3,3/))
        CALL readbuffer(socket, mtxbuf, 9)  ! Inverse of the cell matrix (so we don't have to invert it every time here)
        IF (verbose > 1) WRITE(*,*) "    !read!=> cell-1: ", mtxbuf
        cell_ih = RESHAPE(mtxbuf, (/3,3/))
        hasdata = .true. ! Signal that we have data ready to be passed back to the wrapper

     ELSEIF (trim(header) == "GETFORCE") THEN  ! The driver calculation is finished, it's time to send the results back to the wrapper
        ! Data must be re-formatted (and units converted) in the units and shapes used in the wrapper
        A = reshape([1,2,3,4,5,6,7,8], [4,2])
         write(*,*) A
         write(*,*) size(A)

         msgbuffer = reshape(A, [8])
         write(*,*) msgbuffer

        CALL writebuffer(socket,"FORCEREADY  ",MSGLEN)
        WRITE(*,*) "    !write!=> ", "FORCEREADY  "
        CALL writebuffer(socket,msgbuffer,size(A)) ! Writing the forces
        WRITE(*,*) "    !write!=> forces:", msgbuffer
        hasdata = .false.


     ELSEIF (trim(header) == "GETARRAY") THEN

         A = reshape([1,2,3,4,5,6,7,8], [4,2])
         write(*,*) A

         msgb = reshape(A, [1])
         write(*,*) A

        CALL writebuffer(socket,"ARRAYREADY  ",MSGLEN)
        IF (verbose > 1) WRITE(*,*) "    !write!=> ", "FORCEREADY  "
        CALL writebuffer(socket,msgbuffer,3*nat) ! Writing the forces
        IF (verbose > 1) WRITE(*,*) "    !write!=> forces:", msgbuffer
        hasdata = .false.


     ELSE
        WRITE(*,*) " Unexpected header ", header
        STOP "ENDED"
     ENDIF
    ENDDO
    IF (nat > 0) DEALLOCATE(atoms, forces, msgbuffer)

CONTAINS
    SUBROUTINE helpmessage
     ! Help banner
     WRITE(*,*) " SYNTAX: driver.x [-u] -h hostname -p port -m [gas|lj|sg|harm|morse|zundel|qtip4pf|pswater|lepsm1|lepsm2|qtip4p-efield|eckart|ch4hcbe] "
     WRITE(*,*) "         -o 'comma_separated_parameters' [-v] "
     WRITE(*,*) ""
     WRITE(*,*) " For LJ potential use -o sigma,epsilon,cutoff "
     WRITE(*,*) " For SG potential use -o cutoff "
     WRITE(*,*) " For 1D harmonic oscillator use -o k "
     WRITE(*,*) " For 1D morse oscillator use -o r0,D,a"
     WRITE(*,*) " For qtip4pf-efield use -o Ex,Ey,Ez with Ei in V/nm"
     WRITE(*,*) " For ljpolymer use -o n_monomer,sigma,epsilon,cutoff "
     WRITE(*,*) " For the ideal gas, qtip4pf, zundel, ch4hcbe or nasa no options needed! "
    END SUBROUTINE helpmessage

END PROGRAM
