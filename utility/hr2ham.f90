PROGRAM hr2ham
!
! convert wannier90_hr.dat file to WanT ham file that
! can be directly used by conductor.x to calculated transmission
! compile with "gfortran -o write_ham.x write_ham.f90 -I ../extlibs/iotk/include -L ../extlibs/iotk/lib -liotk"

  USE iotk_module
!
  IMPLICIT NONE
  
  INTEGER            :: i, j, ir, nrtot, file_unit, dimwann, stdout, &
                        nsize, itmp, nr(3)
  CHARACTER(len=33)  :: header, fileout
  CHARACTER(600)     :: attr

  INTEGER,     ALLOCATABLE :: ivr(:,:), iwr(:)
  REAL(kind(1.0d0)),    ALLOCATABLE :: wr(:)
  COMPLEX(kind(1.0d0)), ALLOCATABLE :: rham(:,:,:)

  WRITE(6,*) 'input hr file name'
  READ(5,*) filein
  WRITE(6,*) 'output ham file name'
  READ(5,*) fileout
  WRITE(6,*) 'k or r mesh'
  READ(5,*) nr(1), nr(2), nr(3)

! read hr file
  file_unit = 111
  OPEN (file_unit, file=TRIM(filein), form='formatted', &
          status='unknown')

  READ (file_unit, *) header ! Date and time
  READ (file_unit, *) dimwann
  READ (file_unit, *) nrtot

  ALLOCATE (ivr(3,nrtot), wr(nrtot), iwr(nrtot), rham(dimwann,dimwann,nrtot))
  
  READ (file_unit, '(15I5)') (iwr(i), i=1, nrtot)
  DO ir = 1, nrtot
    DO i = 1, dimwann
      DO j = 1, dimwann
        READ (file_unit, '(5I5,2F12.6)') ivr(:, ir), itmp, itmp, &
              rham(j, i, ir)
      END DO
    END DO
  END DO

  CLOSE (file_unit)

  wr = iwr*1.0d0/SUM(iwr)

! write ham file

  stdout = 222
  CALL iotk_open_write( stdout, FILE=TRIM(fileout), binary=.True.)

  CALL iotk_write_begin(stdout,"HAMILTONIAN")

  CALL iotk_write_attr( attr, "dimwann", dimwann, FIRST=.TRUE. )
  CALL iotk_write_attr( attr, "nrtot", nrtot )
  CALL iotk_write_attr( attr, "nr", nr )
  !CALL iotk_write_attr( attr, "have_overlap", have_overlap )
  !CALL iotk_write_attr( attr, "fermi_energy", fermi_energy )

  CALL iotk_write_empty( stdout, "DATA", ATTR=attr)

  nsize=3*nrtot
  CALL iotk_write_attr( attr, "type", "integer", FIRST=.TRUE. )
  CALL iotk_write_attr( attr, "size", nsize )
  CALL iotk_write_attr( attr, "columns", 3 )
  CALL iotk_write_attr( attr, "units", "crystal" )
  CALL iotk_write_dat( stdout, "IVR", ivr, COLUMNS=3, ATTR=attr )

  CALL iotk_write_attr( attr, "type", "real", FIRST=.TRUE. )
  !CALL iotk_write_attr( attr, "size", nkpts )
  CALL iotk_write_dat( stdout, "WR", wr, ATTR=attr )

  CALL iotk_write_begin(stdout,"RHAM")
  DO ir = 1, nrtot
      CALL iotk_write_dat(stdout,"VR"//TRIM(iotk_index(ir)), rham(:,:,ir))
  END DO
  CALL iotk_write_end(stdout,"RHAM")
  CALL iotk_write_end(stdout,"HAMILTONIAN")

  CALL iotk_close_write( stdout )

END PROGRAM hr2ham
