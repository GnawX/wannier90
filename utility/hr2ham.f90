PROGRAM hr2ham
!
! convert wannier90_hr.dat file to WanT ham file that
! can be directly used by conductor.x to calculated transmission

  DP = KIND(1.0d0)
  
  INTEGER            :: i, j, ir, nrtot, file_unit
  CHARACTER(len=33)  :: header

  INTEGER,     ALLOCATABLE :: ivr(:,:)
  REAL(dp),    ALLOCATABLE :: wr(:)
  COMPLEX(dp), ALLOCATABLE :: rham(:,:,:)

! read hr file
  file_unit = 111
  OPEN (file_unit, file='wannier90_hr.dat', form='formatted', &
          status='unknown', err=101)

  READ (file_unit, *) header ! Date and time
write (file_unit, *) num_wann
write (file_unit, *) nrpts
write (file_unit, '(15I5)') (ndegen(i), i=1, nrpts)
do irpt = 1, nrpts
do i = 1, num_wann
do j = 1, num_wann
      write (file_unit, '(5I5,2F12.6)') irvec(:, irpt), j, i, &
         ham_r(j, i, irpt)
    end do
  end do
end do

close (file_unit)

END PROGRAM hr2ham
