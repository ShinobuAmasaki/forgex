module forgex_unicode_tools_m
   implicit none
   private

   public :: prop2seg

contains
   pure subroutine prop2seg(property, seglist, ierr)
      use :: forgex_unicode_gc_m
      use :: forgex_error_m
      use :: forgex_segment_m
      implicit none
      character(*), intent(in) :: property
      type(segment_t), intent(inout), allocatable :: seglist(:)
      integer, intent(inout) :: ierr

      logical :: is_single_prop, is_longer_prop
      character(:), allocatable :: prop
      
      prop = property
      is_single_prop = len(prop) == 1
      is_longer_prop = 1 < len(prop)

      if (prop == '' .or. len(prop) < 1) then
         ierr = SYNTAX_ERR_EMPTY_PROPERTY
         return
      end if

      !@insert: PROPERTIY_TO_SEGMENT_LIST

      ierr = SYNTAX_VALID

   end subroutine prop2seg

end module forgex_unicode_tools_m
