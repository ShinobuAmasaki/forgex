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

      select case (property)
         case ('Cn')
            seglist = unicode_gc_Cn
         case ('Lu')
            seglist = unicode_gc_Lu
         case ('Ll')
            seglist = unicode_gc_Ll
         case ('Lt')
            seglist = unicode_gc_Lt
         case ('Lm')
            seglist = unicode_gc_Lm
         case ('Lo')
            seglist = unicode_gc_Lo
         case ('Mn')
            seglist = unicode_gc_Mn
         case ('Me')
            seglist = unicode_gc_Me
         case ('Mc')
            seglist = unicode_gc_Mc
         case ('Nd')
            seglist = unicode_gc_Nd
         case ('Nl')
            seglist = unicode_gc_Nl
         case ('No')
            seglist = unicode_gc_No
         case ('Zs')
            seglist = unicode_gc_Zs
         case ('Zl')
            seglist = unicode_gc_Zl
         case ('Zp')
            seglist = unicode_gc_Zp
         case ('Cc')
            seglist = unicode_gc_Cc
         case ('Cf')
            seglist = unicode_gc_Cf
         case ('Co')
            seglist = unicode_gc_Co
         case ('Cs')
            seglist = unicode_gc_Cs
         case ('Pd')
            seglist = unicode_gc_Pd
         case ('Ps')
            seglist = unicode_gc_Ps
         case ('Pe')
            seglist = unicode_gc_Pe
         case ('Pc')
            seglist = unicode_gc_Pc
         case ('Po')
            seglist = unicode_gc_Po
         case ('Sm')
            seglist = unicode_gc_Sm
         case ('Sc')
            seglist = unicode_gc_Sc
         case ('Sk')
            seglist = unicode_gc_Sk
         case ('So')
            seglist = unicode_gc_So
         case ('Pi')
            seglist = unicode_gc_Pi
         case ('Pf')
            seglist = unicode_gc_Pf
         case default
            if (allocated(seglist)) deallocate(seglist)
            ierr = SYNTAX_ERR_INVALID_PROPERTY
            return
         end select


      ierr = SYNTAX_VALID

   end subroutine prop2seg

end module forgex_unicode_tools_m
